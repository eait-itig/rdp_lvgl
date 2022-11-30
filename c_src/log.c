/*
%%
%% RDP UI framework using LVGL
%%
%% Copyright 2022 Alex Wilson <alex@uq.edu.au>, The University of Queensland
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <pthread.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdio.h>
#include <poll.h>
#include <errno.h>
#include <threads.h>
#include <string.h>
#include <strings.h>

#include <stdint.h>
#include <stdatomic.h>

#include <sys/types.h>

#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/wait.h>

#include "log.h"

enum logowner {
	LOG_PRODUCER	= 30303030,
	LOG_CONSUMER	= 40404040,
};

struct logdesc {
	atomic_uint	ld_owner;
	uint8_t		ld_chain;
	char		ld_data[187];
};

struct logdesc_head {
	atomic_uint	ldh_owner;
	uint8_t		ldh_chain;
	uint8_t		ldh_level;
	char		ldh_msg[106];
	char		ldh_role[8];
	char		ldh_func[32];
	char		ldh_file[32];
	uint16_t	ldh_len;
	uint16_t	ldh_line;
	uint32_t	ldh_pid;
};

#define	LOGSHM_NDESC	64

struct logshm {
	pthread_mutex_t		ls_mtx;

	uint			ls_db;
	pthread_cond_t		ls_db_cond;

	uint			ls_pc;
	struct logdesc		ls_ring[LOGSHM_NDESC];
};

struct logerl {
	pthread_mutex_t		 le_mtx;
	pthread_t		 le_consumer;
	ErlNifEnv		*le_env;
	ErlNifPid		 le_owner;
	pid_t			 le_pid;
	const char		*le_role;
};

struct logbuf {
	size_t		 lb_len;
	char		*lb_buf;
};

static struct logshm *logshm = NULL;
static struct logerl *logerl = NULL;
static tss_t log_buf_key;

static void
logbuf_free(void *arg)
{
	struct logbuf *lb = arg;
	free(lb->lb_buf);
	free(lb);
}

static void *log_consumer(void *arg);

void
log_setup(void)
{
	pthread_mutexattr_t mattr;
	pthread_condattr_t cattr;
	uint i;
	int rc;

	rc = tss_create(&log_buf_key, logbuf_free);
	assert(rc == thrd_success);

	logshm = mmap(NULL, sizeof (struct logshm), PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	assert(logshm != MAP_FAILED);

	rc = pthread_mutexattr_init(&mattr);
	assert(rc == 0);
	rc = pthread_mutexattr_setpshared(&mattr, 1);
	assert(rc == 0);
	rc = pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ERRORCHECK);
	assert(rc == 0);

	rc = pthread_mutex_init(&logshm->ls_mtx, &mattr);
	assert(rc == 0);

	rc = pthread_condattr_init(&cattr);
	assert(rc == 0);
	rc = pthread_condattr_setpshared(&cattr, 1);
	assert(rc == 0);

	rc = pthread_cond_init(&logshm->ls_db_cond, &cattr);
	assert(rc == 0);

	for (i = 0; i < LOGSHM_NDESC; ++i)
		atomic_store(&logshm->ls_ring[i].ld_owner, LOG_PRODUCER);

	logerl = calloc(1, sizeof (struct logerl));
	rc = pthread_mutex_init(&logerl->le_mtx, NULL);
	assert(rc == 0);

	logerl->le_env = enif_alloc_env();
	logerl->le_pid = getpid();
	logerl->le_role = "erlang";
	enif_set_pid_undefined(&logerl->le_owner);

	pthread_create(&logerl->le_consumer, NULL, log_consumer, NULL);
	pthread_setname_np(logerl->le_consumer, "lvlog_cons");
}

void
log_take_ownership(ErlNifPid owner)
{
	pthread_mutex_lock(&logerl->le_mtx);
	logerl->le_owner = owner;
	pthread_mutex_unlock(&logerl->le_mtx);
}

void
log_post_fork(const char *role, pid_t newpid)
{
	pthread_mutex_lock(&logerl->le_mtx);
	logerl->le_role = role;
	logerl->le_pid = newpid;
	pthread_mutex_unlock(&logerl->le_mtx);
}

static void
log_ring_doorbell(void)
{
	pthread_mutex_lock(&logshm->ls_mtx);
	++logshm->ls_db;
	pthread_cond_broadcast(&logshm->ls_db_cond);
	pthread_mutex_unlock(&logshm->ls_mtx);
}

static uint
log_read_doorbell(void)
{
	uint db;
	pthread_mutex_lock(&logshm->ls_mtx);
	db = logshm->ls_db;
	pthread_mutex_unlock(&logshm->ls_mtx);
	return (db);
}

static void
log_await_doorbell(uint db)
{
	pthread_mutex_lock(&logshm->ls_mtx);
	while (logshm->ls_db == db)
		pthread_cond_wait(&logshm->ls_db_cond, &logshm->ls_mtx);
	pthread_mutex_unlock(&logshm->ls_mtx);
}

static void
log_reserve_descrs(struct logdesc **pld, uint ndesc)
{
	uint owner;
	uint i = 0;
	pthread_mutex_lock(&logshm->ls_mtx);
	while (i < ndesc) {
		do {
			owner = atomic_load(
			    &logshm->ls_ring[logshm->ls_pc].ld_owner);
		} while (owner != LOG_PRODUCER);
		pld[i++] = &logshm->ls_ring[logshm->ls_pc];
		logshm->ls_pc++;
		if (logshm->ls_pc >= LOGSHM_NDESC)
			logshm->ls_pc = 0;
	}
	pthread_mutex_unlock(&logshm->ls_mtx);
}

static void
log_finish_descr(struct logdesc *ld)
{
	bool swapped;
	uint owner = LOG_PRODUCER;
	swapped = atomic_compare_exchange_strong(&ld->ld_owner, &owner,
	    LOG_CONSUMER);
	assert(swapped);
}

void
_log_write(enum log_level lvl, const char *func, const char *file, uint line,
    const char *fmt, ...)
{
	pid_t pid;
	const char *role;
	va_list ap;
	struct logbuf *lb;
	int rc;
	struct logdesc *ld[16];
	uint ndesc;
	size_t off, rem, take;
	uint i;
	struct logdesc_head *hd;

	pthread_mutex_lock(&logerl->le_mtx);
	role = logerl->le_role;
	pid = logerl->le_pid;
	pthread_mutex_unlock(&logerl->le_mtx);

	lb = tss_get(log_buf_key);
	if (lb == NULL) {
		lb = calloc(1, sizeof (*lb));
		assert(lb != NULL);
		lb->lb_len = 1024;
		lb->lb_buf = malloc(lb->lb_len);
		assert(lb->lb_buf != NULL);
		rc = tss_set(log_buf_key, lb);
		assert(rc == thrd_success);
	}

again:
	va_start(ap, fmt);
	rc = enif_vsnprintf(lb->lb_buf, lb->lb_len, fmt, ap);
	if (rc >= lb->lb_len) {
		lb->lb_len *= 2;
		free(lb->lb_buf);
		lb->lb_buf = malloc(lb->lb_len);
		assert(lb->lb_buf != NULL);
		va_end(ap);
		goto again;
	}
	va_end(ap);

	rem = rc;

	ndesc = 1;
	if (rem > 0) {
		take = sizeof (hd->ldh_msg);
		if (take > rem)
			take = rem;
		++ndesc;
		rem -= take;
	}
	while (rem > 0) {
		take = sizeof (ld[0]->ld_data);
		if (take > rem)
			take = rem;
		++ndesc;
		rem -= take;
	}
	assert(ndesc < 16);

	rem = rc;
	off = 0;

	log_reserve_descrs((struct logdesc **)&ld, ndesc);
	ld[0]->ld_chain = 0;
	hd = (struct logdesc_head *)ld[0];
	hd->ldh_len = rem;
	hd->ldh_level = lvl;
	hd->ldh_line = line;
	hd->ldh_pid = pid;
	strncpy(hd->ldh_func, func, sizeof (hd->ldh_func) - 1);
	hd->ldh_func[sizeof (hd->ldh_func) - 1] = 0;
	strncpy(hd->ldh_file, file, sizeof (hd->ldh_file) - 1);
	hd->ldh_file[sizeof (hd->ldh_file) - 1] = 0;
	strncpy(hd->ldh_role, role, sizeof (hd->ldh_role) - 1);
	hd->ldh_role[sizeof (hd->ldh_role) - 1] = 0;

	take = sizeof (hd->ldh_msg);
	if (take > rem)
		take = rem;
	if (take > 0)
		bcopy(&lb->lb_buf[off], hd->ldh_msg, take);
	rem -= take;
	off += take;

	for (i = 1; i < ndesc; ++i) {
		ld[i]->ld_chain = 0;
		ld[i - 1]->ld_chain = 1;
		take = sizeof (ld[i]->ld_data);
		if (take > rem)
			take = rem;
		if (take > 0)
			bcopy(&lb->lb_buf[off], ld[i]->ld_data, take);
		rem -= take;
		off += take;
	}
	assert(rem == 0);

	for (i = 0; i < ndesc; ++i)
		log_finish_descr(ld[i]);

	log_ring_doorbell();
}

#define	LOG_BUSYWAIT_ITERS	65536

static void
log_finish_cdescr(struct logdesc *ld)
{
	bool swapped;
	uint owner = LOG_CONSUMER;
	swapped = atomic_compare_exchange_strong(&ld->ld_owner, &owner,
	    LOG_PRODUCER);
	assert(swapped);
}

static void *
log_consumer(void *arg)
{
	uint owner, db, iters, i, ndesc, cc;
	struct logdesc *ld[16];
	char *msgbuf;
	size_t msglen;
	ERL_NIF_TERM msg;
	ErlNifEnv *env;
	ErlNifPid pid;
	size_t rem, off, take;
	ErlNifBinary msgbin, funcbin, filebin;
	const char *lvl;
	struct logdesc_head *hd;

	cc = 0;
	ndesc = 0;
	db = log_read_doorbell();

	while (1) {
		iters = 0;
		do {
			owner = atomic_load(&logshm->ls_ring[cc].ld_owner);
			++iters;
		} while (iters < LOG_BUSYWAIT_ITERS && owner != LOG_CONSUMER);
		while (owner != LOG_CONSUMER) {
			log_await_doorbell(db);
			owner = atomic_load(&logshm->ls_ring[cc].ld_owner);
			if (owner != LOG_CONSUMER)
				db = log_read_doorbell();
		}
		ld[ndesc++] = &logshm->ls_ring[cc++];
		if (cc >= LOGSHM_NDESC)
			cc = 0;
		if (ld[ndesc - 1]->ld_chain)
			continue;

		pthread_mutex_lock(&logerl->le_mtx);
		pid = logerl->le_owner;
		env = logerl->le_env;
		logerl->le_env = enif_alloc_env();
		pthread_mutex_unlock(&logerl->le_mtx);

		hd = (struct logdesc_head *)ld[0];
		switch (hd->ldh_level) {
		case LOG_WARN:
			lvl = "warning";
			break;
		case LOG_ERROR:
			lvl = "error";
			break;
		case LOG_DEBUG:
		default:
			lvl = "debug";
			break;
		}

		enif_alloc_binary(strlen(hd->ldh_func), &funcbin);
		bcopy(hd->ldh_func, funcbin.data, funcbin.size);

		enif_alloc_binary(strlen(hd->ldh_file), &filebin);
		bcopy(hd->ldh_file, filebin.data, filebin.size);

		msglen = hd->ldh_len;
		if (enif_is_pid_undefined(&pid)) {
			enif_alloc_binary(msglen + 1, &msgbin);
			msgbin.data[msglen] = 0;
		} else {
			enif_alloc_binary(msglen, &msgbin);
		}
		msgbuf = (char *)msgbin.data;

		rem = msglen;
		off = 0;
		take = sizeof (hd->ldh_msg);
		if (take > rem)
			take = rem;
		if (take > 0)
			bcopy(hd->ldh_msg, &msgbuf[off], take);
		rem -= take;
		off += take;

		for (i = 1; i < ndesc; ++i) {
			take = sizeof (ld[i]->ld_data);
			if (take > rem)
				take = rem;
			if (take > 0)
				bcopy(ld[i]->ld_data, &msgbuf[off], take);
			rem -= take;
			off += take;
		}
		assert(rem == 0);

		if (enif_is_pid_undefined(&pid)) {
			fprintf(stderr, "LVLOG: %s [%s/%d] %s:%d {%s} %s\r\n",
			    lvl, hd->ldh_role, hd->ldh_pid,
			    hd->ldh_file, hd->ldh_line,
			    hd->ldh_func, (char *)msgbin.data);
			enif_release_binary(&funcbin);
			enif_release_binary(&filebin);
			enif_release_binary(&msgbin);
		} else {
			msg = enif_make_tuple8(env,
			    enif_make_atom(env, "lv_nif_log"),
			    enif_make_atom(env, lvl),
			    enif_make_uint(env, hd->ldh_pid),
			    enif_make_atom(env, hd->ldh_role),
			    enif_make_binary(env, &funcbin),
			    enif_make_binary(env, &filebin),
			    enif_make_uint(env, hd->ldh_line),
			    enif_make_binary(env, &msgbin));
			enif_send(NULL, &pid, env, msg);
			enif_free_env(env);
		}

		for (i = 0; i < ndesc; ++i)
			log_finish_cdescr(ld[i]);
		ndesc = 0;
	}

	return (NULL);
}
