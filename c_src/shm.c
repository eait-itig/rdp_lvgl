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

#include <sys/mman.h>

#include "shm.h"

struct shmintf *
alloc_shmintf(void)
{
	struct shmintf *shm = NULL;
	uint i;
	int rc;
	pthread_mutexattr_t mattr;
	pthread_condattr_t cattr;

	shm = calloc(1, sizeof (*shm));
	if (shm == NULL)
		goto err;

	rc = pthread_mutex_init(&shm->si_mtx, NULL);
	if (rc)
		goto err;

	shm->si_lockpg = mmap(NULL, sizeof (struct lockpg),
	    PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_lockpg == MAP_FAILED)
		goto err;

	rc = pthread_mutexattr_init(&mattr);
	assert(rc == 0);
	rc = pthread_mutexattr_setpshared(&mattr, 1);
	assert(rc == 0);

	rc = pthread_mutex_init(&shm->si_lockpg->lp_mtx, &mattr);
	if (rc)
		goto err;

	rc = pthread_condattr_init(&cattr);
	assert(rc == 0);
	rc = pthread_condattr_setpshared(&cattr, 1);
	assert(rc == 0);

	rc = pthread_cond_init(&shm->si_lockpg->lp_cmd_db, &cattr);
	if (rc)
		goto err;
	rc = pthread_cond_init(&shm->si_lockpg->lp_resp_db, &cattr);
	if (rc)
		goto err;
	rc = pthread_cond_init(&shm->si_lockpg->lp_evt_db, &cattr);
	if (rc)
		goto err;
	rc = pthread_cond_init(&shm->si_lockpg->lp_flush_db, &cattr);
	if (rc)
		goto err;

	shm->si_nfbuf = FRAMEBUFS_PER_CHILD;

	shm->si_ncmd = RING_SIZE / sizeof (struct cdesc);
	shm->si_nresp = RING_SIZE / sizeof (struct rdesc);
	shm->si_nev = RING_SIZE / sizeof (struct edesc);
	shm->si_nfl = RING_SIZE / sizeof (struct fdesc);

	shm->si_fbuf = calloc(shm->si_nfbuf, sizeof (struct fbuf));
	if (shm->si_fbuf == NULL)
		goto err;
	for (i = 0; i < shm->si_nfbuf; ++i) {
		struct fbuf *f = &shm->si_fbuf[i];
		f->fb_sz = FRAMEBUFFER_MAX_SIZE;
		f->fb_a = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_a == MAP_FAILED)
			goto err;
		f->fb_b = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_b == MAP_FAILED)
			goto err;
	}

	shm->si_cmdr = mmap(NULL, RING_SIZE, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_cmdr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_ncmd; ++i)
		atomic_store(&shm->si_cmdr[i].cd_owner, OWNER_ERL);

	shm->si_respr = mmap(NULL, RING_SIZE, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_respr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nresp; ++i)
		atomic_store(&shm->si_respr[i].rd_owner, OWNER_LV);

	shm->si_evr = mmap(NULL, RING_SIZE, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_evr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nev; ++i)
		atomic_store(&shm->si_evr[i].ed_owner, OWNER_LV);

	shm->si_flushr = mmap(NULL, RING_SIZE, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_flushr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nfl; ++i)
		atomic_store(&shm->si_flushr[i].fd_owner, OWNER_LV);

	return (shm);

err:
	free_shmintf(shm);
	return (NULL);
}

void
free_shmintf(struct shmintf *shm)
{
	uint i;
	if (shm == NULL)
		return;
	pthread_mutex_destroy(&shm->si_mtx);
	if (shm->si_cmdr != MAP_FAILED)
		munmap(shm->si_cmdr, RING_SIZE);
	if (shm->si_respr != MAP_FAILED)
		munmap(shm->si_respr, RING_SIZE);
	if (shm->si_evr != MAP_FAILED)
		munmap(shm->si_evr, RING_SIZE);
	if (shm->si_flushr != MAP_FAILED)
		munmap(shm->si_flushr, RING_SIZE);
	if (shm->si_lockpg != MAP_FAILED) {
		pthread_mutex_destroy(&shm->si_lockpg->lp_mtx);
		pthread_cond_destroy(&shm->si_lockpg->lp_cmd_db);
		pthread_cond_destroy(&shm->si_lockpg->lp_resp_db);
		munmap(shm->si_lockpg, sizeof (struct lockpg));
	}
	if (shm->si_fbuf != NULL) {
		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			assert(f->fb_state == FBUF_FREE);
			if (f->fb_a != MAP_FAILED && f->fb_sz > 0)
				munmap(f->fb_a, f->fb_sz);
			if (f->fb_b != MAP_FAILED && f->fb_sz > 0)
				munmap(f->fb_b, f->fb_sz);
		}
		free(shm->si_fbuf);
	}
	free(shm);
}

uint
shm_consume_cmd(struct shmintf *shm, struct cdesc **cmd, uint maxchain)
{
	uint iters;
	uint owner;
	uint n = 0;
	assert(shm->si_role == ROLE_LV);
	do {
		iters = 0;
		/* Attempt a busy-wait check first. */
		do {
			owner = atomic_load(&shm->si_cmdr[shm->si_cc].cd_owner);
			++iters;
		} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_LV);
		/* If we didn't find any descriptors ready, sleep. */
		while (owner != OWNER_LV) {
			pthread_mutex_lock(&shm->si_mtx);
			pthread_cond_wait(&shm->si_lockpg->lp_cmd_db,
			    &shm->si_mtx);
			pthread_mutex_unlock(&shm->si_mtx);
			owner = atomic_load(&shm->si_cmdr[shm->si_cc].cd_owner);
		}
		cmd[n] = &shm->si_cmdr[shm->si_cc];
		shm->si_cc++;
		if (shm->si_cc >= shm->si_ncmd)
			shm->si_cc = 0;
		if (!cmd[n++]->cd_chain)
			break;
	} while (n < maxchain);
	return (n);
}

void
shm_produce_rsp(struct shmintf *shm, const struct rdesc *rsp, uint n)
{
	uint owner;
	uint i = 0;
	assert(shm->si_role == ROLE_LV);
	pthread_mutex_lock(&shm->si_mtx);
	while (i < n) {
		do {
			owner = atomic_load(
			    &shm->si_respr[shm->si_rc].rd_owner);
		} while (owner != OWNER_LV);
		bcopy(&rsp[i].rd_chain, &shm->si_respr[shm->si_rc].rd_chain,
		    sizeof(struct rdesc) - offsetof(struct rdesc, rd_chain));
		atomic_store(&shm->si_respr[shm->si_rc].rd_owner, OWNER_ERL);
		shm->si_rc++;
		if (shm->si_rc >= shm->si_nresp)
			shm->si_rc = 0;
		i++;
	}
	pthread_mutex_unlock(&shm->si_mtx);
	pthread_cond_signal(&shm->si_lockpg->lp_resp_db);
}

void
shm_produce_cmd(struct shmintf *shm, const struct cdesc *rsp, uint n)
{
	uint owner;
	uint i = 0;
	assert(shm->si_role == ROLE_ERL);
	pthread_mutex_lock(&shm->si_mtx);
	while (i < n) {
		do {
			owner = atomic_load(
			    &shm->si_cmdr[shm->si_cc].cd_owner);
		} while (owner != OWNER_ERL);
		bcopy(&rsp[i].cd_op, &shm->si_cmdr[shm->si_cc].cd_op,
		    sizeof(struct cdesc) - offsetof(struct cdesc, cd_op));
		atomic_store(&shm->si_cmdr[shm->si_cc].cd_owner, OWNER_LV);
		shm->si_cc++;
		if (shm->si_cc >= shm->si_ncmd)
			shm->si_cc = 0;
		i++;
	}
	pthread_mutex_unlock(&shm->si_mtx);
	pthread_cond_signal(&shm->si_lockpg->lp_cmd_db);
}

void
shm_produce_evt(struct shmintf *shm, const struct edesc *esp)
{
	uint owner;
	assert(shm->si_role == ROLE_LV);
	pthread_mutex_lock(&shm->si_mtx);
	do {
		owner = atomic_load(
		    &shm->si_evr[shm->si_ec].ed_owner);
	} while (owner != OWNER_LV);
	bcopy(&esp->ed_code, &shm->si_evr[shm->si_ec].ed_code,
	    sizeof(struct edesc) - offsetof(struct edesc, ed_code));
	atomic_store(&shm->si_evr[shm->si_ec].ed_owner, OWNER_ERL);
	shm->si_ec++;
	if (shm->si_ec >= shm->si_nev)
		shm->si_ec = 0;
	pthread_mutex_unlock(&shm->si_mtx);
	pthread_cond_signal(&shm->si_lockpg->lp_evt_db);
}

void
shm_produce_flush(struct shmintf *shm, const struct fdesc *fsp)
{
	uint owner;
	assert(shm->si_role == ROLE_LV);
	pthread_mutex_lock(&shm->si_mtx);
	do {
		owner = atomic_load(
		    &shm->si_flushr[shm->si_fc].fd_owner);
	} while (owner != OWNER_LV);
	bcopy(&fsp->fd_fbidx_flag, &shm->si_flushr[shm->si_fc].fd_fbidx_flag,
	    sizeof(struct fdesc) - offsetof(struct fdesc, fd_fbidx_flag));
	atomic_store(&shm->si_flushr[shm->si_fc].fd_owner, OWNER_ERL);
	shm->si_fc++;
	if (shm->si_fc >= shm->si_nfl)
		shm->si_fc = 0;
	fprintf(stderr, "produced flush before si_fc = %u\n", shm->si_fc);
	pthread_mutex_unlock(&shm->si_mtx);
	pthread_cond_signal(&shm->si_lockpg->lp_flush_db);
}

uint
shm_consume_rsp(struct shmintf *shm, struct rdesc **rsp, uint maxchain)
{
	uint iters;
	uint owner;
	uint n = 0;
	assert(shm->si_role == ROLE_ERL);
	do {
		iters = 0;
		/* Attempt a busy-wait check first. */
		do {
			owner = atomic_load(
			    &shm->si_respr[shm->si_rc].rd_owner);
			++iters;
		} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_ERL);
		/* If we didn't find any descriptors ready, sleep. */
		while (owner != OWNER_ERL) {
			pthread_mutex_lock(&shm->si_mtx);
			pthread_cond_wait(&shm->si_lockpg->lp_resp_db,
			    &shm->si_mtx);
			pthread_mutex_unlock(&shm->si_mtx);
			owner = atomic_load(
			    &shm->si_respr[shm->si_rc].rd_owner);
		}
		rsp[n] = &shm->si_respr[shm->si_rc];
		shm->si_rc++;
		if (shm->si_rc >= shm->si_nresp)
			shm->si_rc = 0;
		if (!rsp[n++]->rd_chain)
			break;
	} while (n < maxchain);
	return (n);
}

void
shm_consume_evt(struct shmintf *shm, struct edesc **esp)
{
	uint iters = 0;
	uint owner;
	assert(shm->si_role == ROLE_ERL);
	/* Attempt a busy-wait check first. */
	do {
		owner = atomic_load(&shm->si_evr[shm->si_ec].ed_owner);
		++iters;
	} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_ERL);
	/* If we didn't find any descriptors ready, sleep. */
	while (owner != OWNER_ERL) {
		pthread_mutex_lock(&shm->si_mtx);
		pthread_cond_wait(&shm->si_lockpg->lp_evt_db, &shm->si_mtx);
		pthread_mutex_unlock(&shm->si_mtx);
		owner = atomic_load(&shm->si_evr[shm->si_ec].ed_owner);
	}
	*esp = &shm->si_evr[shm->si_ec];
	shm->si_ec++;
	if (shm->si_ec >= shm->si_nev)
		shm->si_ec = 0;
}

void
shm_consume_flush(struct shmintf *shm, struct fdesc **fsp)
{
	uint iters = 0;
	uint owner;
	assert(shm->si_role == ROLE_ERL);
	/* Attempt a busy-wait check first. */
	do {
		owner = atomic_load(&shm->si_flushr[shm->si_fc].fd_owner);
		++iters;
	} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_ERL);
	/* If we didn't find any descriptors ready, sleep. */
	while (owner != OWNER_ERL) {
		pthread_mutex_lock(&shm->si_mtx);
		pthread_cond_wait(&shm->si_lockpg->lp_flush_db, &shm->si_mtx);
		pthread_mutex_unlock(&shm->si_mtx);
		owner = atomic_load(&shm->si_flushr[shm->si_fc].fd_owner);
	}
	*fsp = &shm->si_flushr[shm->si_fc];
	shm->si_fc++;
	if (shm->si_fc >= shm->si_nfl)
		shm->si_fc = 0;
}

void
shm_finish_cmd(struct cdesc *cd)
{
	bool swapped;
	uint owner = OWNER_LV;
	swapped = atomic_compare_exchange_strong(&cd->cd_owner, &owner,
	    OWNER_ERL);
	assert(swapped);
}

void
shm_finish_rsp(struct rdesc *rd)
{
	bool swapped;
	uint owner = OWNER_ERL;
	swapped = atomic_compare_exchange_strong(&rd->rd_owner, &owner,
	    OWNER_LV);
	assert(swapped);
}

void
shm_finish_evt(struct edesc *ed)
{
	bool swapped;
	uint owner = OWNER_ERL;
	swapped = atomic_compare_exchange_strong(&ed->ed_owner, &owner,
	    OWNER_LV);
	assert(swapped);
}

void
shm_finish_flush(struct fdesc *fd)
{
	bool swapped;
	uint owner = OWNER_ERL;
	swapped = atomic_compare_exchange_strong(&fd->fd_owner, &owner,
	    OWNER_LV);
	assert(swapped);
}

pid_t
shm_fork(struct shmintf *shm)
{
	pid_t kid;

	pthread_mutex_lock(&shm->si_mtx);
	assert(shm->si_role == ROLE_NONE);

	kid = fork();
	switch (kid) {
	case -1:
		break;
	case 0:
		shm->si_role = ROLE_LV;
		break;
	default:
		shm->si_role = ROLE_ERL;
		break;
	}
	pthread_mutex_unlock(&shm->si_mtx);
	return (kid);
}
