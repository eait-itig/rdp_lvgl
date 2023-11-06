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

#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/wait.h>

#include "shm.h"
#include "log.h"

struct shm_settings {
	pthread_rwlock_t	ss_lock;
	size_t			ss_nfbuf;
	size_t			ss_ringsz;
	size_t			ss_fbsize;
	size_t			ss_respringsz;
};
static struct shm_settings settings = {
	.ss_lock = PTHREAD_RWLOCK_INITIALIZER,
	.ss_nfbuf = FRAMEBUFS_PER_CHILD,
	.ss_ringsz = RING_SIZE,
	.ss_fbsize = FRAMEBUFFER_MAX_SIZE,
	.ss_respringsz = RING_SIZE + 4096
};

void
set_fbufs_per_child(size_t nfbuf)
{
	pthread_rwlock_wrlock(&settings.ss_lock);
	settings.ss_nfbuf = nfbuf;
	pthread_rwlock_unlock(&settings.ss_lock);
}

void
set_ring_size(size_t ringsz)
{
	pthread_rwlock_wrlock(&settings.ss_lock);
	settings.ss_ringsz = ringsz;
	/*
	 * Add an extra page to the resp rings so they always have free slots.
	 * This avoids a deadlock between the cmd and rsp rings both looking
	 * for a free slot while blocking progress on the other.
	 */
	settings.ss_respringsz = ringsz + 4096;
	pthread_rwlock_unlock(&settings.ss_lock);
}

void
set_fb_max_res(size_t w, size_t h)
{
	size_t fbsize = w * h * 2;
	pthread_rwlock_wrlock(&settings.ss_lock);
	settings.ss_fbsize = fbsize;
	pthread_rwlock_unlock(&settings.ss_lock);
}

struct shmintf *
alloc_shmintf(void)
{
	struct shmintf *shm = NULL;
	uint i;
	int rc;
	pthread_mutexattr_t mattr;
	pthread_condattr_t cattr;
	size_t ringsz, fbsize;
	size_t respringsz;

	shm = calloc(1, sizeof (*shm));
	if (shm == NULL)
		goto err;

	atomic_store(&shm->si_dead, 0);

	pthread_rwlock_rdlock(&settings.ss_lock);
	shm->si_ringsz = (ringsz = settings.ss_ringsz);
	shm->si_respringsz = (respringsz = settings.ss_respringsz);
	fbsize = settings.ss_fbsize;
	shm->si_nfbuf = settings.ss_nfbuf;
	pthread_rwlock_unlock(&settings.ss_lock);

	shm->si_ncmd = ringsz / sizeof (struct cdesc);
	shm->si_nresp = respringsz / sizeof (struct rdesc);
	shm->si_nev = ringsz / sizeof (struct edesc);
	shm->si_nfl = ringsz / sizeof (struct fdesc);
	shm->si_nph = ringsz / sizeof (struct pdesc);

	rc = pthread_mutex_init(&shm->si_cc_mtx, NULL);
	if (rc)
		goto err;
	rc = pthread_mutex_init(&shm->si_rc_mtx, NULL);
	if (rc)
		goto err;
	rc = pthread_mutex_init(&shm->si_ec_mtx, NULL);
	if (rc)
		goto err;
	rc = pthread_mutex_init(&shm->si_fc_mtx, NULL);
	if (rc)
		goto err;
	rc = pthread_mutex_init(&shm->si_pc_mtx, NULL);
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
	rc = pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ERRORCHECK);
	assert(rc == 0);

	rc = pthread_mutex_init(&shm->si_lockpg->lp_erl_mtx, &mattr);
	if (rc)
		goto err;
	rc = pthread_mutex_init(&shm->si_lockpg->lp_lv_mtx, &mattr);
	if (rc)
		goto err;

	rc = pthread_condattr_init(&cattr);
	assert(rc == 0);
	rc = pthread_condattr_setpshared(&cattr, 1);
	assert(rc == 0);

	rc = pthread_cond_init(&shm->si_lockpg->lp_erl_db_cond, &cattr);
	if (rc)
		goto err;
	rc = pthread_cond_init(&shm->si_lockpg->lp_lv_db_cond, &cattr);
	if (rc)
		goto err;

	shm->si_fbuf = calloc(shm->si_nfbuf, sizeof (struct fbuf));
	if (shm->si_fbuf == NULL)
		goto err;
	for (i = 0; i < shm->si_nfbuf; ++i) {
		struct fbuf *f = &shm->si_fbuf[i];
		f->fb_idx = i;
		f->fb_sz = fbsize;
		f->fb_a = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_a == MAP_FAILED)
			goto err;
		f->fb_b = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_b == MAP_FAILED)
			goto err;
#if defined(MADV_DONTDUMP)
		madvise(f->fb_a, f->fb_sz, MADV_DONTDUMP);
		madvise(f->fb_b, f->fb_sz, MADV_DONTDUMP);
#endif
	}

	shm->si_cmdr = mmap(NULL, ringsz, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_cmdr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_ncmd; ++i)
		atomic_store(&shm->si_cmdr[i].cd_owner, OWNER_ERL);

	shm->si_respr = mmap(NULL, respringsz, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_respr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nresp; ++i)
		atomic_store(&shm->si_respr[i].rd_owner, OWNER_LV);

	shm->si_evr = mmap(NULL, ringsz, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_evr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nev; ++i)
		atomic_store(&shm->si_evr[i].ed_owner, OWNER_LV);

	shm->si_flr = mmap(NULL, ringsz, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_flr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nfl; ++i)
		atomic_store(&shm->si_flr[i].fd_owner, OWNER_LV);

	shm->si_phr = mmap(NULL, ringsz, PROT_READ | PROT_WRITE,
	    MAP_SHARED | MAP_ANON, -1, 0);
	if (shm->si_phr == MAP_FAILED)
		goto err;
	for (i = 0; i < shm->si_nph; ++i)
		atomic_store(&shm->si_phr[i].pd_owner, OWNER_ERL);

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
	pthread_mutex_destroy(&shm->si_cc_mtx);
	pthread_mutex_destroy(&shm->si_rc_mtx);
	pthread_mutex_destroy(&shm->si_ec_mtx);
	pthread_mutex_destroy(&shm->si_fc_mtx);
	pthread_mutex_destroy(&shm->si_pc_mtx);
	if (shm->si_cmdr != MAP_FAILED)
		munmap(shm->si_cmdr, shm->si_ringsz);
	if (shm->si_respr != MAP_FAILED)
		munmap(shm->si_respr, shm->si_respringsz);
	if (shm->si_evr != MAP_FAILED)
		munmap(shm->si_evr, shm->si_ringsz);
	if (shm->si_flr != MAP_FAILED)
		munmap(shm->si_flr, shm->si_ringsz);
	if (shm->si_phr != MAP_FAILED)
		munmap(shm->si_phr, shm->si_ringsz);
	if (shm->si_lockpg != MAP_FAILED) {
		pthread_mutex_destroy(&shm->si_lockpg->lp_erl_mtx);
		pthread_mutex_destroy(&shm->si_lockpg->lp_lv_mtx);
		pthread_cond_destroy(&shm->si_lockpg->lp_erl_db_cond);
		pthread_cond_destroy(&shm->si_lockpg->lp_lv_db_cond);
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
	uint owner, db;
	uint n = 0;
	assert(shm->si_role == ROLE_LV);
	if (atomic_load(&shm->si_dead))
		return (0);
	db = shm_read_doorbell(shm);
	do {
		iters = 0;
		/* Attempt a busy-wait check first. */
		do {
			owner = atomic_load(&shm->si_cmdr[shm->si_cc].cd_owner);
			++iters;
		} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_LV);
		/* If we didn't find any descriptors ready, sleep. */
		while (owner != OWNER_LV) {
			shm_await_doorbell_v(shm, db);
			owner = atomic_load(&shm->si_cmdr[shm->si_cc].cd_owner);
			db = shm_read_doorbell(shm);
			if (atomic_load(&shm->si_dead))
				return (0);
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
	uint iters = 0;
	assert(shm->si_role == ROLE_LV);
	if (atomic_load(&shm->si_dead))
		return;
	pthread_mutex_lock(&shm->si_rc_mtx);
	while (i < n) {
		do {
			owner = atomic_load(
			    &shm->si_respr[shm->si_rc].rd_owner);
			if (owner != OWNER_LV) {
				if (++iters & 0x10000)
					shm_ring_doorbell(shm);
				if (atomic_load(&shm->si_dead))
					return;
			}
		} while (owner != OWNER_LV);
		bcopy(&rsp[i].rd_chain, &shm->si_respr[shm->si_rc].rd_chain,
		    sizeof(struct rdesc) - offsetof(struct rdesc, rd_chain));
		atomic_store(&shm->si_respr[shm->si_rc].rd_owner, OWNER_ERL);
		shm->si_rc++;
		if (shm->si_rc >= shm->si_nresp)
			shm->si_rc = 0;
		i++;
	}
	pthread_mutex_unlock(&shm->si_rc_mtx);
}

void
shm_produce_cmd(struct shmintf *shm, const struct cdesc *rsp, uint n)
{
	uint owner;
	uint iters = 0;
	uint i = 0;
	assert(shm->si_role == ROLE_ERL);
	if (atomic_load(&shm->si_dead))
		return;
	pthread_mutex_lock(&shm->si_cc_mtx);
	while (i < n) {
		do {
			owner = atomic_load(
			    &shm->si_cmdr[shm->si_cc].cd_owner);
			if (owner != OWNER_ERL) {
				if (++iters & 0x10000)
					shm_ring_doorbell(shm);
				if (atomic_load(&shm->si_dead))
					return;
			}
		} while (owner != OWNER_ERL);
		bcopy(&rsp[i].cd_op, &shm->si_cmdr[shm->si_cc].cd_op,
		    sizeof(struct cdesc) - offsetof(struct cdesc, cd_op));
		atomic_store(&shm->si_cmdr[shm->si_cc].cd_owner, OWNER_LV);
		shm->si_cc++;
		if (shm->si_cc >= shm->si_ncmd)
			shm->si_cc = 0;
		i++;
	}
	pthread_mutex_unlock(&shm->si_cc_mtx);
}

void
shm_produce_evt(struct shmintf *shm, const struct edesc *esp)
{
	uint owner;
	uint iters = 0;
	assert(shm->si_role == ROLE_LV);
	if (atomic_load(&shm->si_dead))
		return;
	pthread_mutex_lock(&shm->si_ec_mtx);
	do {
		owner = atomic_load(
		    &shm->si_evr[shm->si_ec].ed_owner);
		if (owner != OWNER_LV) {
			if (++iters & 0x10000)
				shm_ring_doorbell(shm);
			if (atomic_load(&shm->si_dead))
				return;
		}
	} while (owner != OWNER_LV);
	bcopy(&esp->ed_code, &shm->si_evr[shm->si_ec].ed_code,
	    sizeof(struct edesc) - offsetof(struct edesc, ed_code));
	atomic_store(&shm->si_evr[shm->si_ec].ed_owner, OWNER_ERL);
	shm->si_ec++;
	if (shm->si_ec >= shm->si_nev)
		shm->si_ec = 0;
	pthread_mutex_unlock(&shm->si_ec_mtx);
}

void
shm_produce_flush(struct shmintf *shm, const struct fdesc *fsp)
{
	uint owner;
	uint iters = 0;
	assert(shm->si_role == ROLE_LV);
	if (atomic_load(&shm->si_dead))
		return;
	pthread_mutex_lock(&shm->si_fc_mtx);
	do {
		owner = atomic_load(
		    &shm->si_flr[shm->si_fc].fd_owner);
		if (owner != OWNER_LV) {
			if (++iters & 0x10000)
				shm_ring_doorbell(shm);
			if (atomic_load(&shm->si_dead))
				return;
		}
	} while (owner != OWNER_LV);
	bcopy(&fsp->fd_fbidx_flag, &shm->si_flr[shm->si_fc].fd_fbidx_flag,
	    sizeof(struct fdesc) - offsetof(struct fdesc, fd_fbidx_flag));
	atomic_store(&shm->si_flr[shm->si_fc].fd_owner, OWNER_ERL);
	shm->si_fc++;
	if (shm->si_fc >= shm->si_nfl)
		shm->si_fc = 0;
	pthread_mutex_unlock(&shm->si_fc_mtx);
}

void
shm_produce_phlush(struct shmintf *shm, const struct pdesc *psp)
{
	uint owner;
	uint iters = 0;
	assert(shm->si_role == ROLE_ERL);
	if (atomic_load(&shm->si_dead))
		return;
	pthread_mutex_lock(&shm->si_pc_mtx);
	do {
		owner = atomic_load(
		    &shm->si_phr[shm->si_pc].pd_owner);
		if (owner != OWNER_ERL) {
			if (++iters & 0x10000)
				shm_ring_doorbell(shm);
			if (atomic_load(&shm->si_dead))
				return;
		}
	} while (owner != OWNER_ERL);
	bcopy(&psp->pd_flags, &shm->si_phr[shm->si_pc].pd_flags,
	    sizeof(struct pdesc) - offsetof(struct pdesc, pd_flags));
	atomic_store(&shm->si_phr[shm->si_pc].pd_owner, OWNER_LV);
	shm->si_pc++;
	if (shm->si_pc >= shm->si_nph)
		shm->si_pc = 0;
	pthread_mutex_unlock(&shm->si_pc_mtx);
}

uint
shm_consume_rsp(struct shmintf *shm, struct rdesc **rsp, uint maxchain)
{
	uint iters;
	uint owner, db;
	uint n = 0;
	assert(shm->si_role == ROLE_ERL);
	if (atomic_load(&shm->si_dead))
		return (0);
	db = shm_read_doorbell(shm);
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
			shm_await_doorbell_v(shm, db);
			owner = atomic_load(
			    &shm->si_respr[shm->si_rc].rd_owner);
			db = shm_read_doorbell(shm);
			if (atomic_load(&shm->si_dead))
				return (0);
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
	uint owner, db;
	assert(shm->si_role == ROLE_ERL);
	if (atomic_load(&shm->si_dead))
		return;
	db = shm_read_doorbell(shm);
	/* Attempt a busy-wait check first. */
	do {
		owner = atomic_load(&shm->si_evr[shm->si_ec].ed_owner);
		++iters;
	} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_ERL);
	/* If we didn't find any descriptors ready, sleep. */
	while (owner != OWNER_ERL) {
		shm_await_doorbell_v(shm, db);
		owner = atomic_load(&shm->si_evr[shm->si_ec].ed_owner);
		db = shm_read_doorbell(shm);
		if (atomic_load(&shm->si_dead))
			return;
	}
	*esp = &shm->si_evr[shm->si_ec];
	shm->si_ec++;
	if (shm->si_ec >= shm->si_nev)
		shm->si_ec = 0;
}

uint
shm_read_doorbell(struct shmintf *shm)
{
	struct lockpg *lp = shm->si_lockpg;
	uint val;
	switch (shm->si_role) {
	case ROLE_ERL:
		pthread_mutex_lock(&lp->lp_erl_mtx);
		val = lp->lp_erl_db;
		pthread_mutex_unlock(&lp->lp_erl_mtx);
		break;
	case ROLE_LV:
		pthread_mutex_lock(&lp->lp_lv_mtx);
		val = lp->lp_lv_db;
		pthread_mutex_unlock(&lp->lp_lv_mtx);
		break;
	default:
		assert(0);
		return (0);
	}
	return (val);
}

void
shm_await_doorbell_v(struct shmintf *shm, uint orig)
{
	pthread_cond_t *cond;
	pthread_mutex_t *mtx;
	struct lockpg *lp = shm->si_lockpg;
	uint *db;
	switch (shm->si_role) {
	case ROLE_ERL:
		mtx = &lp->lp_erl_mtx;
		cond = &lp->lp_erl_db_cond;
		db = &lp->lp_erl_db;
		break;
	case ROLE_LV:
		mtx = &lp->lp_lv_mtx;
		cond = &lp->lp_lv_db_cond;
		db = &lp->lp_lv_db;
		break;
	default:
		assert(0);
		return;
	}
	pthread_mutex_lock(mtx);
	while (*db == orig)
		pthread_cond_wait(cond, mtx);
	pthread_mutex_unlock(mtx);
}

void
shm_await_doorbell(struct shmintf *shm)
{
	pthread_cond_t *cond;
	pthread_mutex_t *mtx;
	struct lockpg *lp = shm->si_lockpg;
	uint *db, orig;
	switch (shm->si_role) {
	case ROLE_ERL:
		mtx = &lp->lp_erl_mtx;
		cond = &lp->lp_erl_db_cond;
		db = &lp->lp_erl_db;
		break;
	case ROLE_LV:
		mtx = &lp->lp_lv_mtx;
		cond = &lp->lp_lv_db_cond;
		db = &lp->lp_lv_db;
		break;
	default:
		assert(0);
		return;
	}
	pthread_mutex_lock(mtx);
	orig = *db;
	while (*db == orig && !lp->lp_dead)
		pthread_cond_wait(cond, mtx);
	pthread_mutex_unlock(mtx);
}

void
shm_ring_doorbell(struct shmintf *shm)
{
	pthread_cond_t *cond;
	pthread_mutex_t *mtx;
	uint *db;
	struct lockpg *lp = shm->si_lockpg;
	switch (shm->si_role) {
	case ROLE_ERL:
		mtx = &lp->lp_lv_mtx;
		cond = &lp->lp_lv_db_cond;
		db = &lp->lp_lv_db;
		break;
	case ROLE_LV:
		mtx = &lp->lp_erl_mtx;
		cond = &lp->lp_erl_db_cond;
		db = &lp->lp_erl_db;
		break;
	default:
		assert(0);
		return;
	}
	pthread_mutex_lock(mtx);
	(*db)++;
	pthread_cond_broadcast(cond);
	pthread_mutex_unlock(mtx);
}

void
shm_consume_flush(struct shmintf *shm, struct fdesc **fsp)
{
	uint iters = 0;
	uint owner, db;
	assert(shm->si_role == ROLE_ERL);
	if (atomic_load(&shm->si_dead))
		return;
	db = shm_read_doorbell(shm);
	/* Attempt a busy-wait check first. */
	do {
		owner = atomic_load(&shm->si_flr[shm->si_fc].fd_owner);
		++iters;
	} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_ERL);
	/* If we didn't find any descriptors ready, sleep. */
	while (owner != OWNER_ERL) {
		shm_await_doorbell_v(shm, db);
		owner = atomic_load(&shm->si_flr[shm->si_fc].fd_owner);
		db = shm_read_doorbell(shm);
		if (atomic_load(&shm->si_dead))
			return;
	}
	*fsp = &shm->si_flr[shm->si_fc];
	shm->si_fc++;
	if (shm->si_fc >= shm->si_nfl)
		shm->si_fc = 0;
}

void
shm_consume_phlush(struct shmintf *shm, struct pdesc **psp)
{
	uint iters = 0;
	uint owner, db;
	assert(shm->si_role == ROLE_LV);
	if (atomic_load(&shm->si_dead))
		return;
	db = shm_read_doorbell(shm);
	/* Attempt a busy-wait check first. */
	do {
		owner = atomic_load(&shm->si_phr[shm->si_pc].pd_owner);
		++iters;
	} while (iters < RING_BUSYWAIT_ITERS && owner != OWNER_LV);
	/* If we didn't find any descriptors ready, sleep. */
	while (owner != OWNER_LV) {
		shm_await_doorbell_v(shm, db);
		owner = atomic_load(&shm->si_phr[shm->si_pc].pd_owner);
		db = shm_read_doorbell(shm);
		if (atomic_load(&shm->si_dead))
			return;
	}
	*psp = &shm->si_phr[shm->si_pc];
	shm->si_pc++;
	if (shm->si_pc >= shm->si_nph)
		shm->si_pc = 0;
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

void
shm_finish_phlush(struct pdesc *pd)
{
	bool swapped;
	uint owner = OWNER_LV;
	swapped = atomic_compare_exchange_strong(&pd->pd_owner, &owner,
	    OWNER_ERL);
	assert(swapped);
}

static void *
shm_pipewatch(void *arg)
{
	struct shmintf *shm = arg;
	struct pollfd pfd;
	int ret;

	pfd.fd = shm->si_down_pipe[0];
	pfd.events = POLLIN | POLLHUP;

	while (1) {
		ret = poll(&pfd, 1, -1);
		if (ret != 1)
			continue;
		if (read(pfd.fd, &ret, sizeof (ret)) == 0) {
			log_debug("parent died; exiting");
			exit(0);
		}
	}
}

static void *
shm_parent_pipewatch(void *arg)
{
	struct shmintf *shm = arg;
	struct lockpg *lp = shm->si_lockpg;
	struct pollfd pfd;
	int ret;

	pfd.fd = shm->si_up_pipe[0];
	pfd.events = POLLIN | POLLHUP;

	while (1) {
		ret = poll(&pfd, 1, -1);
		if (ret != 1)
			continue;
		if (read(pfd.fd, &ret, sizeof (ret)) == 0) {
			log_warn("child %d died", shm->si_kid);
			atomic_store(&shm->si_dead, 1);

			pthread_mutex_lock(&lp->lp_lv_mtx);
			pthread_mutex_lock(&lp->lp_erl_mtx);
			lp->lp_dead = 1;
			++lp->lp_lv_db;
			++lp->lp_erl_db;
			pthread_cond_broadcast(&lp->lp_erl_db_cond);
			pthread_cond_broadcast(&lp->lp_lv_db_cond);
			pthread_mutex_unlock(&lp->lp_erl_mtx);
			pthread_mutex_unlock(&lp->lp_lv_mtx);

			waitpid(shm->si_kid, NULL, 0);
			return (NULL);
		}
	}
}

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__sun) || defined(LIBBSD_OVERLAY)
# define HAVE_CLOSEFROM	1
#else
# define HAVE_CLOSEFROM	0
#endif

pid_t
shm_fork(struct shmintf *shm)
{
	pid_t kid;
	int fd, maxfd;
	struct fbuf *fb;
	uint i;
#if !HAVE_CLOSEFROM
	int fdlimit;
#endif

	pthread_mutex_lock(&shm->si_cc_mtx);
	assert(shm->si_role == ROLE_NONE);

	if (pipe(shm->si_down_pipe)) {
		pthread_mutex_unlock(&shm->si_cc_mtx);
		return (-1);
	}

	if (pipe(shm->si_up_pipe)) {
		pthread_mutex_unlock(&shm->si_cc_mtx);
		return (-1);
	}

	kid = fork();
	shm->si_kid = kid;

#if defined(MADV_DONTFORK)
	/*
	 * After we've forked, mark the framebuffers and rings so they won't
	 * get shared with any other forked children.
	 */
	madvise(shm->si_cmdr, shm->si_ringsz, MADV_DONTFORK);
	madvise(shm->si_evr, shm->si_ringsz, MADV_DONTFORK);
	madvise(shm->si_flr, shm->si_ringsz, MADV_DONTFORK);
	madvise(shm->si_phr, shm->si_ringsz, MADV_DONTFORK);
	madvise(shm->si_respr, shm->si_respringsz, MADV_DONTFORK);
	for (i = 0; i < shm->si_nfbuf; ++i) {
		fb = &shm->si_fbuf[i];
		madvise(fb->fb_a, fb->fb_sz, MADV_DONTFORK);
		madvise(fb->fb_b, fb->fb_sz, MADV_DONTFORK);
	}
#endif

	switch (kid) {
	case -1:
		break;
	case 0:
		shm->si_role = ROLE_LV;
		log_post_fork("lvkid", getpid());
		close(shm->si_down_pipe[1]);
		close(shm->si_up_pipe[0]);
		maxfd = shm->si_down_pipe[0];
		if (shm->si_up_pipe[1] > maxfd)
			maxfd = shm->si_up_pipe[1];
		for (fd = 0; fd < maxfd; ++fd) {
			if (fd == STDERR_FILENO || fd == shm->si_down_pipe[0] ||
			    fd == shm->si_up_pipe[1])
				continue;
			close(fd);
		}
#if HAVE_CLOSEFROM
		closefrom(maxfd + 1);
#else
# if defined(_SC_OPEN_MAX)
		fdlimit = sysconf(_SC_OPEN_MAX);
# else
		fdlimit = getdtablesize();
# endif
		for (fd = maxfd + 1; fd < fdlimit; ++fd)
			close(fd);
#endif
		pthread_create(&shm->si_pipewatch, NULL, shm_pipewatch, shm);
		pthread_setname_np(shm->si_pipewatch, "shm_pipewatch");
		break;
	default:
		shm->si_role = ROLE_ERL;
		close(shm->si_down_pipe[0]);
		close(shm->si_up_pipe[1]);
		pthread_create(&shm->si_pipewatch, NULL, shm_parent_pipewatch,
		    shm);
		pthread_setname_np(shm->si_pipewatch, "shm_pipewatch");
		break;
	}
	pthread_mutex_unlock(&shm->si_cc_mtx);
	return (kid);
}
