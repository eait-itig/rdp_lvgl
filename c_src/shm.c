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
#include <string.h>

#include <sys/mman.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <sys/socket.h>

#include "shm.h"
#include "log.h"

struct shm_settings {
	pthread_rwlock_t	ss_lock;
	size_t			ss_nfbuf;
	size_t			ss_fbsize;
};
static struct shm_settings settings = {
	.ss_lock = PTHREAD_RWLOCK_INITIALIZER,
	.ss_nfbuf = FRAMEBUFS_PER_CHILD,
	.ss_fbsize = FRAMEBUFFER_MAX_SIZE,
};

void
set_fbufs_per_child(size_t nfbuf)
{
	pthread_rwlock_wrlock(&settings.ss_lock);
	settings.ss_nfbuf = nfbuf;
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

	shm = calloc(1, sizeof (*shm));
	if (shm == NULL)
		goto err;

	atomic_store(&shm->si_dead, 0);

	pthread_rwlock_rdlock(&settings.ss_lock);
	fbsize = settings.ss_fbsize;
	shm->si_nfbuf = settings.ss_nfbuf;
	pthread_rwlock_unlock(&settings.ss_lock);

	rc = pthread_rwlock_init(&shm->si_lock, NULL);
	if (rc != 0)
		goto err;

	if ((rc = pipe(shm->si_down_pipe)))
		goto err;

	if ((rc = pipe(shm->si_up_pipe)))
		goto err;

	shm->si_fbuf = calloc(shm->si_nfbuf, sizeof (struct fbuf));
	if (shm->si_fbuf == NULL)
		goto err;
	for (i = 0; i < shm->si_nfbuf; ++i) {
		struct fbuf *f = &shm->si_fbuf[i];
		f->fb_idx = i;
		f->fb_sz = fbsize;
		f->fb_a = MAP_FAILED;
		f->fb_b = MAP_FAILED;
		f->fb_cmd[0] = -1;
		f->fb_cmd[1] = -1;
		f->fb_event[0] = -1;
		f->fb_event[1] = -1;
		f->fb_flush[0] = -1;
		f->fb_flush[1] = -1;
	}

	for (i = 0; i < shm->si_nfbuf; ++i) {
		struct fbuf *f = &shm->si_fbuf[i];

		f->fb_a = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_a == MAP_FAILED)
			goto err;

		f->fb_b = mmap(NULL, f->fb_sz, PROT_READ | PROT_WRITE,
		    MAP_SHARED | MAP_ANON, -1, 0);
		if (f->fb_b == MAP_FAILED)
			goto err;

		rc = socketpair(AF_UNIX, SOCK_STREAM, 0, f->fb_cmd);
		if (rc != 0)
			goto err;

		rc = socketpair(AF_UNIX, SOCK_STREAM, 0, f->fb_event);
		if (rc != 0)
			goto err;

		rc = socketpair(AF_UNIX, SOCK_STREAM, 0, f->fb_flush);
		if (rc != 0)
			goto err;
	}

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

	pthread_rwlock_wrlock(&shm->si_lock);

	rrefctx_free(shm->si_refs);
	if (shm->si_role == ROLE_NONE || shm->si_role == ROLE_ERL) {
		close(shm->si_down_pipe[1]);
		close(shm->si_up_pipe[0]);
	}
	if (shm->si_role == ROLE_NONE || shm->si_role == ROLE_LV) {
		close(shm->si_down_pipe[0]);
		close(shm->si_up_pipe[1]);
	}
	if (shm->si_fbuf != NULL) {
		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			assert(f->fb_state == FBUF_FREE);

			if (f->fb_a != MAP_FAILED && f->fb_sz > 0)
				munmap(f->fb_a, f->fb_sz);

			if (f->fb_b != MAP_FAILED && f->fb_sz > 0)
				munmap(f->fb_b, f->fb_sz);

			if (shm->si_role == ROLE_NONE ||
			    shm->si_role == ROLE_ERL) {
				if (f->fb_cmd[0] != -1)
					close(f->fb_cmd[0]);
				if (f->fb_event[0] != -1)
					close(f->fb_event[0]);
				if (f->fb_flush[0] != -1)
					close(f->fb_flush[0]);
			}
			if (shm->si_role == ROLE_NONE ||
			    shm->si_role == ROLE_LV) {
				if (f->fb_cmd[1] != -1)
					close(f->fb_cmd[1]);
				if (f->fb_event[1] != -1)
					close(f->fb_event[1]);
				if (f->fb_flush[1] != -1)
					close(f->fb_flush[1]);
			}
		}
		free(shm->si_fbuf);
	}

	pthread_rwlock_unlock(&shm->si_lock);
	pthread_rwlock_destroy(&shm->si_lock);

	free(shm);
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
	uint i;
#if !HAVE_CLOSEFROM
	int fdlimit;
#endif

	pthread_rwlock_wrlock(&shm->si_lock);
	assert(shm->si_role == ROLE_NONE);

	kid = fork();
	shm->si_kid = kid;
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
		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			if (f->fb_cmd[1] > maxfd)
				maxfd = f->fb_cmd[1];
			if (f->fb_event[1] > maxfd)
				maxfd = f->fb_event[1];
			if (f->fb_flush[1] > maxfd)
				maxfd = f->fb_flush[1];
		}
		for (fd = 0; fd < maxfd; ++fd) {
			struct fbuf *f = NULL;
			if (fd == STDERR_FILENO || fd == shm->si_down_pipe[0] ||
			    fd == shm->si_up_pipe[1])
				continue;
			for (i = 0; i < shm->si_nfbuf; ++i) {
				f = &shm->si_fbuf[i];
				if (f->fb_cmd[1] == fd)
					break;
			}
			if (f != NULL && f->fb_cmd[1] == fd)
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
		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			close(f->fb_cmd[1]);
			close(f->fb_event[1]);
			close(f->fb_flush[1]);
		}
		pthread_create(&shm->si_pipewatch, NULL, shm_parent_pipewatch,
		    shm);
		pthread_setname_np(shm->si_pipewatch, "shm_pipewatch");
		break;
	}
	pthread_rwlock_unlock(&shm->si_lock);
	return (kid);
}

int
shm_read_cmd(struct shmintf *shm, struct fbuf **fbp, struct cmd **cmdp)
{
	size_t nfd = 0;
	size_t pfdn;
	struct pollfd *pfds;
	struct fbuf **fbs;
	uint i;
	int rc;

	/* maximum possible number of FDs to poll is the number of fbufs */
	pfdn = shm->si_nfbuf;
	pfds = calloc(pfdn, sizeof (struct pollfd));
	fbs = calloc(pfdn, sizeof (struct fbuf *));

	while (1) {
		assert(shm->si_role == ROLE_LV);

		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			if (f->fb_state != FBUF_FREE) {
				fbs[nfd] = f;
				pfds[nfd++] = (struct pollfd){
				    .fd = f->fb_cmd[1],
				    .events = POLLIN
				};
			}
		}

		rc = poll(pfds, nfd, -1);
		if (rc == -1) {
			free(pfds);
			return (errno);
		}

		for (i = 0; i < nfd; ++i) {
			struct fbuf *f = fbs[i];
			if (pfds[i].revents & (POLLIN | POLLHUP)) {
				rc = rbuf_recv(f->fb_cmd_rx, pfds[i].fd);
				if (rc == -1 && errno == EAGAIN)
					continue;
				if (rc == -1) {
					log_warn("recv got an error on fbuf "
					    "%u: %d (%s)", f->fb_idx,
					    errno, strerror(errno));
					goto out;
				}
				rc = rbuf_read_cmd(f->fb_cmd_rx, cmdp);
				if (rc == RBUF_NOT_ENOUGH_DATA)
					continue;
				if (rc != 0) {
					log_warn("rbuf_read_cmd got an err "
					    "on fbuf %u: %d", f->fb_idx, rc);
					goto out;
				}
				*fbp = f;
				rbuf_shift(f->fb_cmd_rx);
				rc = 0;
				goto out;
			}
		}
	}
out:
	free(pfds);
	return (rc);
}

int
shm_read_phlush(struct shmintf *shm, struct fbuf **fbp, struct phlush_msg **pmp)
{
	size_t nfd = 0;
	size_t pfdn;
	struct pollfd *pfds;
	struct fbuf **fbs;
	uint i;
	int rc;

	/* maximum possible number of FDs to poll is the number of fbufs */
	pfdn = shm->si_nfbuf;
	pfds = calloc(pfdn, sizeof (struct pollfd));
	fbs = calloc(pfdn, sizeof (struct fbuf *));

	while (1) {
		assert(shm->si_role == ROLE_LV);

		for (i = 0; i < shm->si_nfbuf; ++i) {
			struct fbuf *f = &shm->si_fbuf[i];
			if (f->fb_state != FBUF_FREE) {
				fbs[nfd] = f;
				pfds[nfd++] = (struct pollfd){
				    .fd = f->fb_flush[1],
				    .events = POLLIN
				};
			}
		}

		rc = poll(pfds, nfd, -1);
		if (rc == -1) {
			free(pfds);
			return (errno);
		}

		for (i = 0; i < nfd; ++i) {
			struct fbuf *f = fbs[i];
			if (pfds[i].revents & (POLLIN | POLLHUP)) {
				rc = rbuf_recv(f->fb_flush_rx, pfds[i].fd);
				if (rc == -1 && errno == EAGAIN)
					continue;
				if (rc == -1) {
					log_warn("recv got an error on fbuf "
					    "%u: %d (%s)", f->fb_idx,
					    errno, strerror(errno));
					goto out;
				}
				rc = rbuf_read_phlush(f->fb_flush_rx, pmp);
				if (rc == RBUF_NOT_ENOUGH_DATA)
					continue;
				if (rc != 0) {
					log_warn("rbuf_read_phlush got an err "
					    "on fbuf %u: %d", f->fb_idx, rc);
					goto out;
				}
				*fbp = f;
				rbuf_shift(f->fb_flush_rx);
				rc = 0;
				goto out;
			}
		}
	}
out:
	free(pfds);
	return (rc);
}

int
shm_write_ret(struct shmintf *shm, struct fbuf *fb, struct ret *retp)
{
	int rc;

	rc = rbuf_write_ret(fb->fb_cmd_tx, retp);
	if (rc != 0)
		return (rc);

	rc = rbuf_send(fb->fb_cmd_tx, fb->fb_cmd[1]);
	if (rc != 0)
		return (rc);

	rbuf_reset(fb->fb_cmd_tx);

	return (0);
}

int
shm_write_flush(struct shmintf *shm, struct fbuf *fb, struct flush_msg *fm)
{
	int rc;

	rc = rbuf_write_flush(fb->fb_flush_tx, fm);
	if (rc != 0)
		return (rc);

	rc = rbuf_send(fb->fb_flush_tx, fb->fb_flush[1]);
	if (rc != 0)
		return (rc);

	rbuf_reset(fb->fb_flush_tx);

	return (0);
}

int
shm_write_event(struct shmintf *shm, struct fbuf *fb, struct event_msg *em)
{
	int rc;

	rc = rbuf_write_event(fb->fb_event_tx, em);
	if (rc != 0)
		return (rc);

	rc = rbuf_send(fb->fb_event_tx, fb->fb_event[1]);
	if (rc != 0)
		return (rc);

	rbuf_reset(fb->fb_event_tx);

	return (0);
}
