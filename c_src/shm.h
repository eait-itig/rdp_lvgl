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

#if !defined(_SHM_H)
#define _SHM_H

#include <stdint.h>
#include <stdatomic.h>

#include <sys/types.h>

#include "rbuf.h"
#include "cmddef.h"

#include <lvgl.h>

enum fbuf_state {
	FBUF_FREE	= 0,
	FBUF_SETTING_UP,
	FBUF_RUNNING,
	FBUF_TEARDOWN
};

struct fbuf {
	uint		 fb_idx;
	enum fbuf_state	 fb_state;
	void		*fb_priv;

	/* sockets */
	int		 fb_cmd[2];
	int		 fb_event[2];
	int		 fb_flush[2];

	rbuf_t		*fb_cmd_rx;
	rbuf_t		*fb_cmd_tx;

	rbuf_t		*fb_event_rx;
	rbuf_t		*fb_event_tx;

	rbuf_t		*fb_flush_rx;
	rbuf_t		*fb_flush_tx;

	size_t		 fb_w;
	size_t		 fb_h;
	size_t		 fb_sz;

	lv_color_t	*fb_a;
	lv_color_t	*fb_b;
};

enum shmrole {
	ROLE_NONE	= 0,
	ROLE_ERL,
	ROLE_LV
};

/*
 * The SHM interface consists of:
 *  - a set of shared framebuffers
 *  - a set of sockets:
 *    - command socket
 *    - event socket -- widget/lv_object events
 *    - flush socket -- framebuffer flushes
 */

struct shmintf {
	pthread_rwlock_t si_lock;

	enum shmrole	 si_role;

	int		 si_down_pipe[2];
	int		 si_up_pipe[2];
	pid_t		 si_kid;
	atomic_uint	 si_dead;
	pthread_t	 si_pipewatch;

	size_t		 si_nfbuf;
	struct fbuf	*si_fbuf;
};

#define	FRAMEBUFS_PER_CHILD	16
#define	FRAMEBUFFER_MAX_SIZE	(64*1024*1024)

struct shmintf *alloc_shmintf(void);
void free_shmintf(struct shmintf *);

pid_t shm_fork(struct shmintf *);

int shm_read_cmd(struct shmintf *shm, struct fbuf **fb, struct cmd **cmdp);
int shm_write_ret(struct shmintf *shm, struct fbuf *fb, struct ret *retp);

int shm_write_flush(struct shmintf *shm, struct fbuf *fb, struct flush_msg *fm);
int shm_read_phlush(struct shmintf *shm, struct fbuf **fb, struct phlush_msg **pmp);

int shm_write_event(struct shmintf *shm, struct fbuf *fb, struct event_msg *em);

void set_fbufs_per_child(size_t nfbuf);
void set_fb_max_res(size_t w, size_t h);

#endif /* _SHM_H */
