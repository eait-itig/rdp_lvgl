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

#include <lvgl.h>

enum fbuf_state {
	FBUF_FREE	= 0,
	FBUF_SETTING_UP,
	FBUF_RUNNING,
	FBUF_TEARDOWN
};

struct fbuf {
	enum fbuf_state	 fb_state;
	void		*fb_priv;
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
 *  - a set of rings:
 *    - command ring (erl => lv)
 *    - response ring (lv => erl)
 *    - event ring (lv => erl) -- widget/lv_object events
 *    - flush ring (lv => erl) -- framebuffer flushes
 *
 * Each ring at the receiving end has a dedicated thread to handle incoming
 * descriptors. At the sending send, any thread can enqueue, they just have to
 * grab si_mtx before manipulating the counters.
 *
 * The si_cc/si_rc etc counters are used as producer counters on the sending
 * side, and consumer counters on the receiving side.
 */

struct shmintf {
	enum shmrole	 si_role;
	struct lockpg	*si_lockpg;

	size_t		 si_nfbuf;
	struct fbuf	*si_fbuf;

	pthread_mutex_t	 si_mtx;	/* protects outgoing ring counters */

	size_t		 si_ncmd;
	uint		 si_cc;
	struct cdesc	*si_cmdr;

	size_t		 si_nresp;
	uint		 si_rc;
	struct rdesc	*si_respr;

	size_t		 si_nev;
	uint		 si_ec;
	struct edesc	*si_evr;

	size_t		 si_nfl;
	uint		 si_fc;
	struct fdesc	*si_flushr;
};

struct lockpg {
	pthread_mutex_t	lp_mtx;
	pthread_cond_t	lp_cmd_db;
	pthread_cond_t	lp_resp_db;
	pthread_cond_t	lp_evt_db;
	pthread_cond_t	lp_flush_db;
};

enum owner {
	OWNER_ERL	= 0xffe4ff1a,
	OWNER_LV	= 0x17ff61ff
};

enum cmd_op {
	CMD_SETUP	= 1,
	CMD_TEARDOWN,
	CMD_SET_UDATA,
	CMD_CALL,
	CMD_COPY_BUF,
	CMD_FREE_BUF,
	CMD_SETUP_EVENT,
	CMD_TEARDOWN_EVENT,
	CMD_FLUSH_DONE,
	CMD_EXIT_CHILD
};

struct cdesc_setup {
	uint32_t		cds_fbidx;
	uint32_t		cds_w;
	uint32_t		cds_h;
	uint64_t		cds_udata;
};

struct cdesc_teardown {
	uint64_t		cdt_disp_drv;
};

enum arg_type {
	ARG_NONE		= 0,
	ARG_BUFPTR,
	ARG_OBJPTR,
	ARG_PTR,
	ARG_UINT64,
	ARG_UINT32,
	ARG_UINT16,
	ARG_UINT8
};

struct cdesc_call {
	uint64_t		cdc_func;
	uint64_t		cdc_arg[4];
	uint8_t			cdc_argtype[4];
	uint8_t			cdc_rettype;
	uint16_t		cdc_rbuflen;
};

struct cdesc_setudata {
	uint64_t		cdsu_obj;
	uint64_t		cdsu_udata;
};

struct cdesc_copybuf {
	uint32_t		cdcs_len;
	char			cdcs_data[44];
};

struct cdesc_freebuf {
	uint64_t		cdfs_buf;
	uint32_t		cdfs_len;
};

struct cdesc_setupev {
	uint64_t		cdse_obj;
	uint32_t		cdse_event;
	uint64_t		cdse_udata;
};

struct cdesc_teardownev {
	uint64_t		cdte_obj;
	uint64_t		cdte_udata;
};

struct cdesc_flushdone {
	uint64_t		cdfd_disp_drv;
};

struct cdesc {
	atomic_uint		cd_owner;
	uint8_t			cd_op;
	uint8_t			cd_chain;
	uint8_t			cd_pad[2];
	uint64_t		cd_cookie;
	union {
		struct cdesc_setup	cd_setup;
		struct cdesc_teardown	cd_teardown;
		struct cdesc_setudata	cd_set_udata;
		struct cdesc_call	cd_call;
		struct cdesc_copybuf	cd_copy_buf;
		struct cdesc_freebuf	cd_free_buf;
		struct cdesc_setupev	cd_setup_event;
		struct cdesc_teardownev	cd_teardown_event;
		struct cdesc_flushdone	cd_flush_done;
		uint64_t		cd_dword[6];
		uint8_t			cd_data[48];
	};
};

struct edesc {
	atomic_uint	ed_owner;
	uint32_t	ed_code;
	uint32_t	ed_removed;
	uint32_t	ed_pad;
	uint64_t	ed_cookie;
	uint64_t	ed_target;
	uint64_t	ed_target_udata;
	uint64_t	ed_ctarget;
	uint64_t	ed_ctarget_udata;
	uint64_t	ed_param;
	uint64_t	ed_param_data;
};

enum fbidx_flags {
	FBIDX_SYNC	= (1<<31),
	FBIDX_A		= (1<<30),
	FBIDX_B		= (1<<29),
	FBIDX_IDX_MASK	= (1<<16) - 1
};

struct fdesc {
	atomic_uint	fd_owner;
	uint32_t	fd_fbidx_flag;
	uint64_t	fd_udata;
	uint32_t	fd_x1;
	uint32_t	fd_y1;
	uint32_t	fd_x2;
	uint32_t	fd_y2;
};

struct rdesc_setup {
	uint64_t	rds_disp_drv;
	uint64_t	rds_disp;
	uint64_t	rds_kbd_drv;
	uint64_t	rds_kbd;
	uint64_t	rds_mouse_drv;
	uint64_t	rds_mouse;
};

struct rdesc_return {
	uint64_t	rdr_val;
	uint64_t	rdr_class;
	uint64_t	rdr_udata;
};

struct rdesc_retbuf {
	uint32_t	rdrb_len;
	char		rdrb_data[44];
};

struct rdesc {
	atomic_uint		rd_owner;
	uint8_t			rd_chain;
	uint8_t			rd_pad;
	uint16_t		rd_error;
	uint64_t		rd_cookie;
	union {
		struct rdesc_setup	rd_setup;
		struct rdesc_return	rd_return;
		struct rdesc_retbuf	rd_return_buf;
		uint8_t			rd_data[48];
	};
};

#define	FRAMEBUFS_PER_CHILD	32
#define	FRAMEBUFFER_MAX_SIZE	(64*1024*1024)
#define	RING_SIZE		8192

#define	RING_BUSYWAIT_ITERS	16384

struct shmintf *alloc_shmintf(void);
void free_shmintf(struct shmintf *);

pid_t shm_fork(struct shmintf *);

void shm_produce_cmd(struct shmintf *, const struct cdesc *, uint);
uint shm_consume_cmd(struct shmintf *, struct cdesc **, uint);
void shm_finish_cmd(struct cdesc *);

void shm_produce_rsp(struct shmintf *, const struct rdesc *, uint);
uint shm_consume_rsp(struct shmintf *, struct rdesc **, uint);
void shm_finish_rsp(struct rdesc *);

void shm_produce_evt(struct shmintf *, const struct edesc *);
void shm_consume_evt(struct shmintf *, struct edesc **);
void shm_finish_evt(struct edesc *);

void shm_produce_flush(struct shmintf *, const struct fdesc *);
void shm_consume_flush(struct shmintf *, struct fdesc **);
void shm_finish_flush(struct fdesc *);

#endif /* _SHM_H */
