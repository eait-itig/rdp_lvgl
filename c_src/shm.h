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
	uint		 fb_idx;
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

	int		 si_down_pipe[2];
	int		 si_up_pipe[2];
	pid_t		 si_kid;
	atomic_uint	 si_dead;
	pthread_t	 si_pipewatch;

	struct lockpg	*si_lockpg;

	size_t		 si_nfbuf;
	struct fbuf	*si_fbuf;
	size_t		 si_ringsz;

	size_t		 si_ncmd;	/* general commands */
	pthread_mutex_t	 si_cc_mtx;	/* protects si_cc outbound */
	uint		 si_cc;
	struct cdesc	*si_cmdr;

	size_t		 si_nresp;	/* responses to commands on cmdr */
	pthread_mutex_t	 si_rc_mtx;	/* protects si_rc outbound */
	uint		 si_rc;
	struct rdesc	*si_respr;

	size_t		 si_nev;	/* async events (lv => erl) */
	pthread_mutex_t	 si_ec_mtx;	/* protects si_ec outbound */
	uint		 si_ec;
	struct edesc	*si_evr;

	size_t		 si_nfl;	/* flush areas (lv => erl) */
	pthread_mutex_t	 si_fc_mtx;	/* protects si_fc outbound */
	uint		 si_fc;
	struct fdesc	*si_flr;

	size_t		 si_nph;	/* flush done (erl => lv) */
	pthread_mutex_t	 si_pc_mtx;	/* protects si_pc outbound */
	uint		 si_pc;
	struct pdesc	*si_phr;
};

struct lockpg {
	pthread_mutex_t	lp_mtx;
	uint		lp_dead;
	uint		lp_erl_db;
	pthread_cond_t	lp_erl_db_cond;
	uint		lp_lv_db;
	pthread_cond_t	lp_lv_db_cond;
};

enum owner {
	OWNER_ERL	= 0x01344224,	/* 20202020 */
	OWNER_LV	= 0x009a2112	/* 10101010 */
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

#define MAX_ARGS		8

enum arg_type {
	ARG_NONE		= 0,
	ARG_PTR,
	ARG_PTR_BUFFER,
	ARG_PTR_OBJ,
	ARG_PTR_STYLE,
	ARG_PTR_GROUP,
	ARG_PTR_CHART_SER,
	ARG_PTR_CHART_CUR,
	ARG_PTR_METER_SCL,
	ARG_PTR_METER_IND,
	ARG_PTR_SPAN,
	ARG_UINT64,
	ARG_UINT32,
	ARG_UINT16,
	ARG_UINT8,
	ARG_COLOR,
	ARG_POINT,
	ARG_STYLEVAL,
	ARG_INLINE_BUF,
	ARG_INLINE_STR,
	ARG_INL_BUF_ARR
};

struct cdesc_call {
	uint64_t		cdc_func;
	uint64_t		cdc_arg[MAX_ARGS];
	uint8_t			cdc_argtype[MAX_ARGS];
	uint16_t		cdc_rbuflen;
	uint8_t			cdc_rettype;
	uint8_t			cdc_ibuf[29];
};

struct cdesc_setudata {
	uint64_t		cdsu_ptr;
	uint64_t		cdsu_udata;
	uint8_t			cdsu_type;
};

struct cdesc_copybuf {
	uint32_t		cdcs_len;
	char			cdcs_data[108];
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
	uint64_t		cdte_eudata;
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
		uint64_t		cd_dword[14];
		uint8_t			cd_data[112];
	};
};

struct edesc {
	atomic_uint	ed_owner;
	uint32_t	ed_code;
	uint32_t	ed_removed;
	uint32_t	ed_pad;
	uint64_t	ed_udata;
	uint64_t	ed_target;
	uint64_t	ed_target_udata;
	uint64_t	ed_ctarget;
	uint64_t	ed_ctarget_udata;
	uint64_t	ed_param;
	uint64_t	ed_param_data;
};

struct pdesc {
	atomic_uint		pd_owner;
	uint32_t		pd_final;
	uint64_t		pd_disp_drv;
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
	char		rdrb_data[108];
};

struct rdesc_setupev {
	uint64_t	rdse_eudata;
};

struct rdesc {
	atomic_uint		rd_owner;
	uint8_t			rd_chain;
	uint8_t			rd_pad;
	uint16_t		rd_error;
	uint64_t		rd_cookie;
	union {
		struct rdesc_setup	rd_setup;
		struct rdesc_setupev	rd_setup_event;
		struct rdesc_return	rd_return;
		struct rdesc_retbuf	rd_return_buf;
		uint8_t			rd_data[112];
	};
};

#define	FRAMEBUFS_PER_CHILD	16
#define	FRAMEBUFFER_MAX_SIZE	(64*1024*1024)
#define	RING_SIZE		16384

#define	RING_MAX_CHAIN		16

#define	RING_BUSYWAIT_ITERS	16384

#define CDESC_FIRST_INLINE	(sizeof ( ((struct cdesc_call *)0)->cdc_ibuf ))
#define CDESC_REST_INLINE	(sizeof ( ((struct cdesc *)0)->cd_data ))
#define	CDESC_MAX_INLINE	\
	(CDESC_FIRST_INLINE + (RING_MAX_CHAIN - 1) * CDESC_REST_INLINE)

#define RDESC_FIRST_INLINE	(sizeof ( ((struct rdesc_retbuf *)0)->rdrb_data ))
#define RDESC_REST_INLINE	(sizeof ( ((struct rdesc *)0)->rd_data ))
#define	RDESC_MAX_INLINE	\
	(RDESC_FIRST_INLINE + (RING_MAX_CHAIN - 1) * RDESC_REST_INLINE)

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

void shm_produce_phlush(struct shmintf *, const struct pdesc *);
void shm_consume_phlush(struct shmintf *, struct pdesc **);
void shm_finish_phlush(struct pdesc *);

void shm_await_doorbell(struct shmintf *shm);
void shm_ring_doorbell(struct shmintf *shm);
uint shm_read_doorbell(struct shmintf *shm);
void shm_await_doorbell_v(struct shmintf *shm, uint orig);

void set_fbufs_per_child(size_t nfbuf);
void set_ring_size(size_t ringsz);
void set_fb_max_res(size_t w, size_t h);

#endif /* _SHM_H */
