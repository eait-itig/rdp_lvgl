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

#if !defined(_CMDDEF_H)
#define _CMDDEF_H

#include <sys/types.h>
#include <stdint.h>
#include <assert.h>

#include "queue.h"
#include "rbuf.h"
#include "shm.h"

#include <lvgl.h>

typedef struct { rref_t *_ref; }	lv_ref_t;
typedef struct { rref_t *_ref; }	erl_ref_t;

static inline void *
lv_deref(const struct shmintf *shm, lv_ref_t lv, ref_type_t type)
{
	assert(shm->si_role == ROLE_LV);
	return (rref_deref(lv._ref, type));
}

static inline void *
erl_deref(const struct shmintf *shm, erl_ref_t erl, ref_type_t type)
{
	assert(shm->si_role == ROLE_ERL);
	return (rref_deref(erl._ref, type));
}

static inline lv_ref_t
lv_ref(struct shmintf *shm, ref_type_t type, void *ptr)
{
	rref_t *r;
	assert(shm->si_role == ROLE_LV);
	r = rref_make(shm->si_refs, type, ptr);
	return ((lv_ref_t){ ._ref = r });
}

static inline erl_ref_t
erl_ref(struct shmintf *shm, ref_type_t type, void *ptr)
{
	rref_t *r;
	assert(shm->si_role == ROLE_ERL);
	r = rref_make(shm->si_refs, type, ptr);
	return ((erl_ref_t){ ._ref = r });
}

typedef enum cmd_op {
	CMD_SETUP		= 1,
	CMD_TEARDOWN,
	CMD_SET_UDATA,
	CMD_CALL,
	CMD_WRITE_BUF,
	CMD_READ_BUF,
	CMD_FREE_BUF,
	CMD_SETUP_EVENT,
	CMD_TEARDOWN_EVENT,
	CMD_EXIT_CHILD
} cmd_op_t;

typedef enum arg_type {
	ARG_NONE		= 0,
	ARG_PTR,
	ARG_UINT64,
	ARG_UINT32,
	ARG_UINT16,
	ARG_UINT8,
	ARG_COLOR,
	ARG_POINT,
	ARG_STYLEVAL,
	ARG_CSTRING,
	ARG_CSTRING_ARRAY,
	ARG_BUFFER,
	ARG_BUFFER_ARRAY
} arg_type_t;

struct cmd_buf {
	SLIST_ENTRY(cmd_buf)	 cb_entry;
	size_t			 cb_len;
	uint8_t			*cb_data;
};
SLIST_HEAD(cmd_buf_list, cmd_buf);

struct cmd_buf_arr {
	size_t			 cba_nbufs;
	struct cmd_buf_list	*cba_list;
};

struct cmd_arg {
	SLIST_ENTRY(cmd_arg)		 ca_entry;
	arg_type_t			 ca_type;
	union {
		lv_ref_t		 ca_ref;
		uint64_t		 ca_u64;
		uint32_t		 ca_u32;
		uint16_t		 ca_u16;
		uint8_t			 ca_u8;
		lv_color_t		 ca_color;
		lv_point_t		 ca_point;
		lv_style_value_t	 ca_style_val;
		struct cmd_buf		*ca_buf;
		struct cmd_buf_arr	 ca_buf_arr;
	};
};
SLIST_HEAD(cmd_arg_list, cmd_arg);

struct cmd_call {
	char			*cc_func_name;
	arg_type_t		 cc_return_type;
	struct cmd_arg_list	 cc_args;
};
void cmd_call_free(struct cmd_call *cc);

struct ret_call {
	union {
		lv_ref_t		 rc_ref;
		uint64_t		 rc_u64;
		uint32_t		 rc_u32;
		uint16_t		 rc_u16;
		uint8_t			 rc_u8;
		lv_color_t		 rc_color;
		lv_point_t		 rc_point;
		lv_style_value_t	 rc_style_val;
		struct cmd_buf		*rc_buf;
		struct cmd_buf_arr	 rc_buf_arr;
	};
	const void			*rc_class;
	erl_ref_t			 rc_udata;
};
void ret_call_free(struct ret_call *rc);

struct cmd_setup {
	uint32_t	 cs_width;
	uint32_t	 cs_height;
	erl_ref_t	 cs_udata;
};
void cmd_setup_free(struct cmd_setup *cs);

struct ret_setup {
	lv_ref_t	 rs_inst;
	lv_ref_t	 rs_disp_drv;
	lv_ref_t	 rs_disp;
	lv_ref_t	 rs_kbd_drv;
	lv_ref_t	 rs_kbd;
	lv_ref_t	 rs_mouse_drv;
	lv_ref_t	 rs_mouse;
};
void ret_setup_free(struct ret_setup *rs);

struct cmd_teardown {
	lv_ref_t	 ct_inst;
};
void cmd_teardown_free(struct cmd_teardown *ct);

struct cmd_set_udata {
	lv_ref_t	 csu_target;
	erl_ref_t	 csu_udata;
};
void cmd_set_udata_free(struct cmd_set_udata *csu);

struct cmd_write_buf {
	uint32_t	 cwb_len;
	void		*cwb_data;
};
void cmd_write_buf_free(struct cmd_write_buf *cwb);

struct ret_write_buf {
	lv_ref_t	 rwb_buffer;
};
void ret_copy_buf_free(struct ret_write_buf *rwb);

struct cmd_read_buf {
	lv_ref_t	 crb_buffer;
	uint32_t	 crb_offset;
	uint32_t	 crb_len;
};
void cmd_read_buf_free(struct cmd_read_buf *crb);

struct ret_read_buf {
	uint32_t	 rrb_len;
	void		*rrb_data;
};
void ret_read_buf_free(struct ret_read_buf *rrb);

struct cmd_free_buf {
	lv_ref_t	 cfb_buffer;
};
void cmd_free_buf_free(struct cmd_free_buf *cfb);

struct cmd_setup_event {
	lv_ref_t	 cse_obj;
	lv_event_code_t	 cse_event;
	erl_ref_t	 cse_udata;
};
void cmd_setup_event_free(struct cmd_setup_event *cse);

struct ret_setup_event {
	lv_ref_t	 rse_eudata;
};
void ret_setup_event_free(struct ret_setup_event *rse);

struct cmd_teardown_event {
	lv_ref_t	 cte_obj;
	erl_ref_t	 cte_udata;
	lv_ref_t	 cte_eudata;	/* from rse_eudata */
};
void cmd_teardown_event_free(struct cmd_teardown_event *cte);

struct cmd {
	uint64_t			 c_seq;
	cmd_op_t			 c_op;
	union {
		struct cmd_setup	*c_setup;
		struct cmd_teardown	*c_teardown;
		struct cmd_set_udata	*c_set_udata;
		struct cmd_call		*c_call;
		struct cmd_write_buf	*c_write_buf;
		struct cmd_read_buf	*c_read_buf;
		struct cmd_free_buf	*c_free_buf;
		struct cmd_setup_event	*c_setup_event;
		struct cmd_teardown_event	*c_teardown_event;
	};
};
void cmd_free(struct cmd *cmd);

int rbuf_read_cmd(rbuf_t *buf, struct cmd **cmdp);

struct ret {
	uint64_t			 r_seq;
	cmd_op_t			 r_op;
	int				 r_err;
	char				*r_err_msg;
	union {
		struct ret_setup	*r_setup;
		struct ret_call		*r_call;
		struct ret_write_buf	*r_write_buf;
		struct ret_read_buf	*r_read_buf;
		struct ret_setup_event	*r_setup_event;
	};
};
void ret_setup_cmd(struct ret *ret, const struct cmd *cmd, const char *err, ...);
void ret_free(struct ret *ret);

int rbuf_write_ret(rbuf_t *buf, const struct ret *retp);


struct event_msg {
	lv_event_code_t		 em_code;
	uint8_t			 em_last_event;
	erl_ref_t		 em_udata;		/* cse_udata */
	lv_ref_t		 em_target;
	erl_ref_t		 em_target_udata;
	lv_ref_t		 em_ctarget;
	erl_ref_t		 em_ctarget_udata;
	lv_ref_t		 em_param;
	uint64_t		 em_param_data;		/* first 64 bits of em_param */
};
void event_msg_free(struct event_msg *em);

int rbuf_write_event(rbuf_t *buf, const struct event_msg *emp);


enum flush_fbuf {
	FBUF_A	= 1,
	FBUF_B,
};

struct flush_msg {
	erl_ref_t		*fm_udata;
	uint8_t			 fm_fbuf;
	uint8_t			 fm_sync;
	uint32_t		 fm_x1;
	uint32_t		 fm_y1;
	uint32_t		 fm_x2;
	uint32_t		 fm_y2;
};
void flush_msg_free(struct flush_msg *fm);

int rbuf_write_flush(rbuf_t *buf, const struct flush_msg *fm);

struct phlush_msg {
	lv_ref_t		*pm_disp_drv;
	uint8_t			 pm_final;
};
void phlush_msg_free(struct phlush_msg *pm);

int rbuf_read_phlush(rbuf_t *buf, struct phlush_msg **pmp);

#endif
