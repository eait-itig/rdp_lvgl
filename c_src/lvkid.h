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

#if !defined(_LVKID_H)
#define _LVKID_H

#include <unistd.h>
#include <pthread.h>

#include "erl_nif.h"
#include <lvgl.h>
#include <queue.h>

#include "shm.h"

typedef uint64_t lvaddr_t;

struct lvkid;
struct lvkcmd;
struct lvkinst;
struct lvkcall;
struct lvkbuf;
struct lvkobj;
LIST_HEAD(lvkid_list, lvkid);
LIST_HEAD(lvkcmd_list, lvkcmd);
LIST_HEAD(lvkinst_list, lvkinst);
LIST_HEAD(lvkcall_list, lvkcall);
LIST_HEAD(lvkbuf_list, lvkbuf);
LIST_HEAD(lvkobj_list, lvkobj);
LIST_HEAD(lvkevt_list, lvkevt);

enum lvkh_type {
	LVK_NONE	= 0,
	LVK_FBUF,
	LVK_INST,
	LVK_OBJ,
	LVK_BUF,
	LVK_EVT
};

struct lvkhdl {
	struct lvkid		*lvkh_kid;
	struct lvkinst		*lvkh_inst;
	enum lvkh_type		 lvkh_type;
	void			*lvkh_ptr;
	uint			 lvkh_given;
};

struct lvkid {
	struct shmintf		*lvk_shm;
	pthread_rwlock_t	 lvk_lock;
	pid_t			 lvk_pid;
	size_t			 lvk_busy;
	LIST_ENTRY(lvkid)	 lvk_entry;

	struct lvkinst_list	 lvk_insts;
	struct lvkcmd_list	 lvk_cmds;
	struct lvkevt_list	 lvk_evts;
	struct lvkbuf_list	 lvk_bufs;

	pthread_t		 lvk_lv_th;
	pthread_t		 lvk_rsp_th;
	pthread_t		 lvk_evt_th;
	pthread_t		 lvk_flush_th;
};

typedef void (*lvkcmd_cb_t)(struct rdesc **rd, uint nrd, void *priv);

struct lvkcmd {
	struct lvkid		*lvkc_kid;
	LIST_ENTRY(lvkcmd)	 lvkc_entry;
	lvkcmd_cb_t		 lvkc_cb;
	void			*lvkc_priv;
};

enum lvkinst_state {
	LVKINST_NEW	= 0,
	LVKINST_STARTING,
	LVKINST_ALIVE,
	LVKINST_DRAIN,
	LVKINST_FREE
};

struct lvkinst {
	struct lvkid		*lvki_kid;
	pthread_rwlock_t	 lvki_lock;
	LIST_ENTRY(lvkinst)	 lvki_entry;

	struct lvkhdl		*lvki_hdl;
	struct lvkhdl		*lvki_fbhdl;

	enum lvkinst_state	 lvki_state;

	struct fbuf		*lvki_fbuf;
	lvaddr_t		 lvki_disp;
	lvaddr_t		 lvki_disp_drv;
	lvaddr_t		 lvki_kbd;
	lvaddr_t		 lvki_kbd_drv;
	lvaddr_t		 lvki_mouse;
	lvaddr_t		 lvki_mouse_drv;

	struct lvkobj_list	 lvki_objs;

	ErlNifEnv		*lvki_env;
	ERL_NIF_TERM		 lvki_msgref;
	ErlNifPid		 lvki_owner;
};

struct lvkbuf {
	struct lvkid		*lvkb_kid;
	LIST_ENTRY(lvkbuf)	 lvkb_entry;

	struct lvkhdl		*lvkb_hdl;

	lvaddr_t		 lvkb_ptr;
	size_t			 lvkb_len;
	void			*lvkb_data;
};

struct lvkobj {
	struct lvkid		*lvko_kid;
	struct lvkinst		*lvko_inst;
	LIST_ENTRY(lvkobj)	 lvko_entry;

	struct lvkhdl		*lvko_hdl;

	lvaddr_t		 lvko_ptr;
	const lv_obj_class_t	*lvko_class;
	struct lvkevt		*lvko_delevt;
	struct lvkevt_list	 lvko_events;
};

struct lvkevt {
	struct lvkid		*lvke_kid;
	struct lvkobj		*lvke_obj;
	LIST_ENTRY(lvkevt)	 lvke_kid_entry;
	LIST_ENTRY(lvkevt)	 lvke_obj_entry;

	uint			 lvke_teardown;

	struct lvkhdl		*lvke_hdl;

	lv_event_code_t		 lvke_evt;

	ErlNifEnv		*lvke_env;
	ERL_NIF_TERM		 lvke_msgref;
	ErlNifPid		 lvke_owner;
};

extern ErlNifResourceType *lvkid_hdl_rsrc;

void lvk_open_resource_types(ErlNifEnv *env);

struct lvkinst *lvkid_setup_inst(ErlNifPid owner, ERL_NIF_TERM msgref,
    uint width, uint height);

struct lvkhdl *lvkid_make_hdl(enum lvkh_type type, void *ptr, uint *do_release);

struct lvkbuf *lvk_copy_buf(struct lvkinst *, void *, size_t);

struct lvkevt *lvk_add_event(struct lvkobj *, lv_event_code_t, ErlNifPid owner,
    ERL_NIF_TERM msgref);
void lvk_remove_event(struct lvkevt *);

typedef void (*lvk_call_func_t)();
typedef void (*lvk_call_cb_t)(struct lvkid *, uint32_t err, enum arg_type,
    void *arg, void *priv);

void lvk_cmd(struct lvkid *kid, struct cdesc *cd, uint ncd, lvkcmd_cb_t cb,
    void *priv);

void lvk_cast(struct lvkid *kid, enum arg_type rt, lvk_call_func_t f, ...);
void lvk_icast(struct lvkinst *inst, enum arg_type rt, lvk_call_func_t f, ...);
void lvk_call_buf(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    size_t rtblen, lvk_call_func_t f, ...);
void lvk_call(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    enum arg_type rt, lvk_call_func_t f, ...);
void lvk_icall(struct lvkinst *inst, lvk_call_cb_t cb, void *priv,
    enum arg_type rt, lvk_call_func_t f, ...);

void lv_group_send_text(lv_group_t *group, const char *text);

#endif /* _LVKID_H */

