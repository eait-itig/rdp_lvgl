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
struct lvkstyle;
struct lvkgroup;
struct lvkchartser;
struct lvkchartcur;
struct lvkmeterind;
struct lvkmeterscl;
struct lvkspan;
TAILQ_HEAD(lvkid_list, lvkid);
LIST_HEAD(lvkcmd_list, lvkcmd);
LIST_HEAD(lvkinst_list, lvkinst);
LIST_HEAD(lvkcall_list, lvkcall);
LIST_HEAD(lvkbuf_list, lvkbuf);
LIST_HEAD(lvkobj_list, lvkobj);
LIST_HEAD(lvkevt_list, lvkevt);
LIST_HEAD(lvkstyle_list, lvkstyle);
LIST_HEAD(lvkgroup_list, lvkgroup);
LIST_HEAD(lvkchartser_list, lvkchartser);
LIST_HEAD(lvkchartcur_list, lvkchartcur);
LIST_HEAD(lvkmeterind_list, lvkmeterind);
LIST_HEAD(lvkmeterscl_list, lvkmeterscl);
LIST_HEAD(lvkspan_list, lvkspan);

enum lvkh_type {
	LVK_NONE	= 0,
	LVK_FBUF,
	LVK_INST,
	LVK_OBJ,
	LVK_BUF,
	LVK_EVT,
	LVK_STY,
	LVK_GRP,
	LVK_CHART_SER,
	LVK_CHART_CUR,
	LVK_METER_IND,
	LVK_METER_SCL,
	LVK_SPAN,
};

struct lvkhdl {
	struct lvkid		*lvkh_kid;
	struct lvkinst		*lvkh_inst;
	enum lvkh_type		 lvkh_type;
	void			*lvkh_ptr;
	uint			 lvkh_given;
	void			*lvkh_fbuf;
	ErlNifMonitor		 lvkh_mon;
};

#define	LVK_DEFER_TPOOL_SIZE	8

struct lvkid {
	struct shmintf		*lvk_shm;
	pthread_rwlock_t	 lvk_lock;
	pid_t			 lvk_pid;
	size_t			 lvk_busy;
	TAILQ_ENTRY(lvkid)	 lvk_entry;

	pthread_mutex_t		 lvk_cmdlk;
	struct lvkcmd_list	 lvk_cmds;

	struct lvkinst_list	 lvk_insts;
	struct lvkevt_list	 lvk_evts;

	pthread_t		 lvk_lv_th;
	pthread_t		 lvk_rsp_th;
	pthread_t		 lvk_evt_th;
	pthread_t		 lvk_flush_th;

	pthread_t		 lvk_defer_ths[LVK_DEFER_TPOOL_SIZE];
	pthread_cond_t		 lvk_defer_nonempty;
	struct lvkcmd_list	 lvk_defer_cmds;
};

typedef void (*lvkcmd_cb_t)(const struct rdesc *rd, const void *data,
   size_t dlen, void *priv);

struct lvkcmd {
	struct lvkid		 *lvkc_kid;
	LIST_ENTRY(lvkcmd)	  lvkc_entry;
	lvkcmd_cb_t		  lvkc_cb;
	void			 *lvkc_priv;
	uint			  lvkc_defer;
	pthread_t		  lvkc_cbth;
	struct rdesc		  lvkc_rd;
	void			 *lvkc_data;
	size_t			  lvkc_dlen;
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
	uint			 lvki_flushing;

	enum lvkinst_state	 lvki_state;

	struct fbuf		*lvki_fbuf;
	lv_color_t		*lvki_cfb;
	lvaddr_t		 lvki_disp;
	lvaddr_t		 lvki_disp_drv;
	lvaddr_t		 lvki_kbd;
	lvaddr_t		 lvki_kbd_drv;
	lvaddr_t		 lvki_mouse;
	lvaddr_t		 lvki_mouse_drv;

	struct lvkobj_list	 lvki_objs;
	struct lvkstyle_list	 lvki_styles;
	struct lvkgroup_list	 lvki_groups;
	struct lvkbuf_list	 lvki_bufs;
	struct lvkchartser_list	 lvki_chart_series;
	struct lvkchartcur_list	 lvki_chart_cursors;
	struct lvkmeterind_list	 lvki_meter_inds;
	struct lvkmeterscl_list	 lvki_meter_scales;
	struct lvkspan_list	 lvki_spans;

	ErlNifEnv		*lvki_env;
	ERL_NIF_TERM		 lvki_msgref;
	ErlNifPid		 lvki_owner;
};

struct lvkstyle {
	struct lvkid		*lvks_kid;
	LIST_ENTRY(lvkstyle)	 lvks_entry;
	struct lvkinst		*lvks_inst;

	struct lvkhdl		*lvks_hdl;

	lvaddr_t		 lvks_ptr;
};

struct lvkgroup {
	struct lvkid		*lvkg_kid;
	LIST_ENTRY(lvkgroup)	 lvkg_entry;
	struct lvkinst		*lvkg_inst;

	struct lvkhdl		*lvkg_hdl;

	lvaddr_t		 lvkg_ptr;
};

struct lvkchartser {
	struct lvkid		*lvkcs_kid;
	LIST_ENTRY(lvkchartser)	 lvkcs_entry;
	struct lvkinst		*lvkcs_inst;

	struct lvkhdl		*lvkcs_hdl;

	lvaddr_t		 lvkcs_ptr;
};

struct lvkchartcur {
	struct lvkid		*lvkcc_kid;
	LIST_ENTRY(lvkchartcur)	 lvkcc_entry;
	struct lvkinst		*lvkcc_inst;

	struct lvkhdl		*lvkcc_hdl;

	lvaddr_t		 lvkcc_ptr;
};

struct lvkmeterind {
	struct lvkid		*lvkmi_kid;
	LIST_ENTRY(lvkmeterind)	 lvkmi_entry;
	struct lvkinst		*lvkmi_inst;

	struct lvkhdl		*lvkmi_hdl;

	lvaddr_t		 lvkmi_ptr;
};

struct lvkmeterscl {
	struct lvkid		*lvkms_kid;
	LIST_ENTRY(lvkmeterscl)	 lvkms_entry;
	struct lvkinst		*lvkms_inst;

	struct lvkhdl		*lvkms_hdl;

	lvaddr_t		 lvkms_ptr;
};

struct lvkspan {
	struct lvkid		*lvksp_kid;
	LIST_ENTRY(lvkspan)	 lvksp_entry;
	struct lvkinst		*lvksp_inst;

	struct lvkhdl		*lvksp_hdl;

	lvaddr_t		 lvksp_ptr;
};

struct lvkbuf {
	struct lvkid		*lvkb_kid;
	LIST_ENTRY(lvkbuf)	 lvkb_entry;
	struct lvkinst		*lvkb_inst;

	uint			 lvkb_teardown;
	struct lvkhdl		*lvkb_hdl;

	lvaddr_t		 lvkb_ptr;
	size_t			 lvkb_len;
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
	uint			 lvke_custom_msg;

	struct lvkhdl		*lvke_hdl;

	lv_event_code_t		 lvke_evt;
	uint64_t		 lvke_eudata;

	ErlNifEnv		*lvke_env;
	ERL_NIF_TERM		 lvke_msgref;
	ERL_NIF_TERM		 lvke_msg;
	ErlNifPid		 lvke_owner;
};

extern ErlNifResourceType *lvkid_hdl_rsrc;
extern ErlNifResourceType *lvkid_fbhdl_rsrc;

void lvk_open_resource_types(ErlNifEnv *env);

void lvkid_prefork(uint n);

struct lvkinst *lvkid_setup_inst(ErlNifPid owner, ERL_NIF_TERM msgref,
    uint width, uint height);

struct lvkhdl *lvkid_make_hdl(enum lvkh_type type, void *ptr, uint *do_release);

struct lvkbuf *lvk_copy_buf(struct lvkinst *, void *, size_t);

struct lvkevt *lvk_add_event(struct lvkobj *, lv_event_code_t, ErlNifPid owner,
    ERL_NIF_TERM msgref);
void lvk_remove_event(struct lvkevt *);

typedef void *lvk_call_func_t;
typedef void (*lvk_call_cb_t)(struct lvkid *, uint32_t err, enum arg_type,
    void *arg, void *priv);

void lvk_cmd(struct lvkid *kid, struct cdesc *cd, const void *data,
    size_t dlen, lvkcmd_cb_t cb, void *priv);
void lvk_cmd_defer(struct lvkid *kid, struct cdesc *cd, const void *data,
    size_t dlen, lvkcmd_cb_t cb, void *priv);

#define lvk_cmd0(kid, cd, cb, priv) \
	lvk_cmd((kid), (cd), NULL, 0, (cb), (priv))
#define lvk_cmd0_defer(kid, cd, cb, priv) \
	lvk_cmd_defer((kid), (cd), NULL, 0, (cb), (priv))

int lvk_cast(struct lvkid *kid, enum arg_type rt, lvk_call_func_t f, ...);
int lvk_icast(struct lvkinst *inst, enum arg_type rt, lvk_call_func_t f, ...);
int lvk_call_buf(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    size_t rtblen, lvk_call_func_t f, ...);
int lvk_call(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    enum arg_type rt, lvk_call_func_t f, ...);
int lvk_icall(struct lvkinst *inst, lvk_call_cb_t cb, void *priv,
    enum arg_type rt, lvk_call_func_t f, ...);

void lv_ieq_push_mouse(lv_indev_drv_t *drv, lv_indev_state_t state,
    lv_point_t point);
void lv_ieq_push_kbd(lv_indev_drv_t *drv, lv_indev_state_t state,
    uint32_t key);

void set_max_lvkids(size_t max);

#endif /* _LVKID_H */

