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

#include <assert.h>
#include <sys/errno.h>

#include "lvkid.h"
#include "lvkutils.h"
#include "lvcall.h"

static pthread_rwlock_t lvkids_lock = PTHREAD_RWLOCK_INITIALIZER;
static struct lvkid_list lvkids = LIST_HEAD_INITIALIZER(lvkids);

static pthread_mutex_t lv_mtx;

ErlNifResourceType *lvkid_hdl_rsrc;

static void
lvkobj_release(struct lvkobj *obj)
{
	/* must hold lvki_lock and lvk_lock for write */
	struct lvkevt *evt, *nevt;

	if (obj == NULL)
		return;
	if (obj->lvko_hdl != NULL)
		return;
	if (obj->lvko_ptr != 0)
		return;

	LIST_REMOVE(obj, lvko_entry);
	LIST_FOREACH_SAFE(evt, &obj->lvko_events, lvke_obj_entry, nevt) {
		LIST_REMOVE(evt, lvke_obj_entry);
		evt->lvke_obj = NULL;
		/*
		 * Now it's orphaned: it'll get freed either in hdl release or
		 * after the event ring entry marked ed_removed
		 */
	}
	free(obj);
}

static void lvkevt_teardown(struct lvkevt *evt);

static void
lvkid_hdl_dtor(ErlNifEnv *env, void *arg)
{
	struct lvkhdl *hdl = arg;
	struct lvkid *kid = hdl->lvkh_kid;
	struct lvkinst *inst;
	struct lvkobj *obj;
	struct lvkbuf *buf;
	struct lvkevt *evt;
	struct fbuf *fb;

	pthread_rwlock_wrlock(&kid->lvk_lock);
	inst = hdl->lvkh_inst;
	if (inst != NULL)
		pthread_rwlock_wrlock(&inst->lvki_lock);

	switch (hdl->lvkh_type) {
	case LVK_NONE:
		break;
	case LVK_FBUF:
		fb = hdl->lvkh_ptr;
		inst = fb->fb_priv;
		inst->lvki_fbhdl = NULL;
		break;
	case LVK_INST:
		inst = hdl->lvkh_ptr;
		inst->lvki_hdl = NULL;
		/* todo: instance deletion */
		break;
	case LVK_OBJ:
		obj = hdl->lvkh_ptr;
		obj->lvko_hdl = NULL;
		lvkobj_release(obj);
		break;
	case LVK_BUF:
		buf = hdl->lvkh_ptr;
		buf->lvkb_hdl = NULL;
		break;
	case LVK_EVT:
		evt = hdl->lvkh_ptr;
		evt->lvke_hdl = NULL;
		lvkevt_teardown(evt);
		break;
	}

	if (inst != NULL)
		pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);
}

void
lvk_open_resource_types(ErlNifEnv *env)
{
	lvkid_hdl_rsrc = enif_open_resource_type(env, NULL, "lv_handle",
	    lvkid_hdl_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
	    NULL);
}

struct lvkhdl *
lvkid_make_hdl(enum lvkh_type type, void *ptr, uint *do_release)
{
	struct lvkid *kid;
	struct lvkinst *inst;
	struct lvkobj *obj;
	struct lvkbuf *buf;
	struct lvkevt *evt;
	struct fbuf *fb;
	struct lvkhdl **phdl = NULL;

	switch (type) {
	case LVK_NONE:
		return (NULL);
	case LVK_FBUF:
		fb = ptr;
		inst = fb->fb_priv;
		kid = inst->lvki_kid;
		phdl = &inst->lvki_fbhdl;
		break;
	case LVK_INST:
		inst = ptr;
		kid = inst->lvki_kid;
		phdl = &inst->lvki_hdl;
		break;
	case LVK_OBJ:
		obj = ptr;
		kid = obj->lvko_kid;
		phdl = &obj->lvko_hdl;
		break;
	case LVK_BUF:
		buf = ptr;
		kid = buf->lvkb_kid;
		phdl = &buf->lvkb_hdl;
		break;
	case LVK_EVT:
		evt = ptr;
		kid = evt->lvke_kid;
		phdl = &evt->lvke_hdl;
		break;
	}
	assert(phdl != NULL);

	if (*phdl != NULL) {
		if (do_release != NULL) {
			*do_release = !((*phdl)->lvkh_given);
			(*phdl)->lvkh_given = 1;
		}
		return (*phdl);
	}

	*phdl = enif_alloc_resource(lvkid_hdl_rsrc, sizeof (struct lvkhdl));
	bzero(*phdl, sizeof (struct lvkhdl));
	(*phdl)->lvkh_kid = kid;
	(*phdl)->lvkh_type = type;
	(*phdl)->lvkh_ptr = ptr;

	switch (type) {
	case LVK_INST:
	case LVK_FBUF:
		(*phdl)->lvkh_inst = inst;
		break;
	case LVK_OBJ:
		(*phdl)->lvkh_inst = obj->lvko_inst;
		break;
	default:
		break;
	}

	if (do_release != NULL) {
		*do_release = 1;
		(*phdl)->lvkh_given = 1;
	}

	return (*phdl);
}

void
lv_group_send_text(lv_group_t *group, const char *text)
{
	lv_obj_t *act;
	act = lv_group_get_focused(group);
	if (act == NULL)
		return;
	if (lv_obj_has_state(act, LV_STATE_DISABLED))
		return;
	if (!lv_obj_has_class(act, &lv_textarea_class))
		return;
	lv_textarea_add_text(act, text);
}

struct lvinst {
	struct lvkid		*lvi_kid;
	struct fbuf		*lvi_fbuf;
	struct shmintf		*lvi_shm;
	uint			 lvi_fbidx;
	lv_disp_draw_buf_t	 lvi_draw_buf;
	lv_disp_drv_t		 lvi_disp_drv;
	lv_disp_t		*lvi_disp;
	lv_indev_drv_t		 lvi_kbd_drv;
	lv_indev_t		*lvi_kbd;
	lv_indev_drv_t		 lvi_mouse_drv;
	lv_indev_t		*lvi_mouse;
	uint64_t		 lvi_udata;
};

static void
lvkid_lv_wait_cb(lv_disp_drv_t *disp_drv)
{
	pthread_mutex_unlock(&lv_mtx);
	usleep(5000);
	pthread_mutex_lock(&lv_mtx);
}

static void
lvkid_lv_flush_cb(lv_disp_drv_t *disp_drv, const lv_area_t *area,
    lv_color_t *buf)
{
	struct lvinst *inst = disp_drv->user_data;
	struct fbuf *fb = inst->lvi_fbuf;
	struct fdesc fd;
	lv_disp_t *disp = _lv_refr_get_disp_refreshing();
	uint32_t fbidx_flag;
	uint i, last;
	const lv_area_t *a;
	struct shmintf *shm = inst->lvi_shm;

	assert(disp == inst->lvi_disp);

	fbidx_flag = inst->lvi_fbidx;
	if (buf == fb->fb_a)
		fbidx_flag |= FBIDX_A;
	if (buf == fb->fb_b)
		fbidx_flag |= FBIDX_B;

	last = 0;
	for (i = disp->inv_p - 1; i >= 0; --i) {
		if (disp->inv_area_joined[i])
			continue;
		last = i;
		break;
	}

	for (i = 0; i < disp->inv_p; ++i) {
		a = &disp->inv_areas[i];
		if (disp->inv_area_joined[i])
			continue;
		if (i == last)
			fbidx_flag |= FBIDX_SYNC;
		fd = (struct fdesc){
			.fd_fbidx_flag = fbidx_flag,
			.fd_udata = inst->lvi_udata,
			.fd_x1 = a->x1,
			.fd_x2 = a->x2,
			.fd_y1 = a->y1,
			.fd_y2 = a->y2,
		};
		shm_produce_flush(shm, &fd);
	}
}

static void
lvkid_lv_cmd_setup(struct lvkid *kid, struct shmintf *shm, struct cdesc *cd)
{
	struct cdesc_setup *cds = &cd->cd_setup;
	struct rdesc rd;
	struct fbuf *fb;
	struct lvinst *inst;
	size_t sz;

	pthread_mutex_lock(&lv_mtx);

	fb = &shm->si_fbuf[cds->cds_fbidx];
	if (fb->fb_state != FBUF_FREE) {
		pthread_mutex_unlock(&lv_mtx);
		rd = (struct rdesc){
			.rd_error = EBUSY,
			.rd_cookie = cd->cd_cookie
		};
		shm_produce_rsp(shm, &rd, 1);
		return;
	}
	fb->fb_state = FBUF_RUNNING;

	inst = calloc(1, sizeof (struct lvinst));
	fb->fb_priv = inst;
	inst->lvi_fbuf = fb;
	inst->lvi_fbidx = cds->cds_fbidx;
	inst->lvi_udata = cds->cds_udata;
	inst->lvi_kid = kid;
	inst->lvi_shm = shm;

	sz = cds->cds_w * cds->cds_h;
	assert((sizeof (lv_color_t) * sz) <= fb->fb_sz);
	lv_disp_draw_buf_init(&inst->lvi_draw_buf, fb->fb_a, fb->fb_b, sz);

	lv_disp_drv_init(&inst->lvi_disp_drv);
	inst->lvi_disp_drv.draw_buf = &inst->lvi_draw_buf;
	inst->lvi_disp_drv.flush_cb = lvkid_lv_flush_cb;
	inst->lvi_disp_drv.wait_cb = lvkid_lv_wait_cb;
	inst->lvi_disp_drv.hor_res = cds->cds_w;
	inst->lvi_disp_drv.ver_res = cds->cds_h;
	inst->lvi_disp_drv.direct_mode = 1;
	inst->lvi_disp_drv.user_data = inst;
	fprintf(stderr, "created disp_drv %p\n", &inst->lvi_disp_drv);

	inst->lvi_disp = lv_disp_drv_register(&inst->lvi_disp_drv);

	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie,
		.rd_setup = (struct rdesc_setup){
			.rds_disp_drv = (uint64_t)&inst->lvi_disp_drv,
			.rds_disp = (uint64_t)inst->lvi_disp,
		}
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void
lvkid_lv_cmd_flush_done(struct lvkid *kid, struct shmintf *shm,
    struct cdesc *cd)
{
	struct cdesc_flushdone *fd = &cd->cd_flush_done;
	struct rdesc rd;

	lv_disp_drv_t *drv = (lv_disp_drv_t *)fd->cdfd_disp_drv;
	pthread_mutex_lock(&lv_mtx);
	fprintf(stderr, "flush done on disp_drv %p\n", drv);
	lv_disp_flush_ready(drv);
	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void *
lvkid_lv_cmd_ring(void *arg)
{
	struct lvkid *kid = arg;
	struct cdesc *cd[8];
	struct shmintf *shm = kid->lvk_shm;
	uint ncd, i;

	while (1) {
		ncd = shm_consume_cmd(shm, cd, 8);

		fprintf(stderr, "lvkid %p: command %u\n", kid, cd[0]->cd_op);
		switch (cd[0]->cd_op) {
		case CMD_SETUP:
			assert(ncd == 1);
			lvkid_lv_cmd_setup(kid, shm, cd[0]);
			break;
		case CMD_TEARDOWN:
			break;
		case CMD_SET_UDATA:
			break;
		case CMD_CALL:
			assert(ncd == 1);
			pthread_mutex_lock(&lv_mtx);
			lv_do_call(shm, cd[0]);
			pthread_mutex_unlock(&lv_mtx);
			break;
		case CMD_COPY_BUF:
			break;
		case CMD_FREE_BUF:
			break;
		case CMD_SETUP_EVENT:
			break;
		case CMD_TEARDOWN_EVENT:
			break;
		case CMD_FLUSH_DONE:
			assert(ncd == 1);
			lvkid_lv_cmd_flush_done(kid, shm, cd[0]);
			break;
		case CMD_EXIT_CHILD:
			exit(0);
		}

		for (i = 0; i < ncd; ++i)
			shm_finish_cmd(cd[i]);
	}

	return (NULL);
}

static void *
lvkid_lv_startup(void *arg)
{
	struct lvkid *kid = arg;
	pthread_mutexattr_t mattr;

	pthread_mutexattr_init(&mattr);
	pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ERRORCHECK);

	pthread_mutex_init(&lv_mtx, &mattr);

	fprintf(stderr, "lvkid %p: starting up\n", kid);

	lv_init();
	pthread_create(&kid->lvk_rsp_th, NULL, lvkid_lv_cmd_ring, kid);
	while (1) {
		usleep(5000);
		pthread_mutex_lock(&lv_mtx);
		lv_task_handler();
		pthread_mutex_unlock(&lv_mtx);
	}
	return (NULL);
}

static struct lvkobj *
lvk_make_obj(struct lvkid *kid, struct lvkinst *inst, lvaddr_t ptr,
    const lv_obj_class_t *class)
{
	struct lvkobj *obj;
	struct cdesc cd;

	LIST_FOREACH(obj, &inst->lvki_objs, lvko_entry) {
		if (ptr == obj->lvko_ptr && class == obj->lvko_class) {
			return (obj);
		}
	}

	obj = calloc(1, sizeof (*obj));
	obj->lvko_kid = kid;
	obj->lvko_inst = inst;
	LIST_INSERT_HEAD(&inst->lvki_objs, obj, lvko_entry);

	obj->lvko_ptr = ptr;
	obj->lvko_class = class;

	LIST_INIT(&obj->lvko_events);

	obj->lvko_delevt = calloc(1, sizeof (struct lvkevt));
	obj->lvko_delevt->lvke_kid = kid;
	obj->lvko_delevt->lvke_obj = obj;
	obj->lvko_delevt->lvke_evt = LV_EVENT_DELETE;

	cd = (struct cdesc){
		.cd_op = CMD_SET_UDATA,
		.cd_set_udata = (struct cdesc_setudata){
			.cdsu_obj = ptr,
			.cdsu_udata = (uint64_t)obj
		}
	};
	lvk_cmd(kid, &cd, 1, NULL, NULL);

	cd = (struct cdesc){
		.cd_op = CMD_SETUP_EVENT,
		.cd_setup_event = (struct cdesc_setupev){
			.cdse_obj = ptr,
			.cdse_udata = (uint64_t)obj->lvko_delevt,
			.cdse_event = LV_EVENT_DELETE
		}
	};
	lvk_cmd(kid, &cd, 1, NULL, NULL);

	return (obj);
}

void
lvk_cmd(struct lvkid *kid, struct cdesc *cd, uint ncd, lvkcmd_cb_t cb,
    void *priv)
{
	struct lvkcmd *cmd;

	cmd = calloc(1, sizeof (*cmd));
	cmd->lvkc_kid = kid;
	cmd->lvkc_cb = cb;
	cmd->lvkc_priv = priv;

	LIST_INSERT_HEAD(&kid->lvk_cmds, cmd, lvkc_entry);
	cd[0].cd_cookie = (uint64_t)cmd;
	shm_produce_cmd(kid->lvk_shm, cd, ncd);
}

struct lvkcall {
	struct lvkid	*lvkc_kid;
	struct lvkinst	*lvkc_inst;
	enum arg_type	 lvkc_rt;
	lvk_call_cb_t	 lvkc_cb;
	void		*lvkc_priv;
};

static void
lvk_call_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct lvkcall *call = priv;
	struct lvkid *kid = call->lvkc_kid;
	struct lvkinst *inst = call->lvkc_inst;
	struct rdesc_return *rdr = &rd[0]->rd_return;
	struct rdesc_retbuf *rdrb;
	struct lvkobj *obj = NULL;
	const lv_obj_class_t *cls;
	uint64_t u64;
	uint32_t u32;
	uint16_t u16;
	uint8_t u8;
	uint i;

	if (call->lvkc_cb == NULL) {
		free(call);
		return;
	}

	if (rd[0]->rd_error) {
		assert(nrd == 1);
		(*call->lvkc_cb)(kid, rd[0]->rd_error, ARG_NONE, NULL,
		    call->lvkc_priv);
		free(call);
		return;
	}

	if (call->lvkc_rt == ARG_BUFPTR) {
		ErlNifBinary bin;
		size_t take, pos, rem;
		rdrb = &rd[0]->rd_return_buf;

		enif_alloc_binary(rdrb->rdrb_len, &bin);

		rem = rdrb->rdrb_len;
		pos = 0;
		take = sizeof (rdrb->rdrb_data);
		if (take > rem)
			take = rem;
		bcopy(rdrb->rdrb_data, &bin.data[pos], take);
		pos += take;
		rem -= take;

		for (i = 1; i < nrd; ++i) {
			take = sizeof (rd[i]->rd_data);
			if (take > rem)
				take = rem;
			assert(take > 0);
			bcopy(rd[i]->rd_data, &bin.data[pos], take);
			pos += take;
			rem -= take;
		}
		assert(rem == 0);

		(*call->lvkc_cb)(kid, 0, ARG_BUFPTR, &bin, call->lvkc_priv);

		free(call);
		return;
	}

	if (call->lvkc_rt == ARG_OBJPTR) {
		assert(inst != NULL);
		cls = (const lv_obj_class_t *)rdr->rdr_class;
		obj = (struct lvkobj *)rdr->rdr_udata;
		if (obj == NULL) {
			pthread_rwlock_wrlock(&inst->lvki_lock);
			obj = lvk_make_obj(kid, inst, rdr->rdr_val, cls);
			pthread_rwlock_unlock(&inst->lvki_lock);
		}

		(*call->lvkc_cb)(kid, 0, ARG_OBJPTR, &obj, call->lvkc_priv);

		free(call);
		return;
	}

	switch (call->lvkc_rt) {
	case ARG_NONE:
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, NULL, call->lvkc_priv);
		break;
	case ARG_OBJPTR:
	case ARG_BUFPTR:
		/* handled above */
		break;
	case ARG_PTR:
	case ARG_UINT64:
		u64 = rdr->rdr_val;
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &u64, call->lvkc_priv);
		break;
	case ARG_UINT32:
		assert(rdr->rdr_val < UINT32_MAX);
		u32 = rdr->rdr_val;
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &u32, call->lvkc_priv);
		break;
	case ARG_UINT16:
		assert(rdr->rdr_val < UINT16_MAX);
		u16 = rdr->rdr_val;
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &u16, call->lvkc_priv);
		break;
	case ARG_UINT8:
		assert(rdr->rdr_val < UINT8_MAX);
		u8 = rdr->rdr_val;
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &u8, call->lvkc_priv);
		break;
	}
	free(call);
}

static void
lvk_vcall(struct lvkid *kid, struct lvkinst *inst, lvk_call_cb_t cb,
    void *priv, enum arg_type rt, size_t rtblen, lvk_call_func_t f, va_list ap)
{
	struct lvkcall *call;
	struct lvkobj *obj;
	struct lvkbuf *buf;
	struct cdesc cd;
	uint8_t argtype[4];
	uint64_t arg[4] = { ARG_NONE };
	uintptr_t ptmp;
	uint i;

	if (inst == NULL)
		assert(rt != ARG_OBJPTR);

	for (i = 0; i < 4; ++i) {
		argtype[i] = va_arg(ap, enum arg_type);
		if (argtype[i] == ARG_NONE)
			break;
		switch (argtype[i]) {
		case ARG_BUFPTR:
			buf = va_arg(ap, struct lvkbuf *);
			arg[i] = buf->lvkb_ptr;
			break;
		case ARG_OBJPTR:
			obj = va_arg(ap, struct lvkobj *);
			arg[i] = obj->lvko_ptr;
			break;
		case ARG_PTR:
			ptmp = va_arg(ap, uintptr_t);
			arg[i] = ptmp;
			break;
		case ARG_UINT64:
			arg[i] = va_arg(ap, uint64_t);
			break;
		}
	}

	call = calloc(1, sizeof (*call));
	call->lvkc_kid = kid;
	call->lvkc_cb = cb;
	call->lvkc_priv = priv;
	call->lvkc_rt = rt;
	call->lvkc_inst = inst;

	cd = (struct cdesc){
		.cd_op = CMD_CALL,
		.cd_call = (struct cdesc_call){
			.cdc_func = (uint64_t)f,
			.cdc_rettype = rt,
			.cdc_rbuflen = rtblen,
		},
	};
	bcopy(arg, cd.cd_call.cdc_arg, sizeof (arg));
	bcopy(argtype, cd.cd_call.cdc_argtype, sizeof (argtype));
	lvk_cmd(kid, &cd, 1, lvk_call_cb, call);
}

void
lvk_cast(struct lvkid *kid, enum arg_type rt, lvk_call_func_t f, ...)
{
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, NULL, NULL, NULL, rt, 0, f, ap);
	va_end(ap);
}

void
lvk_icast(struct lvkinst *inst, enum arg_type rt, lvk_call_func_t f, ...)
{
	struct lvkid *kid = inst->lvki_kid;
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, inst, NULL, NULL, rt, 0, f, ap);
	va_end(ap);
}

void
lvk_call_buf(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    size_t rtblen, lvk_call_func_t f, ...)
{
	va_list ap;
	va_start(ap, f);
	lvk_vcall(kid, NULL, cb, priv, ARG_BUFPTR, rtblen, f, ap);
	va_end(ap);
}

void
lvk_call(struct lvkid *kid, lvk_call_cb_t cb, void *priv, enum arg_type rt,
    lvk_call_func_t f, ...)
{
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, NULL, cb, priv, rt, 0, f, ap);
	va_end(ap);
}

void
lvk_icall(struct lvkinst *inst, lvk_call_cb_t cb, void *priv, enum arg_type rt,
    lvk_call_func_t f, ...)
{
	struct lvkid *kid = inst->lvki_kid;
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, inst, cb, priv, rt, 0, f, ap);
	va_end(ap);
}

static const char *
evtype_to_atom(lv_event_code_t code)
{
	switch (code) {
	case LV_EVENT_PRESSED:			return ("pressed");
	case LV_EVENT_PRESSING:			return ("pressing");
	case LV_EVENT_PRESS_LOST:		return ("press_lost");
	case LV_EVENT_SHORT_CLICKED:		return ("short_clicked");
	case LV_EVENT_LONG_PRESSED:		return ("long_pressed");
	case LV_EVENT_LONG_PRESSED_REPEAT:	return ("long_pressed_repeat");
	case LV_EVENT_CLICKED:			return ("clicked");
	case LV_EVENT_RELEASED:			return ("released");
	case LV_EVENT_SCROLL_BEGIN:		return ("scroll_begin");
	case LV_EVENT_SCROLL_END:		return ("scroll_end");
	case LV_EVENT_SCROLL:			return ("scroll");
	case LV_EVENT_GESTURE:			return ("gesture");
	case LV_EVENT_KEY:			return ("key");
	case LV_EVENT_FOCUSED:			return ("focused");
	case LV_EVENT_DEFOCUSED:		return ("defocused");
	case LV_EVENT_LEAVE:			return ("leave");
	case LV_EVENT_HIT_TEST:			return ("hit_test");
	case LV_EVENT_COVER_CHECK:		return ("cover_check");
	case LV_EVENT_REFR_EXT_DRAW_SIZE:	return ("refr_ext_draw_size");
	case LV_EVENT_DRAW_MAIN_BEGIN:		return ("draw_main_begin");
	case LV_EVENT_DRAW_MAIN:		return ("draw_main");
	case LV_EVENT_DRAW_MAIN_END:		return ("draw_main_end");
	case LV_EVENT_DRAW_POST_BEGIN:		return ("draw_post_begin");
	case LV_EVENT_DRAW_POST:		return ("draw_post");
	case LV_EVENT_DRAW_POST_END:		return ("draw_post_end");
	case LV_EVENT_DRAW_PART_BEGIN:		return ("draw_part_begin");
	case LV_EVENT_DRAW_PART_END:		return ("draw_part_end");
	case LV_EVENT_VALUE_CHANGED:		return ("value_changed");
	case LV_EVENT_INSERT:			return ("insert");
	case LV_EVENT_REFRESH:			return ("refresh");
	case LV_EVENT_READY:			return ("ready");
	case LV_EVENT_CANCEL:			return ("cancel");
	case LV_EVENT_DELETE:			return ("delete");
	case LV_EVENT_CHILD_CHANGED:		return ("child_changed");
	case LV_EVENT_CHILD_CREATED:		return ("child_created");
	case LV_EVENT_CHILD_DELETED:		return ("child_deleted");
	case LV_EVENT_SCREEN_UNLOAD_START:	return ("screen_unload_start");
	case LV_EVENT_SCREEN_LOAD_START:	return ("screen_load_start");
	case LV_EVENT_SCREEN_LOADED:		return ("screen_loaded");
	case LV_EVENT_SCREEN_UNLOADED:		return ("screen_unloaded");
	case LV_EVENT_SIZE_CHANGED:		return ("size_changed");
	case LV_EVENT_STYLE_CHANGED:		return ("style_changed");
	case LV_EVENT_LAYOUT_CHANGED:		return ("layout_changed");
	case LV_EVENT_GET_SELF_SIZE:		return ("get_self_size");
	default: return ("unknown");
	}
}

static void *
lvkid_erl_flush_ring(void *arg)
{
	struct lvkid *kid = arg;
	struct fdesc *fd;
	struct shmintf *shm = kid->lvk_shm;
	struct lvkinst *inst;
	struct fbuf *fb;
	struct lvkhdl *hdl;
	lv_area_t tile;
	lv_area_t rect;
	lv_color_t *buf;
	uint fbidx;
	ErlNifEnv *env;
	ERL_NIF_TERM ref, msg, pixdata;
	ErlNifPid owner;
	uint do_release;

	fprintf(stderr, "in flush_ring\n");

	while (1) {
		shm_consume_flush(shm, &fd);
		fbidx = fd->fd_fbidx_flag & FBIDX_IDX_MASK;
		fprintf(stderr, "got fdesc for idx %u\n", fbidx);
		assert(fbidx < shm->si_nfbuf);
		fb = &shm->si_fbuf[fbidx];

		pthread_rwlock_rdlock(&kid->lvk_lock);
		if (fb->fb_state != FBUF_RUNNING &&
		    fb->fb_state != FBUF_SETTING_UP) {
		    	pthread_rwlock_unlock(&kid->lvk_lock);
			goto next;
		}

		inst = fb->fb_priv;
		assert(inst == (void *)fd->fd_udata);
		assert(inst->lvki_kid == kid);
		assert(inst->lvki_fbuf == fb);

		buf = NULL;
		if (fd->fd_fbidx_flag & FBIDX_A)
			buf = fb->fb_a;
		if (fd->fd_fbidx_flag & FBIDX_B)
			buf = fb->fb_b;
		assert(buf != NULL);

		pthread_rwlock_wrlock(&inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_FBUF, fb, &do_release);
		pthread_rwlock_unlock(&inst->lvki_lock);

		rect.x1 = fd->fd_x1;
		rect.x2 = fd->fd_x2;
		rect.y1 = fd->fd_y1;
		rect.y2 = fd->fd_y2;
		bzero(&tile, sizeof (tile));
		while (lvk_next_tile(&rect, &tile)) {
			pthread_rwlock_wrlock(&inst->lvki_lock);
			env = inst->lvki_env;
			ref = inst->lvki_msgref;
			inst->lvki_env = enif_alloc_env();
			inst->lvki_msgref = enif_make_copy(inst->lvki_env, ref);
			owner = inst->lvki_owner;
			pthread_rwlock_unlock(&inst->lvki_lock);

			pixdata = lvk_tile_to_iolist(env, fb, buf, hdl, &tile);
			msg = enif_make_tuple4(env,
			    ref,
			    enif_make_atom(env, "flush"),
			    enif_make_tuple4(env,
			    	enif_make_int(env, tile.x1),
			    	enif_make_int(env, tile.y1),
			    	enif_make_int(env, tile.x2),
			    	enif_make_int(env, tile.y2)),
			    pixdata);
			enif_send(NULL, &owner, env, msg);
			enif_free_env(env);
		}

		if (fd->fd_fbidx_flag & FBIDX_SYNC) {
			pthread_rwlock_wrlock(&inst->lvki_lock);
			env = inst->lvki_env;
			ref = inst->lvki_msgref;
			inst->lvki_env = enif_alloc_env();
			inst->lvki_msgref = enif_make_copy(inst->lvki_env, ref);
			owner = inst->lvki_owner;
			pthread_rwlock_unlock(&inst->lvki_lock);

			msg = enif_make_tuple2(env,
			    ref,
			    enif_make_atom(env, "flush_sync"));
			enif_send(NULL, &owner, env, msg);
			enif_free_env(env);
		}

		if (do_release)
			enif_release_resource(hdl);

		pthread_rwlock_unlock(&kid->lvk_lock);
next:
		shm_finish_flush(fd);
	}

	return (NULL);
}

static void *
lvkid_erl_evt_ring(void *arg)
{
	struct lvkid *kid = arg;
	struct edesc *ed;
	struct shmintf *shm = kid->lvk_shm;
	struct lvkevt *evt;
	struct lvkobj *obj, *tgt, *ctgt;
	uint do_release;
	struct lvkinst *inst;
	struct lvkhdl *tgthdl = NULL, *ctgthdl = NULL;
	ErlNifEnv *env;
	ERL_NIF_TERM ref, msg, tgterl, ctgterl;
	ErlNifPid owner;

	while (1) {
		shm_consume_evt(shm, &ed);
		evt = (struct lvkevt *)ed->ed_cookie;
		assert(evt->lvke_kid == kid);

		if (ed->ed_removed) {
			pthread_rwlock_wrlock(&kid->lvk_lock);
			if (evt->lvke_hdl != NULL) {
				evt->lvke_hdl->lvkh_type = LVK_NONE;
				evt->lvke_hdl->lvkh_ptr = 0;
			}
			LIST_REMOVE(evt, lvke_kid_entry);
			pthread_rwlock_unlock(&kid->lvk_lock);
			free(evt);
			goto next;
		}

		obj = evt->lvke_obj;
		if (obj == NULL)
			goto next;

		inst = obj->lvko_inst;
		/* delevt is immutable */
		if (evt == obj->lvko_delevt) {
			pthread_rwlock_wrlock(&kid->lvk_lock);
			pthread_rwlock_wrlock(&inst->lvki_lock);
			obj->lvko_ptr = 0;
			obj->lvko_class = NULL;
			obj->lvko_delevt = NULL;
			lvkobj_release(obj);
			pthread_rwlock_unlock(&inst->lvki_lock);
			pthread_rwlock_unlock(&kid->lvk_lock);
			goto next;
		}

		pthread_rwlock_rdlock(&inst->lvki_lock);
		env = evt->lvke_env;
		ref = evt->lvke_msgref;
		evt->lvke_env = enif_alloc_env();
		evt->lvke_msgref = enif_make_copy(evt->lvke_env, ref);
		owner = evt->lvke_owner;

		if (ed->ed_target_udata != 0) {
			tgt = (struct lvkobj *)ed->ed_target_udata;
			assert(tgt->lvko_kid == kid);
			assert(tgt->lvko_ptr == ed->ed_target);
		} else {
			tgt = NULL;
			LIST_FOREACH(obj, &inst->lvki_objs, lvko_entry) {
				if (obj->lvko_ptr == ed->ed_target) {
					tgt = obj;
					break;
				}
			}
		}
		if (ed->ed_ctarget_udata != 0) {
			ctgt = (struct lvkobj *)ed->ed_ctarget_udata;
			assert(ctgt->lvko_kid == kid);
			assert(ctgt->lvko_ptr == ed->ed_ctarget);
		} else {
			ctgt = NULL;
			LIST_FOREACH(obj, &inst->lvki_objs, lvko_entry) {
				if (obj->lvko_ptr == ed->ed_ctarget) {
					ctgt = obj;
					break;
				}
			}
		}
		if (tgt != NULL)
			tgthdl = tgt->lvko_hdl;
		if (ctgt != NULL)
			ctgthdl = ctgt->lvko_hdl;
		pthread_rwlock_unlock(&inst->lvki_lock);

		tgterl = enif_make_atom(env, "undefined");
		ctgterl = enif_make_atom(env, "undefined");

		if ((tgt != NULL && tgthdl == NULL) ||
		    (ctgt != NULL && ctgthdl == NULL)) {
			pthread_rwlock_wrlock(&inst->lvki_lock);
			if (tgt != NULL) {
				tgthdl = lvkid_make_hdl(LVK_OBJ, tgt,
				    &do_release);
				tgterl = enif_make_resource(env, tgthdl);
				if (do_release)
					enif_release_resource(tgthdl);
			}
			if (ctgt != NULL) {
				ctgthdl = lvkid_make_hdl(LVK_OBJ, ctgt,
				    &do_release);
				ctgterl = enif_make_resource(env, ctgthdl);
				if (do_release)
					enif_release_resource(ctgthdl);
			}
			pthread_rwlock_unlock(&inst->lvki_lock);
		}

		msg = enif_make_tuple5(env,
		    ref,
		    enif_make_atom(env, "event"),
		    enif_make_atom(env, evtype_to_atom(ed->ed_code)),
		    tgterl,
		    ctgterl);

		enif_send(NULL, &owner, env, msg);
		enif_free_env(env);

next:
		shm_finish_evt(ed);
	}

	return (NULL);
}

static void *
lvkid_erl_rsp_ring(void *arg)
{
	struct lvkid *kid = arg;
	struct rdesc *rd[8];
	struct shmintf *shm = kid->lvk_shm;
	struct lvkcmd *cmd;
	uint nrd, i;

	while (1) {
		nrd = shm_consume_rsp(shm, rd, 8);
		cmd = (struct lvkcmd *)rd[0]->rd_cookie;
		assert(cmd->lvkc_kid == kid);
		pthread_rwlock_wrlock(&kid->lvk_lock);
		LIST_REMOVE(cmd, lvkc_entry);
		pthread_rwlock_unlock(&kid->lvk_lock);
		if (cmd->lvkc_cb != NULL)
			(*cmd->lvkc_cb)(rd, nrd, cmd->lvkc_priv);
		free(cmd);
		for (i = 0; i < nrd; ++i)
			shm_finish_rsp(rd[i]);
	}

	return (NULL);
}

static void
lvkid_new(void)
{
	struct lvkid *kid;

	kid = calloc(1, sizeof(struct lvkid));
	assert(kid != NULL);
	pthread_rwlock_init(&kid->lvk_lock, NULL);
	LIST_INIT(&kid->lvk_insts);

	kid->lvk_shm = alloc_shmintf();
	assert(kid->lvk_shm != NULL);

	kid->lvk_pid = shm_fork(kid->lvk_shm);
	assert(kid->lvk_pid != -1);
	if (kid->lvk_pid == 0) {
		pthread_create(&kid->lvk_lv_th, NULL, lvkid_lv_startup, kid);
		pthread_join(kid->lvk_lv_th, NULL);
		exit(0);
	}

	pthread_rwlock_wrlock(&lvkids_lock);
	LIST_INSERT_HEAD(&lvkids, kid, lvk_entry);
	pthread_rwlock_unlock(&lvkids_lock);

	pthread_create(&kid->lvk_rsp_th, NULL, lvkid_erl_rsp_ring, kid);
	pthread_create(&kid->lvk_evt_th, NULL, lvkid_erl_evt_ring, kid);
	pthread_create(&kid->lvk_flush_th, NULL, lvkid_erl_flush_ring, kid);
}

static void
lvk_inst_setup_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct lvkinst *inst = priv;
	struct lvkid *kid = inst->lvki_kid;
	struct rdesc_setup *s = &rd[0]->rd_setup;
	ErlNifEnv *env;
	ERL_NIF_TERM ref, msg;
	ErlNifPid owner;

	assert(nrd == 1);

	pthread_rwlock_wrlock(&kid->lvk_lock);
	pthread_rwlock_wrlock(&inst->lvki_lock);
	inst->lvki_disp = s->rds_disp;
	inst->lvki_disp_drv = s->rds_disp_drv;
	inst->lvki_kbd = s->rds_kbd;
	inst->lvki_kbd_drv = s->rds_kbd_drv;
	inst->lvki_mouse = s->rds_mouse;
	inst->lvki_mouse_drv = s->rds_mouse_drv;
	inst->lvki_state = LVKINST_ALIVE;

	inst->lvki_fbuf->fb_state = FBUF_RUNNING;

	env = inst->lvki_env;
	ref = inst->lvki_msgref;
	inst->lvki_env = enif_alloc_env();
	inst->lvki_msgref = enif_make_copy(inst->lvki_env, ref);
	owner = inst->lvki_owner;
	pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);

	msg = enif_make_tuple2(env,
	    ref,
	    enif_make_atom(env, "setup_done"));
	enif_send(NULL, &owner, env, msg);
	enif_free_env(env);
}

struct lvkinst *
lvkid_setup_inst(ErlNifPid owner, ERL_NIF_TERM msgref, uint width, uint height)
{
	struct lvkid *kid, *nkid = NULL;
	struct cdesc cd;
	struct lvkinst *inst;
	struct shmintf *shm;
	uint i;

	while (nkid == NULL) {
		pthread_rwlock_rdlock(&lvkids_lock);
		LIST_FOREACH(kid, &lvkids, lvk_entry) {
			pthread_rwlock_wrlock(&kid->lvk_lock);
			if (kid->lvk_busy < kid->lvk_shm->si_nfbuf) {
				++kid->lvk_busy;
				nkid = kid;
				break;
			}
			pthread_rwlock_unlock(&kid->lvk_lock);
		}
		pthread_rwlock_unlock(&lvkids_lock);

		if (nkid == NULL)
			lvkid_new();
	}

	inst = calloc(1, sizeof(struct lvkinst));
	assert(inst != NULL);
	inst->lvki_kid = nkid;

	pthread_rwlock_init(&inst->lvki_lock, NULL);
	pthread_rwlock_wrlock(&inst->lvki_lock);

	shm = nkid->lvk_shm;
	for (i = 0; i < shm->si_nfbuf; ++i) {
		if (shm->si_fbuf[i].fb_state == FBUF_FREE)
			break;
	}
	assert(shm->si_fbuf[i].fb_state == FBUF_FREE);
	inst->lvki_fbuf = &shm->si_fbuf[i];

	inst->lvki_fbuf->fb_w = width;
	inst->lvki_fbuf->fb_h = height;
	inst->lvki_fbuf->fb_priv = inst;
	inst->lvki_fbuf->fb_state = FBUF_SETTING_UP;

	LIST_INSERT_HEAD(&nkid->lvk_insts, inst, lvki_entry);
	pthread_rwlock_unlock(&nkid->lvk_lock);

	LIST_INIT(&inst->lvki_objs);

	inst->lvki_env = enif_alloc_env();
	inst->lvki_owner = owner;
	inst->lvki_msgref = enif_make_copy(inst->lvki_env, msgref);

	inst->lvki_state = LVKINST_STARTING;
	cd = (struct cdesc){
		.cd_op = CMD_SETUP,
		.cd_setup = (struct cdesc_setup){
			.cds_fbidx = i,
			.cds_w = width,
			.cds_h = height,
			.cds_udata = (uint64_t)inst,
		}
	};
	lvk_cmd(kid, &cd, 1, lvk_inst_setup_cb, inst);

	pthread_rwlock_unlock(&inst->lvki_lock);

	return (inst);
}

static void
lvkevt_teardown(struct lvkevt *evt)
{
	struct cdesc cd;
	struct lvkid *kid = evt->lvke_kid;
	lvaddr_t ptr = 0;
	if (evt->lvke_teardown)
		return;
	if (evt->lvke_obj != NULL)
		ptr = evt->lvke_obj->lvko_ptr;
	evt->lvke_teardown = 1;
	cd = (struct cdesc){
		.cd_op = CMD_TEARDOWN_EVENT,
		.cd_teardown_event = (struct cdesc_teardownev){
			.cdte_obj = ptr,
			.cdte_udata = (uint64_t)evt
		}
	};
	lvk_cmd(kid, &cd, 1, NULL, NULL);
}
