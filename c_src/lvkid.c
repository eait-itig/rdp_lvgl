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
static pthread_mutex_t lv_flush_mtx;
static pthread_cond_t lv_flush_cond;

struct lv_event_udata {
	struct lvkid			*leu_kid;
	struct shmintf			*leu_shm;
	lv_obj_t			*leu_obj;
	LIST_ENTRY(lv_event_udata)	 leu_entry;
	uint64_t			 leu_udata;
};
LIST_HEAD(lv_event_udata_list, lv_event_udata);
static struct lv_event_udata_list leus;

struct input_event {
	TAILQ_ENTRY(input_event)	ie_entry;
	lv_indev_data_t			ie_data;
};
TAILQ_HEAD(input_event_q, input_event);
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
	uint			 lvi_in_wait;
	struct input_event_q	 lvi_mouse_q;
	struct input_event_q	 lvi_kbd_q;
	lv_obj_t		*lvi_cursor;
};

ErlNifResourceType *lvkid_hdl_rsrc;

static void lvkinst_teardown(struct lvkinst *inst);
static void lvkevt_teardown(struct lvkevt *evt);

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

	obj->lvko_delevt->lvke_obj = NULL;
	assert(obj->lvko_delevt->lvke_hdl == NULL);
	lvkevt_teardown(obj->lvko_delevt);

	LIST_FOREACH_SAFE(evt, &obj->lvko_events, lvke_obj_entry, nevt) {
		LIST_REMOVE(evt, lvke_obj_entry);
		evt->lvke_obj = NULL;
		if (evt->lvke_hdl != NULL)
			evt->lvke_hdl->lvkh_inst = NULL;
		lvkevt_teardown(evt);
		/*
		 * Now it's orphaned: it'll get freed either in hdl release or
		 * after the event ring entry marked ed_removed
		 */
	}
	free(obj);
}

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
		assert(inst == fb->fb_priv);
		assert(inst->lvki_fbhdl == hdl);
		inst->lvki_fbhdl = NULL;
		break;
	case LVK_INST:
		assert(inst == hdl->lvkh_ptr);
		assert(inst->lvki_hdl == hdl);
		inst->lvki_hdl = NULL;
		lvkinst_teardown(inst);
		break;
	case LVK_OBJ:
		obj = hdl->lvkh_ptr;
		assert(obj->lvko_hdl == hdl);
		obj->lvko_hdl = NULL;
		lvkobj_release(obj);
		break;
	case LVK_BUF:
		buf = hdl->lvkh_ptr;
		assert(buf->lvkb_hdl == hdl);
		buf->lvkb_hdl = NULL;
		break;
	case LVK_EVT:
		evt = hdl->lvkh_ptr;
		assert(evt->lvke_hdl == hdl);
		evt->lvke_hdl = NULL;
		lvkevt_teardown(evt);
		break;
	}

	bzero(hdl, sizeof (*hdl));
	hdl->lvkh_type = LVK_NONE;

	if (inst != NULL)
		pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);
}

static void
lvkid_hdl_mon_down(ErlNifEnv *env, void *arg, ErlNifPid *pid,
    ErlNifMonitor *mon)
{
	struct lvkhdl *hdl = arg;
	struct lvkid *kid = hdl->lvkh_kid;
	struct lvkinst *inst;

	pthread_rwlock_wrlock(&kid->lvk_lock);
	inst = hdl->lvkh_inst;
	if (inst != NULL)
		pthread_rwlock_wrlock(&inst->lvki_lock);

	if (hdl->lvkh_type == LVK_INST) {
		assert(hdl->lvkh_ptr == inst);
		lvkinst_teardown(inst);
	}

	if (inst != NULL)
		pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);
}

static ErlNifResourceTypeInit lvkid_hdl_ops = {
	.dtor = lvkid_hdl_dtor,
	.down = lvkid_hdl_mon_down,
};

void
lvk_open_resource_types(ErlNifEnv *env)
{
	lvkid_hdl_rsrc = enif_open_resource_type_x(env, "lv_handle",
	    &lvkid_hdl_ops, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
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

void
lv_disp_scr_load(lv_disp_t *disp, lv_obj_t *scr)
{
	lv_disp_set_default(disp);
	lv_scr_load(scr);
}

lv_obj_t *
lv_disp_obj_create(lv_disp_t *disp, lv_obj_t *parent)
{
	lv_disp_set_default(disp);
	return (lv_obj_create(parent));
}

static void
lvkid_lv_wait_cb(lv_disp_drv_t *disp_drv)
{
	struct lvinst *inst = disp_drv->user_data;
	pthread_mutex_lock(&lv_flush_mtx);
	inst->lvi_in_wait = 1;
	while (disp_drv->draw_buf->flushing) {
		pthread_cond_wait(&lv_flush_cond, &lv_flush_mtx);
	}
	inst->lvi_in_wait = 0;
	pthread_mutex_unlock(&lv_flush_mtx);
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
	uint y;
	const lv_area_t *a;
	lv_color_t *obuf;
	struct shmintf *shm = inst->lvi_shm;

	assert(disp == inst->lvi_disp);

	fbidx_flag = inst->lvi_fbidx;
	if (buf == fb->fb_a) {
		fbidx_flag |= FBIDX_A;
		obuf = fb->fb_b;
	} else if (buf == fb->fb_b) {
		fbidx_flag |= FBIDX_B;
		obuf = fb->fb_a;
	} else {
		assert(0);
		return;
	}

	if (disp->inv_p == 0) {
		lv_disp_flush_ready(disp_drv);
		return;
	}

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
	shm_ring_doorbell(shm);

	for (i = 0; i < disp->inv_p; ++i) {
		a = &disp->inv_areas[i];
		if (disp->inv_area_joined[i])
			continue;
		for (y = a->y1; y < a->y2; ++y) {
			bcopy(&buf[fb->fb_w * y + a->x1],
			    &obuf[fb->fb_w * y + a->x1],
			    sizeof (lv_color_t) * (a->x2 - a->x1));
		}
	}
}

void
lv_ieq_push_mouse(lv_indev_drv_t *drv, lv_indev_state_t state,
    lv_point_t point)
{
	struct lvinst *inst = drv->user_data;
	struct input_event *ev;

	assert(drv == &inst->lvi_mouse_drv);

	ev = calloc(1, sizeof (*ev));

	ev->ie_data.state = state;
	ev->ie_data.point = point;

	TAILQ_INSERT_TAIL(&inst->lvi_mouse_q, ev, ie_entry);
}

void
lv_ieq_push_kbd(lv_indev_drv_t *drv, lv_indev_state_t state,
    uint32_t key)
{
	struct lvinst *inst = drv->user_data;
	struct input_event *ev;

	assert(drv == &inst->lvi_kbd_drv);

	ev = calloc(1, sizeof (*ev));

	ev->ie_data.state = state;
	ev->ie_data.key = key;

	TAILQ_INSERT_TAIL(&inst->lvi_kbd_q, ev, ie_entry);
}

static void
lvkid_lv_read_mouse_cb(lv_indev_drv_t *drv, lv_indev_data_t *data)
{
	struct lvinst *inst = drv->user_data;
	struct input_event *ev;

	ev = TAILQ_FIRST(&inst->lvi_mouse_q);
	if (ev == NULL) {
		data->state = LV_INDEV_STATE_RELEASED;
		return;
	}
	TAILQ_REMOVE(&inst->lvi_mouse_q, ev, ie_entry);

	data->point = ev->ie_data.point;
	data->state = ev->ie_data.state;
	free(ev);

	if (!TAILQ_EMPTY(&inst->lvi_mouse_q))
		data->continue_reading = 1;
}

static void
lvkid_lv_read_kbd_cb(lv_indev_drv_t *drv, lv_indev_data_t *data)
{
	struct lvinst *inst = drv->user_data;
	struct input_event *ev;

	ev = TAILQ_FIRST(&inst->lvi_kbd_q);
	if (ev == NULL) {
		data->state = LV_INDEV_STATE_RELEASED;
		return;
	}
	TAILQ_REMOVE(&inst->lvi_kbd_q, ev, ie_entry);

	data->key = ev->ie_data.key;
	data->state = ev->ie_data.state;
	free(ev);

	if (!TAILQ_EMPTY(&inst->lvi_kbd_q))
		data->continue_reading = 1;
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
	fb->fb_w = cds->cds_w;
	fb->fb_h = cds->cds_h;

	inst = calloc(1, sizeof (struct lvinst));
	fb->fb_priv = inst;
	inst->lvi_fbuf = fb;
	inst->lvi_fbidx = cds->cds_fbidx;
	inst->lvi_udata = cds->cds_udata;
	inst->lvi_kid = kid;
	inst->lvi_shm = shm;
	TAILQ_INIT(&inst->lvi_mouse_q);
	TAILQ_INIT(&inst->lvi_kbd_q);

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

	inst->lvi_disp = lv_disp_drv_register(&inst->lvi_disp_drv);
	fprintf(stderr, "created disp_drv %p => %p\r\n", &inst->lvi_disp_drv,
	    inst->lvi_disp);

	lv_indev_drv_init(&inst->lvi_mouse_drv);
	inst->lvi_mouse_drv.type = LV_INDEV_TYPE_POINTER;
	inst->lvi_mouse_drv.read_cb = lvkid_lv_read_mouse_cb;
	inst->lvi_mouse_drv.user_data = inst;
	inst->lvi_mouse_drv.disp = inst->lvi_disp;

	inst->lvi_mouse = lv_indev_drv_register(&inst->lvi_mouse_drv);
	fprintf(stderr, "created mouse_drv %p => %p\r\n", &inst->lvi_mouse_drv,
	    inst->lvi_mouse);

	inst->lvi_cursor = lv_img_create(lv_disp_get_layer_sys(inst->lvi_disp));
	lv_img_set_src(inst->lvi_cursor, LV_SYMBOL_GPS);
	lv_indev_set_cursor(inst->lvi_mouse, inst->lvi_cursor);

	lv_indev_drv_init(&inst->lvi_kbd_drv);
	inst->lvi_kbd_drv.type = LV_INDEV_TYPE_KEYPAD;
	inst->lvi_kbd_drv.read_cb = lvkid_lv_read_kbd_cb;
	inst->lvi_kbd_drv.user_data = inst;
	inst->lvi_kbd_drv.disp = inst->lvi_disp;

	inst->lvi_kbd = lv_indev_drv_register(&inst->lvi_kbd_drv);
	fprintf(stderr, "created kbd_drv %p => %p\r\n", &inst->lvi_kbd_drv,
	    inst->lvi_kbd);

	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie,
		.rd_setup = (struct rdesc_setup){
			.rds_disp_drv 	= (uint64_t)&inst->lvi_disp_drv,
			.rds_disp 	= (uint64_t)inst->lvi_disp,
			.rds_mouse_drv 	= (uint64_t)&inst->lvi_mouse_drv,
			.rds_mouse 	= (uint64_t)inst->lvi_mouse,
			.rds_kbd_drv 	= (uint64_t)&inst->lvi_kbd_drv,
			.rds_kbd 	= (uint64_t)inst->lvi_kbd,
		}
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void
lvkid_lv_cmd_teardown(struct lvkid *kid, struct shmintf *shm, struct cdesc *cd)
{
	struct cdesc_teardown *cdt = &cd->cd_teardown;
	struct rdesc rd;
	struct fbuf *fb;
	struct lvinst *inst;
	lv_disp_drv_t *disp_drv;

	pthread_mutex_lock(&lv_mtx);
	pthread_mutex_lock(&lv_flush_mtx);

	disp_drv = (lv_disp_drv_t *)cdt->cdt_disp_drv;
	inst = disp_drv->user_data;
	assert(inst->lvi_kid == kid);
	assert(inst->lvi_shm == shm);
	assert(&inst->lvi_disp_drv == disp_drv);

	fprintf(stderr, "processing teardown for %p\r\n", inst);

	fb = inst->lvi_fbuf;
	fb->fb_state = FBUF_FREE;
	fb->fb_priv = NULL;

	if (inst->lvi_kbd != NULL)
		lv_indev_delete(inst->lvi_kbd);
	if (inst->lvi_mouse != NULL)
		lv_indev_delete(inst->lvi_mouse);

	/* lv_disp_remove doesn't free this memory :( */
	disp_drv->draw_ctx_deinit(disp_drv, disp_drv->draw_ctx);
	lv_mem_free(disp_drv->draw_ctx);
	disp_drv->draw_ctx = NULL;

	lv_disp_remove(inst->lvi_disp);

	free(inst);

	pthread_mutex_unlock(&lv_flush_mtx);
	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void
lvkid_lv_cmd_set_udata(struct lvkid *kid, struct shmintf *shm,
    struct cdesc *cd)
{
	struct cdesc_setudata *cdsu = &cd->cd_set_udata;
	struct rdesc rd;
	lv_obj_t *obj;

	pthread_mutex_lock(&lv_mtx);
	obj = (lv_obj_t *)cdsu->cdsu_obj;
	lv_obj_set_user_data(obj, (void *)cdsu->cdsu_udata);
	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void
lvkid_lv_event_cb(lv_event_t *ev)
{
	struct lv_event_udata *leu = lv_event_get_user_data(ev);
	struct shmintf *shm = leu->leu_shm;
	struct edesc ed;
	lv_obj_t *target, *ctarget;
	void *t_udata, *ct_udata;
	void *param;
	uint64_t pdata = 0;

	target = lv_event_get_target(ev);
	ctarget = lv_event_get_current_target(ev);
	t_udata = lv_obj_get_user_data(target);
	ct_udata = lv_obj_get_user_data(ctarget);
	param = lv_event_get_param(ev);

	switch (lv_event_get_code(ev)) {
	case LV_EVENT_KEY:
		pdata = *(uint32_t *)param;
		break;
	default:
		break;
	}

	ed = (struct edesc){
		.ed_code = lv_event_get_code(ev),
		.ed_udata = leu->leu_udata,
		.ed_target = (uint64_t)target,
		.ed_target_udata = (uint64_t)t_udata,
		.ed_ctarget = (uint64_t)ctarget,
		.ed_ctarget_udata = (uint64_t)ct_udata,
		.ed_param = (uint64_t)param,
		.ed_param_data = pdata
	};
	shm_produce_evt(shm, &ed);
	shm_ring_doorbell(shm);
}

static void
lvkid_lv_cmd_setup_event(struct lvkid *kid, struct shmintf *shm,
    struct cdesc *cd)
{
	struct cdesc_setupev *cdse = &cd->cd_setup_event;
	struct rdesc rd;
	lv_obj_t *obj;
	struct lv_event_udata *leu;

	leu = calloc(1, sizeof (*leu));
	leu->leu_kid = kid;
	leu->leu_shm = shm;
	leu->leu_udata = cdse->cdse_udata;
	obj = (lv_obj_t *)cdse->cdse_obj;
	leu->leu_obj = obj;

	pthread_mutex_lock(&lv_mtx);
	lv_obj_add_event_cb(obj, lvkid_lv_event_cb, cdse->cdse_event, leu);
	LIST_INSERT_HEAD(&leus, leu, leu_entry);
	pthread_mutex_unlock(&lv_mtx);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie
	};
	shm_produce_rsp(shm, &rd, 1);
}

static void
lvkid_lv_cmd_teardown_event(struct lvkid *kid, struct shmintf *shm,
    struct cdesc *cd)
{
	struct cdesc_teardownev *cdte = &cd->cd_teardown_event;
	struct rdesc rd;
	struct edesc ed;
	struct lv_event_udata *leu, *tleu;
	lv_obj_t *obj;
	void *obj_udata = NULL;

	pthread_mutex_lock(&lv_mtx);
	leu = (struct lv_event_udata *)cdte->cdte_eudata;
	if (leu == NULL) {
		LIST_FOREACH(tleu, &leus, leu_entry) {
			if (tleu->leu_udata == cdte->cdte_udata) {
				leu = tleu;
				break;
			}
		}
		assert(leu != NULL);
	}
	assert(leu->leu_udata == cdte->cdte_udata);
	obj = (lv_obj_t *)cdte->cdte_obj;
	if (obj != NULL) {
		assert(obj == leu->leu_obj);
		obj_udata = lv_obj_get_user_data(obj);
		lv_obj_remove_event_cb_with_user_data(obj, lvkid_lv_event_cb,
		    (void *)cdte->cdte_udata);
	}
	LIST_REMOVE(leu, leu_entry);
	pthread_mutex_unlock(&lv_mtx);

	ed = (struct edesc){
		.ed_removed = 1,
		.ed_udata = leu->leu_udata,
		.ed_target = (uint64_t)obj,
		.ed_target_udata = (uint64_t)obj_udata,
	};
	shm_produce_evt(shm, &ed);

	rd = (struct rdesc){
		.rd_error = 0,
		.rd_cookie = cd->cd_cookie
	};
	shm_produce_rsp(shm, &rd, 1);

	free(leu);
}

static void *
lvkid_lv_phlush_ring(void *arg)
{
	struct lvkid *kid = arg;
	struct pdesc *pd;
	struct shmintf *shm = kid->lvk_shm;
	lv_disp_drv_t *drv;
	struct lvinst *inst;

	while (1) {
		shm_consume_phlush(shm, &pd);

		pthread_mutex_lock(&lv_flush_mtx);

		drv = (lv_disp_drv_t *)pd->pd_disp_drv;
		inst = drv->user_data;
		assert(inst->lvi_kid == kid);
		assert(drv == &inst->lvi_disp_drv);
		assert(inst->lvi_disp != NULL);

		lv_disp_flush_ready(drv);

		pthread_cond_broadcast(&lv_flush_cond);

		pthread_mutex_unlock(&lv_flush_mtx);

		shm_finish_phlush(pd);
	}

	return (NULL);
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

		switch (cd[0]->cd_op) {
		case CMD_SETUP:
			assert(ncd == 1);
			lvkid_lv_cmd_setup(kid, shm, cd[0]);
			break;
		case CMD_TEARDOWN:
			assert(ncd == 1);
			lvkid_lv_cmd_teardown(kid, shm, cd[0]);
			break;
		case CMD_SET_UDATA:
			assert(ncd == 1);
			lvkid_lv_cmd_set_udata(kid, shm, cd[0]);
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
			assert(ncd == 1);
			lvkid_lv_cmd_setup_event(kid, shm, cd[0]);
			break;
		case CMD_TEARDOWN_EVENT:
			assert(ncd == 1);
			lvkid_lv_cmd_teardown_event(kid, shm, cd[0]);
			break;
		case CMD_EXIT_CHILD:
			exit(0);
		}

		for (i = 0; i < ncd; ++i)
			shm_finish_cmd(cd[i]);

		shm_ring_doorbell(shm);
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
	pthread_mutex_init(&lv_flush_mtx, &mattr);
	pthread_cond_init(&lv_flush_cond, NULL);

	fprintf(stderr, "lvkid %p: starting up\r\n", kid);

	lv_init();
	LIST_INIT(&leus);

	pthread_create(&kid->lvk_rsp_th, NULL, lvkid_lv_cmd_ring, kid);
	pthread_setname_np(kid->lvk_rsp_th, "lvkid_cmd_ring");
	pthread_create(&kid->lvk_flush_th, NULL, lvkid_lv_phlush_ring, kid);
	pthread_setname_np(kid->lvk_flush_th, "lvkid_phl_ring");

	while (1) {
		usleep(5000);
		pthread_mutex_lock(&lv_mtx);
		lv_task_handler();
		pthread_mutex_unlock(&lv_mtx);
	}
	return (NULL);
}

static void
lvkevt_setup_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct lvkevt *evt = priv;
	struct lvkid *kid = evt->lvke_kid;
	assert(nrd == 1);

	pthread_rwlock_wrlock(&kid->lvk_lock);
	evt->lvke_eudata = rd[0]->rd_setup_event.rdse_eudata;
	pthread_rwlock_unlock(&kid->lvk_lock);
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
			.cdse_event = LV_EVENT_DELETE,
		}
	};
	lvk_cmd(kid, &cd, 1, lvkevt_setup_cb, obj->lvko_delevt);

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
	shm_ring_doorbell(kid->lvk_shm);
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
	lv_color_t col;
	lv_point_t pt;
	lv_style_value_t stv;
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

		(*call->lvkc_cb)(kid, 0, ARG_OBJPTR, obj, call->lvkc_priv);

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
	case ARG_COLOR:
		col.full = rdr->rdr_val;
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &col, call->lvkc_priv);
		break;
	case ARG_POINT:
		bcopy(&rdr->rdr_val, &pt, sizeof (pt));
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &pt, call->lvkc_priv);
		break;
	case ARG_STYLEVAL:
		bcopy(&rdr->rdr_val, &stv, sizeof (stv));
		(*call->lvkc_cb)(kid, 0, call->lvkc_rt, &stv, call->lvkc_priv);
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
	uint8_t argtype[8] = { ARG_NONE };
	uint64_t arg[8] = { 0 };
	lv_color_t *col;
	lv_point_t *pt;
	lv_style_value_t *stv;
	uint i;

	if (inst == NULL)
		assert(rt != ARG_OBJPTR);

	for (i = 0; i < 8; ++i) {
		argtype[i] = va_arg(ap, enum arg_type);
		if (argtype[i] == ARG_NONE)
			break;
		switch (argtype[i]) {
		case ARG_BUFPTR:
			buf = va_arg(ap, struct lvkbuf *);
			if (buf != NULL)
				arg[i] = buf->lvkb_ptr;
			break;
		case ARG_OBJPTR:
			obj = va_arg(ap, struct lvkobj *);
			if (obj != NULL)
				arg[i] = obj->lvko_ptr;
			break;
		case ARG_PTR:
			arg[i] = va_arg(ap, uintptr_t);
			break;
		case ARG_UINT64:
			arg[i] = va_arg(ap, uint64_t);
			break;
		case ARG_UINT32:
			arg[i] = va_arg(ap, uint32_t);
			break;
		case ARG_UINT16:
			arg[i] = va_arg(ap, int);
			assert(arg[i] < UINT16_MAX);
			break;
		case ARG_UINT8:
			arg[i] = va_arg(ap, int);
			assert(arg[i] < UINT8_MAX);
			break;
		case ARG_COLOR:
			col = va_arg(ap, lv_color_t *);
			arg[i] = col->full;
			break;
		case ARG_POINT:
			pt = va_arg(ap, lv_point_t *);
			assert(sizeof (*pt) <= sizeof (uint64_t));
			bcopy(pt, &arg[i], sizeof (*pt));
			break;
		case ARG_STYLEVAL:
			stv = va_arg(ap, lv_style_value_t *);
			assert(sizeof (*stv) <= sizeof (uint64_t));
			bcopy(stv, &arg[i], sizeof (*stv));
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
	for (i = 0; i < 8; ++i) {
		cd.cd_call.cdc_argtype[i] = argtype[i];
		cd.cd_call.cdc_arg[i] = arg[i];
	}
	lvk_cmd(kid, &cd, 1, lvk_call_cb, call);
}

int
lvk_cast(struct lvkid *kid, enum arg_type rt, lvk_call_func_t f, ...)
{
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, NULL, NULL, NULL, rt, 0, f, ap);
	va_end(ap);
	return (0);
}

int
lvk_icast(struct lvkinst *inst, enum arg_type rt, lvk_call_func_t f, ...)
{
	struct lvkid *kid = inst->lvki_kid;
	va_list ap;
	if (inst->lvki_state != LVKINST_ALIVE)
		return (EBADF);
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, inst, NULL, NULL, rt, 0, f, ap);
	va_end(ap);
	return (0);
}

int
lvk_call_buf(struct lvkid *kid, lvk_call_cb_t cb, void *priv,
    size_t rtblen, lvk_call_func_t f, ...)
{
	va_list ap;
	va_start(ap, f);
	lvk_vcall(kid, NULL, cb, priv, ARG_BUFPTR, rtblen, f, ap);
	va_end(ap);
	return (0);
}

int
lvk_call(struct lvkid *kid, lvk_call_cb_t cb, void *priv, enum arg_type rt,
    lvk_call_func_t f, ...)
{
	va_list ap;
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, NULL, cb, priv, rt, 0, f, ap);
	va_end(ap);
	return (0);
}

int
lvk_icall(struct lvkinst *inst, lvk_call_cb_t cb, void *priv, enum arg_type rt,
    lvk_call_func_t f, ...)
{
	struct lvkid *kid = inst->lvki_kid;
	va_list ap;
	if (inst->lvki_state != LVKINST_ALIVE)
		return (EBADF);
	assert(rt != ARG_BUFPTR);
	va_start(ap, f);
	lvk_vcall(kid, inst, cb, priv, rt, 0, f, ap);
	va_end(ap);
	return (0);
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

	while (1) {
		shm_consume_flush(shm, &fd);
		fbidx = fd->fd_fbidx_flag & FBIDX_IDX_MASK;
		assert(fbidx < shm->si_nfbuf);
		fb = &shm->si_fbuf[fbidx];

		pthread_rwlock_rdlock(&kid->lvk_lock);
		if (fb->fb_state == FBUF_FREE)
			goto next;

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
		inst->lvki_flushing = 1;
		hdl = lvkid_make_hdl(LVK_FBUF, fb, &do_release);
		if (hdl->lvkh_fbuf == NULL)
			hdl->lvkh_fbuf = buf;
		if (hdl->lvkh_fbuf != buf) {
			pthread_rwlock_unlock(&inst->lvki_lock);
			fprintf(stderr, "dropping due to wrong buf\r\n");
			goto next;
		}
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

next:
		pthread_rwlock_unlock(&kid->lvk_lock);
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
		evt = (struct lvkevt *)ed->ed_udata;
		assert(evt->lvke_kid == kid);

		obj = NULL;
		inst = NULL;

		if (ed->ed_removed || ed->ed_code == LV_EVENT_DELETE) {
			/*
			 * We're going to need to modify a handle which was
			 * possibly already sent to Erlang (either the event
			 * handle itself in the ed_removed case, or the handle
			 * on the object this event is about if it's a delete
			 * event), so we need lvk_lock for write.
			 */
			pthread_rwlock_wrlock(&kid->lvk_lock);
		} else {
			/*
			 * Otherwise we only care that our instance and object
			 * aren't removed or changed underneath us while
			 * running, so take lvk_lock just for read.
			 */
			pthread_rwlock_rdlock(&kid->lvk_lock);
		}

		obj = evt->lvke_obj;

		/* ed_removed indicates that this event is now dead */
		if (ed->ed_removed) {
			if (evt->lvke_hdl != NULL) {
				evt->lvke_hdl->lvkh_type = LVK_NONE;
				evt->lvke_hdl->lvkh_inst = NULL;
				evt->lvke_hdl->lvkh_ptr = NULL;
				evt->lvke_hdl = NULL;
			}
			if (obj != NULL)
				LIST_REMOVE(evt, lvke_kid_entry);
			free(evt);
			goto next;
		}

		if (obj == NULL)
			goto next;

		inst = obj->lvko_inst;
		if (inst != NULL)
			pthread_rwlock_wrlock(&inst->lvki_lock);

		/*
		 * This is the special "deleted" event we set up to clean up
		 * objects when they've been free'd on the LV side (e.g. when
		 * they're children of another object).
		 */
		if (ed->ed_code == LV_EVENT_DELETE && evt == obj->lvko_delevt) {
			obj->lvko_ptr = 0;
			obj->lvko_class = NULL;
			lvkobj_release(obj);
			goto next;
		}

		if (inst == NULL)
			goto next;

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

		tgterl = enif_make_atom(env, "undefined");
		ctgterl = enif_make_atom(env, "undefined");

		if (tgt != NULL) {
			tgthdl = lvkid_make_hdl(LVK_OBJ, tgt, &do_release);
			tgterl = enif_make_resource(env, tgthdl);
			if (do_release)
				enif_release_resource(tgthdl);
		}
		if (ctgt != NULL) {
			ctgthdl = lvkid_make_hdl(LVK_OBJ, ctgt, &do_release);
			ctgterl = enif_make_resource(env, ctgthdl);
			if (do_release)
				enif_release_resource(ctgthdl);
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
		if (inst != NULL)
			pthread_rwlock_unlock(&inst->lvki_lock);
		pthread_rwlock_unlock(&kid->lvk_lock);
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
		pthread_setname_np(kid->lvk_lv_th, "lvkid_lv_thread");
		pthread_setname_np(pthread_self(), "lvkid");
		pthread_join(kid->lvk_lv_th, NULL);
		exit(0);
	}

	LIST_INSERT_HEAD(&lvkids, kid, lvk_entry);

	pthread_create(&kid->lvk_rsp_th, NULL, lvkid_erl_rsp_ring, kid);
	pthread_setname_np(kid->lvk_rsp_th, "lvkid_rsp_ring");
	pthread_create(&kid->lvk_evt_th, NULL, lvkid_erl_evt_ring, kid);
	pthread_setname_np(kid->lvk_evt_th, "lvkid_evt_ring");
	pthread_create(&kid->lvk_flush_th, NULL, lvkid_erl_flush_ring, kid);
	pthread_setname_np(kid->lvk_flush_th, "lvkid_fl_ring");
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
		pthread_rwlock_wrlock(&lvkids_lock);
		LIST_FOREACH(kid, &lvkids, lvk_entry) {
			pthread_rwlock_wrlock(&kid->lvk_lock);
			if (kid->lvk_busy < kid->lvk_shm->si_nfbuf) {
				++kid->lvk_busy;
				nkid = kid;
				break;
			}
			pthread_rwlock_unlock(&kid->lvk_lock);
		}
		if (nkid == NULL) {
			lvkid_new();
		}
		pthread_rwlock_unlock(&lvkids_lock);
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

	fprintf(stderr, "setting up inst %p in fbuf %u\r\n", inst, i);

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
			.cdte_udata = (uint64_t)evt,
			.cdte_eudata = evt->lvke_eudata,
		}
	};
	lvk_cmd(kid, &cd, 1, NULL, NULL);
}

static void
lvk_inst_teardown_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct lvkinst *inst = priv;
	struct lvkid *kid = inst->lvki_kid;
	struct lvkobj *obj, *nobj;

	assert(nrd == 1);
	fprintf(stderr, "inst %p teardown cb\r\n", inst);

	pthread_rwlock_wrlock(&kid->lvk_lock);
	pthread_rwlock_wrlock(&inst->lvki_lock);
	--kid->lvk_busy;
	inst->lvki_state = LVKINST_FREE;
	inst->lvki_fbuf->fb_state = FBUF_FREE;
	inst->lvki_fbuf->fb_priv = NULL;
	inst->lvki_fbuf = NULL;
	LIST_FOREACH_SAFE(obj, &inst->lvki_objs, lvko_entry, nobj) {
		if (obj->lvko_hdl) {
			obj->lvko_hdl->lvkh_type = LVK_NONE;
			obj->lvko_hdl->lvkh_ptr = NULL;
			obj->lvko_hdl->lvkh_inst = NULL;
			obj->lvko_hdl = NULL;
		}
		obj->lvko_inst = NULL;
		lvkobj_release(obj);
	}
	enif_free_env(inst->lvki_env);
	LIST_REMOVE(inst, lvki_entry);
	pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);

	free(inst);
}


static void
lvkinst_teardown(struct lvkinst *inst)
{
	struct lvkid *kid = inst->lvki_kid;
	struct cdesc cd;
	if (inst->lvki_state == LVKINST_DRAIN)
		return;
	inst->lvki_fbuf->fb_state = FBUF_TEARDOWN;
	inst->lvki_state = LVKINST_DRAIN;
	if (inst->lvki_hdl != NULL) {
		inst->lvki_hdl->lvkh_type = LVK_NONE;
		inst->lvki_hdl->lvkh_ptr = NULL;
		inst->lvki_hdl->lvkh_inst = NULL;
		inst->lvki_hdl = NULL;
	}
	if (inst->lvki_fbhdl != NULL) {
		inst->lvki_fbhdl->lvkh_type = LVK_NONE;
		inst->lvki_fbhdl->lvkh_ptr = NULL;
		inst->lvki_fbhdl->lvkh_inst = NULL;
		inst->lvki_fbhdl = NULL;
	}
	fprintf(stderr, "moving inst %p into drain\r\n", inst);
	cd = (struct cdesc){
		.cd_op = CMD_TEARDOWN,
		.cd_teardown = (struct cdesc_teardown){
			.cdt_disp_drv = inst->lvki_disp_drv
		}
	};
	if (inst->lvki_flushing) {
		struct pdesc pd;
		pd = (struct pdesc){
			.pd_disp_drv = inst->lvki_disp_drv
		};
		shm_produce_phlush(kid->lvk_shm, &pd);
		inst->lvki_flushing = 0;
	}
	lvk_cmd(kid, &cd, 1, lvk_inst_teardown_cb, inst);
}
