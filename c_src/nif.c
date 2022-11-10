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
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <assert.h>
#include <sys/wait.h>

#include "erl_nif.h"
#include "shm.h"
#include "lvkid.h"

struct nif_call_data {
	ErlNifEnv		*ncd_env;
	ERL_NIF_TERM		 ncd_msgref;
	ErlNifPid		 ncd_owner;
};

static void
rlvgl_call_cb(struct lvkid *kid, uint32_t err, enum arg_type rt,
    void *rv, void *priv)
{
	struct nif_call_data *ncd = priv;
	struct lvkobj *obj;
	struct lvkhdl *hdl;
	uint64_t *u64;
	uint32_t *u32;
	uint16_t *u16;
	uint8_t *u8;
	ERL_NIF_TERM rterm, msg;
	ErlNifEnv *env = ncd->ncd_env;
	lv_color_t *col;
	lv_point_t *pt;
	lv_color32_t c32;
	ErlNifBinary *bin;
	uint do_release;

	if (err != 0) {
		msg = enif_make_tuple4(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "error"),
		    enif_make_uint(env, err),
		    enif_make_string(env, strerror(err), ERL_NIF_LATIN1));
		goto out;
	}

	switch (rt) {
	case ARG_NONE:
		msg = enif_make_tuple2(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "ok"));
		goto out;
	case ARG_OBJPTR:
		obj = rv;
		pthread_rwlock_wrlock(&obj->lvko_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_OBJ, obj, &do_release);
		pthread_rwlock_unlock(&obj->lvko_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_UINT64:
		u64 = rv;
		rterm = enif_make_uint64(env, *u64);
		break;
	case ARG_UINT32:
		u32 = rv;
		rterm = enif_make_uint(env, *u32);
		break;
	case ARG_UINT16:
		u16 = rv;
		rterm = enif_make_uint(env, *u16);
		break;
	case ARG_UINT8:
		u8 = rv;
		rterm = enif_make_uint(env, *u8);
		break;
	case ARG_POINT:
		pt = rv;
		rterm = enif_make_tuple2(env,
		    enif_make_uint(env, pt->x),
		    enif_make_uint(env, pt->y));
		break;
	case ARG_COLOR:
		col = rv;
		c32.full = lv_color_to32(*col);
		rterm = enif_make_tuple3(env,
		    enif_make_uint(env, c32.ch.red),
		    enif_make_uint(env, c32.ch.green),
		    enif_make_uint(env, c32.ch.blue));
		break;
	case ARG_INLINE_BUF:
	case ARG_INLINE_STR:
		bin = rv;
		rterm = enif_make_binary(env, bin);
		break;
	default:
		assert(0);
		return;
	}
	msg = enif_make_tuple3(env,
	    ncd->ncd_msgref,
	    enif_make_atom(env, "ok"),
	    rterm);
out:
	enif_send(NULL, &ncd->ncd_owner, env, msg);
	enif_free_env(env);
	free(ncd);
}

static ERL_NIF_TERM
rlvgl_setup_instance(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	const ERL_NIF_TERM *sztup;
	int sztuplen;
	uint w, h, do_release;
	ERL_NIF_TERM msgref, instref;
	ErlNifPid self;
	struct lvkinst *inst;
	struct lvkhdl *hdl;

	if (argc != 1)
		return (enif_make_badarg(env));
	if (!enif_get_tuple(env, argv[0], &sztuplen, &sztup))
		return (enif_make_badarg(env));
	if (sztuplen != 2)
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, sztup[0], &w))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, sztup[1], &h))
		return (enif_make_badarg(env));

	enif_self(env, &self);
	msgref = enif_make_ref(env);

	inst = lvkid_setup_inst(self, msgref, w, h);
	if (inst == NULL) {
		return (enif_make_tuple2(env,
		    enif_make_atom(env, "error"),
		    enif_make_atom(env, "lvkid_no_memory")));
	}
	pthread_rwlock_wrlock(&inst->lvki_lock);
	hdl = lvkid_make_hdl(LVK_INST, inst, &do_release);
	enif_monitor_process(env, hdl, &self, &hdl->lvkh_mon);
	pthread_rwlock_unlock(&inst->lvki_lock);

	instref = enif_make_resource(env, hdl);
	if (do_release)
		enif_release_resource(hdl);

	return (enif_make_tuple3(env,
	    enif_make_atom(env, "ok"),
	    instref,
	    msgref));
}

static inline ERL_NIF_TERM
make_errno(ErlNifEnv *env, int rc)
{
	return (enif_make_tuple3(env,
	    enif_make_atom(env, "error"),
	    enif_make_uint(env, rc),
	    enif_make_string(env, strerror(rc), ERL_NIF_LATIN1)));
}

static int
enter_inst_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct lvkhdl **phdl,
    struct lvkinst **pinst, uint wrlock)
{
	struct lvkid *kid;
	struct lvkhdl *hdl;
	struct lvkinst *inst;

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl))
		return (EINVAL);

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_INST) {
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_ptr;
	if (wrlock)
		pthread_rwlock_wrlock(&inst->lvki_lock);
	else
		pthread_rwlock_rdlock(&inst->lvki_lock);

	*phdl = hdl;
	*pinst = inst;

	return (0);
}

static int
enter_inst_obj_hdl(ErlNifEnv *env, ERL_NIF_TERM iterm, ERL_NIF_TERM oterm,
    struct lvkhdl **pihdl, struct lvkinst **pinst,
    struct lvkhdl **pohdl, struct lvkobj **pobj,
    uint wrlock)
{
	struct lvkid *kid;
	struct lvkhdl *ihdl, *ohdl;
	struct lvkinst *inst;
	struct lvkobj *obj;

	if (!enif_get_resource(env, iterm, lvkid_hdl_rsrc, (void **)&ihdl))
		return (EINVAL);
	if (!enif_get_resource(env, oterm, lvkid_hdl_rsrc, (void **)&ohdl))
		return (EINVAL);

	kid = ihdl->lvkh_kid;
	if (ohdl->lvkh_kid != kid)
		return (EINVAL);
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (ihdl->lvkh_type != LVK_INST ||
	    ohdl->lvkh_type != LVK_OBJ) {
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = ihdl->lvkh_ptr;
	if (ohdl->lvkh_inst != inst) {
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	if (wrlock)
		pthread_rwlock_wrlock(&inst->lvki_lock);
	else
		pthread_rwlock_rdlock(&inst->lvki_lock);
	assert(inst->lvki_kid == kid);
	obj = ohdl->lvkh_ptr;
	assert(obj->lvko_inst == inst);
	assert(obj->lvko_kid == kid);

	*pihdl = ihdl;
	*pinst = inst;
	*pohdl = ohdl;
	*pobj = obj;

	return (0);
}

static int
enter_obj_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct lvkhdl **phdl,
    struct lvkobj **pobj, uint wrlock)
{
	struct lvkhdl *hdl;
	struct lvkobj *obj;
	struct lvkinst *inst;
	struct lvkid *kid;

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl))
		return (EINVAL);

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_OBJ) {
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_inst;
	obj = hdl->lvkh_ptr;
	if (wrlock)
		pthread_rwlock_wrlock(&inst->lvki_lock);
	else
		pthread_rwlock_rdlock(&inst->lvki_lock);

	*phdl = hdl;
	*pobj = obj;

	return (0);
}

static void
leave_hdl(struct lvkhdl *hdl)
{
	struct lvkid *kid;
	struct lvkinst *inst;
	if (hdl == NULL)
		return;
	kid = hdl->lvkh_kid;
	inst = hdl->lvkh_inst;
	pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);
}

static int
make_ncd(ErlNifEnv *env, ERL_NIF_TERM *pmsgref, struct nif_call_data **pcd)
{
	struct nif_call_data *ncd;
	ErlNifPid self;
	ERL_NIF_TERM msgref;

	enif_self(env, &self);
	msgref = enif_make_ref(env);

	ncd = calloc(1, sizeof (*ncd));
	ncd->ncd_env = enif_alloc_env();
	ncd->ncd_owner = self;
	ncd->ncd_msgref = enif_make_copy(ncd->ncd_env, msgref);

	*pcd = ncd;
	*pmsgref = msgref;

	return (0);
}

static void
free_ncd(struct nif_call_data *ncd)
{
	if (ncd == NULL)
		return;
	enif_free_env(ncd->ncd_env);
	free(ncd);
}

static ERL_NIF_TERM
rlvgl_disp_set_bg_color(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	uint r, g, b;
	lv_color_t col;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_tuple(env, argv[1], &ctuplen, &ctup))
		return (enif_make_badarg(env));
	if (ctuplen != 3)
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, ctup[0], &r))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, ctup[1], &g))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, ctup[1], &b))
		return (enif_make_badarg(env));

	col = lv_color_make(r, g, b);

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_disp_set_bg_color,
	    ARG_PTR, inst->lvki_disp,
	    ARG_COLOR, &col,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(hdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkobj *parent;
	struct lvkhdl *ihdl = NULL, *phdl = NULL;
	struct nif_call_data *ncd = NULL;
	char atm[8];
	int rc;
	ERL_NIF_TERM msgref, rv;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &ihdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (enif_get_resource(env, argv[1], lvkid_hdl_rsrc, (void **)&phdl)) {
		if (phdl->lvkh_kid != ihdl->lvkh_kid) {
			rv = enif_make_badarg(env);
			goto out;
		}
		if (phdl->lvkh_type != LVK_OBJ) {
			rv = enif_make_badarg(env);
			goto out;
		}
		if (phdl->lvkh_inst != inst) {
			rv = enif_make_badarg(env);
			goto out;
		}
		parent = phdl->lvkh_ptr;
		assert(parent->lvko_inst == inst);
		assert(parent->lvko_kid == inst->lvki_kid);
	} else {
		if (!enif_get_atom(env, argv[1], atm, sizeof (atm),
		    ERL_NIF_LATIN1)) {
			rv = enif_make_badarg(env);
			goto out;
		}
		if (strcmp(atm, "none") != 0) {
			rv = enif_make_badarg(env);
			goto out;
		}
		phdl = NULL;
		parent = NULL;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_disp_obj_create,
	    ARG_PTR, inst->lvki_disp,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ihdl);
	/* same parent and kid, so leave_hdl will release the locks for both */
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_img_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *parent;
	struct lvkhdl *phdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &phdl, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(parent->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_img_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(phdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_spinner_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *parent;
	struct lvkhdl *phdl = NULL;
	struct nif_call_data *ncd = NULL;
	uint time, arcl;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 3)
		return (enif_make_badarg(env));

	if (!enif_get_uint(env, argv[1], &time))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, argv[2], &arcl))
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &phdl, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(parent->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_spinner_create,
	    ARG_OBJPTR, parent,
	    ARG_UINT32, time,
	    ARG_UINT32, arcl,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(phdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_disp_get_layer_sys(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *ihdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &ihdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_disp_get_layer_sys,
	    ARG_PTR, inst->lvki_disp,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ihdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_scr_load(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkobj *screen;
	struct lvkhdl *ihdl = NULL, *shdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_obj_hdl(env, argv[0], argv[1], &ihdl, &inst,
	    &shdl, &screen, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_disp_scr_load,
	    ARG_PTR, inst->lvki_disp,
	    ARG_OBJPTR, screen,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ihdl);
	/* leaving ihdl will drop all the locks */
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_set_mouse_cursor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkobj *cursor;
	struct lvkhdl *ihdl = NULL, *chdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_obj_hdl(env, argv[0], argv[1], &ihdl, &inst,
	    &chdl, &cursor, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_indev_set_cursor,
	    ARG_PTR, inst->lvki_mouse,
	    ARG_OBJPTR, cursor,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ihdl);
	/* leaving ihdl will drop all the locks */
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_center(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *ohdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	int rc;

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &ohdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_center,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ohdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_img_set_src(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *ohdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	int rc;
	char atom[16];
	ErlNifBinary bin;
	enum arg_type atype;
	void *arg;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		atype = ARG_PTR;
		if (strcmp(atom, "gps") == 0) {
			arg = LV_SYMBOL_GPS;
		} else {
			return (enif_make_badarg(env));
		}
	} else if (enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
		atype = ARG_INLINE_BUF;
		if (bin.size > UINT8_MAX)
			return (enif_make_badarg(env));
	} else {
		return (enif_make_badarg(env));
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &ohdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_class != &lv_img_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	if (atype == ARG_PTR) {
		rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
		    ARG_NONE, lv_img_set_src,
		    ARG_OBJPTR, obj,
		    ARG_PTR, arg,
		    ARG_NONE);
	} else if (atype == ARG_INLINE_BUF) {
		rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
		    ARG_NONE, lv_img_set_src,
		    ARG_OBJPTR, obj,
		    ARG_INLINE_BUF, bin.data, bin.size,
		    ARG_NONE);
	}

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ohdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_send_pointer_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	const ERL_NIF_TERM *ptup;
	int ptuplen;
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	uint x, y;
	lv_point_t pt;
	lv_indev_state_t st;
	char atm[9];
	ERL_NIF_TERM rv;
	int rc;

	if (argc != 3)
		return (enif_make_badarg(env));

	if (!enif_get_tuple(env, argv[1], &ptuplen, &ptup))
		return (enif_make_badarg(env));
	if (ptuplen != 2)
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, ptup[0], &x))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, ptup[1], &y))
		return (enif_make_badarg(env));
	pt.x = x;
	pt.y = y;

	if (!enif_get_atom(env, argv[2], atm, sizeof (atm), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atm, "pressed") == 0) {
		st = LV_INDEV_STATE_PRESSED;
	} else if (strcmp(atm, "released") == 0) {
		st = LV_INDEV_STATE_RELEASED;
	} else {
		return (enif_make_badarg(env));
	}

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icast(inst,
	    ARG_NONE, lv_ieq_push_mouse,
	    ARG_PTR, inst->lvki_mouse_drv,
	    ARG_UINT32, st,
	    ARG_POINT, &pt,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rv = enif_make_atom(env, "ok");
out:
	leave_hdl(hdl);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_send_key_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	uint key;
	lv_indev_state_t st;
	char atm[9];
	ERL_NIF_TERM rv;
	int rc;

	if (argc != 3)
		return (enif_make_badarg(env));

	if (enif_get_atom(env, argv[1], atm, sizeof (atm), ERL_NIF_LATIN1)) {
		if (strcmp(atm, "up") == 0)
			key = LV_KEY_UP;
		else if (strcmp(atm, "down") == 0)
			key = LV_KEY_DOWN;
		else if (strcmp(atm, "right") == 0)
			key = LV_KEY_RIGHT;
		else if (strcmp(atm, "left") == 0)
			key = LV_KEY_LEFT;
		else if (strcmp(atm, "esc") == 0)
			key = LV_KEY_ESC;
		else if (strcmp(atm, "del") == 0)
			key = LV_KEY_DEL;
		else if (strcmp(atm, "backspace") == 0)
			key = LV_KEY_BACKSPACE;
		else if (strcmp(atm, "enter") == 0)
			key = LV_KEY_ENTER;
		else if (strcmp(atm, "next") == 0)
			key = LV_KEY_NEXT;
		else if (strcmp(atm, "prev") == 0)
			key = LV_KEY_PREV;
		else if (strcmp(atm, "home") == 0)
			key = LV_KEY_HOME;
		else if (strcmp(atm, "end") == 0)
			key = LV_KEY_END;
		else
			return (enif_make_badarg(env));
	} else if (!enif_get_uint(env, argv[1], &key)) {
		return (enif_make_badarg(env));
	}

	if (!enif_get_atom(env, argv[2], atm, sizeof (atm), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atm, "pressed") == 0) {
		st = LV_INDEV_STATE_PRESSED;
	} else if (strcmp(atm, "released") == 0) {
		st = LV_INDEV_STATE_RELEASED;
	} else {
		return (enif_make_badarg(env));
	}

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icast(inst,
	    ARG_NONE, lv_ieq_push_kbd,
	    ARG_PTR, inst->lvki_mouse_drv,
	    ARG_UINT32, st,
	    ARG_UINT32, key,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rv = enif_make_atom(env, "ok");

out:
	leave_hdl(hdl);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_flush_done(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl;
	struct shmintf *shm;
	struct pdesc pd;
	int rc;
	ERL_NIF_TERM rv;

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	/* Check for an old framebuffer handle. */
	if (inst->lvki_fbhdl != NULL) {
		/*
		 * XXX: should we just delete it like this? this will leave
		 *      all the binaries dangling...
		 */
		inst->lvki_fbhdl->lvkh_type = LVK_NONE;
		inst->lvki_fbhdl->lvkh_ptr = NULL;
		inst->lvki_fbhdl->lvkh_inst = NULL;
		inst->lvki_fbhdl = NULL;
	}

	if (inst->lvki_state != LVKINST_ALIVE || !inst->lvki_flushing) {
		rv = enif_make_tuple2(env,
		    enif_make_atom(env, "error"),
		    enif_make_atom(env, "bad_state"));
		goto out;
	}

	shm = inst->lvki_kid->lvk_shm;
	pd = (struct pdesc){
		.pd_disp_drv = inst->lvki_disp_drv
	};
	shm_produce_phlush(shm, &pd);
	shm_ring_doorbell(shm);

	inst->lvki_flushing = 0;
	rv = enif_make_atom(env, "ok");

out:
	leave_hdl(hdl);
	return (rv);
}

static int
rlvgl_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info)
{
	lvk_open_resource_types(env);
	return (0);
}

static void
rlvgl_nif_unload(ErlNifEnv *env, void *priv_data)
{
}

static ErlNifFunc nif_funcs[] = {
	{ "setup_instance", 1, rlvgl_setup_instance },
	{ "disp_set_bg_color", 2, rlvgl_disp_set_bg_color },
	{ "flush_done", 1, rlvgl_flush_done },
	{ "obj_create", 2, rlvgl_obj_create },
	{ "scr_load", 2, rlvgl_scr_load },
	{ "spinner_create", 3, rlvgl_spinner_create },
	{ "obj_center", 1, rlvgl_obj_center },
	{ "send_pointer_event", 3, rlvgl_send_pointer_event },
	{ "send_key_event", 3, rlvgl_send_key_event },
	{ "img_create", 1, rlvgl_img_create },
	{ "img_set_src", 2, rlvgl_img_set_src },
	{ "disp_get_layer_sys", 1, rlvgl_disp_get_layer_sys },
	{ "set_mouse_cursor", 2, rlvgl_set_mouse_cursor },
};

ERL_NIF_INIT(rdp_lvgl_nif, nif_funcs, rlvgl_nif_load, NULL, NULL,
    rlvgl_nif_unload);
