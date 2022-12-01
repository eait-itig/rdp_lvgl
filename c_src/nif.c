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
#include "lvkutils.h"
#include "log.h"

#include "nif_enums.h"

static inline ERL_NIF_TERM
enif_make_badarg2(ErlNifEnv *env, const char *param, ERL_NIF_TERM v)
{
	return (enif_raise_exception(env, enif_make_tuple3(env,
	    enif_make_atom(env, "badarg"),
	    enif_make_atom(env, param),
	    v)));
}

static inline ERL_NIF_TERM
enif_make_badargc(ErlNifEnv *env, int argc)
{
	return (enif_raise_exception(env, enif_make_tuple2(env,
	    enif_make_atom(env, "bad_args_count"),
	    enif_make_int(env, argc))));
}

static void
gen_enum(ErlNifEnv *env, const struct enum_spec *specs, bool multi,
    int flags, ERL_NIF_TERM *pterm)
{
	ERL_NIF_TERM list, flag;
	const struct enum_spec *es;
	if (!multi) {
		for (es = specs; es->es_str != NULL; ++es) {
			if (es->es_val == flags)
				break;
		}
		if (es->es_str == NULL) {
			*pterm = enif_make_int(env, flags);
			return;
		}
		*pterm = enif_make_atom(env, es->es_str);
		return;
	}
	list = enif_make_list(env, 0);
	for (es = specs; es->es_str != NULL; ++es) {
		if ((flags & es->es_val) == es->es_val) {
			flag = enif_make_atom(env, es->es_str);
			list = enif_make_list_cell(env, flag, list);
		}
	}
	*pterm = list;
}

static int
parse_enum(ErlNifEnv *env, ERL_NIF_TERM term, const struct enum_spec *specs,
    bool multi, int *pflags)
{
	ERL_NIF_TERM flag;
	int flags = 0;
	char atom[32];
	const struct enum_spec *es;
	if (enif_is_atom(env, term)) {
		if (!enif_get_atom(env, term, atom, sizeof (atom),
		    ERL_NIF_LATIN1)) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "bad_enum_value_atom"),
			    flag));
			return (EINVAL);
		}
		for (es = specs; es->es_str != NULL; ++es) {
			if (strcmp(es->es_str, atom) == 0) {
				flags = es->es_val;
				break;
			}
		}
		if (es->es_str == NULL) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "unknown_enum_value"),
			    term));
			return (EINVAL);
		}
		*pflags = flags;
		return (0);
	}
	if (!multi) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "unknown_single_enum_value"),
		    term));
		return (EINVAL);
	}
	while (enif_get_list_cell(env, term, &flag, &term)) {
		if (!enif_get_atom(env, flag, atom, sizeof (atom),
		    ERL_NIF_LATIN1)) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "bad_enum_value_atom"),
			    flag));
			return (EINVAL);
		}
		for (es = specs; es->es_str != NULL; ++es) {
			if (strcmp(es->es_str, atom) == 0) {
				flags |= es->es_val;
				break;
			}
		}
		if (es->es_str == NULL) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "unknown_enum_value"),
			    flag));
			return (EINVAL);
		}
	}
	*pflags = flags;
	return (0);
}

struct nif_call_data {
	ErlNifEnv		*ncd_env;
	ERL_NIF_TERM		 ncd_msgref;
	ErlNifPid		 ncd_owner;
	void			*ncd_priv;
	const struct enum_spec	*ncd_enum;
	bool			 ncd_multi;
};

static void
rlvgl_call_cb(struct lvkid *kid, uint32_t err, enum arg_type rt,
    void *rv, void *priv)
{
	struct nif_call_data *ncd = priv;
	struct lvkobj *obj;
	struct lvkstyle *sty;
	struct lvkgroup *grp;
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
	case ARG_STYPTR:
		sty = rv;
		pthread_rwlock_wrlock(&sty->lvks_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_STY, sty, &do_release);
		pthread_rwlock_unlock(&sty->lvks_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_GRPPTR:
		grp = rv;
		pthread_rwlock_wrlock(&grp->lvkg_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_GRP, grp, &do_release);
		pthread_rwlock_unlock(&grp->lvkg_inst->lvki_lock);
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
		if (ncd->ncd_enum != NULL) {
			gen_enum(env, ncd->ncd_enum, ncd->ncd_multi, *u32,
			    &rterm);
		} else {
			rterm = enif_make_uint(env, *u32);
		}
		break;
	case ARG_UINT16:
		u16 = rv;
		if (ncd->ncd_enum != NULL) {
			gen_enum(env, ncd->ncd_enum, ncd->ncd_multi, *u16,
			    &rterm);
		} else {
			rterm = enif_make_uint(env, *u16);
		}
		break;
	case ARG_UINT8:
		u8 = rv;
		if (ncd->ncd_enum != NULL) {
			gen_enum(env, ncd->ncd_enum, ncd->ncd_multi, *u8,
			    &rterm);
		} else {
			rterm = enif_make_uint(env, *u8);
		}
		rterm = enif_make_uint(env, *u8);
		break;
	case ARG_POINT:
		pt = rv;
		rterm = enif_make_tuple2(env,
		    enif_make_int(env, pt->x),
		    enif_make_int(env, pt->y));
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

enum nif_arg_type {
	NIF_ARG_NONE = 0,

};

struct binding_nif_info {
	void			*bni_func;
	enum nif_arg_type	 bni_arg_types[8];
	const struct enum_spec	*bni_enums[8];
};

static ERL_NIF_TERM
rlvgl_take_log_ownership(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifPid self;
	if (argc != 0)
		return (enif_make_badarg(env));
	enif_self(env, &self);
	log_take_ownership(self);
	return (enif_make_atom(env, "ok"));
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

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_INST) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_instance"), term));
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

	if (!enif_get_resource(env, iterm, lvkid_hdl_rsrc, (void **)&ihdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), iterm));
		return (EINVAL);
	}
	if (!enif_get_resource(env, oterm, lvkid_hdl_rsrc, (void **)&ohdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), oterm));
		return (EINVAL);
	}

	kid = ihdl->lvkh_kid;
	if (ohdl->lvkh_kid != kid) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "obj_handle_not_same_kid"), oterm));
		return (EINVAL);
	}
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (ihdl->lvkh_type != LVK_INST) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_instance"), iterm));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}
	if (ohdl->lvkh_type != LVK_OBJ) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_object"), oterm));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = ihdl->lvkh_ptr;
	if (ohdl->lvkh_inst != inst) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "obj_handle_not_same_inst"), oterm));
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
unpack_buf_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct lvkhdl *ohdl,
    struct lvkbuf **pbuf)
{
	struct lvkhdl *hdl;
	struct lvkid *kid;

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (ohdl->lvkh_kid != kid) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "buf_hdl_wrong_lvkid"), term));
		return (EINVAL);
	}

	if (hdl->lvkh_type != LVK_BUF) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_buf"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	*pbuf = hdl->lvkh_ptr;

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

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_OBJ) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_object"), term));
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

static int
enter_sty_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct lvkhdl **phdl,
    struct lvkstyle **psty, uint wrlock)
{
	struct lvkhdl *hdl;
	struct lvkstyle *sty;
	struct lvkinst *inst;
	struct lvkid *kid;

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_STY) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_style"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_inst;
	sty = hdl->lvkh_ptr;
	if (wrlock)
		pthread_rwlock_wrlock(&inst->lvki_lock);
	else
		pthread_rwlock_rdlock(&inst->lvki_lock);

	*phdl = hdl;
	*psty = sty;

	return (0);
}


static int
enter_grp_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct lvkhdl **phdl,
    struct lvkgroup **pgrp, uint wrlock)
{
	struct lvkhdl *hdl;
	struct lvkgroup *grp;
	struct lvkinst *inst;
	struct lvkid *kid;

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (wrlock)
		pthread_rwlock_wrlock(&kid->lvk_lock);
	else
		pthread_rwlock_rdlock(&kid->lvk_lock);
	if (hdl->lvkh_type != LVK_GRP) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_group"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_inst;
	grp = hdl->lvkh_ptr;
	if (wrlock)
		pthread_rwlock_wrlock(&inst->lvki_lock);
	else
		pthread_rwlock_rdlock(&inst->lvki_lock);

	*phdl = hdl;
	*pgrp = grp;

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

static int
enif_get_color(ErlNifEnv *env, ERL_NIF_TERM term, lv_color_t *pcol)
{
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	uint r, g, b;

	if (!enif_get_tuple(env, term, &ctuplen, &ctup))
		return (0);
	if (ctuplen != 3)
		return (0);
	if (!enif_get_uint(env, ctup[0], &r))
		return (0);
	if (!enif_get_uint(env, ctup[1], &g))
		return (0);
	if (!enif_get_uint(env, ctup[1], &b))
		return (0);

	*pcol = lv_color_make(r, g, b);

	return (1);
}

static ERL_NIF_TERM
rlvgl_disp_set_bg_color(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	lv_color_t col;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_color(env, argv[1], &col))
		return (enif_make_badarg(env));

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

#define	GENERATE_WIDGET_CREATE(widget) \
static ERL_NIF_TERM \
rlvgl_ ## widget ## _create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
	struct lvkobj *parent; \
	struct lvkhdl *phdl = NULL; \
	struct nif_call_data *ncd = NULL; \
	ERL_NIF_TERM msgref, rv; \
	int rc; \
\
	if (argc != 1) \
		return (enif_make_badarg(env)); \
\
	rc = make_ncd(env, &msgref, &ncd); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	rc = enter_obj_hdl(env, argv[0], &phdl, &parent, 0); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	rc = lvk_icall(parent->lvko_inst, rlvgl_call_cb, ncd, \
	    ARG_OBJPTR, lv_ ## widget ## _create, \
	    ARG_OBJPTR, parent, \
	    ARG_NONE); \
\
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	ncd = NULL;	/* rlvgl_call_cb owns it now */ \
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref); \
\
out: \
	leave_hdl(phdl); \
	free_ncd(ncd); \
	return (rv); \
}

GENERATE_WIDGET_CREATE(btn)
GENERATE_WIDGET_CREATE(checkbox)
GENERATE_WIDGET_CREATE(textarea)
GENERATE_WIDGET_CREATE(img)
GENERATE_WIDGET_CREATE(label)
GENERATE_WIDGET_CREATE(btnmatrix)
GENERATE_WIDGET_CREATE(dropdown)
GENERATE_WIDGET_CREATE(imgbtn)
GENERATE_WIDGET_CREATE(led)
GENERATE_WIDGET_CREATE(list)
GENERATE_WIDGET_CREATE(menu)
GENERATE_WIDGET_CREATE(roller)
GENERATE_WIDGET_CREATE(slider)
GENERATE_WIDGET_CREATE(switch)
GENERATE_WIDGET_CREATE(table)

#define GENERATE_WIDGET_SET_TEXT_EXTRA(widget, suffix) \
static ERL_NIF_TERM \
rlvgl_ ## widget ## _set ## suffix (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
	struct lvkobj *obj; \
	struct lvkhdl *hdl = NULL; \
	struct nif_call_data *ncd = NULL; \
	ERL_NIF_TERM msgref, rv; \
	int rc; \
	ErlNifBinary bin; \
\
	if (argc != 2) \
		return (enif_make_badarg(env)); \
\
	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)) \
		return (enif_make_badarg(env)); \
\
	rc = make_ncd(env, &msgref, &ncd); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	if (obj->lvko_class != &lv_ ## widget ## _class) { \
		rv = make_errno(env, EINVAL); \
		goto out; \
	} \
\
	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd, \
	    ARG_NONE, lv_ ## widget ## _set ## suffix, \
	    ARG_OBJPTR, obj, \
	    ARG_INLINE_BUF, bin.data, bin.size, \
	    ARG_NONE); \
\
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	ncd = NULL;	/* rlvgl_call_cb owns it now */ \
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref); \
\
out: \
	leave_hdl(hdl); \
	free_ncd(ncd); \
	return (rv); \
}
#define GENERATE_WIDGET_SET_TEXT(widget) \
	GENERATE_WIDGET_SET_TEXT_EXTRA(widget, _text)

GENERATE_WIDGET_SET_TEXT(label)
GENERATE_WIDGET_SET_TEXT(textarea)
GENERATE_WIDGET_SET_TEXT(checkbox)
GENERATE_WIDGET_SET_TEXT_EXTRA(textarea, _placeholder_text)
GENERATE_WIDGET_SET_TEXT_EXTRA(dropdown, _options)

static ERL_NIF_TERM
rlvgl_dropdown_add_option(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	ErlNifBinary bin;
	int index;

	if (argc != 3)
		return (enif_make_badarg(env));

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return (enif_make_badarg(env));
	if (!enif_get_int(env, argv[2], &index))
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_dropdown_add_option,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, bin.data, bin.size,
	    ARG_UINT32, index,
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
rlvgl_dropdown_get_selected(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
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

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_dropdown_get_selected,
	    ARG_OBJPTR, obj,
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
rlvgl_dropdown_get_selected_str(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
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

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_dropdown_get_selected_str,
	    ARG_OBJPTR, obj,
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
rlvgl_group_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
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

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_GRPPTR, lv_group_create,
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
rlvgl_group_add_obj(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkgroup *grp;
	struct lvkobj *obj;
	struct lvkhdl *ghdl = NULL, *ohdl = NULL;
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

	rc = enter_grp_hdl(env, argv[0], &ghdl, &grp, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!enif_get_resource(env, argv[1], lvkid_hdl_rsrc, (void **)&ohdl)) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ohdl->lvkh_kid != ghdl->lvkh_kid) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ohdl->lvkh_type != LVK_OBJ) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ohdl->lvkh_inst != grp->lvkg_inst) {
		rv = enif_make_badarg(env);
		goto out;
	}
	obj = ohdl->lvkh_ptr;
	assert(obj->lvko_inst == grp->lvkg_inst);
	assert(obj->lvko_kid == grp->lvkg_kid);

	rc = lvk_icall(grp->lvkg_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_add_obj,
	    ARG_GRPPTR, grp,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_hdl(ghdl);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_style_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
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

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_STYPTR, lv_style_alloc,
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
rlvgl_set_kbd_group(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkgroup *grp;
	struct lvkhdl *ihdl = NULL, *ghdl = NULL;
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

	rc = enter_inst_hdl(env, argv[0], &ihdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!enif_get_resource(env, argv[1], lvkid_hdl_rsrc, (void **)&ghdl)) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ghdl->lvkh_kid != ihdl->lvkh_kid) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ghdl->lvkh_type != LVK_GRP) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (ghdl->lvkh_inst != inst) {
		rv = enif_make_badarg(env);
		goto out;
	}
	grp = ghdl->lvkh_ptr;
	assert(grp->lvkg_inst == inst);
	assert(grp->lvkg_kid == inst->lvki_kid);

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_indev_set_group,
	    ARG_PTR, inst->lvki_kbd,
	    ARG_GRPPTR, grp,
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
rlvgl_scr_load_anim(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkobj *screen;
	struct lvkhdl *ihdl = NULL, *shdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	char atom[16];
	lv_scr_load_anim_t anim;
	uint time;
	uint delay;
	uint8_t autodel;
	const struct scr_load_anim *sla;

	if (argc != 6)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	for (sla = scr_load_anims; sla->sla_str != NULL; ++sla) {
		if (strcmp(sla->sla_str, atom) == 0) {
			anim = sla->sla_val;
			break;
		}
	}
	if (sla->sla_str == NULL)
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, argv[3], &time))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, argv[4], &delay))
		return (enif_make_badarg(env));
	if (!enif_get_atom(env, argv[5], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atom, "true") == 0)
		autodel = 1;
	else if (strcmp(atom, "false") == 0)
		autodel = 0;
	else
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
	    ARG_NONE, lv_disp_scr_load_anim,
	    ARG_PTR, inst->lvki_disp,
	    ARG_OBJPTR, screen,
	    ARG_UINT32, anim,
	    ARG_UINT32, time,
	    ARG_UINT32, delay,
	    ARG_UINT8, autodel,
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

static int
parse_flags(ErlNifEnv *env, ERL_NIF_TERM list, lv_obj_flag_t *pflags)
{
	ERL_NIF_TERM flag;
	lv_obj_flag_t flags = 0;
	char atom[32];
	const struct obj_flag *of;
	while (enif_get_list_cell(env, list, &flag, &list)) {
		if (!enif_get_atom(env, flag, atom, sizeof (atom),
		    ERL_NIF_LATIN1)) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "bad_flag"),
			    flag));
			return (EINVAL);
		}
		for (of = obj_flags; of->of_str != NULL; ++of) {
			if (strcmp(of->of_str, atom) == 0) {
				flags |= of->of_val;
				break;
			}
		}
		if (of->of_str == NULL) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "unknown_flag"),
			    flag));
			return (EINVAL);
		}
	}
	*pflags = flags;
	return (0);
}

static int
parse_font_spec(ErlNifEnv *env, ERL_NIF_TERM term, const lv_font_t **pfont)
{
	char *family = strdup("montserrat");
	char *variant = NULL;
	char atom[32];
	uint size = 14;
	ErlNifBinary bin;
	const struct font_spec *fs;
	int tuplen;
	const ERL_NIF_TERM *tup;
	int rc;

	if (enif_get_uint(env, term, &size)) {
		goto search;
	}

	if (enif_get_atom(env, term, atom, sizeof (atom), ERL_NIF_LATIN1)) {
		free(variant);
		variant = strdup(atom);
		goto search;
	}

	if (enif_inspect_iolist_as_binary(env, term, &bin)) {
		free(family);
		family = malloc(bin.size + 1);
		bcopy(bin.data, family, bin.size);
		family[bin.size] = '\0';
		goto search;
	}

	if (enif_get_tuple(env, term, &tuplen, &tup)) {
		if (tuplen == 2 && enif_is_atom(env, tup[0])) {
			if (!enif_get_atom(env, tup[0], atom, sizeof (atom),
			    ERL_NIF_LATIN1)) {
			    	goto err;
			}
			if (!enif_get_uint(env, tup[1], &size))
				goto err;
			free(variant);
			variant = strdup(atom);
			goto search;

		} else if (tuplen == 2) {
			if (!enif_inspect_iolist_as_binary(env, tup[0], &bin))
				goto err;
			if (!enif_get_uint(env, tup[1], &size))
				goto err;
			free(family);
			family = malloc(bin.size + 1);
			bcopy(bin.data, family, bin.size);
			family[bin.size] = '\0';
			goto search;

		} else if (tuplen == 3) {
			if (!enif_inspect_iolist_as_binary(env, tup[0], &bin))
				goto err;
			if (!enif_get_atom(env, tup[1], atom, sizeof (atom),
			    ERL_NIF_LATIN1)) {
			    	goto err;
			}
			if (!enif_get_uint(env, tup[2], &size))
				goto err;
			free(variant);
			variant = strdup(atom);
			free(family);
			family = malloc(bin.size + 1);
			bcopy(bin.data, family, bin.size);
			family[bin.size] = '\0';
			goto search;
		}
	}

	goto err;

search:
	/* Round 1: exact match including variant */
	for (fs = font_specs; fs->fs_font != NULL; ++fs) {
		if (strcasecmp(fs->fs_family, family) != 0)
			continue;
		if (variant != NULL && fs->fs_variant == NULL)
			continue;
		if (variant == NULL && fs->fs_variant != NULL)
			continue;
		if (variant != NULL && strcasecmp(fs->fs_variant, variant) != 0)
			continue;
		if (fs->fs_size != size)
			continue;
		break;
	}
	if (fs->fs_font != NULL) {
		*pfont = fs->fs_font;
		rc = 0;
		goto out;
	}
	/* Round 2: family and size */
	for (fs = font_specs; fs->fs_font != NULL; ++fs) {
		if (strcasecmp(fs->fs_family, family) != 0)
			continue;
		if (fs->fs_size != size)
			continue;
		break;
	}
	if (fs->fs_font != NULL) {
		*pfont = fs->fs_font;
		rc = 0;
		goto out;
	}
	/* Round 3: family and next larger size */
	for (fs = font_specs; fs->fs_font != NULL; ++fs) {
		if (strcasecmp(fs->fs_family, family) != 0)
			continue;
		if (fs->fs_size < size)
			continue;
		break;
	}
	if (fs->fs_font != NULL) {
		*pfont = fs->fs_font;
		rc = 0;
		goto out;
	}
	/* Round 4: size only */
	for (fs = font_specs; fs->fs_font != NULL; ++fs) {
		if (fs->fs_size < size)
			continue;
		break;
	}
	if (fs->fs_font != NULL) {
		*pfont = fs->fs_font;
		rc = 0;
		goto out;
	}

err:
	enif_raise_exception(env, enif_make_tuple2(env,
	    enif_make_atom(env, "bad_font_spec"),
	    term));
	rc = EINVAL;
	goto out;

out:
	free(family);
	free(variant);
	return (rc);
}

static ERL_NIF_TERM
rlvgl_obj_add_flags(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	lv_obj_flag_t flags;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = parse_flags(env, argv[1], &flags);
	if (rc != 0)
		return (enif_make_uint(env, 0));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_flag,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, flags,
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
rlvgl_obj_add_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	int flags;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = parse_enum(env, argv[1], obj_state_specs, true, &flags);
	if (rc != 0)
		return (make_errno(env, rc));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_state,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, flags,
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
rlvgl_obj_clear_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	int flags;

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = parse_enum(env, argv[1], obj_state_specs, true, &flags);
	if (rc != 0)
		return (make_errno(env, rc));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_clear_state,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, flags,
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
rlvgl_obj_get_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
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
	ncd->ncd_enum = obj_state_specs;
	ncd->ncd_multi = true;

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_UINT16, lv_obj_get_state,
	    ARG_OBJPTR, obj,
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
rlvgl_obj_add_style(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL, *shdl = NULL;
	struct lvkstyle *sty;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	int style_sel = 0;

	if (argc != 2 && argc != 3)
		return (enif_make_badarg(env));

	if (argc == 3) {
		rc = parse_enum(env, argv[2], style_selector_specs, true,
		    &style_sel);
		if (rc != 0)
			return (make_errno(env, rc));
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!enif_get_resource(env, argv[1], lvkid_hdl_rsrc, (void **)&shdl)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}
	if (shdl->lvkh_kid != hdl->lvkh_kid) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (shdl->lvkh_type != LVK_STY) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (shdl->lvkh_inst != obj->lvko_inst) {
		rv = enif_make_badarg(env);
		goto out;
	}
	sty = shdl->lvkh_ptr;
	assert(sty->lvks_inst == obj->lvko_inst);
	assert(sty->lvks_kid == obj->lvko_kid);

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_style,
	    ARG_OBJPTR, obj,
	    ARG_STYPTR, sty,
	    ARG_UINT32, style_sel,
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

static int
parse_coord(ErlNifEnv *env, ERL_NIF_TERM term, lv_coord_t *pcoord)
{
	int tuplen;
	const ERL_NIF_TERM *tup;
	int pix;
	char atom[16];

	if (enif_get_int(env, term, &pix)) {
		*pcoord = pix;
		return (0);
	}

	if (enif_get_atom(env, term, atom, sizeof (atom), ERL_NIF_LATIN1)) {
		if (strcmp(atom, "content") == 0) {
			*pcoord = LV_SIZE_CONTENT;
			return (0);
		} else {
			goto bad;
		}
	}

	if (enif_get_tuple(env, term, &tuplen, &tup)) {
		if (tuplen != 2)
			goto bad;

		if (!enif_get_atom(env, tup[0], atom, sizeof (atom),
		    ERL_NIF_LATIN1)) {
			goto bad;
		}

		if (!enif_get_int(env, tup[1], &pix))
			goto bad;

		if (strcmp(atom, "percent") == 0) {
			*pcoord = LV_PCT(pix);
			return (0);
		}

		goto bad;
	}

bad:
	enif_raise_exception(env, enif_make_tuple2(env,
	    enif_make_atom(env, "bad_coord"),
	    term));
	return (EINVAL);
}

static ERL_NIF_TERM
rlvgl_obj_set_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *ohdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	lv_coord_t w, h;
	const ERL_NIF_TERM *tup;
	int tuplen;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_tuple(env, argv[1], &tuplen, &tup))
		return (enif_make_badarg(env));
	if (tuplen != 2)
		return (enif_make_badarg(env));
	if ((rc = parse_coord(env, tup[0], &w)))
		return (make_errno(env, rc));
	if ((rc = parse_coord(env, tup[1], &h)))
		return (make_errno(env, rc));

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
	    ARG_NONE, lv_obj_set_size,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, w,
	    ARG_UINT32, h,
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
rlvgl_obj_get_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_POINT, lv_obj_get_size,
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
rlvgl_obj_get_pos(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_POINT, lv_obj_get_pos,
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
rlvgl_style_set_flex_flow(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkstyle *sty;
	struct lvkhdl *hdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	int rc;
	char atom[32];
	lv_flex_flow_t flow;

	if (argc != 2)
		return (enif_make_badargc(env, argc));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg2(env, "flex_flow", argv[1]));

	if (strcmp(atom, "row") == 0)
		flow = LV_FLEX_FLOW_ROW;
	else if (strcmp(atom, "column") == 0)
		flow = LV_FLEX_FLOW_COLUMN;
	else if (strcmp(atom, "row_wrap") == 0)
		flow = LV_FLEX_FLOW_ROW_WRAP;
	else if (strcmp(atom, "row_reverse") == 0)
		flow = LV_FLEX_FLOW_ROW_REVERSE;
	else if (strcmp(atom, "row_wrap_reverse") == 0)
		flow = LV_FLEX_FLOW_ROW_WRAP_REVERSE;
	else if (strcmp(atom, "column_wrap") == 0)
		flow = LV_FLEX_FLOW_COLUMN_WRAP;
	else if (strcmp(atom, "column_reverse") == 0)
		flow = LV_FLEX_FLOW_COLUMN_REVERSE;
	else if (strcmp(atom, "column_wrap_reverse") == 0)
		flow = LV_FLEX_FLOW_COLUMN_WRAP_REVERSE;
	else
		return (enif_make_badarg2(env, "flex_flow", argv[1]));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_sty_hdl(env, argv[0], &hdl, &sty, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(sty->lvks_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_style_set_flex_flow,
	    ARG_STYPTR, sty,
	    ARG_UINT32, flow,
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
rlvgl_style_set_prop(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkstyle *sty;
	struct lvkhdl *hdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	int rc;
	lv_style_value_t v;
	lv_style_prop_t prop;
	lv_color_t col;
	const lv_font_t *font;
	const struct style_prop *sp;
	char atom[32];
	int intval;

	if (argc != 3)
		return (enif_make_badargc(env, argc));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg2(env, "style_prop", argv[1]));
	for (sp = style_props; sp->sp_str != NULL; ++sp) {
		if (strcmp(sp->sp_str, atom) == 0)
			break;
	}
	if (sp->sp_str == NULL)
		return (enif_make_badarg2(env, "style_prop", argv[1]));

	prop = sp->sp_prop;

	switch (sp->sp_type) {
	case SPT_INT32:
		if (!enif_get_int(env, argv[2], &intval)) {
			return (enif_make_badarg2(env, "number", argv[2]));
		}
		v = (lv_style_value_t){ .num = intval };
		break;
	case SPT_COLOR:
		if (!enif_get_color(env, argv[2], &col))
			return (enif_make_badarg2(env, "color", argv[2]));
		v = (lv_style_value_t){ .color = col };
		break;
	case SPT_ENUM:
		rc = parse_enum(env, argv[2], sp->sp_enum, false, &intval);
		if (rc)
			return (make_errno(env, rc));
		v = (lv_style_value_t){ .num = intval };
		break;
	case SPT_MULTI_ENUM:
		rc = parse_enum(env, argv[2], sp->sp_enum, true, &intval);
		if (rc)
			return (make_errno(env, rc));
		v = (lv_style_value_t){ .num = intval };
		break;
	case SPT_FONT:
		rc = parse_font_spec(env, argv[2], &font);
		if (rc)
			return (make_errno(env, rc));
		v = (lv_style_value_t){ .ptr = font };
		break;
	default:
		assert(0);
		return (enif_make_badarg(env));
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_sty_hdl(env, argv[0], &hdl, &sty, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(sty->lvks_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_style_set_prop,
	    ARG_STYPTR, sty,
	    ARG_UINT32, prop,
	    ARG_STYLEVAL, &v,
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

static int
parse_flex_align(ErlNifEnv *env, ERL_NIF_TERM term, lv_flex_align_t *out)
{
	char atom[16];
	lv_flex_align_t v;
	if (!enif_get_atom(env, term, atom, sizeof (atom), ERL_NIF_LATIN1)) {
		enif_make_badarg2(env, "flex_align", term);
		return (EINVAL);
	}
	if (strcmp(atom, "start") == 0)
		v = LV_FLEX_ALIGN_START;
	else if (strcmp(atom, "end") == 0)
		v = LV_FLEX_ALIGN_END;
	else if (strcmp(atom, "center") == 0)
		v = LV_FLEX_ALIGN_CENTER;
	else if (strcmp(atom, "space_evenly") == 0)
		v = LV_FLEX_ALIGN_SPACE_EVENLY;
	else if (strcmp(atom, "space_around") == 0)
		v = LV_FLEX_ALIGN_SPACE_AROUND;
	else if (strcmp(atom, "space_between") == 0)
		v = LV_FLEX_ALIGN_SPACE_BETWEEN;
	else {
		enif_make_badarg2(env, "flex_align", term);
		return (EINVAL);
	}
	*out = v;
	return (0);
}

static ERL_NIF_TERM
rlvgl_style_set_flex_align(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkstyle *sty;
	struct lvkhdl *hdl;
	struct nif_call_data *ncd;
	ERL_NIF_TERM msgref, rv;
	int rc;
	lv_flex_align_t main, cross, tracks;

	if (argc != 4)
		return (enif_make_badargc(env, argc));

	if (parse_flex_align(env, argv[1], &main) ||
	    parse_flex_align(env, argv[2], &cross) ||
	    parse_flex_align(env, argv[3], &tracks)) {
		/* dummy value, will use exception */
		return (enif_make_uint(env, 0));
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_sty_hdl(env, argv[0], &hdl, &sty, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(sty->lvks_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_style_set_flex_align,
	    ARG_STYPTR, sty,
	    ARG_UINT32, main,
	    ARG_UINT32, cross,
	    ARG_UINT32, tracks,
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

#define GENERATE_WIDGET_GET_TEXT(widget) \
static ERL_NIF_TERM \
rlvgl_ ## widget ## _get_text(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
	struct lvkobj *obj; \
	struct lvkhdl *hdl = NULL; \
	struct nif_call_data *ncd = NULL; \
	ERL_NIF_TERM msgref, rv; \
	int rc; \
\
	if (argc != 1) \
		return (enif_make_badarg(env)); \
\
	rc = make_ncd(env, &msgref, &ncd); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0); \
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	if (obj->lvko_class != &lv_ ## widget ## _class) { \
		rv = make_errno(env, EINVAL); \
		goto out; \
	} \
\
	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd, \
	    ARG_INLINE_STR, lv_ ## widget ## _get_text, \
	    ARG_OBJPTR, obj, \
	    ARG_NONE); \
\
	if (rc != 0) { \
		rv = make_errno(env, rc); \
		goto out; \
	} \
\
	ncd = NULL;	/* rlvgl_call_cb owns it now */ \
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref); \
\
out: \
	leave_hdl(hdl); \
	free_ncd(ncd); \
	return (rv); \
}

GENERATE_WIDGET_GET_TEXT(textarea)
GENERATE_WIDGET_GET_TEXT(checkbox)

static ERL_NIF_TERM
rlvgl_textarea_set_accepted_chars(ErlNifEnv *env, int argc,
    const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct lvkbuf *buf = NULL;
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

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = unpack_buf_hdl(env, argv[1], hdl, &buf);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_accepted_chars,
	    ARG_OBJPTR, obj,
	    ARG_BUFPTR, buf,
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
rlvgl_textarea_set_one_line(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	uint8_t val;
	char atom[8];

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atom, "true") == 0)
		val = 1;
	else if (strcmp(atom, "false") == 0)
		val = 0;
	else
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_one_line,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, val,
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
rlvgl_textarea_set_password_mode(ErlNifEnv *env, int argc,
    const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	uint8_t val;
	char atom[8];

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atom, "true") == 0)
		val = 1;
	else if (strcmp(atom, "false") == 0)
		val = 0;
	else
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_password_mode,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, val,
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
rlvgl_textarea_set_text_selection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	uint8_t val;
	char atom[8];

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	if (strcmp(atom, "true") == 0)
		val = 1;
	else if (strcmp(atom, "false") == 0)
		val = 0;
	else
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_text_selection,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, val,
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
rlvgl_img_set_offset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	int x, y;
	lv_point_t pt;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_tuple(env, argv[1], &ctuplen, &ctup))
		return (enif_make_badarg(env));
	if (ctuplen != 2)
		return (enif_make_badarg(env));
	if (!enif_get_int(env, ctup[0], &x))
		return (enif_make_badarg(env));
	if (!enif_get_int(env, ctup[1], &y))
		return (enif_make_badarg(env));
	pt.x = x;
	pt.y = y;

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_img_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_offset,
	    ARG_OBJPTR, obj,
	    ARG_POINT, &pt,
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
rlvgl_obj_align_to(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj, *refobj;
	struct lvkhdl *hdl = NULL, *rhdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	int x, y;
	lv_align_t align;
	int intval;

	if (argc != 3 && argc != 4)
		return (enif_make_badarg(env));

	rc = parse_enum(env, argv[2], align_specs, false, &intval);
	if (rc)
		return (make_errno(env, rc));
	align = intval;

	if (argc == 4) {
		if (!enif_get_tuple(env, argv[3], &ctuplen, &ctup))
			return (enif_make_badarg(env));
		if (ctuplen != 2)
			return (enif_make_badarg(env));
		if (!enif_get_int(env, ctup[0], &x))
			return (enif_make_badarg(env));
		if (!enif_get_int(env, ctup[1], &y))
			return (enif_make_badarg(env));
	} else {
		x = 0;
		y = 0;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (enif_get_resource(env, argv[1], lvkid_hdl_rsrc, (void **)&rhdl)) {
		rv = enif_make_badarg(env);
		goto out;
	}
	if (rhdl->lvkh_kid != hdl->lvkh_kid ||
	    rhdl->lvkh_type != LVK_OBJ ||
	    rhdl->lvkh_inst != hdl->lvkh_inst) {
		rv = enif_make_badarg(env);
		goto out;
	}
	refobj = rhdl->lvkh_ptr;
	assert(refobj->lvko_inst == obj->lvko_inst);
	assert(refobj->lvko_kid == obj->lvko_kid);

	rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_align_to,
	    ARG_OBJPTR, obj,
	    ARG_OBJPTR, refobj,
	    ARG_UINT32, align,
	    ARG_UINT32, x,
	    ARG_UINT32, y,
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
rlvgl_obj_align(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *hdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	int x, y;
	lv_align_t align;
	int intval;

	if (argc != 2 && argc != 3)
		return (enif_make_badarg(env));

	if ((rc = parse_enum(env, argv[1], align_specs, false, &intval)))
		return (make_errno(env, rc));
	align = intval;

	if (argc == 3) {
		if (!enif_get_tuple(env, argv[2], &ctuplen, &ctup))
			return (enif_make_badarg(env));
		if (ctuplen != 2)
			return (enif_make_badarg(env));
		if (!enif_get_int(env, ctup[0], &x))
			return (enif_make_badarg(env));
		if (!enif_get_int(env, ctup[1], &y))
			return (enif_make_badarg(env));
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &hdl, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (argc == 3) {
		rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
		    ARG_NONE, lv_obj_align,
		    ARG_OBJPTR, obj,
		    ARG_UINT32, align,
		    ARG_UINT32, x,
		    ARG_UINT32, y,
		    ARG_NONE);
	} else {
		rc = lvk_icall(obj->lvko_inst, rlvgl_call_cb, ncd,
		    ARG_NONE, lv_obj_set_align,
		    ARG_OBJPTR, obj,
		    ARG_UINT32, align,
		    ARG_NONE);
	}

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
rlvgl_img_set_src(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *ohdl = NULL;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	char atom[24];
	ErlNifBinary bin;
	enum arg_type atype;
	const struct symbol_src *ss;
	void *arg;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		atype = ARG_PTR;
		for (ss = symbol_srcs; ss->ss_atom != NULL; ++ss) {
			if (strcmp(atom, ss->ss_atom) == 0)
				break;
		}
		if (ss->ss_atom == NULL)
			return (enif_make_badarg(env));
		arg = (void *)ss->ss_value;
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

static void
rlvgl_setup_buf_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct nif_call_data *ncd = priv;
	struct lvkbuf *buf = ncd->ncd_priv;
	struct lvkid *kid = buf->lvkb_kid;
	struct lvkinst *inst;
	ERL_NIF_TERM msg;
	ErlNifEnv *env = ncd->ncd_env;

	assert(nrd == 1);

	if (rd[0]->rd_error != 0) {
		msg = enif_make_tuple4(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "error"),
		    enif_make_uint(env, rd[0]->rd_error),
		    enif_make_string(env, strerror(rd[0]->rd_error),
		    ERL_NIF_LATIN1));
	} else {
		pthread_rwlock_wrlock(&kid->lvk_lock);
		inst = buf->lvkb_inst;
		if (inst != NULL)
			pthread_rwlock_wrlock(&inst->lvki_lock);
		buf->lvkb_ptr = rd[0]->rd_return.rdr_val;
		if (inst != NULL)
			pthread_rwlock_unlock(&inst->lvki_lock);
		pthread_rwlock_unlock(&kid->lvk_lock);

		msg = enif_make_tuple2(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "ok"));
	}

	enif_send(NULL, &ncd->ncd_owner, env, msg);
	enif_free_env(env);
	free(ncd);
}

static void
rlvgl_setup_event_cb(struct rdesc **rd, uint nrd, void *priv)
{
	struct nif_call_data *ncd = priv;
	ERL_NIF_TERM msg;
	ErlNifEnv *env = ncd->ncd_env;

	assert(nrd == 1);

	if (rd[0]->rd_error != 0) {
		msg = enif_make_tuple4(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "error"),
		    enif_make_uint(env, rd[0]->rd_error),
		    enif_make_string(env, strerror(rd[0]->rd_error),
		    ERL_NIF_LATIN1));
	} else {
		msg = enif_make_tuple2(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "ok"));
	}

	enif_send(NULL, &ncd->ncd_owner, env, msg);
	enif_free_env(env);
	free(ncd);
}

static ERL_NIF_TERM
rlvgl_setup_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkobj *obj;
	struct lvkhdl *ohdl = NULL, *ehdl = NULL;
	struct nif_call_data *ncd = NULL;
	struct cdesc cd;
	struct lvkid *kid;
	struct lvkevt *evt = NULL;
	lv_event_code_t filter;
	char atom[20];
	const struct event_code *ec;
	ERL_NIF_TERM rv, msgref;
	uint do_release;
	int rc;

	if (argc != 2 && argc != 3)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));
	for (ec = event_codes; ec->ec_str != NULL; ++ec) {
		if (strcmp(ec->ec_str, atom) == 0) {
			filter = ec->ec_code;
			break;
		}
	}
	if (ec->ec_str == NULL)
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_obj_hdl(env, argv[0], &ohdl, &obj, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	kid = obj->lvko_kid;

	evt = calloc(1, sizeof (struct lvkevt));
	assert(evt != NULL);
	evt->lvke_kid = kid;
	evt->lvke_obj = obj;
	evt->lvke_evt = filter;

	evt->lvke_env = enif_alloc_env();
	evt->lvke_msgref = enif_make_copy(evt->lvke_env, msgref);
	evt->lvke_owner = ncd->ncd_owner;

	if (argc == 3) {
		evt->lvke_msg = enif_make_copy(evt->lvke_env, argv[2]);
		evt->lvke_custom_msg = 1;
	}

	LIST_INSERT_HEAD(&kid->lvk_evts, evt, lvke_kid_entry);
	LIST_INSERT_HEAD(&obj->lvko_events, evt, lvke_obj_entry);

	ehdl = lvkid_make_hdl(LVK_EVT, evt, &do_release);
	enif_monitor_process(env, ehdl, &evt->lvke_owner, &ehdl->lvkh_mon);

	cd = (struct cdesc){
		.cd_op = CMD_SETUP_EVENT,
		.cd_setup_event = (struct cdesc_setupev){
			.cdse_obj = obj->lvko_ptr,
			.cdse_udata = (uint64_t)evt,
			.cdse_event = filter
		}
	};
	lvk_cmd(kid, &cd, 1, rlvgl_setup_event_cb, ncd);

	ncd = NULL;
	evt = NULL;
	rv = enif_make_tuple3(env,
	    enif_make_atom(env, "async"),
	    enif_make_resource(env, ehdl),
	    msgref);

	if (do_release)
		enif_release_resource(ehdl);

out:
	leave_hdl(ohdl);
	free_ncd(ncd);
	free(evt);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_make_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkbuf *buf = NULL;
	struct lvkinst *inst = NULL;
	struct lvkhdl *ihdl = NULL, *bhdl = NULL;
	struct nif_call_data *ncd = NULL;
	struct cdesc cd[8];
	struct lvkid *kid;
	ERL_NIF_TERM rv, msgref;
	uint do_release;
	int rc;
	ErlNifBinary bin;
	uint nc = 1;
	size_t rem, off, take;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &ihdl, &inst, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	kid = inst->lvki_kid;

	buf = calloc(1, sizeof (struct lvkbuf));
	assert(buf != NULL);
	buf->lvkb_kid = kid;
	buf->lvkb_inst = inst;
	buf->lvkb_len = bin.size;

	ncd->ncd_priv = buf;

	LIST_INSERT_HEAD(&inst->lvki_bufs, buf, lvkb_entry);

	bhdl = lvkid_make_hdl(LVK_BUF, buf, &do_release);

	cd[0] = (struct cdesc){
		.cd_op = CMD_COPY_BUF,
		.cd_chain = 0,
		.cd_copy_buf = (struct cdesc_copybuf){
			.cdcs_len = bin.size,
		}
	};
	rem = bin.size;
	off = 0;

	take = sizeof (cd[0].cd_copy_buf.cdcs_data);
	if (take > rem)
		take = rem;
	bcopy(&bin.data[off], cd[0].cd_copy_buf.cdcs_data, take);
	rem -= take;
	off += take;

	nc = 1;
	while (nc < 8 && rem > 0) {
		cd[nc - 1].cd_chain = 1;
		cd[nc] = (struct cdesc){
			.cd_op = CMD_COPY_BUF,
			.cd_chain = 0,
		};
		take = sizeof (cd[nc].cd_data);
		if (take > rem)
			take = rem;
		bcopy(&bin.data[off], cd[nc].cd_data, take);
		rem -= take;
		off += take;
		++nc;
	}
	lvk_cmd(kid, cd, nc, rlvgl_setup_buf_cb, ncd);

	ncd = NULL;
	buf = NULL;
	rv = enif_make_tuple3(env,
	    enif_make_atom(env, "async"),
	    enif_make_resource(env, bhdl),
	    msgref);

	if (do_release)
		enif_release_resource(bhdl);

out:
	leave_hdl(ihdl);
	free_ncd(ncd);
	free(buf);
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
rlvgl_send_wheel_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	int dy;
	int rc;
	ERL_NIF_TERM rv;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_int(env, argv[1], &dy))
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icast(inst,
	    ARG_NONE, lv_wheel_scroll_by,
	    ARG_PTR, inst->lvki_disp,
	    ARG_PTR, inst->lvki_mouse,
	    ARG_UINT32, dy,
	    ARG_UINT32, LV_ANIM_ON,
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
	char atm[16];
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
	    ARG_PTR, inst->lvki_kbd_drv,
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
rlvgl_send_text(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
	ErlNifBinary bin;
	ERL_NIF_TERM msgref, rv;
	struct nif_call_data *ncd = NULL;
	int rc;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return (enif_make_badarg(env));

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
	    ARG_NONE, lv_indev_send_text,
	    ARG_PTR, inst->lvki_kbd,
	    ARG_INLINE_BUF, bin.data, bin.size,
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
rlvgl_flush_done(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL;
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
		rv = enif_make_tuple2(env,
		    enif_make_atom(env, "error"),
		    enif_make_atom(env, "busy"));
		goto out;
	}

	if (inst->lvki_state != LVKINST_ALIVE || !inst->lvki_flushing) {
		rv = enif_make_tuple2(env,
		    enif_make_atom(env, "error"),
		    enif_make_atom(env, "teardown"));
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

static ERL_NIF_TERM
rlvgl_read_framebuffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl = NULL, *fbhdl;
	struct fbuf *fb;
	lv_color_t *buf;
	int rc;
	ERL_NIF_TERM rv, pixdata, pixtup;
	uint x1, x2, y1, y2;
	const ERL_NIF_TERM *tup;
	int tuplen;
	lv_area_t rect, tile;
	uint do_release;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_tuple(env, argv[1], &tuplen, &tup))
		return (enif_make_badarg(env));
	if (tuplen != 4)
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, tup[0], &x1))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, tup[1], &y1))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, tup[2], &x2))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, tup[3], &y2))
		return (enif_make_badarg(env));

	rect.x1 = x1;
	rect.x2 = x2;
	rect.y1 = y1;
	rect.y2 = y2;
	bzero(&tile, sizeof (tile));

	rc = enter_inst_hdl(env, argv[0], &hdl, &inst, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	fb = inst->lvki_fbuf;
	buf = inst->lvki_cfb;
	fbhdl = lvkid_make_hdl(LVK_FBUF, fb, &do_release);
	if (hdl->lvkh_fbuf == NULL)
		hdl->lvkh_fbuf = buf;

	if (rect.x2 >= fb->fb_w)
		rect.x2 = fb->fb_w - 1;
	if (rect.y2 >= fb->fb_h)
		rect.y2 = fb->fb_h - 1;

	rv = enif_make_list(env, 0);
	while (lvk_next_tile(&rect, &tile)) {
		pixdata = lvk_tile_to_iolist(env, fb, buf, fbhdl, &tile);
		pixtup = enif_make_tuple2(env,
		    enif_make_tuple4(env,
		    	enif_make_int(env, tile.x1),
		    	enif_make_int(env, tile.y1),
		    	enif_make_int(env, tile.x2),
		    	enif_make_int(env, tile.y2)),
		    pixdata);
		rv = enif_make_list_cell(env, pixtup, rv);
	}

	rv = enif_make_tuple2(env, enif_make_atom(env, "ok"), rv);

	if (do_release)
		enif_release_resource(fbhdl);

out:
	leave_hdl(hdl);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_prefork(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	uint n;

	if (argc != 1)
		return (enif_make_badarg(env));

	if (!enif_get_uint(env, argv[0], &n))
		return (enif_make_badarg(env));

	lvkid_prefork(n);

	return (enif_make_atom(env, "ok"));
}

static int
rlvgl_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info)
{
	lvk_open_resource_types(env);
	log_setup();
	return (0);
}

static void
rlvgl_nif_unload(ErlNifEnv *env, void *priv_data)
{
}

static ErlNifFunc nif_funcs[] = {
	/* control commands */
	{ "setup_instance", 	1, rlvgl_setup_instance },
	{ "setup_event",	2, rlvgl_setup_event },
	{ "setup_event",	3, rlvgl_setup_event },
	{ "set_kbd_group",	2, rlvgl_set_kbd_group },
	{ "send_text",		2, rlvgl_send_text },
	{ "send_pointer_event", 3, rlvgl_send_pointer_event },
	{ "send_key_event", 	3, rlvgl_send_key_event },
	{ "send_wheel_event",	2, rlvgl_send_wheel_event },
	{ "flush_done", 	1, rlvgl_flush_done },
	{ "prefork",		1, rlvgl_prefork },
	{ "read_framebuffer",	2, rlvgl_read_framebuffer },
	{ "make_buffer",	2, rlvgl_make_buffer },
	{ "take_log_ownership",	0, rlvgl_take_log_ownership },

	/* lvgl APIs */
	//{ "obj_clear_flags",		2, rlvgl_obj_clear_flags },
	//{ "obj_has_all_flags",		2, rlvgl_obj_has_all_flags },
	//{ "obj_has_any_flags",		2, rlvgl_obj_has_any_flags },
	//{ "style_set_layout",		2, rlvgl_style_set_layout },
	{ "btn_create",			1, rlvgl_btn_create },
	{ "btnmatrix_create",		1, rlvgl_btnmatrix_create },
	{ "checkbox_create",		1, rlvgl_checkbox_create },
	{ "checkbox_get_text",		1, rlvgl_checkbox_get_text },
	{ "checkbox_set_text",		2, rlvgl_checkbox_set_text },
	{ "disp_get_layer_sys", 	1, rlvgl_disp_get_layer_sys },
	{ "disp_set_bg_color", 		2, rlvgl_disp_set_bg_color },
	{ "dropdown_create",		1, rlvgl_dropdown_create },
	{ "group_add_obj",		2, rlvgl_group_add_obj },
	{ "group_create",		1, rlvgl_group_create },
	{ "img_create", 		1, rlvgl_img_create },
	{ "img_set_offset", 		2, rlvgl_img_set_offset },
	{ "img_set_src", 		2, rlvgl_img_set_src },
	{ "imgbtn_create",		1, rlvgl_imgbtn_create },
	{ "label_create",		1, rlvgl_label_create },
	{ "label_set_text",		2, rlvgl_label_set_text },
	{ "led_create",			1, rlvgl_led_create },
	{ "list_create",		1, rlvgl_list_create },
	{ "menu_create",		1, rlvgl_menu_create },
	{ "obj_add_flags",		2, rlvgl_obj_add_flags },
	{ "obj_add_state",		2, rlvgl_obj_add_state },
	{ "obj_add_style",		2, rlvgl_obj_add_style },
	{ "obj_add_style",		3, rlvgl_obj_add_style },
	{ "obj_align", 			2, rlvgl_obj_align },
	{ "obj_align", 			3, rlvgl_obj_align },
	{ "obj_align_to", 		3, rlvgl_obj_align_to },
	{ "obj_align_to", 		4, rlvgl_obj_align_to },
	{ "obj_center", 		1, rlvgl_obj_center },
	{ "obj_clear_state",		2, rlvgl_obj_clear_state },
	{ "obj_create", 		2, rlvgl_obj_create },
	{ "obj_get_pos",		1, rlvgl_obj_get_pos },
	{ "obj_get_size",		1, rlvgl_obj_get_size },
	{ "obj_get_state",		1, rlvgl_obj_get_state },
	{ "obj_set_size",		2, rlvgl_obj_set_size },
	{ "roller_create",		1, rlvgl_roller_create },
	{ "scr_load", 			2, rlvgl_scr_load },
	{ "scr_load_anim",		6, rlvgl_scr_load_anim },
	{ "set_mouse_cursor", 		2, rlvgl_set_mouse_cursor },
	{ "slider_create",		1, rlvgl_slider_create },
	{ "spinner_create", 		3, rlvgl_spinner_create },
	{ "style_create",		1, rlvgl_style_create },
	{ "style_set_flex_align",	4, rlvgl_style_set_flex_align },
	{ "style_set_flex_flow",	2, rlvgl_style_set_flex_flow },
	{ "style_set_prop",		3, rlvgl_style_set_prop },
	{ "switch_create",		1, rlvgl_switch_create },
	{ "table_create",		1, rlvgl_table_create },
	{ "textarea_create",		1, rlvgl_textarea_create },
	{ "textarea_get_text",		1, rlvgl_textarea_get_text },
	{ "textarea_set_accepted_chars", 2, rlvgl_textarea_set_accepted_chars },
	{ "textarea_set_one_line",	2, rlvgl_textarea_set_one_line },
	{ "textarea_set_password_mode",	2, rlvgl_textarea_set_password_mode },
	{ "textarea_set_placeholder_text", 2, rlvgl_textarea_set_placeholder_text },
	{ "textarea_set_text",		2, rlvgl_textarea_set_text },
	{ "textarea_set_text_selection",	2, rlvgl_textarea_set_text_selection },
};

ERL_NIF_INIT(rdp_lvgl_nif, nif_funcs, rlvgl_nif_load, NULL, NULL,
    rlvgl_nif_unload);
