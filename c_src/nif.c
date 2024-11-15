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

struct zerobuf {
	char	*zb_data;
	size_t	 zb_len;
	bool	 zb_secure;
};

static void
zerobuf_dtor(ErlNifEnv *env, void *arg)
{
	struct zerobuf *zb = arg;
	if (zb->zb_secure) {
		lvk_secure_free(zb->zb_data, zb->zb_len);
	} else {
		explicit_bzero(zb->zb_data, zb->zb_len);
		free(zb->zb_data);
	}
	zb->zb_data = NULL;
	zb->zb_len = 0;
}

static ErlNifResourceType *zerobuf_rsrc;
static ErlNifResourceTypeInit zerobuf_ops = {
	.dtor	= zerobuf_dtor,
};

struct nif_call_data {
	ErlNifEnv		*ncd_env;
	ERL_NIF_TERM		 ncd_msgref;
	ErlNifPid		 ncd_owner;
	void			*ncd_priv;
	const struct enum_spec	*ncd_enum;
	bool			 ncd_multi;
	bool			 ncd_ret_secure;
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
	struct lvkchartser *cser;
	struct lvkchartcur *ccur;
	struct lvkmeterind *mi;
	struct lvkmeterscl *ms;
	struct lvkspan *sp;
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
	struct zerobuf *zb;

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
	case ARG_PTR_OBJ:
		obj = rv;
		if (obj == NULL) {
			rterm = enif_make_atom(env, "null");
			break;
		}
		pthread_rwlock_wrlock(&obj->lvko_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_OBJ, obj, &do_release);
		pthread_rwlock_unlock(&obj->lvko_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_STYLE:
		sty = rv;
		pthread_rwlock_wrlock(&sty->lvks_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_STY, sty, &do_release);
		pthread_rwlock_unlock(&sty->lvks_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_GROUP:
		grp = rv;
		pthread_rwlock_wrlock(&grp->lvkg_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_GRP, grp, &do_release);
		pthread_rwlock_unlock(&grp->lvkg_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_CHART_SER:
		cser = rv;
		pthread_rwlock_wrlock(&cser->lvkcs_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_CHART_SER, cser, &do_release);
		pthread_rwlock_unlock(&cser->lvkcs_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_CHART_CUR:
		ccur = rv;
		pthread_rwlock_wrlock(&ccur->lvkcc_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_CHART_CUR, ccur, &do_release);
		pthread_rwlock_unlock(&ccur->lvkcc_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_METER_IND:
		mi = rv;
		pthread_rwlock_wrlock(&mi->lvkmi_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_METER_IND, mi, &do_release);
		pthread_rwlock_unlock(&mi->lvkmi_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_METER_SCL:
		ms = rv;
		pthread_rwlock_wrlock(&ms->lvkms_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_METER_SCL, ms, &do_release);
		pthread_rwlock_unlock(&ms->lvkms_inst->lvki_lock);
		rterm = enif_make_resource(env, hdl);
		if (do_release)
			enif_release_resource(hdl);
		break;
	case ARG_PTR_SPAN:
		sp = rv;
		pthread_rwlock_wrlock(&sp->lvksp_inst->lvki_lock);
		hdl = lvkid_make_hdl(LVK_SPAN, sp, &do_release);
		pthread_rwlock_unlock(&sp->lvksp_inst->lvki_lock);
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

		zb = enif_alloc_resource(zerobuf_rsrc, sizeof (struct zerobuf));
		assert(zb != NULL);
		bzero(zb, sizeof (*zb));

		zb->zb_len = bin->size;
		if (ncd->ncd_ret_secure) {
			zb->zb_data = lvk_secure_alloc(zb->zb_len);
			zb->zb_secure = true;
		} else {
			zb->zb_data = malloc(zb->zb_len);
		}
		assert(zb->zb_data != NULL);
		bcopy(bin->data, zb->zb_data, zb->zb_len);

		rterm = enif_make_resource_binary(env, zb, zb->zb_data,
		    zb->zb_len);

		enif_release_resource(zb);

		explicit_bzero(bin->data, bin->size);
		enif_release_binary(bin);

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
rlvgl_configure(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char atom[32];
	uint uval, uval2;
	int tuplen;
	const ERL_NIF_TERM *tup;

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[0], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));

	if (strcmp(atom, "fbufs_per_child") == 0) {
		if (!enif_get_uint(env, argv[1], &uval))
			return (enif_make_badarg(env));
		set_fbufs_per_child(uval);

	} else if (strcmp(atom, "ring_size") == 0) {
		if (!enif_get_uint(env, argv[1], &uval))
			return (enif_make_badarg(env));
		set_ring_size(uval);

	} else if (strcmp(atom, "fb_max_res") == 0) {
		if (!enif_get_tuple(env, argv[1], &tuplen, &tup))
			return (enif_make_badarg(env));
		if (tuplen != 2)
			return (enif_make_badarg(env));
		if (!enif_get_uint(env, tup[0], &uval))
			return (enif_make_badarg(env));
		if (!enif_get_uint(env, tup[1], &uval2))
			return (enif_make_badarg(env));
		set_fb_max_res(uval, uval2);

	} else if (strcmp(atom, "max_lvkids") == 0) {
		if (!enif_get_uint(env, argv[1], &uval))
			return (enif_make_badarg(env));
		set_max_lvkids(uval);

	} else {
		return (enif_make_badarg(env));
	}

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

struct nif_lock_state {
	uint		 nls_wrlock;
	struct lvkhdl	*nls_first_hdl;
	struct lvkid	*nls_kid;
	struct lvkinst	*nls_inst;
};

static int
enter_inst_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct nif_lock_state *nls,
    struct lvkinst **pinst, uint wrlock)
{
	struct lvkid *kid;
	struct lvkhdl *hdl;
	struct lvkinst *inst;

	if (nls->nls_first_hdl != NULL)
		assert(nls->nls_wrlock == wrlock);

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&kid->lvk_lock);
		else
			pthread_rwlock_rdlock(&kid->lvk_lock);
	} else if (nls->nls_kid != kid) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "inst_hdl_not_same_kid"), term));
		return (EINVAL);
	}

	if (hdl->lvkh_type != LVK_INST) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_instance"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_ptr;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&inst->lvki_lock);
		else
			pthread_rwlock_rdlock(&inst->lvki_lock);
		nls->nls_first_hdl = hdl;
		nls->nls_kid = kid;
		nls->nls_inst = inst;
		nls->nls_wrlock = wrlock;
	}

	assert(inst->lvki_kid == kid);

	*pinst = inst;

	return (0);
}

static int
unpack_buf_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct nif_lock_state *nls,
    struct lvkbuf **pbuf)
{
	struct lvkhdl *hdl;
	struct lvkid *kid;

	assert(nls->nls_first_hdl != NULL);

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;
	if (nls->nls_kid != kid) {
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
enter_obj_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct nif_lock_state *nls,
    struct lvkobj **pobj, uint wrlock)
{
	struct lvkhdl *hdl;
	struct lvkobj *obj;
	struct lvkinst *inst;
	struct lvkid *kid;

	if (nls->nls_first_hdl != NULL)
		assert(nls->nls_wrlock == wrlock);

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		if (nls->nls_first_hdl != NULL && enif_is_atom(env, term)) {
			char atom[32];
			if (!enif_get_atom(env, term, atom, sizeof (atom),
			    ERL_NIF_LATIN1)) {
				enif_raise_exception(env, enif_make_tuple2(env,
				    enif_make_atom(env, "bad_lvgl_handle"),
				    term));
				return (EINVAL);
			}
			if (strcmp(atom, "none") != 0 &&
			    strcmp(atom, "null") != 0) {
				enif_raise_exception(env, enif_make_tuple2(env,
				    enif_make_atom(env, "bad_lvgl_handle"),
				    term));
				return (EINVAL);
			}
			*pobj = NULL;
			return (0);
		}
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&kid->lvk_lock);
		else
			pthread_rwlock_rdlock(&kid->lvk_lock);
	} else if (nls->nls_kid != kid) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "obj_hdl_not_same_kid"), term));
		return (EINVAL);
	}

	if (hdl->lvkh_type != LVK_OBJ) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "not_lv_object"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_inst;
	obj = hdl->lvkh_ptr;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&inst->lvki_lock);
		else
			pthread_rwlock_rdlock(&inst->lvki_lock);
		nls->nls_first_hdl = hdl;
		nls->nls_kid = kid;
		nls->nls_inst = inst;
		nls->nls_wrlock = wrlock;
	}

	assert(obj->lvko_kid == kid);

	*pobj = obj;

	return (0);
}

static inline int
enter_hdl(ErlNifEnv *env, ERL_NIF_TERM term, enum lvkh_type want_type,
    struct nif_lock_state *nls, void **pptr, uint wrlock)
{
	struct lvkhdl *hdl;
	struct lvkinst *inst;
	struct lvkid *kid;

	if (nls->nls_first_hdl != NULL)
		assert(nls->nls_wrlock == wrlock);

	if (!enif_get_resource(env, term, lvkid_hdl_rsrc, (void **)&hdl)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "bad_lvgl_handle"), term));
		return (EINVAL);
	}

	kid = hdl->lvkh_kid;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&kid->lvk_lock);
		else
			pthread_rwlock_rdlock(&kid->lvk_lock);
	} else if (nls->nls_kid != kid) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "hdl_not_same_kid"), term));
		return (EINVAL);
	}

	if (hdl->lvkh_type != want_type) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "wrong_hdl_type"), term));
		pthread_rwlock_unlock(&kid->lvk_lock);
		return (EBADF);
	}

	inst = hdl->lvkh_inst;
	*pptr = hdl->lvkh_ptr;

	if (nls->nls_first_hdl == NULL) {
		if (wrlock)
			pthread_rwlock_wrlock(&inst->lvki_lock);
		else
			pthread_rwlock_rdlock(&inst->lvki_lock);
		nls->nls_first_hdl = hdl;
		nls->nls_kid = kid;
		nls->nls_inst = inst;
		nls->nls_wrlock = wrlock;
	}

	return (0);
}

static int
enter_style_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct nif_lock_state *nls,
    struct lvkstyle **psty, uint wrlock)
{
	int rc;
	struct lvkstyle *sty;

	rc = enter_hdl(env, term, LVK_STY, nls, (void **)&sty, wrlock);
	if (rc != 0)
		return (rc);

	assert(sty->lvks_kid == nls->nls_kid);
	assert(sty->lvks_inst == nls->nls_inst);

	*psty = sty;

	return (0);
}

static int
enter_group_hdl(ErlNifEnv *env, ERL_NIF_TERM term, struct nif_lock_state *nls,
    struct lvkgroup **pgrp, uint wrlock)
{
	int rc;
	struct lvkgroup *grp;

	rc = enter_hdl(env, term, LVK_GRP, nls, (void **)&grp, wrlock);
	if (rc != 0)
		return (rc);

	assert(grp->lvkg_kid == nls->nls_kid);
	assert(grp->lvkg_inst == nls->nls_inst);

	*pgrp = grp;

	return (0);
}

static int
enter_chartser_hdl(ErlNifEnv *env, ERL_NIF_TERM term,
    struct nif_lock_state *nls, struct lvkchartser **pcser, uint wrlock)
{
	int rc;
	struct lvkchartser *cser;

	rc = enter_hdl(env, term, LVK_CHART_SER, nls, (void **)&cser, wrlock);
	if (rc != 0)
		return (rc);

	assert(cser->lvkcs_kid == nls->nls_kid);
	assert(cser->lvkcs_inst == nls->nls_inst);

	*pcser = cser;

	return (0);
}

#if 0
static int
enter_chartcur_hdl(ErlNifEnv *env, ERL_NIF_TERM term,
    struct nif_lock_state *nls, struct lvkchartcur **pccur, uint wrlock)
{
	int rc;
	struct lvkchartcur *ccur;

	rc = enter_hdl(env, term, LVK_CHART_CUR, nls, (void **)&ccur, wrlock);
	if (rc != 0)
		return (rc);

	assert(ccur->lvkcc_kid == nls->nls_kid);
	assert(ccur->lvkcc_inst == nls->nls_inst);

	*pccur = ccur;

	return (0);
}
#endif

static int
enter_meterind_hdl(ErlNifEnv *env, ERL_NIF_TERM term,
    struct nif_lock_state *nls, struct lvkmeterind **pmi, uint wrlock)
{
	int rc;
	struct lvkmeterind *mi;

	rc = enter_hdl(env, term, LVK_METER_IND, nls, (void **)&mi, wrlock);
	if (rc != 0)
		return (rc);

	assert(mi->lvkmi_kid == nls->nls_kid);
	assert(mi->lvkmi_inst == nls->nls_inst);

	*pmi = mi;

	return (0);
}

static int
enter_meterscl_hdl(ErlNifEnv *env, ERL_NIF_TERM term,
    struct nif_lock_state *nls, struct lvkmeterscl **pms, uint wrlock)
{
	int rc;
	struct lvkmeterscl *ms;

	rc = enter_hdl(env, term, LVK_METER_SCL, nls, (void **)&ms, wrlock);
	if (rc != 0)
		return (rc);

	assert(ms->lvkms_kid == nls->nls_kid);
	assert(ms->lvkms_inst == nls->nls_inst);

	*pms = ms;

	return (0);
}

static int
enter_span_hdl(ErlNifEnv *env, ERL_NIF_TERM term,
    struct nif_lock_state *nls, struct lvkspan **psp, uint wrlock)
{
	int rc;
	struct lvkspan *sp;

	rc = enter_hdl(env, term, LVK_SPAN, nls, (void **)&sp, wrlock);
	if (rc != 0)
		return (rc);

	assert(sp->lvksp_kid == nls->nls_kid);
	assert(sp->lvksp_inst == nls->nls_inst);

	*psp = sp;

	return (0);
}

static void
leave_nif(struct nif_lock_state *nls)
{
	struct lvkid *kid;
	struct lvkinst *inst;
	if (nls->nls_first_hdl == NULL)
		return;
	kid = nls->nls_kid;
	inst = nls->nls_inst;
	pthread_rwlock_unlock(&inst->lvki_lock);
	pthread_rwlock_unlock(&kid->lvk_lock);
	bzero(nls, sizeof (*nls));
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
	if (!enif_get_uint(env, ctup[2], &b))
		return (0);

	*pcol = lv_color_make(r, g, b);

	return (1);
}

static ERL_NIF_TERM
rlvgl_obj_get_class1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	ERL_NIF_TERM rv;
	int rc;
	struct lvkobj *obj;
	const struct class_spec *cs;
	const char *name;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	for (cs = class_specs; cs->cs_name != NULL; ++cs) {
		if (cs->cs_class == obj->lvko_class)
			break;
	}
	name = cs->cs_name;
	if (name == NULL)
		name = "unknown";

	rv = enif_make_atom(env, name);

out:
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_has_class2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	ERL_NIF_TERM rv;
	int rc;
	struct lvkobj *obj;
	const struct class_spec *cs;
	char atom[32];

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1))
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	for (cs = class_specs; cs->cs_name != NULL; ++cs) {
		if (strcmp(cs->cs_name, atom) == 0)
			break;
	}
	if (cs->cs_name == NULL) {
		rv = enif_make_badarg(env);
		goto out;
	}

	if (lv_obj_class_has_base(obj->lvko_class, cs->cs_class)) {
		rv = enif_make_atom(env, "true");
	} else {
		rv = enif_make_atom(env, "false");
	}

out:
	leave_nif(&nls);
	return (rv);
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

static int
parse_style_prop_val(ErlNifEnv *env, ERL_NIF_TERM kterm, ERL_NIF_TERM vterm,
    lv_style_prop_t *pprop, lv_style_value_t *pval)
{
	char atom[32];
	lv_color_t col;
	const lv_font_t *font;
	const struct style_prop *sp;
	int intval;
	int rc;
	lv_coord_t coord;

	if (!enif_get_atom(env, kterm, atom, sizeof (atom), ERL_NIF_LATIN1)) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "invalid_style_prop"),
		    kterm));
		return (EINVAL);
	}
	for (sp = style_props; sp->sp_str != NULL; ++sp) {
		if (strcmp(sp->sp_str, atom) == 0)
			break;
	}
	if (sp->sp_str == NULL) {
		enif_raise_exception(env, enif_make_tuple2(env,
		    enif_make_atom(env, "unsupported_style_prop"),
		    kterm));
		return (EINVAL);
	}

	*pprop = sp->sp_prop;

	switch (sp->sp_type) {
	case SPT_INT32:
		if (!enif_get_int(env, vterm, &intval)) {
			enif_raise_exception(env, enif_make_tuple3(env,
			    enif_make_atom(env, "bad_number"),
			    kterm, vterm));
			return (EINVAL);
		}
		*pval = (lv_style_value_t){ .num = intval };
		break;
	case SPT_COORD:
		if ((rc = parse_coord(env, vterm, &coord)))
			return (rc);
		*pval = (lv_style_value_t){ .num = (int32_t)coord };
		break;
	case SPT_COLOR:
		if (!enif_get_color(env, vterm, &col)) {
			enif_raise_exception(env, enif_make_tuple3(env,
			    enif_make_atom(env, "bad_color"),
			    kterm, vterm));
			return (EINVAL);
		}
		*pval = (lv_style_value_t){ .color = col };
		break;
	case SPT_ENUM:
		rc = parse_enum(env, vterm, sp->sp_enum, false, &intval);
		if (rc)
			return (rc);
		*pval = (lv_style_value_t){ .num = intval };
		break;
	case SPT_MULTI_ENUM:
		rc = parse_enum(env, vterm, sp->sp_enum, true, &intval);
		if (rc)
			return (rc);
		*pval = (lv_style_value_t){ .num = intval };
		break;
	case SPT_FONT:
		rc = parse_font_spec(env, vterm, &font);
		if (rc)
			return (rc);
		*pval = (lv_style_value_t){ .ptr = font };
		break;
	default:
		assert(0);
		return (EINVAL);
	}

	return (0);
}

static int
parse_img_src(ErlNifEnv *env, ERL_NIF_TERM term, ErlNifBinary *bin,
    enum arg_type *patype, void **parg)
{
	char atom[32];
	const struct symbol_src *ss;

	if (enif_get_atom(env, term, atom, sizeof (atom), ERL_NIF_LATIN1)) {
		*patype = ARG_PTR;
		if (strcmp(atom, "none") == 0) {
			*parg = 0;
			return (0);
		}
		for (ss = symbol_srcs; ss->ss_atom != NULL; ++ss) {
			if (strcmp(atom, ss->ss_atom) == 0)
				break;
		}
		if (ss->ss_atom == NULL) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "invalid_symbol_atom"),
			    term));
			return (EINVAL);
		}
		*parg = (void *)ss->ss_value;
		return (0);
	}

	if (enif_inspect_iolist_as_binary(env, term, bin) && bin->size > 0) {
		*patype = ARG_INLINE_BUF;
		if (bin->size > UINT8_MAX) {
			enif_raise_exception(env, enif_make_tuple2(env,
			    enif_make_atom(env, "inline_str_too_long"),
			    term));
			return (E2BIG);
		}
		*parg = bin;
		return (0);
	}

	enif_raise_exception(env, enif_make_tuple2(env,
	    enif_make_atom(env, "invalid_img_src"),
	    term));
	return (EINVAL);
}

static void
rlvgl_setup_buf_cb(const struct rdesc *rd, const void *data, size_t dlen, void *priv)
{
	struct nif_call_data *ncd = priv;
	struct lvkbuf *buf = ncd->ncd_priv;
	struct lvkid *kid = buf->lvkb_kid;
	struct lvkinst *inst;
	ERL_NIF_TERM msg;
	ErlNifEnv *env = ncd->ncd_env;

	assert(dlen == 0);

	if (rd->rd_error != 0) {
		msg = enif_make_tuple4(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "error"),
		    enif_make_uint(env, rd->rd_error),
		    enif_make_string(env, strerror(rd->rd_error),
		    ERL_NIF_LATIN1));
	} else {
		pthread_rwlock_wrlock(&kid->lvk_lock);
		inst = buf->lvkb_inst;
		if (inst != NULL)
			pthread_rwlock_wrlock(&inst->lvki_lock);
		buf->lvkb_ptr = rd->rd_return.rdr_val;
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
rlvgl_setup_event_cb(const struct rdesc *rd, const void *data, size_t dlen, void *priv)
{
	struct nif_call_data *ncd = priv;
	ERL_NIF_TERM msg;
	ErlNifEnv *env = ncd->ncd_env;

	assert(dlen == 0);

	if (rd->rd_error != 0) {
		msg = enif_make_tuple4(env,
		    ncd->ncd_msgref,
		    enif_make_atom(env, "error"),
		    enif_make_uint(env, rd->rd_error),
		    enif_make_string(env, strerror(rd->rd_error),
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
	struct nif_lock_state nls;
	struct lvkhdl *ehdl;
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

	bzero(&nls, sizeof (nls));

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

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 1);
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
	lvk_cmd0(kid, &cd, rlvgl_setup_event_cb, ncd);

	ncd = NULL;
	evt = NULL;
	rv = enif_make_tuple3(env,
	    enif_make_atom(env, "async"),
	    enif_make_resource(env, ehdl),
	    msgref);

	if (do_release)
		enif_release_resource(ehdl);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	free(evt);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_make_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkbuf *buf = NULL;
	struct lvkinst *inst = NULL;
	struct lvkhdl *bhdl;
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	struct cdesc cd;
	struct lvkid *kid;
	ERL_NIF_TERM rv, msgref;
	uint do_release;
	int rc;
	ErlNifBinary bin;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return (enif_make_badarg(env));

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 1);
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

	cd = (struct cdesc){
		.cd_op = CMD_COPY_BUF,
		.cd_copy_buf = (struct cdesc_copybuf){
			.cdcs_len = bin.size,
		}
	};
	lvk_cmd(kid, &cd, bin.data, bin.size, rlvgl_setup_buf_cb, ncd);

	ncd = NULL;
	buf = NULL;
	rv = enif_make_tuple3(env,
	    enif_make_atom(env, "async"),
	    enif_make_resource(env, bhdl),
	    msgref);

	if (do_release)
		enif_release_resource(bhdl);

out:
	leave_nif(&nls);
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
	struct nif_lock_state nls;
	uint x, y;
	lv_point_t pt;
	lv_indev_state_t st;
	char atm[9];
	ERL_NIF_TERM rv;
	int rc;

	bzero(&nls, sizeof (nls));

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

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
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
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_send_wheel_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct nif_lock_state nls;
	int dy;
	int rc;
	ERL_NIF_TERM rv;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_get_int(env, argv[1], &dy))
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
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
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_send_key_event(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct nif_lock_state nls;
	uint key;
	lv_indev_state_t st;
	char atm[16];
	ERL_NIF_TERM rv;
	int rc;

	bzero(&nls, sizeof (nls));

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

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
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
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_send_text(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct nif_lock_state nls;
	ErlNifBinary bin;
	ERL_NIF_TERM msgref, rv;
	struct nif_call_data *ncd = NULL;
	int rc;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return (enif_make_badarg(env));

	if (bin.size == 0) {
		bin.data = (unsigned char *)"\0";
		bin.size = 1;
	}

	if (bin.size > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = lvk_icall(inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_indev_send_text,
	    ARG_PTR, inst->lvki_kbd,
	    ARG_INLINE_BUF, &bin,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;	/* rlvgl_call_cb owns it now */
	rv = enif_make_tuple2(env, enif_make_atom(env, "async"), msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_flush_done(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct nif_lock_state nls;
	struct shmintf *shm;
	struct pdesc pd;
	int rc;
	ERL_NIF_TERM rv;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 1);
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
		.pd_flags	= 0,
		.pd_disp_drv	= inst->lvki_disp_drv
	};
	shm_produce_phlush(shm, &pd);
	shm_ring_doorbell(shm);

	inst->lvki_flushing = 0;
	rv = enif_make_atom(env, "ok");

out:
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_flush_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct nif_lock_state nls;
	int rc;
	ERL_NIF_TERM rv;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (inst->lvki_fbhdl != NULL) {
		rv = enif_make_atom(env, "busy");
		goto out;
	}

	if (inst->lvki_state != LVKINST_ALIVE) {
		rv = enif_make_atom(env, "teardown");
		goto out;
	}

	if (!inst->lvki_flushing) {
		rv = enif_make_atom(env, "idle");
		goto out;
	}

	rv = enif_make_atom(env, "flushing");

out:
	leave_nif(&nls);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_read_framebuffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *fbhdl;
	struct nif_lock_state nls;
	struct fbuf *fb;
	lv_color_t *buf;
	int rc;
	ERL_NIF_TERM rv, pixdata, pixtup;
	uint x1, x2, y1, y2;
	const ERL_NIF_TERM *tup;
	int tuplen;
	lv_area_t rect, tile;

	bzero(&nls, sizeof (nls));

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

	if (x2 <= x1)
		return (enif_make_badarg(env));
	if (y2 <= y1)
		return (enif_make_badarg(env));

	rect.x1 = x1;
	rect.x2 = x2;
	rect.y1 = y1;
	rect.y2 = y2;
	bzero(&tile, sizeof (tile));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 1);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	fb = inst->lvki_fbuf;
	buf = inst->lvki_cfb;
	fbhdl = lvkid_make_hdl(LVK_FBUF, fb, NULL);

	if (!inst->lvki_flushing) {
		/*
		 * We're going to pretend that the display flush has started
		 * early.
		 *
		 * We might be called before the first flush for frame 1: if
		 * so, use framebuffer A. LVGL will render to that framebuffer
		 * first.
		 *
		 * Otherwise, set the fbhdl's lvkh_fbuf to the _other_ buffer.
		 * We will take pixel data from the non-drawing buffer here
		 * (buf), but the flush belongs to the one currently drawing.
		 */
		if (buf == NULL) {
			buf = fb->fb_a;
			fbhdl->lvkh_fbuf = buf;
		} else if (buf == fb->fb_a) {
			fbhdl->lvkh_fbuf = fb->fb_b;
		} else if (buf == fb->fb_b) {
			fbhdl->lvkh_fbuf = fb->fb_a;
		}
	}

	inst->lvki_cfb = buf;
	inst->lvki_flushing = 1;

	/*
	 * Ensure we're producing at least one valid pixel that's on the screen,
	 * or the logic below will break
	 */
	if (rect.x1 >= fb->fb_w - 1)
		rect.x1 = fb->fb_w - 2;
	if (rect.y1 >= fb->fb_h - 1)
		rect.y1 = fb->fb_h - 2;
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

out:
	leave_nif(&nls);
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

#include "nif_gen.h"

static int
rlvgl_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info)
{
	lvk_open_resource_types(env);
	log_setup();
	zerobuf_rsrc = enif_open_resource_type_x(env, "lv_zerobuf",
	    &zerobuf_ops, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
	    NULL);
	return (0);
}

static void
rlvgl_nif_unload(ErlNifEnv *env, void *priv_data)
{
}

static ErlNifFunc nif_funcs[] = {
	/* control commands */
	{ "configure",		2, rlvgl_configure },
	{ "setup_instance", 	1, rlvgl_setup_instance },
	{ "setup_event",	2, rlvgl_setup_event },
	{ "setup_event",	3, rlvgl_setup_event },
	{ "send_text",		2, rlvgl_send_text },
	{ "send_pointer_event", 3, rlvgl_send_pointer_event },
	{ "send_key_event", 	3, rlvgl_send_key_event },
	{ "send_wheel_event",	2, rlvgl_send_wheel_event },
	{ "flush_done", 	1, rlvgl_flush_done },
	{ "flush_state",	1, rlvgl_flush_state },
	{ "prefork",		1, rlvgl_prefork },
	{ "read_framebuffer",	2, rlvgl_read_framebuffer },
	{ "make_buffer",	2, rlvgl_make_buffer },
	{ "take_log_ownership",	0, rlvgl_take_log_ownership },

	/* lvgl APIs */
	AUTOGEN_NIFS,
	{ "obj_get_class",		1, rlvgl_obj_get_class1 },
	{ "obj_has_class",		2, rlvgl_obj_has_class2 },
};

ERL_NIF_INIT(rdp_lvgl_nif, nif_funcs, rlvgl_nif_load, NULL, NULL,
    rlvgl_nif_unload);
