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
	pthread_rwlock_unlock(&inst->lvki_lock);

	instref = enif_make_resource(env, hdl);
	if (do_release)
		enif_release_resource(hdl);

	return (enif_make_tuple3(env,
	    enif_make_atom(env, "ok"),
	    instref,
	    msgref));
}

static ERL_NIF_TERM
rlvgl_disp_set_bg_color(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	const ERL_NIF_TERM *ctup;
	int ctuplen;
	struct lvkinst *inst;
	struct lvkhdl *hdl;
	uint r, g, b;
	lv_color_t col;

	if (argc != 2)
		return (enif_make_badarg(env));
	if (!enif_get_resource(env, argv[0], lvkid_hdl_rsrc, (void **)&hdl))
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

	if (hdl->lvkh_type != LVK_INST)
		return (enif_make_badarg(env));

	col = lv_color_make(r, g, b);

	inst = hdl->lvkh_ptr;
	pthread_rwlock_rdlock(&inst->lvki_lock);

	lvk_icast(inst, ARG_NONE, lv_disp_set_bg_color,
	    ARG_PTR, inst->lvki_disp,
	    ARG_UINT16, col.full,
	    ARG_NONE);

	pthread_rwlock_unlock(&inst->lvki_lock);

	return (enif_make_atom(env, "ok"));
}

static ERL_NIF_TERM
rlvgl_flush_done(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct lvkinst *inst;
	struct lvkhdl *hdl;
	struct cdesc cd;

	if (argc != 1)
		return (enif_make_badarg(env));
	if (!enif_get_resource(env, argv[0], lvkid_hdl_rsrc, (void **)&hdl))
		return (enif_make_badarg(env));

	if (hdl->lvkh_type != LVK_INST)
		return (enif_make_badarg(env));

	inst = hdl->lvkh_ptr;
	pthread_rwlock_rdlock(&inst->lvki_lock);

	if (inst->lvki_fbhdl != NULL) {
		pthread_rwlock_unlock(&inst->lvki_lock);
		return (enif_make_tuple2(env,
		    enif_make_atom(env, "error"),
		    enif_make_atom(env, "busy")));
	}

	cd = (struct cdesc){
		.cd_op = CMD_FLUSH_DONE,
		.cd_flush_done = (struct cdesc_flushdone){
			.cdfd_disp_drv = inst->lvki_disp_drv
		}
	};
	lvk_cmd(inst->lvki_kid, &cd, 1, NULL, NULL);

	pthread_rwlock_unlock(&inst->lvki_lock);

	return (enif_make_atom(env, "ok"));
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
};

ERL_NIF_INIT(rdp_lvgl_nif, nif_funcs, rlvgl_nif_load, NULL, NULL,
    rlvgl_nif_unload);
