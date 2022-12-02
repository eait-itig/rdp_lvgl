static ERL_NIF_TERM
rlvgl_bar_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_bar_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_bar_set_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int value;
	char atom[32];
	int anim;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &value)) {
		rv = enif_make_badarg2(env, "value", argv[1]);
		goto out;
	}
	if (!enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "anim", argv[2]);
		goto out;
	}
	if (strcmp(atom, "on") == 0) {
		anim = LV_ANIM_ON;
	} else if (strcmp(atom, "off") == 0) {
		anim = LV_ANIM_OFF;
	} else {
		rv = enif_make_badarg2(env, "anim", argv[2]);
		goto out;
	}

	if (obj->lvko_class != &lv_bar_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_bar_set_value,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, value,
	    ARG_UINT32, anim,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_bar_set_start_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int value;
	char atom[32];
	int anim;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &value)) {
		rv = enif_make_badarg2(env, "value", argv[1]);
		goto out;
	}
	if (!enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "anim", argv[2]);
		goto out;
	}
	if (strcmp(atom, "on") == 0) {
		anim = LV_ANIM_ON;
	} else if (strcmp(atom, "off") == 0) {
		anim = LV_ANIM_OFF;
	} else {
		rv = enif_make_badarg2(env, "anim", argv[2]);
		goto out;
	}

	if (obj->lvko_class != &lv_bar_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_bar_set_start_value,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, value,
	    ARG_UINT32, anim,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_bar_set_range3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int min;
	int max;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &min)) {
		rv = enif_make_badarg2(env, "min", argv[1]);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &max)) {
		rv = enif_make_badarg2(env, "max", argv[2]);
		goto out;
	}

	if (obj->lvko_class != &lv_bar_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_bar_set_range,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, min,
	    ARG_UINT32, max,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_bar_set_mode2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int mode;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], bar_mode, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_bar_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_bar_set_mode,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, mode,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_bar_get_value1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_bar_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_bar_get_value,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_btn_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_btn_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_checkbox_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_checkbox_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_checkbox_set_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_checkbox_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_checkbox_set_text,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, text.data, text.size,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_checkbox_get_text1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_checkbox_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_checkbox_get_text,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_textarea_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_text,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, text.data, text.size,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_get_text1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_textarea_get_text,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_placeholder_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_placeholder_text,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, text.data, text.size,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_text_selection2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	uint state;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		state = 1;
	} else if (strcmp(atom, "false") == 0) {
		state = 0;
	} else {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_text_selection,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, state,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_password_mode2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	uint state;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		state = 1;
	} else if (strcmp(atom, "false") == 0) {
		state = 0;
	} else {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_password_mode,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, state,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_one_line2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	uint state;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		state = 1;
	} else if (strcmp(atom, "false") == 0) {
		state = 0;
	} else {
		rv = enif_make_badarg2(env, "state", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_one_line,
	    ARG_OBJPTR, obj,
	    ARG_UINT8, state,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_textarea_set_accepted_chars2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkbuf *buf;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = unpack_buf_hdl(env, argv[1], &nls, &buf);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_textarea_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_accepted_chars,
	    ARG_OBJPTR, obj,
	    ARG_BUFPTR, buf,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_img_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_img_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_img_set_offset2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int tuplen;
	const ERL_NIF_TERM *tup;
	lv_point_t pt;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_tuple(env, argv[1], &tuplen, &tup)) {
		rv = enif_make_badarg2(env, "pt", argv[1]);
		goto out;
	}
	if (tuplen != 2) {
		rv = enif_make_badarg2(env, "pt", argv[1]);
		goto out;
	}
	if ((rc = parse_coord(env, tup[0], &pt.x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, tup[1], &pt.y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_img_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_offset,
	    ARG_OBJPTR, obj,
	    ARG_POINT, &pt,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_label_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_label_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_label_set_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_label_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_label_set_text,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, text.data, text.size,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_btnmatrix_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_btnmatrix_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_dropdown_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_set_options2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary opts;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &opts)) {
		rv = enif_make_badarg2(env, "opts", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_dropdown_set_options,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, opts.data, opts.size,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_add_option3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;
	int index;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &index)) {
		rv = enif_make_badarg2(env, "index", argv[2]);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_dropdown_add_option,
	    ARG_OBJPTR, obj,
	    ARG_INLINE_BUF, text.data, text.size,
	    ARG_UINT32, index,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_get_selected1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_dropdown_get_selected,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_set_selected2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int index;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_dropdown_set_selected,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, index,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_clear_options1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_dropdown_clear_options,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_dropdown_get_selected_str1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (obj->lvko_class != &lv_dropdown_class) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_dropdown_get_selected_str,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_imgbtn_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_imgbtn_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_led_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_led_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_list_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_list_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_menu_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_menu_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_roller_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_roller_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_slider_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_slider_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_switch_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_switch_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_table_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_table_create,
	    ARG_OBJPTR, parent,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_spinner_create3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;
	uint time;
	uint arclen;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &time)) {
		rv = enif_make_badarg2(env, "time", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &arclen)) {
		rv = enif_make_badarg2(env, "arclen", argv[2]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_spinner_create,
	    ARG_OBJPTR, parent,
	    ARG_UINT32, time,
	    ARG_UINT32, arclen,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_center1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_center,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_add_flag2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int flags;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], obj_flags, true, &flags))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_flag,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, flags,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_clear_flag2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int flags;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], obj_flags, true, &flags))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_clear_flag,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, flags,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_add_state2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int states;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], obj_state_specs, true, &states))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_state,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, states,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_clear_state2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int states;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], obj_state_specs, true, &states))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_clear_state,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, states,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_get_state1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd->ncd_enum = obj_state_specs;
	ncd->ncd_multi = true;

	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_obj_get_state,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_add_style3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkstyle *style;
	int sel;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_sty_hdl(env, argv[1], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], style_selector_specs, true, &sel))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_add_style,
	    ARG_OBJPTR, obj,
	    ARG_STYPTR, style,
	    ARG_UINT32, sel,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_align3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int align;
	int tuplen;
	const ERL_NIF_TERM *tup;
	lv_point_t offset;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], align_specs, false, &align))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_tuple(env, argv[2], &tuplen, &tup)) {
		rv = enif_make_badarg2(env, "offset", argv[2]);
		goto out;
	}
	if (tuplen != 2) {
		rv = enif_make_badarg2(env, "offset", argv[2]);
		goto out;
	}
	if ((rc = parse_coord(env, tup[0], &offset.x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, tup[1], &offset.y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_align,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, align,
	    ARG_UINT32, offset.x,
	    ARG_UINT32, offset.y,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_align2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int align;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], align_specs, false, &align))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_set_align,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, align,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_align_to4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *tobj;
	int align;
	int tuplen;
	const ERL_NIF_TERM *tup;
	lv_point_t offset;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &tobj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], align_specs, false, &align))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_tuple(env, argv[3], &tuplen, &tup)) {
		rv = enif_make_badarg2(env, "offset", argv[3]);
		goto out;
	}
	if (tuplen != 2) {
		rv = enif_make_badarg2(env, "offset", argv[3]);
		goto out;
	}
	if ((rc = parse_coord(env, tup[0], &offset.x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, tup[1], &offset.y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_align_to,
	    ARG_OBJPTR, obj,
	    ARG_OBJPTR, tobj,
	    ARG_UINT32, align,
	    ARG_UINT32, offset.x,
	    ARG_UINT32, offset.y,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_get_pos1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_POINT, lv_obj_get_pos,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_get_size1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_POINT, lv_obj_get_size,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_obj_set_size2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int tuplen;
	const ERL_NIF_TERM *tup;
	lv_point_t size;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_tuple(env, argv[1], &tuplen, &tup)) {
		rv = enif_make_badarg2(env, "size", argv[1]);
		goto out;
	}
	if (tuplen != 2) {
		rv = enif_make_badarg2(env, "size", argv[1]);
		goto out;
	}
	if ((rc = parse_coord(env, tup[0], &size.x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, tup[1], &size.y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_set_size,
	    ARG_OBJPTR, obj,
	    ARG_UINT32, size.x,
	    ARG_UINT32, size.y,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_group_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_GRPPTR, lv_group_create,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_group_add_obj2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;
	struct lvkobj *obj;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_grp_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_add_obj,
	    ARG_GRPPTR, group,
	    ARG_OBJPTR, obj,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_style_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_STYPTR, lv_style_alloc,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_style_set_flex_align4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkstyle *style;
	int main;
	int cross;
	int tracks;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_sty_hdl(env, argv[0], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], flex_align, false, &main))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], flex_align, false, &cross))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[3], flex_align, false, &tracks))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_style_set_flex_align,
	    ARG_STYPTR, style,
	    ARG_UINT32, main,
	    ARG_UINT32, cross,
	    ARG_UINT32, tracks,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_style_set_flex_flow2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkstyle *style;
	int flow;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_sty_hdl(env, argv[0], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], flex_flow, false, &flow))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_style_set_flex_flow,
	    ARG_STYPTR, style,
	    ARG_UINT32, flow,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_disp_set_bg_color2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	lv_color_t color;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_color(env, argv[1], &color)) {
		rv = enif_make_badarg2(env, "color", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_disp_set_bg_color,
	    ARG_PTR, inst->lvki_disp,
	    ARG_COLOR, &color,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_disp_get_layer_sys1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_OBJPTR, lv_disp_get_layer_sys,
	    ARG_PTR, inst->lvki_disp,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_set_kbd_group2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	struct lvkgroup *group;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_grp_hdl(env, argv[1], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_indev_set_group,
	    ARG_PTR, inst->lvki_kbd,
	    ARG_GRPPTR, group,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_scr_load2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	struct lvkobj *screen;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &screen, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_disp_scr_load,
	    ARG_PTR, inst->lvki_disp,
	    ARG_OBJPTR, screen,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_scr_load_anim6(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	struct lvkobj *screen;
	int anim;
	uint time;
	uint delay;
	char atom[32];
	uint autodel;

	bzero(&nls, sizeof (nls));

	if (argc != 6)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &screen, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], scr_load_anims, false, &anim))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[3], &time)) {
		rv = enif_make_badarg2(env, "time", argv[3]);
		goto out;
	}
	if (!enif_get_uint(env, argv[4], &delay)) {
		rv = enif_make_badarg2(env, "delay", argv[4]);
		goto out;
	}
	if (!enif_get_atom(env, argv[5], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "autodel", argv[5]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		autodel = 1;
	} else if (strcmp(atom, "false") == 0) {
		autodel = 0;
	} else {
		rv = enif_make_badarg2(env, "autodel", argv[5]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
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

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

static ERL_NIF_TERM
rlvgl_set_mouse_cursor2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	struct lvkobj *cursor;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &cursor, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_indev_set_cursor,
	    ARG_PTR, inst->lvki_mouse,
	    ARG_OBJPTR, cursor,
	    ARG_NONE);

	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	ncd = NULL;
	rv = enif_make_tuple2(env,
	    enif_make_atom(env, "async"),
	    msgref);

out:
	leave_nif(&nls);
	free_ncd(ncd);
	return (rv);
}

#define AUTOGEN_NIFS \
{ "bar_create",				1, rlvgl_bar_create1 }, \
{ "bar_set_value",			3, rlvgl_bar_set_value3 }, \
{ "bar_set_start_value",		3, rlvgl_bar_set_start_value3 }, \
{ "bar_set_range",			3, rlvgl_bar_set_range3 }, \
{ "bar_set_mode",			2, rlvgl_bar_set_mode2 }, \
{ "bar_get_value",			1, rlvgl_bar_get_value1 }, \
{ "btn_create",				1, rlvgl_btn_create1 }, \
{ "checkbox_create",			1, rlvgl_checkbox_create1 }, \
{ "checkbox_set_text",			2, rlvgl_checkbox_set_text2 }, \
{ "checkbox_get_text",			1, rlvgl_checkbox_get_text1 }, \
{ "textarea_create",			1, rlvgl_textarea_create1 }, \
{ "textarea_set_text",			2, rlvgl_textarea_set_text2 }, \
{ "textarea_get_text",			1, rlvgl_textarea_get_text1 }, \
{ "textarea_set_placeholder_text",	2, rlvgl_textarea_set_placeholder_text2 }, \
{ "textarea_set_text_selection",	2, rlvgl_textarea_set_text_selection2 }, \
{ "textarea_set_password_mode",		2, rlvgl_textarea_set_password_mode2 }, \
{ "textarea_set_one_line",		2, rlvgl_textarea_set_one_line2 }, \
{ "textarea_set_accepted_chars",	2, rlvgl_textarea_set_accepted_chars2 }, \
{ "img_create",				1, rlvgl_img_create1 }, \
{ "img_set_offset",			2, rlvgl_img_set_offset2 }, \
{ "label_create",			1, rlvgl_label_create1 }, \
{ "label_set_text",			2, rlvgl_label_set_text2 }, \
{ "btnmatrix_create",			1, rlvgl_btnmatrix_create1 }, \
{ "dropdown_create",			1, rlvgl_dropdown_create1 }, \
{ "dropdown_set_options",		2, rlvgl_dropdown_set_options2 }, \
{ "dropdown_add_option",		3, rlvgl_dropdown_add_option3 }, \
{ "dropdown_get_selected",		1, rlvgl_dropdown_get_selected1 }, \
{ "dropdown_set_selected",		2, rlvgl_dropdown_set_selected2 }, \
{ "dropdown_clear_options",		1, rlvgl_dropdown_clear_options1 }, \
{ "dropdown_get_selected_str",		1, rlvgl_dropdown_get_selected_str1 }, \
{ "imgbtn_create",			1, rlvgl_imgbtn_create1 }, \
{ "led_create",				1, rlvgl_led_create1 }, \
{ "list_create",			1, rlvgl_list_create1 }, \
{ "menu_create",			1, rlvgl_menu_create1 }, \
{ "roller_create",			1, rlvgl_roller_create1 }, \
{ "slider_create",			1, rlvgl_slider_create1 }, \
{ "switch_create",			1, rlvgl_switch_create1 }, \
{ "table_create",			1, rlvgl_table_create1 }, \
{ "spinner_create",			3, rlvgl_spinner_create3 }, \
{ "obj_center",				1, rlvgl_obj_center1 }, \
{ "obj_add_flag",			2, rlvgl_obj_add_flag2 }, \
{ "obj_clear_flag",			2, rlvgl_obj_clear_flag2 }, \
{ "obj_add_state",			2, rlvgl_obj_add_state2 }, \
{ "obj_clear_state",			2, rlvgl_obj_clear_state2 }, \
{ "obj_get_state",			1, rlvgl_obj_get_state1 }, \
{ "obj_add_style",			3, rlvgl_obj_add_style3 }, \
{ "obj_align",				3, rlvgl_obj_align3 }, \
{ "obj_align",				2, rlvgl_obj_align2 }, \
{ "obj_align_to",			4, rlvgl_obj_align_to4 }, \
{ "obj_get_pos",			1, rlvgl_obj_get_pos1 }, \
{ "obj_get_size",			1, rlvgl_obj_get_size1 }, \
{ "obj_set_size",			2, rlvgl_obj_set_size2 }, \
{ "group_create",			1, rlvgl_group_create1 }, \
{ "group_add_obj",			2, rlvgl_group_add_obj2 }, \
{ "style_create",			1, rlvgl_style_create1 }, \
{ "style_set_flex_align",		4, rlvgl_style_set_flex_align4 }, \
{ "style_set_flex_flow",		2, rlvgl_style_set_flex_flow2 }, \
{ "disp_set_bg_color",			2, rlvgl_disp_set_bg_color2 }, \
{ "disp_get_layer_sys",			1, rlvgl_disp_get_layer_sys1 }, \
{ "set_kbd_group",			2, rlvgl_set_kbd_group2 }, \
{ "scr_load",				2, rlvgl_scr_load2 }, \
{ "scr_load_anim",			6, rlvgl_scr_load_anim6 }, \
{ "set_mouse_cursor",			2, rlvgl_set_mouse_cursor2 }
