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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_bar_create,
	    ARG_PTR_OBJ, parent,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_bar_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_bar_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_bar_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], bar_mode, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_bar_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_bar_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_btn_create,
	    ARG_PTR_OBJ, parent,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_checkbox_create,
	    ARG_PTR_OBJ, parent,
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
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_checkbox_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_checkbox_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_textarea_create,
	    ARG_PTR_OBJ, parent,
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
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = unpack_buf_hdl(env, argv[1], &nls, &buf);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_BUFFER, buf,
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
rlvgl_textarea_set_max_length2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint len;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &len)) {
		rv = enif_make_badarg2(env, "len", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_max_length,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT32, len,
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
rlvgl_textarea_get_label1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_textarea_get_label,
	    ARG_PTR_OBJ, obj,
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
rlvgl_textarea_set_password_show_time2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint time;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &time)) {
		rv = enif_make_badarg2(env, "time", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_textarea_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_textarea_set_password_show_time,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, time,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_img_create,
	    ARG_PTR_OBJ, parent,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
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
	    ARG_PTR_OBJ, obj,
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
rlvgl_img_set_src2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary src_bin;
	enum arg_type src_type;
	void *src;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = parse_img_src(env, argv[1], &src_bin, &src_type,
	    &src);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_src,
	    ARG_PTR_OBJ, obj,
	    src_type, src,
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
rlvgl_img_set_angle2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint angle;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &angle)) {
		rv = enif_make_badarg2(env, "angle", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_angle,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, angle,
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
rlvgl_img_set_pivot2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int tuplen;
	const ERL_NIF_TERM *tup;
	lv_point_t pivot;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_tuple(env, argv[1], &tuplen, &tup)) {
		rv = enif_make_badarg2(env, "pivot", argv[1]);
		goto out;
	}
	if (tuplen != 2) {
		rv = enif_make_badarg2(env, "pivot", argv[1]);
		goto out;
	}
	if ((rc = parse_coord(env, tup[0], &pivot.x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, tup[1], &pivot.y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_pivot,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, pivot.x,
	    ARG_UINT16, pivot.y,
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
rlvgl_img_set_zoom2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint zoom;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &zoom)) {
		rv = enif_make_badarg2(env, "zoom", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_zoom,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, zoom,
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
rlvgl_img_set_antialias2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	uint ena;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "ena", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		ena = 1;
	} else if (strcmp(atom, "false") == 0) {
		ena = 0;
	} else {
		rv = enif_make_badarg2(env, "ena", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_img_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_img_set_antialias,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, ena,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_label_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_label_get_text1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_label_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_label_get_text,
	    ARG_PTR_OBJ, obj,
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
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_label_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
rlvgl_label_set_text_sel_start2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint index;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_label_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_label_set_text_sel_start,
	    ARG_PTR_OBJ, obj,
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
rlvgl_label_set_text_sel_end2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint index;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_label_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_label_set_text_sel_end,
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_btnmatrix_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_btnmatrix_set_map2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary map[16];
	size_t map_n = 0;
	ERL_NIF_TERM map_list, map_hd;
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	map_list = argv[1];
	while (enif_get_list_cell(env, map_list, &map_hd, &map_list)) {
		assert(map_n < 16);
		if (!enif_inspect_iolist_as_binary(env, map_hd, &map[map_n])) {
			rv = enif_make_badarg2(env, "map", argv[1]);
		goto out;
		}
		total_inline += map[map_n].size;
		++map_n;
	}
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_map,
	    ARG_PTR_OBJ, obj,
	    ARG_INL_BUF_ARR, map, map_n,
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
rlvgl_btnmatrix_set_btn_ctrl3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], btnmatrix_ctrls, true, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_btn_ctrl,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
	    ARG_UINT16, ctrl,
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
rlvgl_btnmatrix_clear_btn_ctrl3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], btnmatrix_ctrls, true, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_clear_btn_ctrl,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
	    ARG_UINT16, ctrl,
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
rlvgl_btnmatrix_set_btn_ctrl_all2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], btnmatrix_ctrls, true, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_btn_ctrl_all,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, ctrl,
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
rlvgl_btnmatrix_clear_btn_ctrl_all2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], btnmatrix_ctrls, true, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_clear_btn_ctrl_all,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, ctrl,
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
rlvgl_btnmatrix_set_btn_width3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;
	uint width;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &width)) {
		rv = enif_make_badarg2(env, "width", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_btn_width,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
	    ARG_UINT8, width,
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
rlvgl_btnmatrix_set_one_checked2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	uint checked;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "checked", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		checked = 1;
	} else if (strcmp(atom, "false") == 0) {
		checked = 0;
	} else {
		rv = enif_make_badarg2(env, "checked", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_one_checked,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, checked,
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
rlvgl_btnmatrix_get_selected_btn1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT16, lv_btnmatrix_get_selected_btn,
	    ARG_PTR_OBJ, obj,
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
rlvgl_btnmatrix_set_selected_btn2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_btnmatrix_set_selected_btn,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
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
rlvgl_btnmatrix_get_btn_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_btnmatrix_get_btn_text,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
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
rlvgl_btnmatrix_has_btn_ctrl3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint idx;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &idx)) {
		rv = enif_make_badarg2(env, "idx", argv[1]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], btnmatrix_ctrls, true, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_btnmatrix_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT8, lv_btnmatrix_has_btn_ctrl,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, idx,
	    ARG_UINT16, ctrl,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_dropdown_create,
	    ARG_PTR_OBJ, parent,
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
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &opts)) {
		rv = enif_make_badarg2(env, "opts", argv[1]);
		goto out;
	}
	if (opts.size == 0) {
		opts.data = (unsigned char *)"\0";
		opts.size = 1;
	}
	total_inline += opts.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &opts,
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
	size_t total_inline = 0;
	int index;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (!enif_get_int(env, argv[2], &index)) {
		rv = enif_make_badarg2(env, "index", argv[2]);
		goto out;
	}
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_dropdown_class)) {
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
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_imgbtn_create,
	    ARG_PTR_OBJ, parent,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_led_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_led_set_color2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	lv_color_t color;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_color(env, argv[1], &color)) {
		rv = enif_make_badarg2(env, "color", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_led_set_color,
	    ARG_PTR_OBJ, obj,
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
rlvgl_led_set_brightness2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint bright;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &bright)) {
		rv = enif_make_badarg2(env, "bright", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_led_set_brightness,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, bright,
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
rlvgl_led_on1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_led_on,
	    ARG_PTR_OBJ, obj,
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
rlvgl_led_off1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_led_off,
	    ARG_PTR_OBJ, obj,
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
rlvgl_led_toggle1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_led_toggle,
	    ARG_PTR_OBJ, obj,
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
rlvgl_led_get_brightness1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_led_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT8, lv_led_get_brightness,
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_list_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_list_add_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary text;
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_list_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_list_add_text,
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &text,
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
rlvgl_list_add_btn3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary icon_bin;
	enum arg_type icon_type;
	void *icon;
	ErlNifBinary text_buf;
	enum arg_type text_type;
	void *text;
	char atom[32];
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = parse_img_src(env, argv[1], &icon_bin, &icon_type,
	    &icon);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		if (strcmp(atom, "none") == 0) {
			text_type = ARG_PTR;
			text = NULL;
		} else {
			rv = enif_make_badarg2(env, "text", argv[2]);
		goto out;
		}
	} else if (enif_inspect_iolist_as_binary(env, argv[2], &text_buf)) {
		text_type = ARG_INLINE_BUF;
		text = &text_buf;
		total_inline += text_buf.size;
	} else {
		rv = enif_make_badarg2(env, "text", argv[2]);
		goto out;
	}
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_list_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_list_add_btn,
	    ARG_PTR_OBJ, obj,
	    icon_type, icon,
	    text_type, text,
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
rlvgl_list_get_btn_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *btn;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &btn, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (btn == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (btn->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_list_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_list_get_btn_text,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, btn,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_menu_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_menu_page_create2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	ErlNifBinary title;
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &title)) {
		rv = enif_make_badarg2(env, "title", argv[1]);
		goto out;
	}
	if (title.size == 0) {
		title.data = (unsigned char *)"\0";
		title.size = 1;
	}
	total_inline += title.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_menu_page_create,
	    ARG_PTR_OBJ, obj,
	    ARG_INLINE_BUF, &title,
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
rlvgl_menu_cont_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_menu_cont_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_menu_section_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_menu_section_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_menu_separator_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_menu_separator_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_menu_set_page2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *page;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &page, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (page == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (page->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_menu_set_page,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, page,
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
rlvgl_menu_set_sidebar_page2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *page;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &page, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (page == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (page->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_menu_set_sidebar_page,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, page,
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
rlvgl_menu_set_mode_root_back_btn2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], menu_mode_root_back_btn, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_menu_set_mode_root_back_btn,
	    ARG_PTR_OBJ, obj,
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
rlvgl_menu_set_mode_header2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], menu_mode_header, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_menu_set_mode_header,
	    ARG_PTR_OBJ, obj,
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
rlvgl_menu_set_load_page_event3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *btn;
	struct lvkobj *page;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &btn, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (btn == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (btn->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[2], &nls, &page, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (page == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (page->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_menu_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_menu_set_load_page_event,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, btn,
	    ARG_PTR_OBJ, page,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_roller_create,
	    ARG_PTR_OBJ, parent,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_slider_create,
	    ARG_PTR_OBJ, parent,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_switch_create,
	    ARG_PTR_OBJ, parent,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_table_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_table_set_row_cnt2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint rows;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &rows)) {
		rv = enif_make_badarg2(env, "rows", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_set_row_cnt,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, rows,
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
rlvgl_table_set_col_cnt2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint cols;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &cols)) {
		rv = enif_make_badarg2(env, "cols", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_set_col_cnt,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, cols,
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
rlvgl_table_set_col_width3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint col_idx;
	lv_coord_t width;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &col_idx)) {
		rv = enif_make_badarg2(env, "col_idx", argv[1]);
		goto out;
	}
	if ((rc = parse_coord(env, argv[2], &width))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_set_col_width,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, col_idx,
	    ARG_UINT16, width,
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
rlvgl_table_set_cell_value4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint row;
	uint col;
	ErlNifBinary text;
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &row)) {
		rv = enif_make_badarg2(env, "row", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &col)) {
		rv = enif_make_badarg2(env, "col", argv[2]);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[3], &text)) {
		rv = enif_make_badarg2(env, "text", argv[3]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_set_cell_value,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, row,
	    ARG_UINT16, col,
	    ARG_INLINE_BUF, &text,
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
rlvgl_table_add_cell_ctrl4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint row;
	uint col;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &row)) {
		rv = enif_make_badarg2(env, "row", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &col)) {
		rv = enif_make_badarg2(env, "col", argv[2]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[3], table_cell_ctrls, false, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_add_cell_ctrl,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, row,
	    ARG_UINT16, col,
	    ARG_UINT8, ctrl,
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
rlvgl_table_clear_cell_ctrl4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint row;
	uint col;
	int ctrl;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &row)) {
		rv = enif_make_badarg2(env, "row", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &col)) {
		rv = enif_make_badarg2(env, "col", argv[2]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[3], table_cell_ctrls, false, &ctrl))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_table_clear_cell_ctrl,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, row,
	    ARG_UINT16, col,
	    ARG_UINT8, ctrl,
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
rlvgl_table_get_selected_cell_pt1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_table_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_POINT, lv_table_get_selected_cell_pt,
	    ARG_PTR_OBJ, obj,
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
rlvgl_msgbox_create5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *parent;
	ErlNifBinary title;
	size_t total_inline = 0;
	ErlNifBinary text;
	ErlNifBinary btns[16];
	size_t btns_n = 0;
	ERL_NIF_TERM btns_list, btns_hd;
	char atom[32];
	uint add_close;

	bzero(&nls, sizeof (nls));

	if (argc != 5)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &parent, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &title)) {
		rv = enif_make_badarg2(env, "title", argv[1]);
		goto out;
	}
	if (title.size == 0) {
		title.data = (unsigned char *)"\0";
		title.size = 1;
	}
	total_inline += title.size;
	if (!enif_inspect_iolist_as_binary(env, argv[2], &text)) {
		rv = enif_make_badarg2(env, "text", argv[2]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	btns_list = argv[3];
	while (enif_get_list_cell(env, btns_list, &btns_hd, &btns_list)) {
		assert(btns_n < 16);
		if (!enif_inspect_iolist_as_binary(env, btns_hd, &btns[btns_n])) {
			rv = enif_make_badarg2(env, "btns", argv[3]);
		goto out;
		}
		total_inline += btns[btns_n].size;
		++btns_n;
	}
	if (!enif_get_atom(env, argv[4], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "add_close", argv[4]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		add_close = 1;
	} else if (strcmp(atom, "false") == 0) {
		add_close = 0;
	} else {
		rv = enif_make_badarg2(env, "add_close", argv[4]);
		goto out;
	}
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_msgbox_create,
	    ARG_PTR_OBJ, parent,
	    ARG_INLINE_BUF, &title,
	    ARG_INLINE_BUF, &text,
	    ARG_INL_BUF_ARR, btns, btns_n,
	    ARG_UINT8, add_close,
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
rlvgl_msgbox_get_active_btn1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_msgbox_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT16, lv_msgbox_get_active_btn,
	    ARG_PTR_OBJ, obj,
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
rlvgl_msgbox_get_active_btn_text1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_msgbox_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_INLINE_STR, lv_msgbox_get_active_btn_text,
	    ARG_PTR_OBJ, obj,
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
rlvgl_msgbox_close1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_msgbox_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_msgbox_close,
	    ARG_PTR_OBJ, obj,
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
rlvgl_msgbox_get_btns1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_msgbox_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_msgbox_get_btns,
	    ARG_PTR_OBJ, obj,
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, lv_spinner_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_meter_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_meter_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_meter_add_scale1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_METER_SCL, lv_meter_add_scale,
	    ARG_PTR_OBJ, obj,
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
rlvgl_meter_set_scale_ticks6(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterscl *scale;
	uint cnt;
	uint width;
	uint len;
	lv_color_t color;

	bzero(&nls, sizeof (nls));

	if (argc != 6)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterscl_hdl(env, argv[1], &nls, &scale, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (scale->lvkms_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &cnt)) {
		rv = enif_make_badarg2(env, "cnt", argv[2]);
		goto out;
	}
	if (!enif_get_uint(env, argv[3], &width)) {
		rv = enif_make_badarg2(env, "width", argv[3]);
		goto out;
	}
	if (!enif_get_uint(env, argv[4], &len)) {
		rv = enif_make_badarg2(env, "len", argv[4]);
		goto out;
	}
	if (!enif_get_color(env, argv[5], &color)) {
		rv = enif_make_badarg2(env, "color", argv[5]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_scale_ticks,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_SCL, scale,
	    ARG_UINT16, cnt,
	    ARG_UINT16, width,
	    ARG_UINT16, len,
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
rlvgl_meter_set_scale_major_ticks7(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterscl *scale;
	uint nth;
	uint width;
	uint len;
	lv_color_t color;
	int label_gap;

	bzero(&nls, sizeof (nls));

	if (argc != 7)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterscl_hdl(env, argv[1], &nls, &scale, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (scale->lvkms_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &nth)) {
		rv = enif_make_badarg2(env, "nth", argv[2]);
		goto out;
	}
	if (!enif_get_uint(env, argv[3], &width)) {
		rv = enif_make_badarg2(env, "width", argv[3]);
		goto out;
	}
	if (!enif_get_uint(env, argv[4], &len)) {
		rv = enif_make_badarg2(env, "len", argv[4]);
		goto out;
	}
	if (!enif_get_color(env, argv[5], &color)) {
		rv = enif_make_badarg2(env, "color", argv[5]);
		goto out;
	}
	if (!enif_get_int(env, argv[6], &label_gap)) {
		rv = enif_make_badarg2(env, "label_gap", argv[6]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_scale_major_ticks,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_SCL, scale,
	    ARG_UINT16, nth,
	    ARG_UINT16, width,
	    ARG_UINT16, len,
	    ARG_COLOR, &color,
	    ARG_UINT16, label_gap,
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
rlvgl_meter_set_scale_range6(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterscl *scale;
	int min;
	int max;
	uint angle_range;
	uint rotation;

	bzero(&nls, sizeof (nls));

	if (argc != 6)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterscl_hdl(env, argv[1], &nls, &scale, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (scale->lvkms_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &min)) {
		rv = enif_make_badarg2(env, "min", argv[2]);
		goto out;
	}
	if (!enif_get_int(env, argv[3], &max)) {
		rv = enif_make_badarg2(env, "max", argv[3]);
		goto out;
	}
	if (!enif_get_uint(env, argv[4], &angle_range)) {
		rv = enif_make_badarg2(env, "angle_range", argv[4]);
		goto out;
	}
	if (!enif_get_uint(env, argv[5], &rotation)) {
		rv = enif_make_badarg2(env, "rotation", argv[5]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_scale_range,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_SCL, scale,
	    ARG_UINT32, min,
	    ARG_UINT32, max,
	    ARG_UINT32, angle_range,
	    ARG_UINT32, rotation,
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
rlvgl_meter_add_needle_line5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterscl *scale;
	uint width;
	lv_color_t color;
	int r_mod;

	bzero(&nls, sizeof (nls));

	if (argc != 5)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterscl_hdl(env, argv[1], &nls, &scale, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (scale->lvkms_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &width)) {
		rv = enif_make_badarg2(env, "width", argv[2]);
		goto out;
	}
	if (!enif_get_color(env, argv[3], &color)) {
		rv = enif_make_badarg2(env, "color", argv[3]);
		goto out;
	}
	if (!enif_get_int(env, argv[4], &r_mod)) {
		rv = enif_make_badarg2(env, "r_mod", argv[4]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_METER_IND, lv_meter_add_needle_line,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_SCL, scale,
	    ARG_UINT16, width,
	    ARG_COLOR, &color,
	    ARG_UINT16, r_mod,
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
rlvgl_meter_add_arc5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterscl *scale;
	uint width;
	lv_color_t color;
	int r_mod;

	bzero(&nls, sizeof (nls));

	if (argc != 5)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterscl_hdl(env, argv[1], &nls, &scale, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (scale->lvkms_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &width)) {
		rv = enif_make_badarg2(env, "width", argv[2]);
		goto out;
	}
	if (!enif_get_color(env, argv[3], &color)) {
		rv = enif_make_badarg2(env, "color", argv[3]);
		goto out;
	}
	if (!enif_get_int(env, argv[4], &r_mod)) {
		rv = enif_make_badarg2(env, "r_mod", argv[4]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_METER_IND, lv_meter_add_arc,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_SCL, scale,
	    ARG_UINT16, width,
	    ARG_COLOR, &color,
	    ARG_UINT16, r_mod,
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
rlvgl_meter_set_indicator_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterind *ind;
	int value;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterind_hdl(env, argv[1], &nls, &ind, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (ind->lvkmi_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &value)) {
		rv = enif_make_badarg2(env, "value", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_indicator_value,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_IND, ind,
	    ARG_UINT32, value,
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
rlvgl_meter_set_indicator_start_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterind *ind;
	int value;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterind_hdl(env, argv[1], &nls, &ind, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (ind->lvkmi_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &value)) {
		rv = enif_make_badarg2(env, "value", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_indicator_start_value,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_IND, ind,
	    ARG_UINT32, value,
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
rlvgl_meter_set_indicator_end_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkmeterind *ind;
	int value;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_meterind_hdl(env, argv[1], &nls, &ind, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (ind->lvkmi_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[2], &value)) {
		rv = enif_make_badarg2(env, "value", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_meter_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_meter_set_indicator_end_value,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_METER_IND, ind,
	    ARG_UINT32, value,
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
rlvgl_spangroup_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_spangroup_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_spangroup_set_align2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], text_align_specs, false, &align))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_spangroup_set_align,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, align,
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
rlvgl_spangroup_new_span1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_SPAN, lv_spangroup_new_span,
	    ARG_PTR_OBJ, obj,
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
rlvgl_spangroup_get_child_cnt1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_spangroup_get_child_cnt,
	    ARG_PTR_OBJ, obj,
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
rlvgl_spangroup_get_child2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int id;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &id)) {
		rv = enif_make_badarg2(env, "id", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_SPAN, lv_spangroup_get_child,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT32, id,
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
rlvgl_spangroup_del_span2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkspan *span;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_span_hdl(env, argv[1], &nls, &span, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (span->lvksp_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_spangroup_del_span,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_SPAN, span,
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
rlvgl_spangroup_set_mode2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], span_modes, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_spangroup_set_mode,
	    ARG_PTR_OBJ, obj,
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
rlvgl_spangroup_set_overflow2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], span_overflow_modes, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_spangroup_set_overflow,
	    ARG_PTR_OBJ, obj,
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
rlvgl_spangroup_refr_mode1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_spangroup_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_spangroup_refr_mode,
	    ARG_PTR_OBJ, obj,
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
rlvgl_span_set_text2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkspan *span;
	ErlNifBinary text;
	size_t total_inline = 0;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_span_hdl(env, argv[0], &nls, &span, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (span->lvksp_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_inspect_iolist_as_binary(env, argv[1], &text)) {
		rv = enif_make_badarg2(env, "text", argv[1]);
		goto out;
	}
	if (text.size == 0) {
		text.data = (unsigned char *)"\0";
		text.size = 1;
	}
	total_inline += text.size;
	if (total_inline > CDESC_MAX_INLINE) {
		rv = make_errno(env, ENOSPC);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_span_set_text,
	    ARG_PTR_SPAN, span,
	    ARG_INLINE_BUF, &text,
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
rlvgl_span_set_style2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkspan *span;
	struct lvkstyle *sty;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_span_hdl(env, argv[0], &nls, &span, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (span->lvksp_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_style_hdl(env, argv[1], &nls, &sty, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (sty->lvks_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_span_set_style,
	    ARG_PTR_SPAN, span,
	    ARG_PTR_STYLE, sty,
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
rlvgl_chart_create1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (parent->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_chart_create,
	    ARG_PTR_OBJ, parent,
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
rlvgl_chart_set_type2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int type;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], chart_types, false, &type))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_type,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, type,
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
rlvgl_chart_set_point_count2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint count;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &count)) {
		rv = enif_make_badarg2(env, "count", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_point_count,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, count,
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
rlvgl_chart_set_range4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int axis;
	char atom[32];
	lv_coord_t min;
	lv_coord_t max;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], chart_axes, false, &axis))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		min = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[2], &min))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[3], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		max = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[3], &max))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_range,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, axis,
	    ARG_UINT16, min,
	    ARG_UINT16, max,
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
rlvgl_chart_set_update_mode2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], chart_update_modes, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_update_mode,
	    ARG_PTR_OBJ, obj,
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
rlvgl_chart_set_div_line_count3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint hdiv;
	uint vdiv;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &hdiv)) {
		rv = enif_make_badarg2(env, "hdiv", argv[1]);
		goto out;
	}
	if (!enif_get_uint(env, argv[2], &vdiv)) {
		rv = enif_make_badarg2(env, "vdiv", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_div_line_count,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, hdiv,
	    ARG_UINT8, vdiv,
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
rlvgl_chart_set_zoom_x2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint zoom;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &zoom)) {
		rv = enif_make_badarg2(env, "zoom", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_zoom_x,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, zoom,
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
rlvgl_chart_set_zoom_y2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	uint zoom;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &zoom)) {
		rv = enif_make_badarg2(env, "zoom", argv[1]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_zoom_y,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, zoom,
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
rlvgl_chart_set_axis_tick8(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int axis;
	lv_coord_t major_len;
	lv_coord_t minor_len;
	lv_coord_t major_cnt;
	lv_coord_t minor_cnt;
	char atom[32];
	uint label_en;
	lv_coord_t draw_size;

	bzero(&nls, sizeof (nls));

	if (argc != 8)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], chart_axes, false, &axis))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, argv[2], &major_len))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, argv[3], &minor_len))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, argv[4], &major_cnt))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_coord(env, argv[5], &minor_cnt))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[6], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "label_en", argv[6]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		label_en = 1;
	} else if (strcmp(atom, "false") == 0) {
		label_en = 0;
	} else {
		rv = enif_make_badarg2(env, "label_en", argv[6]);
		goto out;
	}
	if ((rc = parse_coord(env, argv[7], &draw_size))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_axis_tick,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, axis,
	    ARG_UINT16, major_len,
	    ARG_UINT16, minor_len,
	    ARG_UINT16, major_cnt,
	    ARG_UINT16, minor_cnt,
	    ARG_UINT8, label_en,
	    ARG_UINT16, draw_size,
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
rlvgl_chart_get_point_count1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT16, lv_chart_get_point_count,
	    ARG_PTR_OBJ, obj,
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
rlvgl_chart_add_series3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	lv_color_t color;
	int axis;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_color(env, argv[1], &color)) {
		rv = enif_make_badarg2(env, "color", argv[1]);
		goto out;
	}
	if ((rc = parse_enum(env, argv[2], chart_axes, false, &axis))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_CHART_SER, lv_chart_add_series,
	    ARG_PTR_OBJ, obj,
	    ARG_COLOR, &color,
	    ARG_UINT8, axis,
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
rlvgl_chart_remove_series2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_remove_series,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
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
rlvgl_chart_hide_series3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;
	char atom[32];
	uint hide;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "hide", argv[2]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		hide = 1;
	} else if (strcmp(atom, "false") == 0) {
		hide = 0;
	} else {
		rv = enif_make_badarg2(env, "hide", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_hide_series,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
	    ARG_UINT8, hide,
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
rlvgl_chart_set_series_color3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;
	lv_color_t color;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_color(env, argv[2], &color)) {
		rv = enif_make_badarg2(env, "color", argv[2]);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_series_color,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
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
rlvgl_chart_get_series_next2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_CHART_SER, lv_chart_get_series_next,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
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
rlvgl_chart_set_all_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;
	char atom[32];
	lv_coord_t y;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		y = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[2], &y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_all_value,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
	    ARG_UINT16, y,
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
rlvgl_chart_set_next_value3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;
	char atom[32];
	lv_coord_t y;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		y = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[2], &y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_next_value,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
	    ARG_UINT16, y,
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
rlvgl_chart_set_next_value24(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkchartser *series;
	char atom[32];
	lv_coord_t x;
	lv_coord_t y;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_chartser_hdl(env, argv[1], &nls, &series, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (series->lvkcs_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[2], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		x = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[2], &x))) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (enif_get_atom(env, argv[3], atom, sizeof (atom), ERL_NIF_LATIN1) &&
	    strcmp(atom, "none") == 0) {
		y = LV_CHART_POINT_NONE;
	} else if ((rc = parse_coord(env, argv[3], &y))) {
		rv = make_errno(env, rc);
		goto out;
	}

	if (!lv_obj_class_has_base(obj->lvko_class, &lv_chart_class)) {
		rv = make_errno(env, EINVAL);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_chart_set_next_value2,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_CHART_SER, series,
	    ARG_UINT16, x,
	    ARG_UINT16, y,
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
rlvgl_obj_create2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &parent, 0);
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
	    ARG_PTR_OBJ, lv_disp_obj_create,
	    ARG_PTR, inst->lvki_disp,
	    ARG_PTR_OBJ, parent,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_has_flag2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_UINT8, lv_obj_has_flag,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_has_flag_any2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_UINT8, lv_obj_has_flag_any,
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_style_hdl(env, argv[1], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (style->lvks_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_STYLE, style,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
	    ARG_UINT32, align,
	    ARG_UINT16, offset.x,
	    ARG_UINT16, offset.y,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &tobj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (tobj == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (tobj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, tobj,
	    ARG_UINT32, align,
	    ARG_UINT16, offset.x,
	    ARG_UINT16, offset.y,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, obj,
	    ARG_UINT16, size.x,
	    ARG_UINT16, size.y,
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
rlvgl_obj_set_local_style_prop4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	lv_style_value_t sty_val;
	lv_style_prop_t sty_prop;
	int sel;

	bzero(&nls, sizeof (nls));

	if (argc != 4)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = parse_style_prop_val(env, argv[1], argv[2],
	    &sty_prop, &sty_val);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[3], style_selector_specs, true, &sel))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_set_local_style_prop,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT32, sty_prop,
	    ARG_STYLEVAL, &sty_val,
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
rlvgl_obj_move_foreground1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_move_foreground,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_move_background1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_move_background,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_swap2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *other;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &other, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (other == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (other->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_swap,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, other,
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
rlvgl_obj_set_parent2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	struct lvkobj *parent;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &parent, 0);
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
	    ARG_NONE, lv_obj_set_parent,
	    ARG_PTR_OBJ, obj,
	    ARG_PTR_OBJ, parent,
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
rlvgl_obj_move_to_index2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_move_to_index,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_get_index1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_obj_get_index,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_get_child_cnt1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_UINT32, lv_obj_get_child_cnt,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_get_child2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_int(env, argv[1], &index)) {
		rv = enif_make_badarg2(env, "index", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_obj_get_child,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_get_parent1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_obj_get_parent,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_get_screen1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_obj_get_screen,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_clean1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_clean,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_del1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_del,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_refresh_ext_draw_size1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_refresh_ext_draw_size,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_set_scrollbar_mode2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], scrollbar_modes, false, &mode))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_set_scrollbar_mode,
	    ARG_PTR_OBJ, obj,
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
rlvgl_obj_set_scroll_dir2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	int dir;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if ((rc = parse_enum(env, argv[1], dir_specs, true, &dir))) {
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_set_scroll_dir,
	    ARG_PTR_OBJ, obj,
	    ARG_UINT8, dir,
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
rlvgl_obj_scroll_to_view2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkobj *obj;
	char atom[32];
	int anim;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_obj_hdl(env, argv[0], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "anim", argv[1]);
		goto out;
	}
	if (strcmp(atom, "on") == 0) {
		anim = LV_ANIM_ON;
	} else if (strcmp(atom, "off") == 0) {
		anim = LV_ANIM_OFF;
	} else {
		rv = enif_make_badarg2(env, "anim", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_obj_scroll_to_view,
	    ARG_PTR_OBJ, obj,
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
	    ARG_PTR_GROUP, lv_group_create,
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

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_GROUP, group,
	    ARG_PTR_OBJ, obj,
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
rlvgl_group_focus_obj1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_focus_obj,
	    ARG_PTR_OBJ, obj,
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
rlvgl_group_remove_obj2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = enter_obj_hdl(env, argv[1], &nls, &obj, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (obj->lvko_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_remove_obj,
	    ARG_PTR_GROUP, group,
	    ARG_PTR_OBJ, obj,
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
rlvgl_group_remove_all_objs1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_remove_all_objs,
	    ARG_PTR_GROUP, group,
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
rlvgl_group_focus_next1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_focus_next,
	    ARG_PTR_GROUP, group,
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
rlvgl_group_focus_prev1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_focus_prev,
	    ARG_PTR_GROUP, group,
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
rlvgl_group_set_wrap2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;
	char atom[32];
	uint wrap;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "wrap", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		wrap = 1;
	} else if (strcmp(atom, "false") == 0) {
		wrap = 0;
	} else {
		rv = enif_make_badarg2(env, "wrap", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_set_wrap,
	    ARG_PTR_GROUP, group,
	    ARG_UINT8, wrap,
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
rlvgl_group_focus_freeze2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;
	char atom[32];
	uint freeze;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_atom(env, argv[1], atom, sizeof (atom), ERL_NIF_LATIN1)) {
		rv = enif_make_badarg2(env, "freeze", argv[1]);
		goto out;
	}
	if (strcmp(atom, "true") == 0) {
		freeze = 1;
	} else if (strcmp(atom, "false") == 0) {
		freeze = 0;
	} else {
		rv = enif_make_badarg2(env, "freeze", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_group_focus_freeze,
	    ARG_PTR_GROUP, group,
	    ARG_UINT8, freeze,
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
rlvgl_group_get_focused1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkgroup *group;

	bzero(&nls, sizeof (nls));

	if (argc != 1)
		return (enif_make_badarg(env));

	rc = enter_group_hdl(env, argv[0], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_PTR_OBJ, lv_group_get_focused,
	    ARG_PTR_GROUP, group,
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
	    ARG_PTR_STYLE, lv_style_alloc,
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

	rc = enter_style_hdl(env, argv[0], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (style->lvks_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_STYLE, style,
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

	rc = enter_style_hdl(env, argv[0], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (style->lvks_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_STYLE, style,
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
rlvgl_style_set_prop3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkstyle *style;
	lv_style_value_t sty_val;
	lv_style_prop_t sty_prop;

	bzero(&nls, sizeof (nls));

	if (argc != 3)
		return (enif_make_badarg(env));

	rc = enter_style_hdl(env, argv[0], &nls, &style, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (style->lvks_ptr == 0) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	rc = parse_style_prop_val(env, argv[1], argv[2],
	    &sty_prop, &sty_val);
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
	    ARG_NONE, lv_style_set_prop,
	    ARG_PTR_STYLE, style,
	    ARG_UINT32, sty_prop,
	    ARG_STYLEVAL, &sty_val,
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
rlvgl_disp_set_bg_image2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	ErlNifBinary src_bin;
	enum arg_type src_type;
	void *src;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	rc = parse_img_src(env, argv[1], &src_bin, &src_type,
	    &src);
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
	    ARG_NONE, lv_disp_set_bg_image,
	    ARG_PTR, inst->lvki_disp,
	    src_type, src,
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
rlvgl_disp_set_bg_opa2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct nif_lock_state nls;
	struct nif_call_data *ncd = NULL;
	ERL_NIF_TERM msgref, rv;
	int rc;
	struct lvkinst *inst;
	uint opacity;

	bzero(&nls, sizeof (nls));

	if (argc != 2)
		return (enif_make_badarg(env));

	rc = enter_inst_hdl(env, argv[0], &nls, &inst, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (!enif_get_uint(env, argv[1], &opacity)) {
		rv = enif_make_badarg2(env, "opacity", argv[1]);
		goto out;
	}

	rc = make_ncd(env, &msgref, &ncd);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}


	rc = lvk_icall(nls.nls_inst, rlvgl_call_cb, ncd,
	    ARG_NONE, lv_disp_set_bg_opa,
	    ARG_PTR, inst->lvki_disp,
	    ARG_UINT8, opacity,
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
rlvgl_disp_get_inactive_time1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_UINT32, lv_disp_get_inactive_time,
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
rlvgl_disp_trig_activity1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_NONE, lv_disp_trig_activity,
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
	    ARG_PTR_OBJ, lv_disp_get_layer_sys,
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
rlvgl_disp_get_layer_top1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_PTR_OBJ, lv_disp_get_layer_top,
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
rlvgl_disp_get_scr_act1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_PTR_OBJ, lv_disp_get_scr_act,
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
rlvgl_disp_get_scr_prev1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_PTR_OBJ, lv_disp_get_scr_prev,
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
	rc = enter_group_hdl(env, argv[1], &nls, &group, 0);
	if (rc != 0) {
		rv = make_errno(env, rc);
		goto out;
	}
	if (group->lvkg_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_GROUP, group,
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
	if (screen == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (screen->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, screen,
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
	if (screen == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (screen->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, screen,
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
	if (cursor == NULL) {
		rc = ENOENT;
		rv = make_errno(env, rc);
		goto out;
	}
	if (cursor->lvko_ptr == 0) {
		rc = ENOENT;
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
	    ARG_PTR_OBJ, cursor,
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
rlvgl_indev_get_focused1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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
	    ARG_PTR_OBJ, lv_indev_get_focused,
	    ARG_PTR, inst->lvki_kbd,
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
{ "textarea_set_max_length",		2, rlvgl_textarea_set_max_length2 }, \
{ "textarea_get_label",			1, rlvgl_textarea_get_label1 }, \
{ "textarea_set_password_show_time",	2, rlvgl_textarea_set_password_show_time2 }, \
{ "img_create",				1, rlvgl_img_create1 }, \
{ "img_set_offset",			2, rlvgl_img_set_offset2 }, \
{ "img_set_src",			2, rlvgl_img_set_src2 }, \
{ "img_set_angle",			2, rlvgl_img_set_angle2 }, \
{ "img_set_pivot",			2, rlvgl_img_set_pivot2 }, \
{ "img_set_zoom",			2, rlvgl_img_set_zoom2 }, \
{ "img_set_antialias",			2, rlvgl_img_set_antialias2 }, \
{ "label_create",			1, rlvgl_label_create1 }, \
{ "label_get_text",			1, rlvgl_label_get_text1 }, \
{ "label_set_text",			2, rlvgl_label_set_text2 }, \
{ "label_set_text_sel_start",		2, rlvgl_label_set_text_sel_start2 }, \
{ "label_set_text_sel_end",		2, rlvgl_label_set_text_sel_end2 }, \
{ "btnmatrix_create",			1, rlvgl_btnmatrix_create1 }, \
{ "btnmatrix_set_map",			2, rlvgl_btnmatrix_set_map2 }, \
{ "btnmatrix_set_btn_ctrl",		3, rlvgl_btnmatrix_set_btn_ctrl3 }, \
{ "btnmatrix_clear_btn_ctrl",		3, rlvgl_btnmatrix_clear_btn_ctrl3 }, \
{ "btnmatrix_set_btn_ctrl_all",		2, rlvgl_btnmatrix_set_btn_ctrl_all2 }, \
{ "btnmatrix_clear_btn_ctrl_all",	2, rlvgl_btnmatrix_clear_btn_ctrl_all2 }, \
{ "btnmatrix_set_btn_width",		3, rlvgl_btnmatrix_set_btn_width3 }, \
{ "btnmatrix_set_one_checked",		2, rlvgl_btnmatrix_set_one_checked2 }, \
{ "btnmatrix_get_selected_btn",		1, rlvgl_btnmatrix_get_selected_btn1 }, \
{ "btnmatrix_set_selected_btn",		2, rlvgl_btnmatrix_set_selected_btn2 }, \
{ "btnmatrix_get_btn_text",		2, rlvgl_btnmatrix_get_btn_text2 }, \
{ "btnmatrix_has_btn_ctrl",		3, rlvgl_btnmatrix_has_btn_ctrl3 }, \
{ "dropdown_create",			1, rlvgl_dropdown_create1 }, \
{ "dropdown_set_options",		2, rlvgl_dropdown_set_options2 }, \
{ "dropdown_add_option",		3, rlvgl_dropdown_add_option3 }, \
{ "dropdown_get_selected",		1, rlvgl_dropdown_get_selected1 }, \
{ "dropdown_set_selected",		2, rlvgl_dropdown_set_selected2 }, \
{ "dropdown_clear_options",		1, rlvgl_dropdown_clear_options1 }, \
{ "dropdown_get_selected_str",		1, rlvgl_dropdown_get_selected_str1 }, \
{ "imgbtn_create",			1, rlvgl_imgbtn_create1 }, \
{ "led_create",				1, rlvgl_led_create1 }, \
{ "led_set_color",			2, rlvgl_led_set_color2 }, \
{ "led_set_brightness",			2, rlvgl_led_set_brightness2 }, \
{ "led_on",				1, rlvgl_led_on1 }, \
{ "led_off",				1, rlvgl_led_off1 }, \
{ "led_toggle",				1, rlvgl_led_toggle1 }, \
{ "led_get_brightness",			1, rlvgl_led_get_brightness1 }, \
{ "list_create",			1, rlvgl_list_create1 }, \
{ "list_add_text",			2, rlvgl_list_add_text2 }, \
{ "list_add_btn",			3, rlvgl_list_add_btn3 }, \
{ "list_get_btn_text",			2, rlvgl_list_get_btn_text2 }, \
{ "menu_create",			1, rlvgl_menu_create1 }, \
{ "menu_page_create",			2, rlvgl_menu_page_create2 }, \
{ "menu_cont_create",			1, rlvgl_menu_cont_create1 }, \
{ "menu_section_create",		1, rlvgl_menu_section_create1 }, \
{ "menu_separator_create",		1, rlvgl_menu_separator_create1 }, \
{ "menu_set_page",			2, rlvgl_menu_set_page2 }, \
{ "menu_set_sidebar_page",		2, rlvgl_menu_set_sidebar_page2 }, \
{ "menu_set_mode_root_back_btn",	2, rlvgl_menu_set_mode_root_back_btn2 }, \
{ "menu_set_mode_header",		2, rlvgl_menu_set_mode_header2 }, \
{ "menu_set_load_page_event",		3, rlvgl_menu_set_load_page_event3 }, \
{ "roller_create",			1, rlvgl_roller_create1 }, \
{ "slider_create",			1, rlvgl_slider_create1 }, \
{ "switch_create",			1, rlvgl_switch_create1 }, \
{ "table_create",			1, rlvgl_table_create1 }, \
{ "table_set_row_cnt",			2, rlvgl_table_set_row_cnt2 }, \
{ "table_set_col_cnt",			2, rlvgl_table_set_col_cnt2 }, \
{ "table_set_col_width",		3, rlvgl_table_set_col_width3 }, \
{ "table_set_cell_value",		4, rlvgl_table_set_cell_value4 }, \
{ "table_add_cell_ctrl",		4, rlvgl_table_add_cell_ctrl4 }, \
{ "table_clear_cell_ctrl",		4, rlvgl_table_clear_cell_ctrl4 }, \
{ "table_get_selected_cell_pt",		1, rlvgl_table_get_selected_cell_pt1 }, \
{ "msgbox_create",			5, rlvgl_msgbox_create5 }, \
{ "msgbox_get_active_btn",		1, rlvgl_msgbox_get_active_btn1 }, \
{ "msgbox_get_active_btn_text",		1, rlvgl_msgbox_get_active_btn_text1 }, \
{ "msgbox_close",			1, rlvgl_msgbox_close1 }, \
{ "msgbox_get_btns",			1, rlvgl_msgbox_get_btns1 }, \
{ "spinner_create",			3, rlvgl_spinner_create3 }, \
{ "meter_create",			1, rlvgl_meter_create1 }, \
{ "meter_add_scale",			1, rlvgl_meter_add_scale1 }, \
{ "meter_set_scale_ticks",		6, rlvgl_meter_set_scale_ticks6 }, \
{ "meter_set_scale_major_ticks",	7, rlvgl_meter_set_scale_major_ticks7 }, \
{ "meter_set_scale_range",		6, rlvgl_meter_set_scale_range6 }, \
{ "meter_add_needle_line",		5, rlvgl_meter_add_needle_line5 }, \
{ "meter_add_arc",			5, rlvgl_meter_add_arc5 }, \
{ "meter_set_indicator_value",		3, rlvgl_meter_set_indicator_value3 }, \
{ "meter_set_indicator_start_value",	3, rlvgl_meter_set_indicator_start_value3 }, \
{ "meter_set_indicator_end_value",	3, rlvgl_meter_set_indicator_end_value3 }, \
{ "spangroup_create",			1, rlvgl_spangroup_create1 }, \
{ "spangroup_set_align",		2, rlvgl_spangroup_set_align2 }, \
{ "spangroup_new_span",			1, rlvgl_spangroup_new_span1 }, \
{ "spangroup_get_child_cnt",		1, rlvgl_spangroup_get_child_cnt1 }, \
{ "spangroup_get_child",		2, rlvgl_spangroup_get_child2 }, \
{ "spangroup_del_span",			2, rlvgl_spangroup_del_span2 }, \
{ "spangroup_set_mode",			2, rlvgl_spangroup_set_mode2 }, \
{ "spangroup_set_overflow",		2, rlvgl_spangroup_set_overflow2 }, \
{ "spangroup_refr_mode",		1, rlvgl_spangroup_refr_mode1 }, \
{ "span_set_text",			2, rlvgl_span_set_text2 }, \
{ "span_set_style",			2, rlvgl_span_set_style2 }, \
{ "chart_create",			1, rlvgl_chart_create1 }, \
{ "chart_set_type",			2, rlvgl_chart_set_type2 }, \
{ "chart_set_point_count",		2, rlvgl_chart_set_point_count2 }, \
{ "chart_set_range",			4, rlvgl_chart_set_range4 }, \
{ "chart_set_update_mode",		2, rlvgl_chart_set_update_mode2 }, \
{ "chart_set_div_line_count",		3, rlvgl_chart_set_div_line_count3 }, \
{ "chart_set_zoom_x",			2, rlvgl_chart_set_zoom_x2 }, \
{ "chart_set_zoom_y",			2, rlvgl_chart_set_zoom_y2 }, \
{ "chart_set_axis_tick",		8, rlvgl_chart_set_axis_tick8 }, \
{ "chart_get_point_count",		1, rlvgl_chart_get_point_count1 }, \
{ "chart_add_series",			3, rlvgl_chart_add_series3 }, \
{ "chart_remove_series",		2, rlvgl_chart_remove_series2 }, \
{ "chart_hide_series",			3, rlvgl_chart_hide_series3 }, \
{ "chart_set_series_color",		3, rlvgl_chart_set_series_color3 }, \
{ "chart_get_series_next",		2, rlvgl_chart_get_series_next2 }, \
{ "chart_set_all_value",		3, rlvgl_chart_set_all_value3 }, \
{ "chart_set_next_value",		3, rlvgl_chart_set_next_value3 }, \
{ "chart_set_next_value2",		4, rlvgl_chart_set_next_value24 }, \
{ "obj_create",				2, rlvgl_obj_create2 }, \
{ "obj_center",				1, rlvgl_obj_center1 }, \
{ "obj_add_flag",			2, rlvgl_obj_add_flag2 }, \
{ "obj_clear_flag",			2, rlvgl_obj_clear_flag2 }, \
{ "obj_has_flag",			2, rlvgl_obj_has_flag2 }, \
{ "obj_has_flag_any",			2, rlvgl_obj_has_flag_any2 }, \
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
{ "obj_set_local_style_prop",		4, rlvgl_obj_set_local_style_prop4 }, \
{ "obj_move_foreground",		1, rlvgl_obj_move_foreground1 }, \
{ "obj_move_background",		1, rlvgl_obj_move_background1 }, \
{ "obj_swap",				2, rlvgl_obj_swap2 }, \
{ "obj_set_parent",			2, rlvgl_obj_set_parent2 }, \
{ "obj_move_to_index",			2, rlvgl_obj_move_to_index2 }, \
{ "obj_get_index",			1, rlvgl_obj_get_index1 }, \
{ "obj_get_child_cnt",			1, rlvgl_obj_get_child_cnt1 }, \
{ "obj_get_child",			2, rlvgl_obj_get_child2 }, \
{ "obj_get_parent",			1, rlvgl_obj_get_parent1 }, \
{ "obj_get_screen",			1, rlvgl_obj_get_screen1 }, \
{ "obj_clean",				1, rlvgl_obj_clean1 }, \
{ "obj_del",				1, rlvgl_obj_del1 }, \
{ "obj_refresh_ext_draw_size",		1, rlvgl_obj_refresh_ext_draw_size1 }, \
{ "obj_set_scrollbar_mode",		2, rlvgl_obj_set_scrollbar_mode2 }, \
{ "obj_set_scroll_dir",			2, rlvgl_obj_set_scroll_dir2 }, \
{ "obj_scroll_to_view",			2, rlvgl_obj_scroll_to_view2 }, \
{ "group_create",			1, rlvgl_group_create1 }, \
{ "group_add_obj",			2, rlvgl_group_add_obj2 }, \
{ "group_focus_obj",			1, rlvgl_group_focus_obj1 }, \
{ "group_remove_obj",			2, rlvgl_group_remove_obj2 }, \
{ "group_remove_all_objs",		1, rlvgl_group_remove_all_objs1 }, \
{ "group_focus_next",			1, rlvgl_group_focus_next1 }, \
{ "group_focus_prev",			1, rlvgl_group_focus_prev1 }, \
{ "group_set_wrap",			2, rlvgl_group_set_wrap2 }, \
{ "group_focus_freeze",			2, rlvgl_group_focus_freeze2 }, \
{ "group_get_focused",			1, rlvgl_group_get_focused1 }, \
{ "style_create",			1, rlvgl_style_create1 }, \
{ "style_set_flex_align",		4, rlvgl_style_set_flex_align4 }, \
{ "style_set_flex_flow",		2, rlvgl_style_set_flex_flow2 }, \
{ "style_set_prop",			3, rlvgl_style_set_prop3 }, \
{ "disp_set_bg_color",			2, rlvgl_disp_set_bg_color2 }, \
{ "disp_set_bg_image",			2, rlvgl_disp_set_bg_image2 }, \
{ "disp_set_bg_opa",			2, rlvgl_disp_set_bg_opa2 }, \
{ "disp_get_inactive_time",		1, rlvgl_disp_get_inactive_time1 }, \
{ "disp_trig_activity",			1, rlvgl_disp_trig_activity1 }, \
{ "disp_get_layer_sys",			1, rlvgl_disp_get_layer_sys1 }, \
{ "disp_get_layer_top",			1, rlvgl_disp_get_layer_top1 }, \
{ "disp_get_scr_act",			1, rlvgl_disp_get_scr_act1 }, \
{ "disp_get_scr_prev",			1, rlvgl_disp_get_scr_prev1 }, \
{ "set_kbd_group",			2, rlvgl_set_kbd_group2 }, \
{ "scr_load",				2, rlvgl_scr_load2 }, \
{ "scr_load_anim",			6, rlvgl_scr_load_anim6 }, \
{ "set_mouse_cursor",			2, rlvgl_set_mouse_cursor2 }, \
{ "indev_get_focused",			1, rlvgl_indev_get_focused1 }
