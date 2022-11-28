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

struct scr_load_anim {
	const char		*sla_str;
	lv_scr_load_anim_t	 sla_val;
};
const static struct scr_load_anim scr_load_anims[] = {
	{ "none",		LV_SCR_LOAD_ANIM_NONE },
	{ "fade_in",		LV_SCR_LOAD_ANIM_FADE_IN },
	{ "fade_out",		LV_SCR_LOAD_ANIM_FADE_OUT },
	{ "over_left",		LV_SCR_LOAD_ANIM_OVER_LEFT },
	{ "over_right",		LV_SCR_LOAD_ANIM_OVER_RIGHT },
	{ "over_top",		LV_SCR_LOAD_ANIM_OVER_TOP },
	{ "over_bottom",	LV_SCR_LOAD_ANIM_OVER_BOTTOM },
	{ "move_left",		LV_SCR_LOAD_ANIM_MOVE_LEFT },
	{ "move_right",		LV_SCR_LOAD_ANIM_MOVE_RIGHT },
	{ "move_top",		LV_SCR_LOAD_ANIM_MOVE_TOP },
	{ "move_bottom",	LV_SCR_LOAD_ANIM_MOVE_BOTTOM },
	{ "out_left",		LV_SCR_LOAD_ANIM_OUT_LEFT },
	{ "out_right",		LV_SCR_LOAD_ANIM_OUT_RIGHT },
	{ "out_top",		LV_SCR_LOAD_ANIM_OUT_TOP },
	{ "out_bottom",		LV_SCR_LOAD_ANIM_OUT_BOTTOM },
	{ NULL, 0 }
};

struct obj_flag {
	const char	*of_str;
	lv_obj_flag_t	 of_val;
};
const static struct obj_flag obj_flags[] = {
	{ "hidden",		LV_OBJ_FLAG_HIDDEN },
	{ "clickable",		LV_OBJ_FLAG_CLICKABLE },
	{ "click_focusable",	LV_OBJ_FLAG_CLICK_FOCUSABLE },
	{ "checkable",		LV_OBJ_FLAG_CHECKABLE },
	{ "flex_in_new_track",	LV_OBJ_FLAG_FLEX_IN_NEW_TRACK },
	{ "scrollable",		LV_OBJ_FLAG_SCROLLABLE },
	{ "scroll_elastic",	LV_OBJ_FLAG_SCROLL_ELASTIC },
	{ "scroll_momentum",	LV_OBJ_FLAG_SCROLL_MOMENTUM },
	{ "scroll_one",		LV_OBJ_FLAG_SCROLL_ONE },
	{ "scroll_chain_hor",	LV_OBJ_FLAG_SCROLL_CHAIN_HOR },
	{ "scroll_chain_ver",	LV_OBJ_FLAG_SCROLL_CHAIN_VER },
	{ "scroll_on_focus",	LV_OBJ_FLAG_SCROLL_ON_FOCUS },
	{ "scroll_with_arrow",	LV_OBJ_FLAG_SCROLL_WITH_ARROW },
	{ "snappable",		LV_OBJ_FLAG_SNAPPABLE },
	{ "press_lock",		LV_OBJ_FLAG_PRESS_LOCK },
	{ "event_bubble",	LV_OBJ_FLAG_EVENT_BUBBLE },
	{ "gesture_bubble",	LV_OBJ_FLAG_GESTURE_BUBBLE },
	{ "adv_hittest",	LV_OBJ_FLAG_ADV_HITTEST },
	{ "ignore_layout",	LV_OBJ_FLAG_IGNORE_LAYOUT },
	{ "floating",		LV_OBJ_FLAG_FLOATING },
	{ "overflow_visible",	LV_OBJ_FLAG_OVERFLOW_VISIBLE },
	{ NULL, 0 }
};

struct enum_spec {
	const char 	*es_str;
	uint32_t	 es_val;
};
const static struct enum_spec align_specs[] = {
	{ "center",		LV_ALIGN_CENTER },
	{ "top_left",		LV_ALIGN_TOP_LEFT },
	{ "top_mid",		LV_ALIGN_TOP_MID },
	{ "top_right",		LV_ALIGN_TOP_RIGHT },
	{ "left_mid",		LV_ALIGN_LEFT_MID },
	{ "right_mid",		LV_ALIGN_RIGHT_MID },
	{ "bottom_left",	LV_ALIGN_BOTTOM_LEFT },
	{ "bottom_mid",		LV_ALIGN_BOTTOM_MID },
	{ "bottom_right",	LV_ALIGN_BOTTOM_RIGHT },
	{ "out_top_left",	LV_ALIGN_OUT_TOP_LEFT },
	{ "out_top_mid",	LV_ALIGN_OUT_TOP_MID },
	{ "out_top_right",	LV_ALIGN_OUT_TOP_RIGHT },
	{ "out_right_top",	LV_ALIGN_OUT_RIGHT_TOP },
	{ "out_right_mid",	LV_ALIGN_OUT_RIGHT_MID },
	{ "out_right_bottom",	LV_ALIGN_OUT_RIGHT_BOTTOM },
	{ "out_bottom_right",	LV_ALIGN_OUT_BOTTOM_RIGHT },
	{ "out_bottom_mid",	LV_ALIGN_OUT_BOTTOM_MID },
	{ "out_bottom_left",	LV_ALIGN_OUT_BOTTOM_LEFT },
	{ "out_left_bottom",	LV_ALIGN_OUT_LEFT_BOTTOM },
	{ "out_left_mid",	LV_ALIGN_OUT_LEFT_MID },
	{ "out_left_top",	LV_ALIGN_OUT_LEFT_MID },
	{ NULL, 0 }
};
const static struct enum_spec dir_specs[] = {
	{ "none",		LV_DIR_NONE },
	{ "left",		LV_DIR_LEFT },
	{ "right",		LV_DIR_RIGHT },
	{ "top",		LV_DIR_TOP },
	{ "bottom",		LV_DIR_BOTTOM },
	{ "horizontal",		LV_DIR_HOR },
	{ "vertical",		LV_DIR_VER },
	{ "all",		LV_DIR_ALL },
	{ NULL, 0 }
};
const static struct enum_spec base_dir_specs[] = {
	{ "left_to_right",	LV_BASE_DIR_LTR },
	{ "right_to_left",	LV_BASE_DIR_RTL },
	{ "auto",		LV_BASE_DIR_AUTO },
	{ NULL, 0 }
};
const static struct enum_spec grad_dir_specs[] = {
	{ "none",		LV_GRAD_DIR_NONE },
	{ "vertical",		LV_GRAD_DIR_VER },
	{ "horizontal",		LV_GRAD_DIR_HOR },
	{ NULL, 0 }
};
const static struct enum_spec boolean_specs[] = {
	{ "true",		1 },
	{ "false",		0 },
	{ NULL, 0 }
};
const static struct enum_spec border_side_specs[] = {
	{ "none",		LV_BORDER_SIDE_NONE },
	{ "bottom",		LV_BORDER_SIDE_BOTTOM },
	{ "top",		LV_BORDER_SIDE_TOP },
	{ "left",		LV_BORDER_SIDE_LEFT },
	{ "right",		LV_BORDER_SIDE_RIGHT },
	{ "full",		LV_BORDER_SIDE_FULL },
	{ NULL, 0 }
};
const static struct enum_spec text_decor_specs[] = {
	{ "none",		LV_TEXT_DECOR_NONE },
	{ "underline",		LV_TEXT_DECOR_UNDERLINE },
	{ "strikethrough",	LV_TEXT_DECOR_STRIKETHROUGH },
	{ NULL, 0 }
};
const static struct enum_spec text_align_specs[] = {
	{ "auto",		LV_TEXT_ALIGN_AUTO },
	{ "left",		LV_TEXT_ALIGN_LEFT },
	{ "center",		LV_TEXT_ALIGN_CENTER },
	{ "right",		LV_TEXT_ALIGN_RIGHT },
	{ NULL, 0 }
};
const static struct enum_spec blend_mode_specs[] = {
	{ "normal",		LV_BLEND_MODE_NORMAL },
	{ "additive",		LV_BLEND_MODE_ADDITIVE },
	{ "subtractive",	LV_BLEND_MODE_SUBTRACTIVE },
	{ "multiply",		LV_BLEND_MODE_MULTIPLY },
	{ "replace",		LV_BLEND_MODE_REPLACE },
	{ NULL, 0 }
};

struct style_prop {
	const char		*sp_str;
	lv_style_prop_t		 sp_prop;
	enum arg_type		 sp_type;
	const struct enum_spec	*sp_enum;
	bool			 sp_multi_enum;
};
const static struct style_prop style_props[] = {
	{ "width",		LV_STYLE_WIDTH,			ARG_UINT32 },
	{ "min_width",		LV_STYLE_MIN_WIDTH,		ARG_UINT32 },
	{ "max_width",		LV_STYLE_MAX_WIDTH,		ARG_UINT32 },
	{ "height",		LV_STYLE_HEIGHT,		ARG_UINT32 },
	{ "min_height",		LV_STYLE_MIN_HEIGHT,		ARG_UINT32 },
	{ "max_height",		LV_STYLE_MAX_HEIGHT,		ARG_UINT32 },
	{ "x",			LV_STYLE_X,			ARG_UINT32 },
	{ "y",			LV_STYLE_Y,			ARG_UINT32 },
	{ "align",		LV_STYLE_ALIGN,			ARG_UINT32,
					.sp_enum = align_specs },
	{ "radius",		LV_STYLE_RADIUS,		ARG_UINT32 },
	{ "pad_top",		LV_STYLE_PAD_TOP,		ARG_UINT32 },
	{ "pad_bottom",		LV_STYLE_PAD_BOTTOM,		ARG_UINT32 },
	{ "pad_left",		LV_STYLE_PAD_LEFT,		ARG_UINT32 },
	{ "pad_right",		LV_STYLE_PAD_RIGHT,		ARG_UINT32 },
	{ "pad_row",		LV_STYLE_PAD_ROW,		ARG_UINT32 },
	{ "pad_column",		LV_STYLE_PAD_COLUMN,		ARG_UINT32 },
	{ "base_dir",		LV_STYLE_BASE_DIR,		ARG_UINT32,
					.sp_enum = base_dir_specs },
	{ "clip_corner",	LV_STYLE_CLIP_CORNER,		ARG_UINT32,
					.sp_enum = boolean_specs },
	{ "bg_color",		LV_STYLE_BG_COLOR,		ARG_COLOR },
	{ "bg_opa",		LV_STYLE_BG_OPA,		ARG_UINT32 },
	{ "bg_grad_color",	LV_STYLE_BG_GRAD_COLOR,		ARG_COLOR },
	{ "bg_grad_dir",	LV_STYLE_BG_GRAD_DIR,		ARG_UINT32,
					.sp_enum = grad_dir_specs },
	{ "bg_main_stop",	LV_STYLE_BG_MAIN_STOP,		ARG_UINT32 },
	{ "bg_grad_stop",	LV_STYLE_BG_GRAD_STOP,		ARG_UINT32 },
	{ "bg_img_opa",		LV_STYLE_BG_IMG_OPA,		ARG_UINT32 },
	{ "bg_img_recolor",	LV_STYLE_BG_IMG_RECOLOR,	ARG_COLOR },
	{ "bg_img_recolor_opa",	LV_STYLE_BG_IMG_RECOLOR_OPA,	ARG_UINT32 },
	{ "bg_img_tiled",	LV_STYLE_BG_IMG_TILED,		ARG_UINT32,
					.sp_enum = boolean_specs },
	{ "border_color",	LV_STYLE_BORDER_COLOR,		ARG_COLOR },
	{ "border_opa",		LV_STYLE_BORDER_OPA,		ARG_UINT32 },
	{ "border_width",	LV_STYLE_BORDER_WIDTH,		ARG_UINT32 },
	{ "border_side",	LV_STYLE_BORDER_SIDE,		ARG_UINT32,
					.sp_enum = border_side_specs,
					.sp_multi_enum = true },
	{ "border_post",	LV_STYLE_BORDER_POST,		ARG_UINT32,
					.sp_enum = boolean_specs },
	{ "outline_width",	LV_STYLE_OUTLINE_WIDTH,		ARG_UINT32 },
	{ "outline_color",	LV_STYLE_OUTLINE_COLOR,		ARG_COLOR },
	{ "outline_opa",	LV_STYLE_OUTLINE_OPA,		ARG_UINT32 },
	{ "outline_pad",	LV_STYLE_OUTLINE_PAD,		ARG_UINT32 },
	{ "shadow_width",	LV_STYLE_SHADOW_WIDTH,		ARG_UINT32 },
	{ "shadow_ofs_x",	LV_STYLE_SHADOW_OFS_X,		ARG_UINT32 },
	{ "shadow_ofs_y",	LV_STYLE_SHADOW_OFS_Y,		ARG_UINT32 },
	{ "shadow_spread",	LV_STYLE_SHADOW_SPREAD,		ARG_UINT32 },
	{ "shadow_color",	LV_STYLE_SHADOW_COLOR,		ARG_COLOR },
	{ "shadow_opa",		LV_STYLE_SHADOW_OPA,		ARG_UINT32 },
	{ "img_opa",		LV_STYLE_IMG_OPA,		ARG_UINT32 },
	{ "img_recolor",	LV_STYLE_IMG_RECOLOR,		ARG_COLOR },
	{ "img_recolor_opa",	LV_STYLE_IMG_RECOLOR_OPA,	ARG_UINT32 },
	{ "line_width",		LV_STYLE_LINE_WIDTH,		ARG_UINT32 },
	{ "line_dash_width",	LV_STYLE_LINE_DASH_WIDTH,	ARG_UINT32 },
	{ "line_dash_gap",	LV_STYLE_LINE_DASH_GAP,		ARG_UINT32 },
	{ "line_rounded",	LV_STYLE_LINE_ROUNDED,		ARG_UINT32,
					.sp_enum = boolean_specs },
	{ "line_color",		LV_STYLE_LINE_COLOR,		ARG_COLOR },
	{ "line_opa",		LV_STYLE_LINE_OPA,		ARG_UINT32 },
	{ "arc_width",		LV_STYLE_ARC_WIDTH,		ARG_UINT32 },
	{ "arc_rounded",	LV_STYLE_ARC_ROUNDED,		ARG_UINT32,
					.sp_enum = boolean_specs },
	{ "arc_color",		LV_STYLE_ARC_COLOR,		ARG_COLOR },
	{ "arc_opa",		LV_STYLE_ARC_OPA,		ARG_UINT32 },
	{ "text_color",		LV_STYLE_TEXT_COLOR,		ARG_COLOR },
	{ "text_opa",		LV_STYLE_TEXT_OPA,		ARG_UINT32 },
	{ "text_letter_space",	LV_STYLE_TEXT_LETTER_SPACE,	ARG_UINT32 },
	{ "text_line_space",	LV_STYLE_TEXT_LINE_SPACE,	ARG_UINT32 },
	{ "text_decor",		LV_STYLE_TEXT_DECOR,		ARG_UINT32,
					.sp_enum = text_decor_specs,
					.sp_multi_enum = true },
	{ "text_align",		LV_STYLE_TEXT_ALIGN,		ARG_UINT32,
					.sp_enum = text_align_specs },
	{ "opa",		LV_STYLE_OPA,			ARG_UINT32 },
	{ "color_filter_opa",	LV_STYLE_COLOR_FILTER_OPA,	ARG_UINT32 },
	{ "anim_time",		LV_STYLE_ANIM_TIME,		ARG_UINT32 },
	{ "anim_speed",		LV_STYLE_ANIM_SPEED,		ARG_UINT32 },
	{ "blend_mode",		LV_STYLE_BLEND_MODE,		ARG_UINT32,
					.sp_enum = blend_mode_specs },
	{ "transform_width",	LV_STYLE_TRANSFORM_WIDTH,	ARG_UINT32 },
	{ "transform_height",	LV_STYLE_TRANSFORM_HEIGHT,	ARG_UINT32 },
	{ "translate_x",	LV_STYLE_TRANSLATE_X,		ARG_UINT32 },
	{ "translate_y",	LV_STYLE_TRANSLATE_Y,		ARG_UINT32 },
	{ "transform_zoom",	LV_STYLE_TRANSFORM_ZOOM,	ARG_UINT32 },
	{ "transform_angle",	LV_STYLE_TRANSFORM_ANGLE,	ARG_UINT32 },
	{ "transform_pivot_x",	LV_STYLE_TRANSFORM_PIVOT_X,	ARG_UINT32 },
	{ "transform_pivot_y",	LV_STYLE_TRANSFORM_PIVOT_Y,	ARG_UINT32 },
	{ NULL, 0 }
};

struct event_code {
	const char	*ec_str;
	lv_event_code_t	 ec_code;
};
const static struct event_code event_codes[] = {
	{ "all",			LV_EVENT_ALL },
	{ "pressed",			LV_EVENT_PRESSED },
	{ "pressing",			LV_EVENT_PRESSING },
	{ "press_lost",			LV_EVENT_PRESS_LOST },
	{ "short_clicked",		LV_EVENT_SHORT_CLICKED },
	{ "long_pressed",		LV_EVENT_LONG_PRESSED },
	{ "long_pressed_repeat",	LV_EVENT_LONG_PRESSED_REPEAT },
	{ "clicked",			LV_EVENT_CLICKED },
	{ "released",			LV_EVENT_RELEASED },
	{ "scroll_begin",		LV_EVENT_SCROLL_BEGIN },
	{ "scroll_end",			LV_EVENT_SCROLL_END },
	{ "scroll",			LV_EVENT_SCROLL },
	{ "gesture",			LV_EVENT_GESTURE },
	{ "key",			LV_EVENT_KEY },
	{ "focused",			LV_EVENT_FOCUSED },
	{ "defocused",			LV_EVENT_DEFOCUSED },
	{ "leave",			LV_EVENT_LEAVE },
	{ "value_changed",		LV_EVENT_VALUE_CHANGED },
	{ "insert",			LV_EVENT_INSERT },
	{ "refresh",			LV_EVENT_REFRESH },
	{ "ready",			LV_EVENT_READY },
	{ "cancel",			LV_EVENT_CANCEL },
	{ NULL, 0 }
};
