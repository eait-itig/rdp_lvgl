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
const static struct enum_spec anim_enable_specs[] = {
	{ "off",		LV_ANIM_OFF },
	{ "on",			LV_ANIM_ON },
	{ NULL, 0 }
};
const static struct enum_spec obj_state_specs[] = {
	{ "default",		LV_STATE_DEFAULT },
	{ "checked",		LV_STATE_CHECKED },
	{ "focused",		LV_STATE_FOCUSED },
	{ "focus_key",		LV_STATE_FOCUS_KEY },
	{ "edited",		LV_STATE_EDITED },
	{ "hovered",		LV_STATE_HOVERED },
	{ "pressed",		LV_STATE_PRESSED },
	{ "scrolled",		LV_STATE_SCROLLED },
	{ "disabled",		LV_STATE_DISABLED },
	{ "user_1",		LV_STATE_USER_1 },
	{ "user_2",		LV_STATE_USER_2 },
	{ "user_3",		LV_STATE_USER_3 },
	{ "user_4",		LV_STATE_USER_4 },
	{ "any",		LV_STATE_ANY },
	{ NULL, 0 }
};
const static struct enum_spec obj_part_specs[] = {
	{ "main",		LV_PART_MAIN },
	{ "scrollbar",		LV_PART_SCROLLBAR },
	{ "indicator",		LV_PART_INDICATOR },
	{ "knob",		LV_PART_KNOB },
	{ "selected",		LV_PART_SELECTED },
	{ "items",		LV_PART_ITEMS },
	{ "ticks",		LV_PART_TICKS },
	{ "cursor",		LV_PART_CURSOR },
	{ "any",		LV_PART_ANY },
	{ NULL, 0 }
};
const static struct enum_spec style_selector_specs[] = {
	{ "default",		LV_STATE_DEFAULT },
	{ "checked",		LV_STATE_CHECKED },
	{ "focused",		LV_STATE_FOCUSED },
	{ "focus_key",		LV_STATE_FOCUS_KEY },
	{ "edited",		LV_STATE_EDITED },
	{ "hovered",		LV_STATE_HOVERED },
	{ "pressed",		LV_STATE_PRESSED },
	{ "scrolled",		LV_STATE_SCROLLED },
	{ "disabled",		LV_STATE_DISABLED },
	{ "user_1",		LV_STATE_USER_1 },
	{ "user_2",		LV_STATE_USER_2 },
	{ "user_3",		LV_STATE_USER_3 },
	{ "user_4",		LV_STATE_USER_4 },
	{ "main",		LV_PART_MAIN },
	{ "scrollbar",		LV_PART_SCROLLBAR },
	{ "indicator",		LV_PART_INDICATOR },
	{ "knob",		LV_PART_KNOB },
	{ "selected",		LV_PART_SELECTED },
	{ "items",		LV_PART_ITEMS },
	{ "ticks",		LV_PART_TICKS },
	{ "cursor",		LV_PART_CURSOR },
	{ "any_part",		LV_PART_ANY },
	{ "any_state",		LV_STATE_ANY },
	{ NULL, 0 }
};
const static struct enum_spec obj_flags[] = {
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
const static struct enum_spec flex_align[] = {
	{ "start",		LV_FLEX_ALIGN_START },
	{ "end",		LV_FLEX_ALIGN_END },
	{ "center",		LV_FLEX_ALIGN_CENTER },
	{ "space_evenly",	LV_FLEX_ALIGN_SPACE_EVENLY },
	{ "space_around",	LV_FLEX_ALIGN_SPACE_AROUND },
	{ "space_between",	LV_FLEX_ALIGN_SPACE_BETWEEN },
	{ NULL, 0 }
};
const static struct enum_spec flex_flow[] = {
	{ "row",		LV_FLEX_FLOW_ROW },
	{ "column",		LV_FLEX_FLOW_COLUMN },
	{ "row_wrap",		LV_FLEX_FLOW_ROW_WRAP },
	{ "row_reverse",	LV_FLEX_FLOW_ROW_REVERSE },
	{ "row_wrap_reverse",	LV_FLEX_FLOW_ROW_WRAP_REVERSE },
	{ "column_wrap",	LV_FLEX_FLOW_COLUMN_WRAP },
	{ "column_reverse",	LV_FLEX_FLOW_COLUMN_REVERSE },
	{ "column_wrap_reverse", LV_FLEX_FLOW_COLUMN_WRAP_REVERSE },
	{ NULL, 0 }
};
const static struct enum_spec scr_load_anims[] = {
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
const static struct enum_spec bar_mode[] = {
	{ "normal",		LV_BAR_MODE_NORMAL },
	{ "symmetrical",	LV_BAR_MODE_SYMMETRICAL },
	{ "range",		LV_BAR_MODE_RANGE },
	{ NULL, 0 }
};
const static struct enum_spec menu_mode_root_back_btn[] = {
	{ "disabled",		LV_MENU_ROOT_BACK_BTN_DISABLED },
	{ "enabled", 		LV_MENU_ROOT_BACK_BTN_ENABLED },
	{ NULL, 0 }
};
const static struct enum_spec menu_mode_header[] = {
	{ "top_fixed",		LV_MENU_HEADER_TOP_FIXED },
	{ "top_unfixed",	LV_MENU_HEADER_TOP_UNFIXED },
	{ "bottom_fixed",	LV_MENU_HEADER_BOTTOM_FIXED },
	{ NULL, 0 }
};
const static struct enum_spec table_cell_ctrls[] = {
	{ "merge_right",	LV_TABLE_CELL_CTRL_MERGE_RIGHT },
	{ "text_crop",		LV_TABLE_CELL_CTRL_TEXT_CROP },
	{ "custom_1",		LV_TABLE_CELL_CTRL_CUSTOM_1 },
	{ "custom_2",		LV_TABLE_CELL_CTRL_CUSTOM_2 },
	{ "custom_3",		LV_TABLE_CELL_CTRL_CUSTOM_3 },
	{ "custom_4",		LV_TABLE_CELL_CTRL_CUSTOM_4 },
	{ NULL, 0 }
};
const static struct enum_spec btnmatrix_ctrls[] = {
	{ "hidden",		LV_BTNMATRIX_CTRL_HIDDEN },
	{ "no_repeat",		LV_BTNMATRIX_CTRL_NO_REPEAT },
	{ "disabled",		LV_BTNMATRIX_CTRL_DISABLED },
	{ "checkable",		LV_BTNMATRIX_CTRL_CHECKABLE },
	{ "checked",		LV_BTNMATRIX_CTRL_CHECKED },
	{ "click_trig",		LV_BTNMATRIX_CTRL_CLICK_TRIG },
	{ "popover",		LV_BTNMATRIX_CTRL_POPOVER },
	{ "recolor",		LV_BTNMATRIX_CTRL_RECOLOR },
	{ "custom_1",		LV_BTNMATRIX_CTRL_CUSTOM_1 },
	{ "custom_2",		LV_BTNMATRIX_CTRL_CUSTOM_2 },
	{ NULL, 0 }
};
const static struct enum_spec chart_types[] = {
	{ "none",		LV_CHART_TYPE_NONE },
	{ "line",		LV_CHART_TYPE_LINE },
	{ "bar",		LV_CHART_TYPE_BAR },
	{ "scatter",		LV_CHART_TYPE_SCATTER },
	{ NULL, 0 }
};
const static struct enum_spec chart_update_modes[] = {
	{ "shift",		LV_CHART_UPDATE_MODE_SHIFT },
	{ "circular",		LV_CHART_UPDATE_MODE_CIRCULAR },
	{ NULL, 0 }
};
const static struct enum_spec chart_axes[] = {
	{ "primary_y",		LV_CHART_AXIS_PRIMARY_Y },
	{ "secondary_y",	LV_CHART_AXIS_SECONDARY_Y },
	{ "primary_x",		LV_CHART_AXIS_PRIMARY_X },
	{ "secondary_x",	LV_CHART_AXIS_SECONDARY_Y },
	{ NULL, 0 }
};

enum style_prop_type {
	SPT_INT32,
	SPT_ENUM,
	SPT_MULTI_ENUM,
	SPT_COLOR,
	SPT_FONT,
	SPT_COORD,
};

struct style_prop {
	const char		*sp_str;
	lv_style_prop_t		 sp_prop;
	enum style_prop_type	 sp_type;
	const struct enum_spec	*sp_enum;
};
const static struct style_prop style_props[] = {
	{ "width",		LV_STYLE_WIDTH,			SPT_COORD },
	{ "min_width",		LV_STYLE_MIN_WIDTH,		SPT_COORD },
	{ "max_width",		LV_STYLE_MAX_WIDTH,		SPT_COORD },
	{ "height",		LV_STYLE_HEIGHT,		SPT_COORD },
	{ "min_height",		LV_STYLE_MIN_HEIGHT,		SPT_COORD },
	{ "max_height",		LV_STYLE_MAX_HEIGHT,		SPT_COORD },
	{ "x",			LV_STYLE_X,			SPT_COORD },
	{ "y",			LV_STYLE_Y,			SPT_COORD },
	{ "align",		LV_STYLE_ALIGN,			SPT_ENUM,
					.sp_enum = align_specs },
	{ "radius",		LV_STYLE_RADIUS,		SPT_COORD },
	{ "pad_top",		LV_STYLE_PAD_TOP,		SPT_COORD },
	{ "pad_bottom",		LV_STYLE_PAD_BOTTOM,		SPT_COORD },
	{ "pad_left",		LV_STYLE_PAD_LEFT,		SPT_COORD },
	{ "pad_right",		LV_STYLE_PAD_RIGHT,		SPT_COORD },
	{ "pad_row",		LV_STYLE_PAD_ROW,		SPT_COORD },
	{ "pad_column",		LV_STYLE_PAD_COLUMN,		SPT_COORD },
	{ "base_dir",		LV_STYLE_BASE_DIR,		SPT_ENUM,
					.sp_enum = base_dir_specs },
	{ "clip_corner",	LV_STYLE_CLIP_CORNER,		SPT_ENUM,
					.sp_enum = boolean_specs },
	{ "bg_color",		LV_STYLE_BG_COLOR,		SPT_COLOR },
	{ "bg_opa",		LV_STYLE_BG_OPA,		SPT_INT32 },
	{ "bg_grad_color",	LV_STYLE_BG_GRAD_COLOR,		SPT_COLOR },
	{ "bg_grad_dir",	LV_STYLE_BG_GRAD_DIR,		SPT_ENUM,
					.sp_enum = grad_dir_specs },
	{ "bg_main_stop",	LV_STYLE_BG_MAIN_STOP,		SPT_COORD },
	{ "bg_grad_stop",	LV_STYLE_BG_GRAD_STOP,		SPT_COORD },
	{ "bg_img_opa",		LV_STYLE_BG_IMG_OPA,		SPT_INT32 },
	{ "bg_img_recolor",	LV_STYLE_BG_IMG_RECOLOR,	SPT_COLOR },
	{ "bg_img_recolor_opa",	LV_STYLE_BG_IMG_RECOLOR_OPA,	SPT_INT32 },
	{ "bg_img_tiled",	LV_STYLE_BG_IMG_TILED,		SPT_ENUM,
					.sp_enum = boolean_specs },
	{ "border_color",	LV_STYLE_BORDER_COLOR,		SPT_COLOR },
	{ "border_opa",		LV_STYLE_BORDER_OPA,		SPT_INT32 },
	{ "border_width",	LV_STYLE_BORDER_WIDTH,		SPT_COORD },
	{ "border_side",	LV_STYLE_BORDER_SIDE,		SPT_MULTI_ENUM,
					.sp_enum = border_side_specs },
	{ "border_post",	LV_STYLE_BORDER_POST,		SPT_ENUM,
					.sp_enum = boolean_specs },
	{ "outline_width",	LV_STYLE_OUTLINE_WIDTH,		SPT_COORD },
	{ "outline_color",	LV_STYLE_OUTLINE_COLOR,		SPT_COLOR },
	{ "outline_opa",	LV_STYLE_OUTLINE_OPA,		SPT_INT32 },
	{ "outline_pad",	LV_STYLE_OUTLINE_PAD,		SPT_COORD },
	{ "shadow_width",	LV_STYLE_SHADOW_WIDTH,		SPT_COORD },
	{ "shadow_ofs_x",	LV_STYLE_SHADOW_OFS_X,		SPT_COORD },
	{ "shadow_ofs_y",	LV_STYLE_SHADOW_OFS_Y,		SPT_COORD },
	{ "shadow_spread",	LV_STYLE_SHADOW_SPREAD,		SPT_COORD },
	{ "shadow_color",	LV_STYLE_SHADOW_COLOR,		SPT_COLOR },
	{ "shadow_opa",		LV_STYLE_SHADOW_OPA,		SPT_INT32 },
	{ "img_opa",		LV_STYLE_IMG_OPA,		SPT_INT32 },
	{ "img_recolor",	LV_STYLE_IMG_RECOLOR,		SPT_COLOR },
	{ "img_recolor_opa",	LV_STYLE_IMG_RECOLOR_OPA,	SPT_INT32 },
	{ "line_width",		LV_STYLE_LINE_WIDTH,		SPT_COORD },
	{ "line_dash_width",	LV_STYLE_LINE_DASH_WIDTH,	SPT_COORD },
	{ "line_dash_gap",	LV_STYLE_LINE_DASH_GAP,		SPT_COORD },
	{ "line_rounded",	LV_STYLE_LINE_ROUNDED,		SPT_ENUM,
					.sp_enum = boolean_specs },
	{ "line_color",		LV_STYLE_LINE_COLOR,		SPT_COLOR },
	{ "line_opa",		LV_STYLE_LINE_OPA,		SPT_INT32 },
	{ "arc_width",		LV_STYLE_ARC_WIDTH,		SPT_COORD },
	{ "arc_rounded",	LV_STYLE_ARC_ROUNDED,		SPT_ENUM,
					.sp_enum = boolean_specs },
	{ "arc_color",		LV_STYLE_ARC_COLOR,		SPT_COLOR },
	{ "arc_opa",		LV_STYLE_ARC_OPA,		SPT_INT32 },
	{ "text_color",		LV_STYLE_TEXT_COLOR,		SPT_COLOR },
	{ "text_opa",		LV_STYLE_TEXT_OPA,		SPT_INT32 },
	{ "text_letter_space",	LV_STYLE_TEXT_LETTER_SPACE,	SPT_COORD },
	{ "text_line_space",	LV_STYLE_TEXT_LINE_SPACE,	SPT_COORD },
	{ "text_decor",		LV_STYLE_TEXT_DECOR,		SPT_MULTI_ENUM,
					.sp_enum = text_decor_specs },
	{ "text_align",		LV_STYLE_TEXT_ALIGN,		SPT_ENUM,
					.sp_enum = text_align_specs },
	{ "opa",		LV_STYLE_OPA,			SPT_INT32 },
	{ "color_filter_opa",	LV_STYLE_COLOR_FILTER_OPA,	SPT_INT32 },
	{ "anim_time",		LV_STYLE_ANIM_TIME,		SPT_INT32 },
	{ "anim_speed",		LV_STYLE_ANIM_SPEED,		SPT_INT32 },
	{ "blend_mode",		LV_STYLE_BLEND_MODE,		SPT_ENUM,
					.sp_enum = blend_mode_specs },
	{ "transform_width",	LV_STYLE_TRANSFORM_WIDTH,	SPT_COORD },
	{ "transform_height",	LV_STYLE_TRANSFORM_HEIGHT,	SPT_COORD },
	{ "translate_x",	LV_STYLE_TRANSLATE_X,		SPT_COORD },
	{ "translate_y",	LV_STYLE_TRANSLATE_Y,		SPT_COORD },
	{ "transform_zoom",	LV_STYLE_TRANSFORM_ZOOM,	SPT_COORD },
	{ "transform_angle",	LV_STYLE_TRANSFORM_ANGLE,	SPT_COORD },
	{ "transform_pivot_x",	LV_STYLE_TRANSFORM_PIVOT_X,	SPT_COORD },
	{ "transform_pivot_y",	LV_STYLE_TRANSFORM_PIVOT_Y,	SPT_COORD },
	{ "text_font",		LV_STYLE_TEXT_FONT,		SPT_FONT },
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

struct font_spec {
	const char	*fs_family;
	const char	*fs_variant;
	uint		 fs_size;
	const lv_font_t	*fs_font;
};
const static struct font_spec font_specs[] = {
	{ "montserrat", NULL, 	10, &lv_font_montserrat_10 },
	{ "montserrat", NULL, 	12, &lv_font_montserrat_12 },
	{ "montserrat", NULL, 	14, &lv_font_montserrat_14 },
	{ "montserrat", NULL, 	16, &lv_font_montserrat_16 },
	{ "montserrat", NULL, 	18, &lv_font_montserrat_18 },
	{ "montserrat", NULL, 	20, &lv_font_montserrat_20 },
	{ "montserrat", NULL, 	22, &lv_font_montserrat_22 },
	{ "montserrat", NULL, 	24, &lv_font_montserrat_24 },
	{ "montserrat", NULL, 	36, &lv_font_montserrat_36 },
	{ "roboto", 	"bold",	20, &roboto_20 },
	{ "roboto", 	"bold",	24, &roboto_24 },
	{ NULL, NULL, 0, NULL }
};

struct symbol_src {
	const char	*ss_atom;
	const char	*ss_value;
};
const static struct symbol_src symbol_srcs[] = {
	{ "bullet",		LV_SYMBOL_BULLET },
	{ "audio",		LV_SYMBOL_AUDIO },
	{ "video",		LV_SYMBOL_VIDEO },
	{ "list",		LV_SYMBOL_LIST },
	{ "ok",			LV_SYMBOL_OK },
	{ "close",		LV_SYMBOL_CLOSE },
	{ "power",		LV_SYMBOL_POWER },
	{ "settings",		LV_SYMBOL_SETTINGS },
	{ "home",		LV_SYMBOL_HOME },
	{ "download",		LV_SYMBOL_DOWNLOAD },
	{ "drive",		LV_SYMBOL_DRIVE },
	{ "refresh",		LV_SYMBOL_REFRESH },
	{ "mute",		LV_SYMBOL_MUTE },
	{ "volume_mid",		LV_SYMBOL_VOLUME_MID },
	{ "volume_max",		LV_SYMBOL_VOLUME_MAX },
	{ "image",		LV_SYMBOL_IMAGE },
	{ "tint",		LV_SYMBOL_TINT },
	{ "prev",		LV_SYMBOL_PREV },
	{ "play",		LV_SYMBOL_PLAY },
	{ "pause",		LV_SYMBOL_PAUSE },
	{ "stop",		LV_SYMBOL_STOP },
	{ "next",		LV_SYMBOL_NEXT },
	{ "eject",		LV_SYMBOL_EJECT },
	{ "left",		LV_SYMBOL_LEFT },
	{ "right",		LV_SYMBOL_RIGHT },
	{ "plus",		LV_SYMBOL_PLUS },
	{ "minus",		LV_SYMBOL_MINUS },
	{ "eye_open",		LV_SYMBOL_EYE_OPEN },
	{ "eye_close",		LV_SYMBOL_EYE_CLOSE },
	{ "warning",		LV_SYMBOL_WARNING },
	{ "shuffle",		LV_SYMBOL_SHUFFLE },
	{ "up",			LV_SYMBOL_UP },
	{ "down",		LV_SYMBOL_DOWN },
	{ "loop",		LV_SYMBOL_LOOP },
	{ "directory",		LV_SYMBOL_DIRECTORY },
	{ "upload",		LV_SYMBOL_UPLOAD },
	{ "call",		LV_SYMBOL_CALL },
	{ "cut",		LV_SYMBOL_CUT },
	{ "copy",		LV_SYMBOL_COPY },
	{ "save",		LV_SYMBOL_SAVE },
	{ "bars",		LV_SYMBOL_BARS },
	{ "envelope",		LV_SYMBOL_ENVELOPE },
	{ "charge",		LV_SYMBOL_CHARGE },
	{ "paste",		LV_SYMBOL_PASTE },
	{ "bell",		LV_SYMBOL_BELL },
	{ "keyboard",		LV_SYMBOL_KEYBOARD },
	{ "gps",		LV_SYMBOL_GPS },
	{ "file",		LV_SYMBOL_FILE },
	{ "wifi",		LV_SYMBOL_WIFI },
	{ "battery_full",	LV_SYMBOL_BATTERY_FULL },
	{ "battery_3",		LV_SYMBOL_BATTERY_3 },
	{ "battery_2",		LV_SYMBOL_BATTERY_2 },
	{ "battery_1",		LV_SYMBOL_BATTERY_1 },
	{ "battery_empty",	LV_SYMBOL_BATTERY_EMPTY },
	{ "usb",		LV_SYMBOL_USB },
	{ "bluetooth",		LV_SYMBOL_BLUETOOTH },
	{ "trash",		LV_SYMBOL_TRASH },
	{ "edit",		LV_SYMBOL_EDIT },
	{ "backspace",		LV_SYMBOL_BACKSPACE },
	{ "sd_card",		LV_SYMBOL_SD_CARD },
	{ "new_line",		LV_SYMBOL_NEW_LINE },
	{ "dummy",		LV_SYMBOL_DUMMY },
	{ NULL, 0 }
};

struct class_spec {
	const char		*cs_name;
	const lv_obj_class_t	*cs_class;
};
const static struct class_spec class_specs[] = {
	{ "lv_animimg",			&lv_animimg_class },
	{ "lv_arc",			&lv_arc_class },
	{ "lv_bar",			&lv_bar_class },
	{ "lv_btn",			&lv_btn_class },
	{ "lv_btnmatrix",		&lv_btnmatrix_class },
	{ "lv_calendar",		&lv_calendar_class },
	{ "lv_calendar_header_arrow",	&lv_calendar_header_arrow_class },
	{ "lv_calendar_header_dropdown",&lv_calendar_header_dropdown_class },
	{ "lv_canvas",			&lv_canvas_class },
	{ "lv_chart",			&lv_chart_class },
	{ "lv_checkbox",		&lv_checkbox_class },
	{ "lv_colorwheel",		&lv_colorwheel_class },
	{ "lv_dropdown",		&lv_dropdown_class },
	{ "lv_dropdownlist",		&lv_dropdownlist_class },
	{ "lv_img",			&lv_img_class },
	{ "lv_imgbtn",			&lv_imgbtn_class },
	{ "lv_keyboard",		&lv_keyboard_class },
	{ "lv_label",			&lv_label_class },
	{ "lv_led",			&lv_led_class },
	{ "lv_line",			&lv_line_class },
	{ "lv_list_btn",		&lv_list_btn_class },
	{ "lv_list",			&lv_list_class },
	{ "lv_list_text",		&lv_list_text_class },
	{ "lv_menu",			&lv_menu_class },
	{ "lv_menu_cont",		&lv_menu_cont_class },
	{ "lv_menu_main_cont",		&lv_menu_main_cont_class },
	{ "lv_menu_main_header_cont",	&lv_menu_main_header_cont_class },
	{ "lv_menu_page",		&lv_menu_page_class },
	{ "lv_menu_section",		&lv_menu_section_class },
	{ "lv_menu_separator",		&lv_menu_separator_class },
	{ "lv_menu_sidebar_cont",	&lv_menu_sidebar_cont_class },
	{ "lv_menu_sidebar_header_cont",&lv_menu_sidebar_header_cont_class },
	{ "lv_meter",			&lv_meter_class },
	{ "lv_msgbox_backdrop",		&lv_msgbox_backdrop_class },
	{ "lv_msgbox",			&lv_msgbox_class },
	{ "lv_msgbox_content",		&lv_msgbox_content_class },
	{ "lv_obj",			&lv_obj_class },
	{ "lv_roller",			&lv_roller_class },
	{ "lv_slider",			&lv_slider_class },
	{ "lv_spangroup",		&lv_spangroup_class },
	{ "lv_spinbox",			&lv_spinbox_class },
	{ "lv_spinner",			&lv_spinner_class },
	{ "lv_switch",			&lv_switch_class },
	{ "lv_table",			&lv_table_class },
	{ "lv_tabview",			&lv_tabview_class },
	{ "lv_textarea",		&lv_textarea_class },
	{ "lv_tileview",		&lv_tileview_class },
	{ "lv_tileview_tile",		&lv_tileview_tile_class },
	{ "lv_win",			&lv_win_class },
	{ NULL, NULL }
};
