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

#include "lvkutils.h"

#define	TILE_XSIZE	120
#define TILE_YSIZE	64

uint
lvk_next_tile(const lv_area_t *area, lv_area_t *tile)
{
	if (tile->x1 == 0 && tile->y1 == 0 && tile->x2 == 0 && tile->y2 == 0) {
		tile->x1 = area->x1;
		tile->y1 = area->y1;
		tile->x2 = area->x1 + TILE_XSIZE - 1;
		tile->y2 = area->y1 + TILE_YSIZE - 1;
		if (tile->y2 > area->y2)
			tile->y2 = area->y2;
		if (tile->x2 > area->x2)
			tile->x2 = area->x2;
		return (1);
	}
	if (tile->x2 >= area->x2) {
		if (tile->y2 >= area->y2) {
			return (0);
		}
		tile->x1 = area->x1;
		tile->y1 += TILE_YSIZE;
		tile->y2 = tile->y1 + TILE_YSIZE - 1;
		if (tile->y2 > area->y2)
			tile->y2 = area->y2;
	} else {
		tile->x1 += TILE_XSIZE;
	}
	tile->x2 = tile->x1 + TILE_XSIZE - 1;
	if (tile->x2 > area->x2)
		tile->x2 = area->x2;
	return (1);
}

ERL_NIF_TERM
lvk_tile_to_iolist(ErlNifEnv *env, struct fbuf *fb, lv_color_t *buf,
    void *rsrc, const lv_area_t *tile)
{
	ERL_NIF_TERM list, *row;
	lv_color_t *p;
	uint nrow, ncol, i, y;
	nrow = tile->y2 - tile->y1 + 1;
	ncol = tile->x2 - tile->x1 + 1;
	row = alloca(nrow * sizeof (ERL_NIF_TERM));
	for (i = 0, y = tile->y1; i < nrow; ++i, ++y) {
		p = &buf[fb->fb_w * y + tile->x1];
		row[i] = enif_make_resource_binary(env, rsrc, p,
		    ncol * sizeof (lv_color_t));
	}
	list = enif_make_list_from_array(env, row, nrow);
	return (list);
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
lv_indev_send_text(lv_indev_t *indev, const char *text)
{
	lv_group_t *group;
	group = indev->group;
	if (group == NULL)
		return;
	lv_group_send_text(group, text);
}

void
lv_img_set_offset(lv_obj_t *obj, lv_point_t pt)
{
	lv_img_set_offset_x(obj, pt.x);
	lv_img_set_offset_y(obj, pt.y);
}

void
lv_disp_scr_load(lv_disp_t *disp, lv_obj_t *scr)
{
	lv_disp_set_default(disp);
	lv_scr_load(scr);
}

void
lv_disp_scr_load_anim(lv_disp_t *disp, lv_obj_t *scr,
    lv_scr_load_anim_t anim_type, uint32_t time, uint32_t delay,
    bool auto_del)
{
	lv_disp_set_default(disp);
	lv_scr_load_anim(scr, anim_type, time, delay, auto_del);
}

lv_obj_t *
lv_disp_obj_create(lv_disp_t *disp, lv_obj_t *parent)
{
	lv_disp_set_default(disp);
	return (lv_obj_create(parent));
}

lv_style_t *
lv_style_alloc(void)
{
	lv_style_t *sty = calloc(1, sizeof (*sty));
	assert(sty != NULL);
	lv_style_init(sty);
	return (sty);
}

void
lv_style_set_flex_align(lv_style_t *style, lv_flex_align_t main_place,
    lv_flex_align_t cross_place, lv_flex_align_t track_cross_place)
{
	lv_style_set_flex_main_place(style, main_place);
	lv_style_set_flex_cross_place(style, cross_place);
	lv_style_set_flex_track_place(style, track_cross_place);
	lv_style_set_layout(style, LV_LAYOUT_FLEX);
}

lv_point_t
lv_obj_get_size(lv_obj_t *obj)
{
	lv_point_t pt;
	pt = (lv_point_t){
		.x = lv_obj_get_self_width(obj),
		.y = lv_obj_get_self_height(obj)
	};
	return (pt);
}

lv_point_t
lv_obj_get_pos(lv_obj_t *obj)
{
	lv_point_t pt;
	pt = (lv_point_t){
		.x = lv_obj_get_x(obj),
		.y = lv_obj_get_y(obj)
	};
	return (pt);
}

void
lv_wheel_scroll_by(lv_disp_t *disp, lv_indev_t *mouse, int dy,
    lv_anim_enable_t anim)
{
	lv_point_t pt;
	lv_obj_t *target;

	lv_indev_get_point(mouse, &pt);

	target = lv_indev_search_obj(disp->act_scr, &pt);

	while (target != NULL &&
	    !lv_obj_has_flag(target, LV_OBJ_FLAG_SCROLLABLE)) {
		target = lv_obj_get_parent(target);
	}

	if (target == NULL)
		return;

	if (target != NULL)
		lv_obj_scroll_by_bounded(target, 0, dy, anim);
}
