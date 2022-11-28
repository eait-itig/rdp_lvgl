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

#if !defined(_LVK_UTILS_H)
#define _LVK_UTILS_H

#include "erl_nif.h"
#include <lvgl.h>
#include <stdint.h>
#include "shm.h"

extern lv_font_t roboto_20;
extern lv_font_t roboto_24;

uint lvk_next_tile(const lv_area_t *area, lv_area_t *tile);

ERL_NIF_TERM lvk_tile_to_iolist(ErlNifEnv *env, struct fbuf *fb,
    lv_color_t *buf, void *rsrc, const lv_area_t *tile);

void lv_group_send_text(lv_group_t *group, const char *text);
void lv_indev_send_text(lv_indev_t *indev, const char *text);
void lv_disp_scr_load(lv_disp_t *disp, lv_obj_t *scr);
void lv_disp_scr_load_anim(lv_disp_t *disp, lv_obj_t *src,
    lv_scr_load_anim_t anim_type, uint32_t time, uint32_t delay,
    bool auto_del);
lv_obj_t *lv_disp_obj_create(lv_disp_t *disp, lv_obj_t *parent);
void lv_img_set_offset(lv_obj_t *obj, lv_point_t pt);
lv_style_t *lv_style_alloc(void);
void lv_style_set_flex_align(lv_style_t *style, lv_flex_align_t main_place,
    lv_flex_align_t cross_place, lv_flex_align_t track_cross_place);
lv_point_t lv_obj_get_size(lv_obj_t *obj);
lv_point_t lv_obj_get_pos(lv_obj_t *obj);
void lv_wheel_scroll_by(lv_disp_t *disp, lv_indev_t *mouse, int dy,
    lv_anim_enable_t anim);

#endif /* _LVK_UTILS_H */
