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

-module(lv_style).

-export([
    create/1,
    set_layout/2,
    set_flex_flow/2,
    set_flex_grow/2,
    set_flex_align/4,
    set_width/2,
    set_min_width/2,
    set_max_width/2,
    set_height/2,
    set_min_height/2,
    set_max_height/2,
    set_x/2,
    set_y/2,
    set_align/2,
    set_radius/2,
    set_pad_top/2,
    set_pad_bottom/2,
    set_pad_left/2,
    set_pad_right/2,
    set_pad_row/2,
    set_pad_column/2,
    set_base_dir/2,
    set_clip_corner/2,
    set_bg_color/2,
    set_bg_opa/2,
    set_bg_grad_color/2,
    set_bg_grad_dir/2,
    set_bg_main_stop/2,
    set_bg_grad_stop/2,
    set_bg_img_opa/2,
    set_bg_img_recolor/2,
    set_bg_img_recolor_opa/2,
    set_bg_img_tiled/2,
    set_border_color/2,
    set_border_opa/2,
    set_border_width/2,
    set_border_side/2,
    set_border_post/2,
    set_outline_width/2,
    set_outline_color/2,
    set_outline_opa/2,
    set_outline_pad/2,
    set_shadow_width/2,
    set_shadow_ofs_x/2,
    set_shadow_ofs_y/2,
    set_shadow_spread/2,
    set_shadow_color/2,
    set_shadow_opa/2,
    set_img_opa/2,
    set_img_recolor/2,
    set_img_recolor_opa/2,
    set_line_width/2,
    set_line_dash_width/2,
    set_line_dash_gap/2,
    set_line_rounded/2,
    set_line_color/2,
    set_line_opa/2,
    set_arc_width/2,
    set_arc_rounded/2,
    set_arc_color/2,
    set_arc_opa/2,
    set_text_color/2,
    set_text_opa/2,
    set_text_letter_space/2,
    set_text_line_space/2,
    set_text_decor/2,
    set_text_align/2,
    set_opa/2,
    set_color_filter_opa/2,
    set_anim_time/2,
    set_anim_speed/2,
    set_blend_mode/2,
    set_transform_width/2,
    set_transform_height/2,
    set_translate_x/2,
    set_translate_y/2,
    set_transform_zoom/2,
    set_transform_angle/2,
    set_transform_pivot_x/2,
    set_transform_pivot_y/2,
    set_text_font/2
    ]).

-export_type([
    layout/0, flex_flow/0, flex_align/0,
    border_side/0, text_align/0, text_decor/0, blend_mode/0
    ]).

-include("async_wrappers.hrl").

-type layout() :: flex | grid.
-type flex_flow() :: row | column | row_wrap | row_reverse | row_wrap_reverse |
    column_wrap | column_reverse | column_wrap_reverse.
-type flex_align() :: start | 'end' | center | space_evenly | space_around |
    space_between.
-type text_align() :: auto | left | center | right.
-type text_decor() :: underline | strikethrough.
-type border_side() :: none | bottom | top | left | right | full.
-type blend_mode() :: normal | additive | subtractive | multiply | replace.

-spec create(lv:instance()) -> {ok, lv:style()} | lv:error().
create(Inst) ->
    ?async_wrapper(style_create, Inst).

-spec set_layout(lv:style(), layout()) -> ok | lv:error().
set_layout(Style, Layout) ->
    ?async_void_wrapper(style_set_layout, Style, Layout).

%% @doc Sets the 'flex_flow' style property.
%% @see flex_flow()
-spec set_flex_flow(lv:style(), flex_flow()) -> ok | lv:error().
set_flex_flow(Style, Flow) ->
    ?async_void_wrapper(style_set_flex_flow, Style, Flow).

%% @doc Sets the 'flex_grow' style property.
-spec set_flex_grow(lv:style(), integer()) -> ok | lv:error().
set_flex_grow(Style, Grow) ->
    ?async_void_wrapper(style_set_flex_grow, Style, Grow).

%% @doc Sets the 'flex_align' style property.
%% @see flex_align()
-spec set_flex_align(lv:style(), flex_align(), flex_align(), flex_align()) ->
    ok | lv:error().
set_flex_align(Style, Main, Cross, Track) ->
    ?async_void_wrapper(style_set_flex_align, Style, Main, Cross, Track).

%% @doc Sets the 'width' style property.
%% @see lv:coord()
-spec set_width(lv:style(), lv:coord()) -> ok | lv:error().
set_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, width, Value).
%% @doc Sets the 'min_width' style property.
%% @see lv:coord()
-spec set_min_width(lv:style(), lv:coord()) -> ok | lv:error().
set_min_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, min_width, Value).
%% @doc Sets the 'max_width' style property.
%% @see lv:coord()
-spec set_max_width(lv:style(), lv:coord()) -> ok | lv:error().
set_max_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, max_width, Value).
%% @doc Sets the 'height' style property.
%% @see lv:coord()
-spec set_height(lv:style(), lv:coord()) -> ok | lv:error().
set_height(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, height, Value).
%% @doc Sets the 'min_height' style property.
%% @see lv:coord()
-spec set_min_height(lv:style(), lv:coord()) -> ok | lv:error().
set_min_height(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, min_height, Value).
%% @doc Sets the 'max_height' style property.
%% @see lv:coord()
-spec set_max_height(lv:style(), lv:coord()) -> ok | lv:error().
set_max_height(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, max_height, Value).
%% @doc Sets the 'x' style property.
%% @see lv:coord()
-spec set_x(lv:style(), lv:coord()) -> ok | lv:error().
set_x(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, x, Value).
%% @doc Sets the 'y' style property.
%% @see lv:coord()
-spec set_y(lv:style(), lv:coord()) -> ok | lv:error().
set_y(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, y, Value).
%% @doc Sets the 'align' style property.
%% @see lv_obj:align_spec()
-spec set_align(lv:style(), lv_obj:align_spec()) -> ok | lv:error().
set_align(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, align, Value).
%% @doc Sets the 'radius' style property.
%% @see lv:coord()
-spec set_radius(lv:style(), lv:coord()) -> ok | lv:error().
set_radius(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, radius, Value).
%% @doc Sets the 'pad_top' style property.
%% @see lv:coord()
-spec set_pad_top(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_top(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_top, Value).
%% @doc Sets the 'pad_bottom' style property.
%% @see lv:coord()
-spec set_pad_bottom(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_bottom(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_bottom, Value).
%% @doc Sets the 'pad_left' style property.
%% @see lv:coord()
-spec set_pad_left(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_left(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_left, Value).
%% @doc Sets the 'pad_right' style property.
%% @see lv:coord()
-spec set_pad_right(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_right(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_right, Value).
%% @doc Sets the 'pad_row' style property.
%% @see lv:coord()
-spec set_pad_row(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_row(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_row, Value).
%% @doc Sets the 'pad_column' style property.
%% @see lv:coord()
-spec set_pad_column(lv:style(), lv:coord()) -> ok | lv:error().
set_pad_column(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, pad_column, Value).
%% @doc Sets the 'base_dir' style property.
%% @see lv_obj:dir_spec()
-spec set_base_dir(lv:style(), lv_obj:dir_spec()) -> ok | lv:error().
set_base_dir(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, base_dir, Value).
%% @doc Sets the 'clip_corner' style property.
-spec set_clip_corner(lv:style(), boolean()) -> ok | lv:error().
set_clip_corner(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, clip_corner, Value).
%% @doc Sets the 'bg_color' style property.
%% @see lv:color()
-spec set_bg_color(lv:style(), lv:color()) -> ok | lv:error().
set_bg_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_color, Value).
%% @doc Sets the 'bg_opa' style property.
%% @see lv_color:opacity()
-spec set_bg_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_bg_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_opa, round(Value * 255)).
%% @doc Sets the 'bg_grad_color' style property.
%% @see lv:color()
-spec set_bg_grad_color(lv:style(), lv:color()) -> ok | lv:error().
set_bg_grad_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_grad_color, Value).
%% @doc Sets the 'bg_grad_dir' style property.
%% @see lv_color:grad_dir()
-spec set_bg_grad_dir(lv:style(), lv_color:grad_dir()) -> ok | lv:error().
set_bg_grad_dir(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_grad_dir, Value).
%% @doc Sets the 'bg_main_stop' style property.
%% @see lv:coord()
-spec set_bg_main_stop(lv:style(), lv:coord()) -> ok | lv:error().
set_bg_main_stop(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_main_stop, Value).
%% @doc Sets the 'bg_grad_stop' style property.
%% @see lv:coord()
-spec set_bg_grad_stop(lv:style(), lv:coord()) -> ok | lv:error().
set_bg_grad_stop(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_grad_stop, Value).
%% @doc Sets the 'bg_img_opa' style property.
%% @see lv_color:opacity()
-spec set_bg_img_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_bg_img_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_img_opa, round(Value * 255)).
%% @doc Sets the 'bg_img_recolor' style property.
%% @see lv:color()
-spec set_bg_img_recolor(lv:style(), lv:color()) -> ok | lv:error().
set_bg_img_recolor(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_img_recolor, Value).
%% @doc Sets the 'bg_img_recolor_opa' style property.
%% @see lv_color:opacity()
-spec set_bg_img_recolor_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_bg_img_recolor_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_img_recolor_opa, round(Value * 255)).
%% @doc Sets the 'bg_img_tiled' style property.
-spec set_bg_img_tiled(lv:style(), boolean()) -> ok | lv:error().
set_bg_img_tiled(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, bg_img_tiled, Value).
%% @doc Sets the 'border_color' style property.
%% @see lv:color()
-spec set_border_color(lv:style(), lv:color()) -> ok | lv:error().
set_border_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, border_color, Value).
%% @doc Sets the 'border_opa' style property.
%% @see lv_color:opacity()
-spec set_border_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_border_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, border_opa, round(Value * 255)).
%% @doc Sets the 'border_width' style property.
%% @see lv:coord()
-spec set_border_width(lv:style(), lv:coord()) -> ok | lv:error().
set_border_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, border_width, Value).
%% @doc Sets the 'border_side' style property.
%% @see lv_style:border_side()
-spec set_border_side(lv:style(), lv:flags(lv_style:border_side())) -> ok | lv:error().
set_border_side(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, border_side, Value).
%% @doc Sets the 'border_post' style property.
-spec set_border_post(lv:style(), boolean()) -> ok | lv:error().
set_border_post(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, border_post, Value).
%% @doc Sets the 'outline_width' style property.
%% @see lv:coord()
-spec set_outline_width(lv:style(), lv:coord()) -> ok | lv:error().
set_outline_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, outline_width, Value).
%% @doc Sets the 'outline_color' style property.
%% @see lv:color()
-spec set_outline_color(lv:style(), lv:color()) -> ok | lv:error().
set_outline_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, outline_color, Value).
%% @doc Sets the 'outline_opa' style property.
%% @see lv_color:opacity()
-spec set_outline_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_outline_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, outline_opa, round(Value * 255)).
%% @doc Sets the 'outline_pad' style property.
%% @see lv:coord()
-spec set_outline_pad(lv:style(), lv:coord()) -> ok | lv:error().
set_outline_pad(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, outline_pad, Value).
%% @doc Sets the 'shadow_width' style property.
%% @see lv:coord()
-spec set_shadow_width(lv:style(), lv:coord()) -> ok | lv:error().
set_shadow_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_width, Value).
%% @doc Sets the 'shadow_ofs_x' style property.
%% @see lv:coord()
-spec set_shadow_ofs_x(lv:style(), lv:coord()) -> ok | lv:error().
set_shadow_ofs_x(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_ofs_x, Value).
%% @doc Sets the 'shadow_ofs_y' style property.
%% @see lv:coord()
-spec set_shadow_ofs_y(lv:style(), lv:coord()) -> ok | lv:error().
set_shadow_ofs_y(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_ofs_y, Value).
%% @doc Sets the 'shadow_spread' style property.
%% @see lv:coord()
-spec set_shadow_spread(lv:style(), lv:coord()) -> ok | lv:error().
set_shadow_spread(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_spread, Value).
%% @doc Sets the 'shadow_color' style property.
%% @see lv:color()
-spec set_shadow_color(lv:style(), lv:color()) -> ok | lv:error().
set_shadow_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_color, Value).
%% @doc Sets the 'shadow_opa' style property.
%% @see lv_color:opacity()
-spec set_shadow_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_shadow_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, shadow_opa, round(Value * 255)).
%% @doc Sets the 'img_opa' style property.
%% @see lv_color:opacity()
-spec set_img_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_img_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, img_opa, round(Value * 255)).
%% @doc Sets the 'img_recolor' style property.
%% @see lv:color()
-spec set_img_recolor(lv:style(), lv:color()) -> ok | lv:error().
set_img_recolor(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, img_recolor, Value).
%% @doc Sets the 'img_recolor_opa' style property.
%% @see lv_color:opacity()
-spec set_img_recolor_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_img_recolor_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, img_recolor_opa, round(Value * 255)).
%% @doc Sets the 'line_width' style property.
%% @see lv:coord()
-spec set_line_width(lv:style(), lv:coord()) -> ok | lv:error().
set_line_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_width, Value).
%% @doc Sets the 'line_dash_width' style property.
%% @see lv:coord()
-spec set_line_dash_width(lv:style(), lv:coord()) -> ok | lv:error().
set_line_dash_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_dash_width, Value).
%% @doc Sets the 'line_dash_gap' style property.
%% @see lv:coord()
-spec set_line_dash_gap(lv:style(), lv:coord()) -> ok | lv:error().
set_line_dash_gap(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_dash_gap, Value).
%% @doc Sets the 'line_rounded' style property.
-spec set_line_rounded(lv:style(), boolean()) -> ok | lv:error().
set_line_rounded(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_rounded, Value).
%% @doc Sets the 'line_color' style property.
%% @see lv:color()
-spec set_line_color(lv:style(), lv:color()) -> ok | lv:error().
set_line_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_color, Value).
%% @doc Sets the 'line_opa' style property.
%% @see lv_color:opacity()
-spec set_line_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_line_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, line_opa, round(Value * 255)).
%% @doc Sets the 'arc_width' style property.
%% @see lv:coord()
-spec set_arc_width(lv:style(), lv:coord()) -> ok | lv:error().
set_arc_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, arc_width, Value).
%% @doc Sets the 'arc_rounded' style property.
-spec set_arc_rounded(lv:style(), boolean()) -> ok | lv:error().
set_arc_rounded(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, arc_rounded, Value).
%% @doc Sets the 'arc_color' style property.
%% @see lv:color()
-spec set_arc_color(lv:style(), lv:color()) -> ok | lv:error().
set_arc_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, arc_color, Value).
%% @doc Sets the 'arc_opa' style property.
%% @see lv_color:opacity()
-spec set_arc_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_arc_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, arc_opa, round(Value * 255)).
%% @doc Sets the 'text_color' style property.
%% @see lv:color()
-spec set_text_color(lv:style(), lv:color()) -> ok | lv:error().
set_text_color(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_color, Value).
%% @doc Sets the 'text_opa' style property.
%% @see lv_color:opacity()
-spec set_text_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_text_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_opa, round(Value * 255)).
%% @doc Sets the 'text_letter_space' style property.
%% @see lv:coord()
-spec set_text_letter_space(lv:style(), lv:coord()) -> ok | lv:error().
set_text_letter_space(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_letter_space, Value).
%% @doc Sets the 'text_line_space' style property.
%% @see lv:coord()
-spec set_text_line_space(lv:style(), lv:coord()) -> ok | lv:error().
set_text_line_space(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_line_space, Value).
%% @doc Sets the 'text_decor' style property.
%% @see lv_style:text_decor()
-spec set_text_decor(lv:style(), lv:flags(lv_style:text_decor())) -> ok | lv:error().
set_text_decor(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_decor, Value).
%% @doc Sets the 'text_align' style property.
%% @see lv_style:text_align()
-spec set_text_align(lv:style(), lv_style:text_align()) -> ok | lv:error().
set_text_align(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_align, Value).
%% @doc Sets the 'opa' style property.
%% @see lv_color:opacity()
-spec set_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, opa, round(Value * 255)).
%% @doc Sets the 'color_filter_opa' style property.
%% @see lv_color:opacity()
-spec set_color_filter_opa(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_color_filter_opa(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, color_filter_opa, round(Value * 255)).
%% @doc Sets the 'anim_time' style property.
-spec set_anim_time(lv:style(), integer()) -> ok | lv:error().
set_anim_time(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, anim_time, Value).
%% @doc Sets the 'anim_speed' style property.
-spec set_anim_speed(lv:style(), integer()) -> ok | lv:error().
set_anim_speed(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, anim_speed, Value).
%% @doc Sets the 'blend_mode' style property.
%% @see lv_style:blend_mode()
-spec set_blend_mode(lv:style(), lv_style:blend_mode()) -> ok | lv:error().
set_blend_mode(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, blend_mode, Value).
%% @doc Sets the 'transform_width' style property.
%% @see lv:coord()
-spec set_transform_width(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_width(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_width, Value).
%% @doc Sets the 'transform_height' style property.
%% @see lv:coord()
-spec set_transform_height(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_height(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_height, Value).
%% @doc Sets the 'translate_x' style property.
%% @see lv:coord()
-spec set_translate_x(lv:style(), lv:coord()) -> ok | lv:error().
set_translate_x(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, translate_x, Value).
%% @doc Sets the 'translate_y' style property.
%% @see lv:coord()
-spec set_translate_y(lv:style(), lv:coord()) -> ok | lv:error().
set_translate_y(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, translate_y, Value).
%% @doc Sets the 'transform_zoom' style property.
%% @see lv:coord()
-spec set_transform_zoom(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_zoom(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_zoom, Value).
%% @doc Sets the 'transform_angle' style property.
%% @see lv:coord()
-spec set_transform_angle(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_angle(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_angle, Value).
%% @doc Sets the 'transform_pivot_x' style property.
%% @see lv:coord()
-spec set_transform_pivot_x(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_pivot_x(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_pivot_x, Value).
%% @doc Sets the 'transform_pivot_y' style property.
%% @see lv:coord()
-spec set_transform_pivot_y(lv:style(), lv:coord()) -> ok | lv:error().
set_transform_pivot_y(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, transform_pivot_y, Value).
%% @doc Sets the 'text_font' style property.
%% @see lv:font()
-spec set_text_font(lv:style(), lv:font()) -> ok | lv:error().
set_text_font(Style, Value) -> ?async_void_wrapper(style_set_prop, Style, text_font, Value).
