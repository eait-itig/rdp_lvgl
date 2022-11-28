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

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    layout/0, flex_flow/0, flex_align/0
    ]).

-include("async_wrappers.hrl").

-type px() :: integer().
-type layout() :: flex | grid.
-type flex_flow() :: row | column | row_wrap | row_reverse | row_wrap_reverse |
    column_wrap | column_reverse | column_wrap_reverse.
-type flex_align() :: start | 'end' | center | space_evenly | space_around |
    space_between.
-type text_align() :: auto | left | center | right.
-type border_side() :: none | bottom | top | left | right | full.

-spec create(lv:instance()) -> {ok, lv:style()} | lv:error().
create(Inst) ->
    ?async_wrapper(style_create, Inst).

-spec set_layout(lv:style(), layout()) -> ok | lv:error().
set_layout(Style, Layout) ->
    ?async_void_wrapper(style_set_layout, Style, Layout).

-spec set_flex_flow(lv:style(), flex_flow()) -> ok | lv:error().
set_flex_flow(Style, Flow) ->
    ?async_void_wrapper(style_set_flex_flow, Style, Flow).

-spec set_flex_grow(lv:style(), integer()) -> ok | lv:error().
set_flex_grow(Style, Grow) ->
    ?async_void_wrapper(style_set_flex_grow, Style, Grow).

-spec set_flex_align(lv:style(), flex_align(), flex_align(), flex_align()) ->
    ok | lv:error().
set_flex_align(Style, Main, Cross, Track) ->
    ?async_void_wrapper(style_set_flex_align, Style, Main, Cross, Track).

-spec set_bg_opacity(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_bg_opacity(Style, Opacity) ->
    ?async_void_wrapper(style_set_prop, Style, bg_opa, Opacity).

-spec set_border_opacity(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_border_opacity(Style, Opacity) ->
    ?async_void_wrapper(style_set_prop, Style, border_opa, Opacity).

-spec set_outline_opacity(lv:style(), lv_color:opacity()) -> ok | lv:error().
set_outline_opacity(Style, Opacity) ->
    ?async_void_wrapper(style_set_prop, Style, outline_opa, Opacity).

-spec set_text_color(lv:style(), lv:color()) -> ok | lv:error().
set_text_color(Style, Color) ->
    ?async_void_wrapper(style_set_prop, Style, text_color, Color).

-spec set_border_side(lv:style(), border_side() | [border_side()]) ->
    ok | lv:error().
set_border_side(Style, Sides) ->
    ?async_void_wrapper(style_set_prop, Style, border_side, Sides).

-spec set_border_color(lv:style(), lv:color()) -> ok | lv:error().
set_text_color(Style, Color) ->
    ?async_void_wrapper(style_set_prop, Style, border_color, Color).

-spec set_border_post(lv:style(), boolean()) -> ok | lv:error().
set_border_post(Style, State) ->
    ?async_void_wrapper(style_set_prop, Style, border_post, State).

-spec set_radius(lv:style(), px()) -> ok | lv:error().
set_radius(Style, Radius) ->
    ?async_void_wrapper(style_set_prop, Style, radius, Radius).

-spec set_pad_top(lv:style(), px()) -> ok | lv:error().
set_pad_top(Style, Radius) ->
    ?async_void_wrapper(style_set_prop, Style, pad_top, Radius).

-spec set_pad_bottom(lv:style(), px()) -> ok | lv:error().
set_pad_bottom(Style, Radius) ->
    ?async_void_wrapper(style_set_prop, Style, pad_bottom, Radius).

-spec set_pad_left(lv:style(), px()) -> ok | lv:error().
set_pad_left(Style, Radius) ->
    ?async_void_wrapper(style_set_prop, Style, pad_left, Radius).

-spec set_pad_right(lv:style(), px()) -> ok | lv:error().
set_pad_right(Style, Radius) ->
    ?async_void_wrapper(style_set_prop, Style, pad_right, Radius).

-spec set_text_align(lv:style(), text_align()) -> ok | lv:error().
set_text_align(Style, Align) ->
    ?async_void_wrapper(style_set_prop, Style, text_align, Align).
