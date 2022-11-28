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

-module(lv_obj).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-export_type([
    flag/0, align_spec/0, dir_spec/0
    ]).

-type flag() :: hidden | clickable | click_focusable | checkable |
    scrollable | scroll_elastic | scroll_momentum | scroll_one |
    scroll_chain_hor | scroll_chain_ver | scroll_on_focus | scroll_with_arrow |
    snappable | press_lock | event_bubble | gesture_bubble | adv_hittest |
    ignore_layout | floating | overflow_visible | flex_in_new_track.

-type align_spec() :: out_top_left | out_top_mid | out_top_right |
    out_right_top | out_right_mid | out_right_bottom | out_bottom_right |
    out_bottom_mid | out_bottom_left | out_left_bottom | out_left_mid |
    out_left_top | top_left | top_mid | top_right | left_mid | center |
    right_mid | bottom_left | bottom_mid | bottom_right.

-type dir_spec() :: top | bottom | left | right | horizontal | vertical | all.

-spec add_flags(lv:object(), [flag()]) -> ok | lv:error().
add_flags(Obj, Flags) ->
    ?async_void_wrapper(obj_add_flags, Obj, Flags).

-spec clear_flags(lv:object(), [flag()]) -> ok | lv:error().
clear_flags(Obj, Flags) ->
    ?async_void_wrapper(obj_clear_flags, Obj, Flags).

-spec center(lv:object()) -> ok | lv:error().
center(Obj) ->
    ?async_void_wrapper(obj_center, Obj).

-spec create(lv:instance(), lv:object()) -> {ok, lv:object()} | lv:error().
create(Inst, Parent) ->
    ?async_wrapper(obj_create, Inst, Parent).

-spec align(lv:object(), align_spec(), lv:point()) -> ok | lv:error().
align(Obj, Spec, Offset) ->
    ?async_void_wrapper(obj_align, Obj, Spec, Offset).

-spec align(lv:object(), align_spec()) -> ok | lv:error().
align(Obj, Spec) ->
    ?async_void_wrapper(obj_align, Obj, Spec).

-spec align_to(lv:object(), lv:object(), align_spec(), lv:point()) -> ok | lv:error().
align_to(Obj, RefObj, Spec, Offset) ->
    ?async_void_wrapper(obj_align_to, Obj, RefObj, Spec, Offset).

-spec align_to(lv:object(), lv:object(), align_spec()) -> ok | lv:error().
align_to(Obj, RefObj, Spec) ->
    ?async_void_wrapper(obj_align_to, Obj, RefObj, Spec).

-spec set_size(lv:object(), lv:size()) -> ok | lv:error().
set_size(Obj, Size) ->
    ?async_void_wrapper(obj_set_size, Obj, Size).

-spec add_style(lv:object(), lv:style()) -> ok | lv:error().
add_style(Obj, Style) ->
    ?async_void_wrapper(obj_add_style, Obj, Style).

-spec get_size(lv:object()) -> {ok, lv:size()} | lv:error().
get_size(Obj) ->
    ?async_wrapper(obj_get_size, Obj).

-spec get_pos(lv:object()) -> {ok, lv:point()} | lv:error().
get_pos(Obj) ->
    ?async_wrapper(obj_get_pos, Obj).
