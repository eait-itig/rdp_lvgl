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

-module(lv_btnmatrix).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-export_type([
    ctrl/0
    ]).

-spec create(lv:object()) -> {ok, lv:btnmatrix()} | lv:error().
create(Parent) ->
    ?async_wrapper(btnmatrix_create, Parent).

-spec set_map(lv:btnmatrix(), [string()]) -> ok | lv:error().
set_map(Widget, Map) ->
    ?async_void_wrapper(btnmatrix_set_map, Widget, Map).

-type ctrl() :: hidden | no_repeat | disabled | checkable | checked |
    click_trig | popover | recolor.
-type index() :: integer().

-spec set_btn_ctrl(lv:btnmatrix(), index(), lv:flags(ctrl())) -> ok | lv:error().
set_btn_ctrl(Widget, Index, Ctrl) ->
    ?async_void_wrapper(btnmatrix_set_btn_ctrl, Widget, Index, Ctrl).

-spec clear_btn_ctrl(lv:btnmatrix(), index(), lv:flags(ctrl())) -> ok | lv:error().
clear_btn_ctrl(Widget, Index, Ctrl) ->
    ?async_void_wrapper(btnmatrix_clear_btn_ctrl, Widget, Index, Ctrl).

-spec set_btn_ctrl_all(lv:btnmatrix(), lv:flags(ctrl())) -> ok | lv:error().
set_btn_ctrl_all(Widget, Ctrl) ->
    ?async_void_wrapper(btnmatrix_set_btn_ctrl_all, Widget, Ctrl).

-spec clear_btn_ctrl_all(lv:btnmatrix(), lv:flags(ctrl())) -> ok | lv:error().
clear_btn_ctrl_all(Widget, Ctrl) ->
    ?async_void_wrapper(btnmatrix_clear_btn_ctrl_all, Widget, Ctrl).

-spec set_selected_btn(lv:btnmatrix(), index()) -> ok | lv:error().
set_selected_btn(Widget, Index) ->
    ?async_void_wrapper(btnmatrix_set_selected_btn, Widget, Index).

-spec get_selected_btn(lv:btnmatrix()) -> {ok, index()} | lv:error().
get_selected_btn(Widget) ->
    ?async_wrapper(btnmatrix_get_selected_btn, Widget).

-spec get_btn_text(lv:btnmatrix(), index()) -> {ok, string()} | lv:error().
get_btn_text(Widget, Index) ->
    ?async_wrapper(btnmatrix_get_btn_text, Widget, Index).

-spec has_btn_ctrl(lv:btnmatrix(), index(), lv:flags(ctrl())) -> {ok, boolean()} | lv:error().
has_btn_ctrl(Widget, Index, Ctrl) ->
    ?async_wrapper(btnmatrix_has_btn_ctrl, Widget, Index, Ctrl).

-spec set_btn_width(lv:btnmatrix(), index(), integer()) -> ok | lv:error().
set_btn_width(Widget, Index, Width) ->
    ?async_void_wrapper(btnmatrix_set_btn_width, Widget, Index, Width).

-spec set_one_checked(lv:btnmatrix(), boolean()) -> ok | lv:error().
set_one_checked(Widget, State) ->
    ?async_void_wrapper(btnmatrix_set_one_checked, Widget, State).
