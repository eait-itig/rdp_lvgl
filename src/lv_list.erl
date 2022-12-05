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

-module(lv_list).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-type text() :: lv:object().
-type btn() :: lv:object().

-spec create(lv:object()) -> {ok, lv:listview()} | lv:error().
create(Parent) ->
    ?async_wrapper(list_create, Parent).

-spec add_text(lv:listview(), string()) -> {ok, text()} | lv:error().
add_text(Widget, Text) ->
    ?async_wrapper(list_add_text, Widget, Text).

-spec add_btn(lv:listview(), lv_img:src(), string()) -> {ok, btn()} | lv:error().
add_btn(Widget, Icon, Text) ->
    ?async_wrapper(list_add_btn, Widget, Icon, Text).

-spec get_btn_text(lv:listview(), btn()) -> {ok, string()} | lv:error().
get_btn_text(Widget, Btn) ->
    ?async_wrapper(list_get_btn_text, Widget, Btn).
