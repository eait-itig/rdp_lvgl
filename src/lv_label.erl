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

-module(lv_label).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-spec create(lv:object()) -> {ok, lv:label()} | lv:error().
create(Parent) ->
    ?async_wrapper(label_create, Parent).

-spec set_text(lv:label(), string()) -> ok | lv:error().
set_text(Lbl, Text) ->
    ?async_void_wrapper(label_set_text, Lbl, Text).

-spec get_text(lv:label()) -> {ok, binary()} | lv:error().
get_text(Lbl) ->
    ?async_wrapper(label_get_text, Lbl).

-type index() :: integer().

-spec set_text_sel_start(lv:label(), index()) -> ok | lv:error().
set_text_sel_start(Label, Index) ->
    ?async_void_wrapper(label_set_text_sel_start, Label, Index).

-spec set_text_sel_end(lv:label(), index()) -> ok | lv:error().
set_text_sel_end(Label, Index) ->
    ?async_void_wrapper(label_set_text_sel_end, Label, Index).

-spec set_text_sel_off(lv:label()) -> ok | lv:error().
set_text_sel_off(Label) ->
    ?async_void_wrapper(label_set_text_sel_start, Label, 16#FFFF).
