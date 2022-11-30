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

-module(lv_dropdown).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-spec create(lv:object()) -> {ok, lv:dropdown()} | lv:error().
create(Parent) ->
    ?async_wrapper(dropdown_create, Parent).

-spec set_options(lv:dropdown(), [string()]) -> ok | lv:error().
set_options(Dropdown, OptsList) ->
    ?async_void_wrapper(dropdown_set_options, Dropdown,
        lists:join($\n, OptsList)).

-type index() :: integer().

-spec add_option(lv:dropdown(), string(), index()) -> ok | lv:error().
add_option(Dropdown, Text, AtIndex) ->
    ?async_void_wrapper(dropdown_add_option, Dropdown, Text, AtIndex).

-spec get_selected(lv:dropdown()) -> {ok, index()} | lv:error().
get_selected(Dropdown) ->
    ?async_wrapper(dropdown_get_selected, Dropdown).

-spec get_selected_str(lv:dropdown()) -> {ok, string()} | lv:error().
get_selected_str(Dropdown) ->
    ?async_wrapper(dropdown_get_selected_str, Dropdown).

-spec set_selected(lv:dropdown(), index()) -> ok | lv:error().
set_selected(Dropdown, Index) ->
    ?async_void_wrapper(dropdown_set_selected, Dropdown, Index).

-spec clear_options(lv:dropdown()) -> ok | lv:error().
clear_options(Dropdown) ->
    ?async_void_wrapper(dropdown_clear_options, Dropdown).
