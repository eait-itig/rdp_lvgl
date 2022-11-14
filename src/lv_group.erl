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

-module(lv_group).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-spec create(lv:instance()) -> {ok, lv:group()} | lv:error().
create(Inst) ->
    ?async_wrapper(group_create, Inst).

-spec add_obj(lv:group(), lv:object()) -> ok | lv:error().
add_obj(Group, Obj) ->
    ?async_void_wrapper(group_add_obj, Group, Obj).

-spec remove_obj(lv:group(), lv:object()) -> ok | lv:error().
remove_obj(Group, Obj) ->
    ?async_void_wrapper(group_remove_obj, Group, Obj).

-spec remove_all_objs(lv:group()) -> ok | lv:error().
remove_all_objs(Group) ->
    ?async_void_wrapper(group_remove_all_objs, Group).

-spec set_wrap(lv:group(), boolean()) -> ok | lv:error().
set_wrap(Group, State) ->
    ?async_void_wrapper(group_set_wrap, Group, State).

-spec focus_freeze(lv:group(), boolean()) -> ok | lv:error().
focus_freeze(Group, State) ->
    ?async_void_wrapper(group_focus_freeze, Group, State).

-spec get_focused(lv:group()) -> {ok, lv:object()} | lv:error().
get_focused(Group) ->
    ?async_wrapper(group_get_focused, Group).

-spec focus_obj(lv:object()) -> ok | lv:error().
focus_obj(Obj) ->
    ?async_wrapper(group_focus_obj, Obj).
