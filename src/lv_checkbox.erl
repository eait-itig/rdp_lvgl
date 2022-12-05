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

-module(lv_checkbox).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-spec create(lv:object()) -> {ok, lv:checkbox()} | lv:error().
create(Parent) ->
    ?async_wrapper(checkbox_create, Parent).

-spec set_text(lv:checkbox(), string()) -> ok | lv:error().
set_text(Checkbox, Text) ->
    ?async_void_wrapper(checkbox_set_text, Checkbox, Text).

-spec get_text(lv:checkbox()) -> {ok, string()} | lv:error().
get_text(Checkbox) ->
    ?async_wrapper(checkbox_get_text, Checkbox).

-spec check(lv:checkbox()) -> ok | lv:error().
check(Checkbox) ->
    lv_obj:add_state(Checkbox, checked).

-spec uncheck(lv:checkbox()) -> ok | lv:error().
uncheck(Checkbox) ->
    lv_obj:clear_state(Checkbox, checked).

-spec is_checked(lv:checkbox()) -> {ok, boolean()} | lv:error().
is_checked(Checkbox) ->
    lv_obj:has_state(Checkbox, checked).
