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

-module(lv_slider).

-compile(export_all).
-compile(nowarn_export_all).

-type value() :: integer().
-type mode() :: normal | symmetrical | range.

-include("async_wrappers.hrl").

-spec create(lv:object()) -> {ok, lv:slider()} | lv:error().
create(Parent) ->
    ?async_wrapper(slider_create, Parent).

% These functions are all inlines in C that call the lv_bar_* versions
% We can't invoke inlines via the NIF interface, so just map it here.

-spec set_value(lv:slider(), value(), lv_anim:enable()) -> ok | lv:error().
set_value(Bar, Val, AnimEna) ->
    ?async_void_wrapper(bar_set_value, Bar, Val, AnimEna).

-spec set_start_value(lv:slider(), value(), lv_anim:enable()) -> ok | lv:error().
set_start_value(Bar, Val, AnimEna) ->
    ?async_void_wrapper(bar_set_start_value, Bar, Val, AnimEna).

-spec set_range(lv:slider(), value(), value()) -> ok | lv:error().
set_range(Bar, LowVal, HighVal) ->
    ?async_void_wrapper(bar_set_range, Bar, LowVal, HighVal).

-spec set_mode(lv:slider(), mode()) -> ok | lv:error().
set_mode(Bar, Mode) ->
    ?async_void_wrapper(bar_set_mode, Bar, Mode).

-spec get_value(lv:slider()) -> {ok, value()} | lv:error().
get_value(Bar) ->
    ?async_wrapper(bar_get_value, Bar).
