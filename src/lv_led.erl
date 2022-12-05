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

-module(lv_led).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-type brightness() :: integer().

-spec create(lv:object()) -> {ok, lv:led()} | lv:error().
create(Parent) ->
    ?async_wrapper(led_create, Parent).

-spec set_color(lv:led(), lv:color()) -> ok | lv:error().
set_color(Widget, Color) ->
    ?async_void_wrapper(led_set_color, Widget, Color).

-spec set_brightness(lv:led(), brightness()) -> ok | lv:error().
set_brightness(Widget, Bright) ->
    ?async_void_wrapper(led_set_brightness, Widget, Bright).

-spec on(lv:led()) -> ok | lv:error().
on(Widget) ->
    ?async_void_wrapper(led_on, Widget).

-spec off(lv:led()) -> ok | lv:error().
off(Widget) ->
    ?async_void_wrapper(led_off, Widget).

-spec toggle(lv:led()) -> ok | lv:error().
toggle(Widget) ->
    ?async_void_wrapper(led_toggle, Widget).

-spec get_brightness(lv:led()) -> {ok, brightness()} | lv:error().
get_brightness(Widget) ->
    ?async_wrapper(led_get_brightness, Widget).
