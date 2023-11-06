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

-module(lv_indev).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    state/0, key/0
    ]).

-include("async_wrappers.hrl").

-type state() :: pressed | released.
-type charcode() :: integer().
-type key() :: up | down | right | left | esc | del | backspace | enter |
    next | prev | home | 'end' | charcode().
-type indev() :: keyboard | mouse.

-type clicks() :: integer().
%% A number of mouse wheel "clicks", can be negative (upwards direction) or
%% positive (downwards direction).

-spec set_mouse_cursor(lv:instance(), lv:object()) -> {ok, lv:object()} | lv:error().
set_mouse_cursor(Inst, Cursor) ->
    ?async_void_wrapper(set_mouse_cursor, Inst, Cursor).

-spec send_pointer_event(lv:instance(), lv:point(), state()) -> ok | lv:error().
send_pointer_event(Inst, Point, State) ->
    rdp_lvgl_nif:send_pointer_event(Inst, Point, State).

-spec send_wheel_event(lv:instance(), clicks()) -> ok | lv:error().
send_wheel_event(Inst, Clicks) ->
    rdp_lvgl_nif:send_wheel_event(Inst, Clicks).

-spec send_key_event(lv:instance(), key(), state()) -> ok | lv:error().
send_key_event(Inst, Key, State) ->
    rdp_lvgl_nif:send_key_event(Inst, Key, State).

-spec set_group(lv:instance(), indev(), lv:group()) -> ok | lv:error().
set_group(Inst, keyboard, Group) ->
    ?async_void_wrapper(set_kbd_group, Inst, Group).

-spec send_text(lv:instance(), string()) -> ok | lv:error().
send_text(Inst, Text) ->
    ?async_void_wrapper(send_text, Inst, Text).

-spec get_focused(lv:instance(), indev()) -> {ok, lv:object() | null} | lv:error().
get_focused(Inst, keyboard) ->
    ?async_wrapper(indev_get_focused, Inst).

-spec reset(lv:instance(), indev()) -> ok | lv:error().
reset(Inst, Device) ->
    reset(Inst, Device, null).

-spec reset(lv:instance(), indev(), lv:object() | null) -> ok | lv:error().
reset(Inst, keyboard, Obj) ->
    ?async_void_wrapper(kbd_reset, Inst, Obj);
reset(Inst, mouse, Obj) ->
    ?async_void_wrapper(mouse_reset, Inst, Obj).

-spec wait_release(lv:instance(), indev()) -> ok | lv:error().
wait_release(Inst, keyboard) ->
    ?async_void_wrapper(kbd_wait_release, Inst);
wait_release(Inst, mouse) ->
    ?async_void_wrapper(mouse_wait_release, Inst).
