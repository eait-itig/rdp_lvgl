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

-module(lv_event).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    type/0
    ]).

-type type() :: all | pressed | pressing | press_lost | short_clicked |
    long_pressed | long_pressed_repeat | clicked | released | scroll_begin |
    scroll_end | scroll | gesture | key | focused | defocused | leave |
    value_changed | insert | refresh | ready | cancel.

-spec setup(lv:object(), type()) -> {ok, lv:event(), reference()} | lv:error().
setup(Obj, Filter) ->
    case rdp_lvgl_nif:setup_event(Obj, Filter) of
        {async, Event, MsgRef} ->
            receive
                {MsgRef, ok} -> {ok, Event, MsgRef};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end.

-spec setup(lv:object(), type(), term()) ->
    {ok, lv:event(), reference()} | lv:error().
setup(Obj, Filter, CustomMsg) ->
    case rdp_lvgl_nif:setup_event(Obj, Filter, CustomMsg) of
        {async, Event, MsgRef} ->
            receive
                {MsgRef, ok} -> {ok, Event, MsgRef};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end.
