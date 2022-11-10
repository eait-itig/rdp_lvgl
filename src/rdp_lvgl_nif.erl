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

%% @private
-module(rdp_lvgl_nif).

-on_load(init/0).

-export([
    setup_instance/1,
    disp_set_bg_color/2,
    flush_done/1,
    obj_create/2,
    scr_load/2,
    spinner_create/3,
    obj_center/1,
    send_pointer_event/3,
    send_key_event/3,
    send_text_event/2
    ]).

try_paths([Last], BaseName) ->
    filename:join([Last, BaseName]);
try_paths([Path | Next], BaseName) ->
    case filelib:is_dir(Path) of
        true ->
            WCard = filename:join([Path, "{lib,}" ++ BaseName ++ ".*"]),
            case filelib:wildcard(WCard) of
                [] -> try_paths(Next, BaseName);
                _ -> filename:join([Path, BaseName])
            end;
        false -> try_paths(Next, BaseName)
    end.

init() ->
    Paths0 = [
        filename:join(["..", lib, pcsc, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(pcsc) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    SoName = try_paths(Paths1, "rdp_lvgl_nif"),
    erlang:load_nif(SoName, 0).

-type instance() :: reference().
-type buffer() :: reference().
-type object() :: reference().
-type event() :: reference().

-type msgref() :: reference().

-type group() :: object().

-type px() :: integer().
-type rect() :: {X1 :: px(), Y1 :: px(), X2 :: px(), Y2 :: px()}.
-type point() :: {X :: px(), Y :: px()}.
-type size() :: {Width :: px(), Height :: px()}.
-type pixeldata() :: iolist().
-type btn_state() :: pressed | released.

-type msec() :: integer().
-type degrees() :: integer().

-type color() :: {R :: integer(), G :: integer(), B :: integer()}.

-type chr() :: integer().
-type key() :: up | down | right | left | esc | del | backspace | enter |
    next | prev | home | 'end' | chr().

-type evtype() :: all | pressed | pressing | press_lost | short_clicked |
    long_pressed | long_pressed_repeat | clicked | released | scroll_begin |
    scroll_end | scroll | gesture | key | focused | defocused | leave |
    value_changed | insert | refresh | ready | cancel.

-spec setup_instance(size()) ->
    {ok, instance(), msgref()} | {error, term()}.
setup_instance(_Size) -> error(no_nif).

-type event_msg() ::
    {msgref(), event, evtype(), Target :: object(), CurTarget :: object()}.

-spec setup_event(object(), evtype()) ->
    {ok, event(), msgref()} | {error, term()}.
setup_event(_Obj, _Type) -> error(no_nif).

-type instance_msg() ::
    {msgref(), setup_done} |
    {msgref(), flush, rect(), pixeldata()} |
    {msgref(), flush_sync}.

-spec flush_done(instance()) -> ok.
flush_done(_Inst) -> error(no_nif).

-spec make_buffer(binary()) -> {ok, buffer()}.
make_buffer(_Data) -> error(no_nif).

-spec read_framebuffer(instance(), rect()) -> {ok, pixeldata()}.
read_framebuffer(_Inst, _Rect) -> error(no_nif).

-spec send_pointer_event(instance(), point(), btn_state()) -> ok.
send_pointer_event(_Inst, _Pt, _Btn) -> error(no_nif).

-spec send_key_event(instance(), key(), btn_state()) -> ok.
send_key_event(_Inst, _Key, _Btn) -> error(no_nif).

-spec send_text_event(instance(), buffer()) -> ok.
send_text_event(_Inst, _Buf) -> error(no_nif).

-type async_msg() ::
    {msgref(), error, integer(), string()} |
    {msgref(), ok, term()}.

-spec obj_create(instance(), Parent :: object() | none) ->
    {async, msgref()} | {error, term()}.
obj_create(_Inst, _Parent) -> error(no_nif).

-spec scr_load(instance(), object()) -> ok | {error, term()}.
scr_load(_Inst, _Screen) -> error(no_nif).

-spec disp_set_bg_color(instance(), color()) -> ok | {error, term()}.
disp_set_bg_color(_Inst, _Color) -> error(no_nif).

-spec spinner_create(object(), Time :: msec(), ArcLen :: degrees()) ->
    {ok, object()} | {error, term()}.
spinner_create(_Obj, _Time, _ArcLen) -> error(no_nif).

-spec obj_set_size(object(), size()) -> ok | {error, term()}.
obj_set_size(_Obj, _Size) -> error(no_nif).

-spec obj_center(object()) -> ok | {error, term()}.
obj_center(_Obj) -> error(no_nif).
