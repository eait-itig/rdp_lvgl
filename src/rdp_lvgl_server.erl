%%
%% rdpproxy
%% remote desktop proxy
%%
%% Copyright 2022 Alex Wilson <alex@uq.edu.au>
%% The University of Queensland
%% All rights reserved.
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
%%

%% @doc Adapter module for writing RDP servers using LVGL.
-module(rdp_lvgl_server).
-behaviour(rdp_server).

-compile([{parse_transform, lager_transform}]).

-include_lib("rdp_proto/include/rdp_server.hrl").
-include_lib("rdp_proto/include/rdpdr.hrl").

-export([
    init/2,
    handle_connect/4,
    init_ui/2,
    handle_event/3,
    terminate/2,
    choose_format/3,
    handle_info/3
    ]).

-export([
    paste_proc/2
    ]).

-export([find_image_path/1]).

-type modkey() :: alt | ctrl | shift | capslock | numlock.

-record(?MODULE, {
    mod :: module(),
    modstate :: term(),
    inst :: lv:instance(),
    suppress = false :: boolean(),
    flushref :: undefined | reference(),
    frame = 1 :: integer(),
    sent_sof = false :: boolean(),
    keydown = none :: none | term(),
    modkeys = #{alt => false, ctrl => false, shift => false,
        capslock => false, numlock => false} :: #{modkey() => boolean()},
    clipmon :: undefined | reference()
    }).

init(Peer, {Mod, ModArgs}) ->
    case erlang:apply(Mod, init, [Peer | ModArgs]) of
        {ok, MS0} ->
            {ok, #?MODULE{mod = Mod, modstate = MS0}};
        Err ->
            Err
    end;
init(Peer, Mod) ->
    case Mod:init(Peer) of
        {ok, MS0} ->
            {ok, #?MODULE{mod = Mod, modstate = MS0}};
        Err ->
            Err
    end.

handle_connect(Cookie, Protocols, Srv, S0 = #?MODULE{mod = Mod, modstate = MS0}) ->
    case Mod:handle_connect(Cookie, Protocols, Srv, MS0) of
        {accept, MS1} ->
            {accept, S0#?MODULE{modstate = MS1}};
        {accept, AcceptOpts, MS1} ->
            {accept, AcceptOpts, S0#?MODULE{modstate = MS1}};
        {accept_raw, MS1} ->
            {accept_raw, S0#?MODULE{modstate = MS1}};
        {reject, _Reason, MS1} ->
            {reject, S0#?MODULE{modstate = MS1}};
        {reject, MS1} ->
            {reject, S0#?MODULE{modstate = MS1}};
        {stop, Reason, MS1} ->
            {stop, Reason, S0#?MODULE{modstate = MS1}}
    end.

choose_format(_Preferred, Supported, S = #?MODULE{}) ->
    lager:debug("using color format 16bpp out of ~p", [Supported]),
    {'16bpp', S}.

handle_info({R, flush, _, _}, _Srv, S = #?MODULE{flushref = R, suppress = true}) ->
    {ok, S};
handle_info({R, flush, {X1, Y1, X2, Y2}, PixData}, Srv, S = #?MODULE{flushref = R}) ->
    W = (X2 - X1) + 1,
    H = (Y2 - Y1) + 1,
    Surf = #ts_surface_set_bits{dest = {X1, Y1},
                                size = {W, H},
                                bpp = 16,
                                codec = 0,
                                data = lists:reverse(PixData)},
    #?MODULE{frame = FrameId, sent_sof = SentSOF} = S,
    SOF = #ts_surface_frame_marker{frame = FrameId, action = start},
    Surfs = case SentSOF of
        true -> [Surf];
        false -> [SOF, Surf]
    end,
    Update = #ts_update_surfaces{surfaces = Surfs},
    rdp_server:send_update(Srv, Update),
    {ok, S};

handle_info({R, flush_sync}, _Srv, S0 = #?MODULE{flushref = R, suppress = true}) ->
    #?MODULE{inst = Inst, frame = FrameId} = S0,
    flush_done(Inst),
    {ok, S0#?MODULE{frame = FrameId + 1, sent_sof = false}};
handle_info({R, flush_sync}, Srv, S0 = #?MODULE{flushref = R}) ->
    #?MODULE{inst = Inst, frame = FrameId} = S0,
    EOF = #ts_surface_frame_marker{frame = FrameId, action = finish},
    Update = #ts_update_surfaces{surfaces = [EOF]},
    rdp_server:send_update(Srv, Update),
    flush_done(Inst),
    {ok, S0#?MODULE{frame = FrameId + 1, sent_sof = false}};

handle_info({'DOWN', ClipMon, process, _Pid, Why}, _Srv, S0 = #?MODULE{clipmon = ClipMon}) ->
    case Why of
        normal -> ok;
        _ -> lager:debug("clipmon died: ~p", [Why])
    end,
    {ok, S0#?MODULE{clipmon = undefined}};

handle_info(Msg, Srv, S0 = #?MODULE{mod = Mod, modstate = MS0}) ->
    case Mod:handle_info(Msg, Srv, MS0) of
        {ok, MS1} ->
            {ok, S0#?MODULE{modstate = MS1}};
        {stop, Reason, MS1} ->
            {stop, Reason, S0#?MODULE{modstate = MS1}}
    end.

flush_done(Inst) ->
    erlang:garbage_collect(),
    case lv:flush_done(Inst) of
        ok -> ok;
        {error, busy} -> flush_done(Inst)
    end.

try_paths([Last], BaseName) ->
    filename:join([Last, BaseName]);
try_paths([Path | Next], BaseName) ->
    case filelib:is_dir(Path) of
        true ->
            WCard = filename:join([Path, BaseName]),
            case filelib:wildcard(WCard) of
                [] -> try_paths(Next, BaseName);
                _ -> filename:join([Path, BaseName])
            end;
        false -> try_paths(Next, BaseName)
    end.

find_image_path(Name) ->
    Paths0 = [
        filename:join(["..", lib, rdp_lvgl, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(rdp_lvgl) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    "A:" ++ try_paths(Paths1, Name).

setup_cursor(Inst) ->
    {ok, SysLayer} = lv_disp:get_layer_sys(Inst),
    {ok, Parent} = lv_img:create(SysLayer),
    ok = lv_obj:add_flags(Parent, [overflow_visible, ignore_layout]),
    {ok, Img} = lv_img:create(Parent),
    ok = lv_img:set_src(Img, find_image_path("mouse_cursor.png")),
    ok = lv_obj:align(Img, top_left, {-4, -4}),
    ok = lv_indev:set_mouse_cursor(Inst, Parent).

init_ui(Srv, S0 = #?MODULE{mod = Mod, modstate = MS0}) ->
    {W, H, 16} = rdp_server:get_canvas(Srv),
    {ok, Inst, MsgRef} = lv:setup({W, H}),
    setup_cursor(Inst),
    %ok = rdp_server:send_update(Srv, #fp_update_mouse{mode = hidden}),

    case Mod:init_ui({Srv, Inst}, MS0) of
        {ok, MS1} ->
            {ok, S0#?MODULE{inst = Inst,
                            flushref = MsgRef,
                            modstate = MS1}};
        {stop, Reason, MS1} ->
            {stop, Reason, S0#?MODULE{modstate = MS1}}
    end.

handle_paste(Srv, S = #?MODULE{clipmon = undefined, inst = Inst}) ->
    {_Pid, MonRef} = spawn_monitor(?MODULE, paste_proc, [Srv, Inst]),
    {ok, S#?MODULE{clipmon = MonRef}};
handle_paste(_Srv, S = #?MODULE{}) ->
    {ok, S}.

paste_proc(Srv, Inst) ->
    case rdp_server:get_vchan_pid(Srv, cliprdr_fsm) of
        {ok, ClipRdr} ->
            case cliprdr_fsm:list_formats(ClipRdr) of
                {ok, Fmts} ->
                    Fmt = case lists:member(unicode, Fmts) of
                        true -> unicode;
                        false -> text
                    end,
                    case cliprdr_fsm:paste(ClipRdr, Fmt) of
                        {ok, Data} ->
                            ok = lv_indev:send_text(Inst, Data);
                        Err ->
                            lager:debug("paste error: ~p", [Err])
                    end;
                Err ->
                    lager:debug("paste requested, failed to list clipboard "
                        "formats: ~p", [Err])
            end;
        _ ->
            ok
    end.

handle_copy(_Srv, S = #?MODULE{}) ->
    {ok, S}.

handle_select_all(_Srv, S = #?MODULE{}) ->
    {ok, S}.

handle_event(#ts_inpevt_mouse{point = Pt, action = move}, _Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, released),
    {ok, S};

handle_event(#ts_inpevt_mouse{point = Pt, action = down}, _Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, pressed),
    {ok, S};

handle_event(#ts_inpevt_mouse{point = Pt, action = up}, _Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, released),
    {ok, S};

handle_event(#ts_inpevt_mouse{}, _Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_key{code = C, action = A}, _Srv, S0 = #?MODULE{})
                        when (C =:= alt) or (C =:= shift) or (C =:= ctrl) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = case A of
        down -> MK0#{C => true};
        up -> MK0#{C => false}
    end,
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = caps, action = down}, _Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{capslock => not (maps:get(capslock, MK0, false))},
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = num, action = down}, _Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{numlock => not (maps:get(numlock, MK0, false))},
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = Code, flags = F, action = down}, Srv, S0 = #?MODULE{keydown = none}) ->
    #?MODULE{inst = Inst, modkeys = MK} = S0,
    Ext = lists:member(extended, F),
    S1 = S0#?MODULE{keydown = {Ext, Code}},
    case ts_key_to_lv(Code, Ext, MK) of
        null -> {ok, S1};
        paste -> handle_paste(Srv, S1);
        copy -> handle_copy(Srv, S1);
        select_all -> handle_select_all(Srv, S1);
        LvKey ->
            ok = lv_indev:send_key_event(Inst, LvKey, pressed),
            {ok, S1}
    end;
handle_event(#ts_inpevt_key{code = Code, flags = F, action = up}, _Srv, S0 = #?MODULE{keydown = {Ext, Code}}) ->
    #?MODULE{inst = Inst, modkeys = MK} = S0,
    S1 = S0#?MODULE{keydown = none},
    Ext = lists:member(extended, F),
    case ts_key_to_lv(Code, Ext, MK) of
        null -> {ok, S1};
        paste -> {ok, S1};
        copy -> {ok, S1};
        select_all -> {ok, S1};
        LvKey ->
            ok = lv_indev:send_key_event(Inst, LvKey, released),
            {ok, S1}
    end;
handle_event(Ev = #ts_inpevt_key{action = down}, Srv, S0 = #?MODULE{keydown = {OldExt, OldCode}}) ->
    F = case OldExt of true -> [extended]; false -> [] end,
    {ok, S1} = handle_event(#ts_inpevt_key{code = OldCode, flags = F, action = up}, Srv, S0),
    handle_event(Ev, Srv, S1);

handle_event(#ts_inpevt_key{}, _Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_unicode{code = Code, action = A}, Srv, S = #?MODULE{}) ->
    handle_event(#ts_inpevt_key{code = {Code,Code}, flags = [], action = A}, Srv, S);

handle_event(#ts_inpevt_wheel{clicks = N}, _Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_wheel_event(Inst, N),
    {ok, S};

handle_event(#ts_inpevt_sync{flags = Flags}, _Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{capslock => lists:member(capslock, Flags)},
    MK2 = MK1#{numlock => lists:member(numlock, Flags)},
    {ok, S0#?MODULE{modkeys = MK2}};

handle_event(#ts_suppress_output{allow_updates = false}, _Srv, S0 = #?MODULE{}) ->
    {ok, S0#?MODULE{suppress = true}};
handle_event(#ts_suppress_output{allow_updates = true, rect = R}, Srv, S0 = #?MODULE{}) ->
    S1 = S0#?MODULE{suppress = false},
    handle_event(#ts_refresh_rect{rects = [R]}, Srv, S1);

handle_event(#ts_refresh_rect{rects = []}, _Srv, S0 = #?MODULE{}) ->
    {ok, S0};
handle_event(#ts_refresh_rect{rects = [R | Rest]}, Srv, S0 = #?MODULE{}) ->
    #?MODULE{inst = Inst, flushref = Ref} = S0,
    {ok, Tiles} = lv:read_framebuffer(Inst, R),
    S1 = lists:foldl(fun ({TileRect, PixData}, SS0) ->
        {ok, SS1} = handle_info({Ref, flush, TileRect, PixData}, Srv, SS0),
        SS1
    end, S0, Tiles),
    handle_event(#ts_refresh_rect{rects = Rest}, Srv, S1);

handle_event(Other, Srv, S0 = #?MODULE{mod = Mod, modstate = MS0}) ->
    case Mod:handle_Event(Other, Srv, MS0) of
        {ok, MS1} ->
            {ok, S0#?MODULE{modstate = MS1}};
        {stop, Reason, MS1} ->
            {stop, Reason, S0#?MODULE{modstate = MS1}}
    end.

terminate(Reason, #?MODULE{mod = Mod, modstate = MS0}) ->
    Mod:terminate(Reason, MS0),
    ok.

ts_key_to_lv({$v, $V}, _, #{ctrl := true}) -> paste;
ts_key_to_lv({$a, $A}, _, #{ctrl := true}) -> select_all;
ts_key_to_lv({$c, $C}, _, #{ctrl := true}) -> copy;
ts_key_to_lv(_, _, #{ctrl := true}) -> null;
ts_key_to_lv(_, _, #{alt := true}) -> null;
ts_key_to_lv(esc, _, _) -> esc;
ts_key_to_lv(bksp, _, _) -> backspace;
ts_key_to_lv(enter, _, _) -> enter;
ts_key_to_lv(tab, _, #{shift := false}) -> next;
ts_key_to_lv(tab, _, #{shift := true}) -> prev;
ts_key_to_lv(home, true, _) -> home;
ts_key_to_lv('end', true, _) -> 'end';
ts_key_to_lv(up, true, _) -> up;
ts_key_to_lv(down, true, _) -> down;
ts_key_to_lv(left, true, _) -> left;
ts_key_to_lv(right, true, _) -> right;
ts_key_to_lv(del, true, _) -> del;
ts_key_to_lv(space, _, _) -> $ ;
ts_key_to_lv(ins, false, #{numlock := true}) -> $0;
ts_key_to_lv('end', false, #{numlock := true}) -> $1;
ts_key_to_lv(down, false, #{numlock := true}) -> $2;
ts_key_to_lv(pgdown, false, #{numlock := true}) -> $3;
ts_key_to_lv(left, false, #{numlock := true}) -> $4;
ts_key_to_lv(center, false, #{numlock := true}) -> $5;
ts_key_to_lv(right, false, #{numlock := true}) -> $6;
ts_key_to_lv(home, false, #{numlock := true}) -> $7;
ts_key_to_lv(up, false, #{numlock := true}) -> $8;
ts_key_to_lv(pgup, false, #{numlock := true}) -> $9;
ts_key_to_lv(del, false, #{numlock := true}) -> $.;
ts_key_to_lv('gray+', _, _) -> $+;
ts_key_to_lv('gray-', _, _) -> $-;
ts_key_to_lv(prisc, _, _) -> $*;
ts_key_to_lv({Plain, _Shifted}, true, _MK) -> Plain;
ts_key_to_lv({Plain, Shifted}, false, MK) ->
    case {maps:get(capslock, MK, false), maps:get(shift, MK, false)} of
        {true, true} when (Plain >= $a) and (Plain =< $z) -> Plain;
        {true, false} when (Plain >= $a) and (Plain =< $z) -> Shifted;
        {_, false} -> Plain;
        {_, true} -> Shifted
    end;
ts_key_to_lv(_, _, _) -> null.
