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

-module(rdpserv_example).
-behaviour(rdp_server).

-compile([{parse_transform, lager_transform}]).

-include_lib("rdp_proto/include/rdp_server.hrl").
-include_lib("rdp_proto/include/rdpdr.hrl").

-export([
    init/1,
    handle_connect/4,
    init_ui/2,
    handle_event/3,
    terminate/2,
    choose_format/3,
    handle_info/3
    ]).

-export([start/0]).

-type modkey() :: alt | ctrl | shift | capslock | numlock.

-record(?MODULE, {
    inst :: rdp_lvgl_nif:instance(),
    flushref :: undefined | reference(),
    frame = 1 :: integer(),
    sent_sof = false :: boolean(),
    ev :: rdp_lvgl_nif:event(),
    evref :: reference(),
    keydown = none :: none | term(),
    modkeys = #{alt => false, ctrl => false, shift => false,
        capslock => false, numlock => false} :: #{modkey() => boolean()}
    }).

start() ->
    rdp_server_sup:start_link(3389, ?MODULE).

%% @arg Peer  the peer address (IPv4 or IPv6) connecting
init(_Peer) ->
    {ok, #?MODULE{}}.

handle_connect(Cookie, Protocols, Srv, S = #?MODULE{}) ->
    {accept, [{certfile, "etc/cert.pem"}, {keyfile, "etc/key.pem"}], S}.
    % SslOptions should probably contain at least [{certfile, ...}, {keyfile, ...}]

choose_format(_Preferred, Supported, S = #?MODULE{}) ->
    lager:debug("using color format 16bpp out of ~p", [Supported]),
    {'16bpp', S}.

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

handle_info({R, flush_sync}, Srv, S0 = #?MODULE{flushref = R}) ->
    #?MODULE{inst = Inst, frame = FrameId} = S0,
    EOF = #ts_surface_frame_marker{frame = FrameId, action = finish},
    Update = #ts_update_surfaces{surfaces = [EOF]},
    rdp_server:send_update(Srv, Update),
    flush_done(Inst),
    {ok, S0#?MODULE{frame = FrameId + 1, sent_sof = false}};

handle_info(Msg, Srv, S = #?MODULE{}) ->
    lager:debug("message? ~p", [Msg]),
    {ok, S}.

flush_done(Inst) ->
    erlang:garbage_collect(),
    case rdp_lvgl_nif:flush_done(Inst) of
        ok -> ok;
        {error, busy} -> flush_done(Inst)
    end.

setup_cursor(Inst) ->
    {ok, SysLayer} = lv_disp:get_layer_sys(Inst),
    {ok, Parent} = lv_img:create(SysLayer),
    ok = lv_obj:add_flags(Parent, [overflow_visible, ignore_layout]),
    {ok, Img} = lv_img:create(Parent),
    ok = lv_img:set_src(Img, "A:priv/mouse_cursor.png"),
    ok = lv_obj:align(Img, top_left, {-4, -4}),
    ok = lv_indev:set_mouse_cursor(Inst, Parent).

init_ui(Srv, S = #?MODULE{}) ->
    {W, H, 16} = rdp_server:get_canvas(Srv),
    Fsm = self(),
    {ok, Inst, MsgRef} = rdp_lvgl_nif:setup_instance({W, H}),
    receive {MsgRef, setup_done} -> ok end,

    {ok, Screen} = lv_scr:create(Inst),
    {ok, Group} = lv_group:create(Inst),

    {ok, Flex} = lv_obj:create(Inst, Screen),

    FlowDir = if (W > H) -> row; true -> column end,
    {ok, FlowStyle} = lv_style:create(Inst),
    ok = lv_style:set_flex_flow(FlowStyle, FlowDir),
    ok = lv_style:set_flex_align(FlowStyle, center, center, center),
    ok = lv_style:set_bg_opacity(FlowStyle, 0),
    ok = lv_obj:add_style(Flex, FlowStyle),
    ok = lv_obj:set_size(Flex, {W, H}),
    ok = lv_obj:center(Flex),

    {ok, Lbl} = lv_label:create(Flex),
    ok = lv_label:set_text(Lbl, "Welcome!"),

    {ok, Text} = lv_textarea:create(Flex),
    ok = lv_textarea:set_one_line(Text, true),
    ok = lv_textarea:set_placeholder_text(Text, "Username"),
    ok = lv_group:add_obj(Group, Text),

    {ok, PwText} = lv_textarea:create(Flex),
    ok = lv_textarea:set_one_line(PwText, true),
    ok = lv_textarea:set_password_mode(PwText, true),
    ok = lv_textarea:set_placeholder_text(PwText, "Password"),
    ok = lv_group:add_obj(Group, PwText),

    {ok, Btn} = lv_btn:create(Flex),
    {ok, BtnLbl} = lv_label:create(Btn),
    ok = lv_label:set_text(BtnLbl, "Login"),
    {ok, Event, EvMsgRef} = lv_event:setup(Btn, pressed),

    {ok, Spinner} = lv_spinner:create(Flex, 1000, 90),
    ok = lv_obj:set_size(Spinner, {50,50}),

    ok = lv_scr:load(Inst, Screen),

    ok = lv_indev:set_group(Inst, keyboard, Group),
    setup_cursor(Inst),
    %ok = rdp_server:send_update(Srv, #fp_update_mouse{mode = hidden}),

    {ok, #?MODULE{inst = Inst, flushref = MsgRef, ev = Event, evref = EvMsgRef}}.


handle_event(#ts_inpevt_mouse{point = Pt, action = move}, Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, released),
    {ok, S};

handle_event(#ts_inpevt_mouse{point = Pt, action = down}, Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, pressed),
    {ok, S};

handle_event(#ts_inpevt_mouse{point = Pt, action = up}, Srv, S = #?MODULE{}) ->
    #?MODULE{inst = Inst} = S,
    ok = lv_indev:send_pointer_event(Inst, Pt, released),
    {ok, S};

handle_event(#ts_inpevt_mouse{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_key{code = C, action = A}, Srv, S0 = #?MODULE{})
                        when (C =:= alt) or (C =:= shift) or (C =:= ctrl) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = case A of
        down -> MK0#{C => true};
        up -> MK0#{C => false}
    end,
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = caps, action = down}, Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{capslock => not (maps:get(capslock, MK0, false))},
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = num, action = down}, Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{numlock => not (maps:get(numlock, MK0, false))},
    {ok, S0#?MODULE{modkeys = MK1}};
handle_event(#ts_inpevt_key{code = Code, flags = F, action = down}, Srv, S0 = #?MODULE{keydown = none}) ->
    #?MODULE{inst = Inst, modkeys = MK} = S0,
    Ext = lists:member(extended, F),
    S1 = S0#?MODULE{keydown = {Ext, Code}},
    lager:debug("ts code = ~p, ext = ~p, mk = ~p", [Code, Ext, MK]),
    case ts_key_to_lv(Code, Ext, MK) of
        null -> {ok, S1};
        paste -> {ok, S1};
        copy -> {ok, S1};
        select_all -> {ok, S1};
        LvKey ->
            ok = lv_indev:send_key_event(Inst, LvKey, pressed),
            {ok, S1}
    end;
handle_event(#ts_inpevt_key{code = Code, flags = F, action = up}, Srv, S0 = #?MODULE{keydown = {Ext, Code}}) ->
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
handle_event(Ev = #ts_inpevt_key{code = Code, action = down}, Srv, S0 = #?MODULE{keydown = {OldExt, OldCode}}) ->
    F = case OldExt of true -> [extended]; false -> [] end,
    {ok, S1} = handle_event(#ts_inpevt_key{code = OldCode, flags = F, action = up}, Srv, S0),
    handle_event(Ev, Srv, S1);

handle_event(#ts_inpevt_key{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_unicode{code = Code, action = A}, Srv, S = #?MODULE{}) ->
    handle_event(#ts_inpevt_key{code = {Code,Code}, flags = [], action = A}, Srv, S);

handle_event(#ts_inpevt_wheel{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_sync{flags = Flags}, Srv, S0 = #?MODULE{}) ->
    #?MODULE{modkeys = MK0} = S0,
    MK1 = MK0#{capslock => lists:member(capslock, Flags)},
    MK2 = MK1#{numlock => lists:member(numlock, Flags)},
    {ok, S0#?MODULE{modkeys = MK2}}.

terminate(_Reason, #?MODULE{}) ->
    % any cleanup you need to do at exit
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
ts_key_to_lv({Plain, Shifted}, true, MK) -> Plain;
ts_key_to_lv({Plain, Shifted}, false, MK) ->
    case {maps:get(capslock, MK, false), maps:get(shift, MK, false)} of
        {true, true} when (Plain >= $a) and (Plain =< $z) -> Plain;
        {true, false} when (Plain >= $a) and (Plain =< $z) -> Shifted;
        {_, false} -> Plain;
        {_, true} -> Shifted
    end;
ts_key_to_lv(_, _, _) -> null.
