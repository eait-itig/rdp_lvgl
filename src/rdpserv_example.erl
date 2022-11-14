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
    choose_format/3
    ]).

-export([start/0]).

-record(?MODULE, {
    renderer :: pid(),
    inst :: rdp_lvgl_nif:instance(),
    ev :: rdp_lvgl_nif:event()
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

bitmaps_to_orders(Bms) ->
    lists:reverse(bitmaps_to_orders(0, [], Bms)).

bitmaps_to_orders(_, [], []) -> [];
bitmaps_to_orders(_, R, []) ->
    [#ts_update_bitmaps{bitmaps = lists:reverse(R)}];
bitmaps_to_orders(Size, R, [Next | Rest]) ->
    #ts_bitmap{data = D} = Next,
    NewSize = Size + byte_size(D),
    if
        (NewSize > 16000) or (length(R) > 16) ->
            [#ts_update_bitmaps{bitmaps = lists:reverse(R)} |
                bitmaps_to_orders(0, [Next], Rest)];
        true ->
            bitmaps_to_orders(NewSize, [Next | R], Rest)
    end.

flush_done(Srv, Inst, MsgRef) ->
    erlang:garbage_collect(),
    case rdp_lvgl_nif:flush_done(Inst) of
        ok ->
            flush_loop(Srv, Inst, MsgRef, []);
        {error, busy} ->
            flush_done(Srv, Inst, MsgRef)
    end.

flush_loop(Srv, Inst, MsgRef, Bitmaps0) ->
    receive
        {MsgRef, flush, {X1, Y1, X2, Y2}, PixData} ->
            W = (X2 - X1),
            H = (Y2 - Y1),
            {ok, Compr} = rle_nif:compress(PixData, W, H, 16),
            CompInfo = #ts_bitmap_comp_info{
                flags = [compressed]},
                % full_size = byte_size(D),
                % scan_width = W},
            true = (byte_size(Compr) < 1 bsl 16),
            Dest = {X1, Y1},
            Bitmap = #ts_bitmap{dest=Dest, size={W,H}, bpp=16, data = Compr,
                comp_info = CompInfo},
            flush_loop(Srv, Inst, MsgRef, [Bitmap | Bitmaps0]);
        {MsgRef, flush_sync} ->
            Updates = bitmaps_to_orders(Bitmaps0),
            Me = self(),
            Sender = spawn(fun () ->
                lists:foreach(fun(U) ->
                    rdp_server:send_update(Srv, U)
                end, Updates),
                Me ! updates_sent
            end),
            receive
                updates_sent -> ok
                after 500 -> ok
            end,
            flush_done(Srv, Inst, MsgRef)
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
    Pid = spawn_link(fun () ->
        {ok, Inst, MsgRef} = rdp_lvgl_nif:setup_instance({W+1, H+1}),
        receive {MsgRef, setup_done} -> ok end,
        Fsm ! {nif_inst, Inst},
        flush_loop(Srv, Inst, MsgRef, [])
    end),
    receive {nif_inst, Inst} -> ok end,

    {ok, Screen} = lv_scr:create(Inst),

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

    {ok, Btn} = lv_btn:create(Flex),
    {ok, BtnLbl} = lv_label:create(Btn),
    ok = lv_label:set_text(BtnLbl, "Login"),
    {ok, Event, MsgRef} = lv_event:setup(Btn, pressed),

    {ok, Spinner} = lv_spinner:create(Flex, 1000, 90),

    ok = lv_scr:load(Inst, Screen),

    setup_cursor(Inst),

    {ok, #?MODULE{renderer = Pid, inst = Inst, ev = Event}}.


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

handle_event(#ts_inpevt_key{code = Code, action = down}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_key{code = Code, action = up}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_key{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_unicode{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_wheel{}, Srv, S = #?MODULE{}) ->
    {ok, S};

handle_event(#ts_inpevt_sync{}, Srv, S = #?MODULE{}) ->
    {ok, S}.

terminate(_Reason, #?MODULE{renderer = Pid}) ->
    exit(Pid, kill),
    % any cleanup you need to do at exit
    ok.
