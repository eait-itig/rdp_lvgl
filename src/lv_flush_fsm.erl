%%
%% RDP UI framework using LVGL
%%
%% Copyright 2023 Alex Wilson <alex@uq.edu.au>, The University of Queensland
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
-module(lv_flush_fsm).

-behaviour(gen_statem).

-export([
    start_link/2,
    attach/2,
    flush_done/1,
    teardown/1,
    teardown_done/1
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4
    ]).

-export([
    detached/3,
    idle/3,
    flushing/3,
    syncing/3,
    closing/3
    ]).

-type fd() :: integer().

-spec start_link(rdp_lvgl_nif:instance(), fd()) -> {ok, pid()}.
start_link(Inst, FD) ->
    gen_statem:start_link(?MODULE, [Inst, FD], []).

-spec attach(pid(), lv:rref()) -> {ok, lv:msgref()} | {error, already_attached} | {error, term()}.
attach(Pid, DispDrv) ->
    gen_statem:call(Pid, {attach, self(), DispDrv}).

-spec read_framebuffer(pid(), lv:rect()) -> {ok, lv:tile()} | {error, term()}.
read_framebuffer(Pid, Rect) ->
    gen_statem:call(Pid, {})

-spec flush_done(pid()) -> ok | {error, not_owner} | {error, stale} | {error, term()}.
flush_done(Pid) ->
    gen_statem:call(Pid, {flush_done, self()}).

% Called by the command FSM after sending the instance teardown command
% Causes lvgl to tear down the display instance.
-spec teardown(pid()) -> ok | {error, term()}.
teardown(Pid) ->
    gen_statem:call(Pid, teardown).

% Called by the event mux FSM after LVGL confirms instance teardown is done.
-spec teardown_done(pid()) -> ok | {error, term()}.
teardown_done(Pid) ->
    gen_statem:call(Pid, teardown_done).

-record(?MODULE, {
    inst :: rdp_lvgl_nif:instance(),
    sock :: gen_tcp:socket(),
    msgref :: undefined | reference(),
    pid :: undefined | pid(),
    mref :: undefined | reference(),
    disp_drv :: undefined | lv:rref(),
    fbuf = 0 :: 0 | 1
    }).

init([Inst, FD]) ->
    {ok, Sock} = gen_tcp:fdopen(FD, [
        {packet, 4}, {mode, binary}, {active, once}]),
    {ok, detached, #?MODULE{inst = Inst, sock = Sock}}.

callback_mode() -> [state_enter, state_functions].

terminate(_Why, _State, #?MODULE{}) ->
    ok.

code_change(_OldVsn, OldState, S0, _Extra) ->
    {ok, OldState, S0}.

detached(enter, _PrevState, S0 = #?MODULE{}) ->
    {keep_state, S0#?MODULE{fbuf = 0}, [hibernate]};
detached(info, {tcp, Sock, _Data}, S0 = #?MODULE{sock = Sock}) ->
    lager:warning("got message while detached!"),
    keep_state_and_data;
detached(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    {stop, normal, S0};
detached({call, From}, {flush_done, _Pid}, #?MODULE{}) ->
    gen_statem:reply(From, {error, not_owner}),
    keep_state_and_data;
detached({call, From}, teardown, #?MODULE{sock = Sock}) ->
    Msg = <<0:32, 0:32, 1>>,
    ok = gen_tcp:send(Sock, <<(byte_size(Msg)):32/big, Msg/binary>>),
    gen_statem:reply(From, ok),
    keep_state_and_data;
detached({call, From}, teardown_done, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
detached({call, From}, {attach, Pid, Drv}, S0 = #?MODULE{}) ->
    {ok, MRef} = erlang:monitor(process, Pid),
    MsgRef = make_ref(),
    S1 = S0#?MODULE{pid = Pid, mref = MRef, msgref = MsgRef,
                    disp_drv = Drv},
    gen_statem:reply(From, {ok, MsgRef}),
    {next_state, idle, S1}.

idle(enter, _PrevState, #?MODULE{}) ->
    {keep_state_and_data, [hibernate]};
idle(info, {tcp, Sock, _Data}, #?MODULE{sock = Sock}) ->
    {next_state, flushing, [postpone]};
idle(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    {stop, normal, S0};
idle(info, {'DOWN', MRef, _, _}, S0 = #?MODULE{mref = MRef}) ->
    {next_state, detached, S0};
idle({call, From}, {flush_done, Pid}, #?MODULE{pid = Pid}) ->
    gen_statem:reply(From, {error, stale}),
    keep_state_and_data;
idle({call, From}, {flush_done, _Pid}, #?MODULE{}) ->
    gen_statem:reply(From, {error, not_owner}),
    keep_state_and_data;
idle({call, From}, teardown, S0 = #?MODULE{sock = Sock, disp_drv = Drv}) ->
    {DrvGen, DrvIdx} = Drv,
    Msg = <<DrvGen:32/big, DrvIdx:32/big, 1>>,
    ok = gen_tcp:send(Sock, <<(byte_size(Msg)):32/big, Msg/binary>>),
    gen_statem:reply(From, ok),
    {next_state, closing, S0};
idle({call, From}, {attach, _Pid, _Drv}, #?MODULE{}) ->
    gen_statem:reply(From, {error, already_attached}),
    keep_state_and_data.

flushing(enter, _PrevState, #?MODULE{}) ->
    keep_state_and_data;
flushing(info, {tcp, Sock, Data}, S0 = #?MODULE{sock = Sock, inst = Inst,
                                                pid = Pid, msgref = MsgRef}) ->
    <<_UDataGen:32/big, _UDataIdx:32/big,
      FBuf, Sync, X1:32/big, Y1:32/big, X2:32/big, Y2:32/big>> = Data,
    Rect = {X1, Y1, X2, Y2},
    {ok, Tiles} = rdp_lvgl_nif:read_framebuffer(Inst, FBuf, Rect),
    Pid ! {MsgRef, flush, Rect, Tiles},
    S1 = S0#?MODULE{fbuf = FBuf},
    case Sync of
        1 -> {next_state, syncing, S1};
        0 -> {keep_state, S1}
    end;
flushing(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    lager:warning("lvkid died during flush!"),
    {stop, normal, S0};
flushing({call, From}, {flush_done, Pid}, #?MODULE{pid = Pid}) ->
    gen_statem:reply(From, {error, stale}),
    keep_state_and_data;
flushing({call, From}, {flush_done, _Pid}, #?MODULE{}) ->
    gen_statem:reply(From, {error, not_owner}),
    keep_state_and_data;
flushing({call, From}, teardown, S0 = #?MODULE{sock = Sock, disp_drv = Drv}) ->
    {DrvGen, DrvIdx} = Drv,
    Msg = <<DrvGen:32/big, DrvIdx:32/big, 1>>,
    ok = gen_tcp:send(Sock, <<(byte_size(Msg)):32/big, Msg/binary>>),
    gen_statem:reply(From, ok),
    {next_state, closing, S0};
flushing(info, {'DOWN', MRef, _, _}, #?MODULE{mref = MRef}) ->
    lager:warning("target died in the middle of a flush"),
    keep_state_and_data;
flushing({call, From}, {attach, _Pid, _Drv}, #?MODULE{}) ->
    gen_statem:reply(From, {error, already_attached}),
    keep_state_and_data.

syncing(enter, _PrevState, #?MODULE{pid = Pid, msgref = MsgRef}) ->
    Pid ! {MsgRef, flush_sync},
    keep_state_and_data;
syncing({call, From}, {flush_done, Pid}, S0 = #?MODULE{sock = Sock, pid = Pid,
                                                       disp_drv = Drv}) ->
    {DrvGen, DrvIdx} = Drv,
    Msg = <<DrvGen:32/big, DrvIdx:32/big, 0>>,
    ok = gen_tcp:send(Sock, <<(byte_size(Msg)):32/big, Msg/binary>>),
    gen_statem:reply(From, ok),
    {next_state, idle, S0};
syncing({call, From}, {flush_done, _Pid}, #?MODULE{}) ->
    gen_statem:reply(From, {error, not_owner}),
    keep_state_and_data;
syncing(info, {tcp, Sock, _Data}, #?MODULE{sock = Sock}) ->
    lager:debug("enqueued another flush message while waiting for last one"),
    {keep_state_and_data, [postpone]};
syncing(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    lager:debug("closed during sync"),
    {stop, normal, S0};
syncing(info, {'DOWN', MRef, _, _}, #?MODULE{mref = MRef}) ->
    lager:warning("target died in the middle of flush sync"),
    keep_state_and_data;
syncing({call, From}, {attach, _Pid, _Drv}, #?MODULE{}) ->
    gen_statem:reply(From, {error, already_attached}),
    keep_state_and_data;
syncing({call, From}, teardown, S0 = #?MODULE{sock = Sock, disp_drv = Drv}) ->
    {DrvGen, DrvIdx} = Drv,
    Msg = <<DrvGen:32/big, DrvIdx:32/big, 1>>,
    ok = gen_tcp:send(Sock, <<(byte_size(Msg)):32/big, Msg/binary>>),
    gen_statem:reply(From, ok),
    {next_state, closing, S0}.

closing(enter, _PrevState, S0 = #?MODULE{mref = MRef}) ->
    erlang:demonitor(MRef, [flush]),
    S1 = S0#?MODULE{pid = undefined, mref = undefined, disp_drv = undefined},
    {keep_state, S1, [hibernate]};
closing({call, From}, {flush_done, _Pid}, #?MODULE{}) ->
    gen_statem:reply(From, {error, not_owner}),
    keep_state_and_data;
closing(info, {tcp, Sock, _Data}, #?MODULE{sock = Sock}) ->
    lager:debug("got data while closing?"),
    keep_state_and_data;
closing(info, {tcp_closed, Sock}, S0 = #?MODULE{sock = Sock}) ->
    {stop, normal, S0};
closing({call, From}, {attach, _Pid, _Drv}, #?MODULE{}) ->
    gen_statem:reply(From, {error, teardown_in_progress}),
    keep_state_and_data;
closing({call, From}, teardown, #?MODULE{}) ->
    gen_statem:reply(From, ok),
    keep_state_and_data;
closing({call, From}, teardown_done, S0 = #?MODULE{}) ->
    gen_statem:reply(From, ok),
    {next_state, detached, S0}.
