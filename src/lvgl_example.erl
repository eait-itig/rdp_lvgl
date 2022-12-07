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

%% @doc An example of using {@link rdp_lvgl_server}
-module(lvgl_example).
-behaviour(rdp_server).

-compile([{parse_transform, lager_transform}]).

-export([
    start/0
    ]).

-export([
    init/1,
    handle_connect/4,
    init_ui/2,
    handle_event/3,
    terminate/2,
    handle_info/3
    ]).

-include_lib("rdp_proto/include/rdp_server.hrl").
-include_lib("rdp_proto/include/rdpdr.hrl").

%% @doc Starts a <code>rdp_server_sup</code> on port 3389 for the example.
start() ->
    rdp_server_sup:start_link(3389, {rdp_lvgl_server, ?MODULE}).

-record(?MODULE, {
    inst :: lv:instance(),
    fsm :: pid()
    }).

%% @private
init(_Peer) ->
    {ok, #?MODULE{}}.

%% @private
handle_connect(_Cookie, _Protocols, _Srv, S0 = #?MODULE{}) ->
    {accept, [{certfile, "etc/cert.pem"}, {keyfile, "etc/key.pem"}], S0}.

%% @private
init_ui({Srv, Inst}, S = #?MODULE{}) ->
    {W, H, _} = rdp_server:get_canvas(Srv),
    {ok, Pid} = lvgl_example_fsm:start_link(Srv, Inst, {W, H}),
    lager:debug("ui fsm in ~p", [Pid]),
    {ok, S#?MODULE{fsm = Pid}}.

%% @private
handle_info(_Msg, _Srv, S0 = #?MODULE{}) ->
    {ok, S0}.

%% @private
handle_event(_Evt, _Srv, S0 = #?MODULE{}) ->
    {ok, S0}.

%% @private
terminate(_Reason, #?MODULE{}) ->
    ok.
