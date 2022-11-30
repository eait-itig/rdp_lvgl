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
-module(rdp_lvgl_logger).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([
    start_link/0
    ]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
    ]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(?MODULE, {}).

init([]) ->
    ok = rdp_lvgl_nif:take_log_ownership(),
    {ok, #?MODULE{}}.

terminate(Why, #?MODULE{}) ->
    ok.

handle_call(Req, _From, S0 = #?MODULE{}) ->
    lager:debug("unknown call: ~p", [Req]),
    {noreply, S0}.

handle_cast(Req, S0 = #?MODULE{}) ->
    lager:debug("unknown cast: ~p", [Req]),
    {noreply, S0}.

handle_info({lv_nif_log, Level, Pid, Role, Func, File, Line, Msg}, S0 = #?MODULE{}) ->
    Md = [
        {pid, {Role, Pid}},
        {module, File},
        {function, Func},
        {line, Line}
    ],
    lager:log(Level, Md, Msg),
    {noreply, S0}.
