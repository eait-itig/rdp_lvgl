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

-compile(export_all).
-compile(nowarn_export_all).

-on_load(init/0).

-export_type([
    lvkid/0, msgref/0
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
        filename:join(["..", lib, rdp_lvgl, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(rdp_lvgl) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    SoName = try_paths(Paths1, "rdp_lvgl_nif"),
    erlang:load_nif(SoName, 0).

-type lvkid() :: reference().
-type instance() :: reference().

-type fd() :: integer().
-type inst_info() :: {instance(), Cmd :: fd(), Event :: fd(), Flush :: fd()}.

-type config() :: #{
    max_res => {integer(), integer()},
    fbufs => integer()}.

-type msgref() :: reference().

-type pixeldata() :: iolist().
-type tile() :: {lv:rect(), pixeldata()}.

-spec fork(config()) -> {ok, lvkid(), [inst_info()]} | lv:error().
fork(_Config) -> error(no_nif).

-type fbuf() :: 0 | 1.

-spec read_framebuffer(instance(), fbuf(), lv:rect()) -> {ok, [tile()]}.
read_framebuffer(_Inst, _Fbuf, _Rect) -> error(no_nif).

-spec take_log_ownership() -> ok.
take_log_ownership() -> error(no_nif).

