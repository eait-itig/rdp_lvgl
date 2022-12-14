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
-module(rdp_lvgl).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    os:set_signal(sigchld, handle),
    DispPerChild = application:get_env(rdp_lvgl, displays_per_child, 16),
    ChildProcsMax = application:get_env(rdp_lvgl, child_procs_max, 8),
    ChildProcsInit = application:get_env(rdp_lvgl, child_procs_init, 2),
    DisplayMaxRes = application:get_env(rdp_lvgl, display_max_res, {3840, 2160}),
    RingSize = application:get_env(rdp_lvgl, ring_size, 16384),
    rdp_lvgl_nif:configure(fbufs_per_child, DispPerChild),
    rdp_lvgl_nif:configure(max_lvkids, ChildProcsMax),
    rdp_lvgl_nif:configure(ring_size, RingSize),
    rdp_lvgl_nif:configure(fb_max_res, DisplayMaxRes),
    rdp_lvgl_nif:prefork(ChildProcsInit),
    rdp_lvgl_sup:start_link().

stop(_State) ->
    ok.

