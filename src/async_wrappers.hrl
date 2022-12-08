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

-define(async_void_wrapper(Func, Arg0),
    case rdp_lvgl_nif:Func(Arg0) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1),
    case rdp_lvgl_nif:Func(Arg0, Arg1) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1, Arg2),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1, Arg2, Arg3),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1, Arg2, Arg3, Arg4),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3, Arg4) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1, Arg2, Arg3, Arg4, Arg5),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_void_wrapper(Func, Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok} -> ok;
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_wrapper(Func, Arg0),
    case rdp_lvgl_nif:Func(Arg0) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok, Res} -> {ok, Res};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_wrapper(Func, Arg0, Arg1),
    case rdp_lvgl_nif:Func(Arg0, Arg1) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok, Res} -> {ok, Res};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_wrapper(Func, Arg0, Arg1, Arg2),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok, Res} -> {ok, Res};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_wrapper(Func, Arg0, Arg1, Arg2, Arg3),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok, Res} -> {ok, Res};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).

-define(async_wrapper(Func, Arg0, Arg1, Arg2, Arg3, Arg4),
    case rdp_lvgl_nif:Func(Arg0, Arg1, Arg2, Arg3, Arg4) of
        {async, MsgRef} ->
            receive
                {MsgRef, ok, Res} -> {ok, Res};
                {MsgRef, error, Why} -> {error, Why};
                {MsgRef, error, Num, Str} -> {error, Num, Str}
            end;
        Err -> Err
    end).
