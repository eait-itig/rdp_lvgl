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

-module(lv_textarea).

-compile(export_all).
-compile(nowarn_export_all).

-include("async_wrappers.hrl").

-type msec() :: integer().
%% Number of milliseconds.

-spec create(lv:object()) -> {ok, lv:textarea()} | lv:error().
create(Parent) ->
    ?async_wrapper(textarea_create, Parent).

-spec set_text(lv:textarea(), string()) -> ok | lv:error().
set_text(Obj, Text) ->
    ?async_void_wrapper(textarea_set_text, Obj, Text).

%% @doc Retrieves the label part of the text area.
%%
%% @see lv_label
-spec get_label(lv:textarea()) -> {ok, lv:label()} | lv:error().
get_label(Obj) ->
    ?async_wrapper(textarea_get_label, Obj).

-spec set_password_mode(lv:textarea(), boolean()) -> ok | lv:error().
set_password_mode(Obj, State) ->
    ?async_void_wrapper(textarea_set_password_mode, Obj, State).

-spec set_one_line(lv:textarea(), boolean()) -> ok | lv:error().
set_one_line(Obj, State) ->
    ?async_void_wrapper(textarea_set_one_line, Obj, State).

-spec set_cursor_hidden(lv:textarea(), boolean()) -> ok | lv:error().
set_cursor_hidden(Obj, State) ->
    ?async_void_wrapper(textarea_set_cursor_hidden, Obj, State).

-spec set_placeholder_text(lv:textarea(), string()) -> ok | lv:error().
set_placeholder_text(Obj, Text) ->
    ?async_void_wrapper(textarea_set_placeholder_text, Obj, Text).

-spec add_text(lv:textarea(), string()) -> ok | lv:error().
add_text(Obj, Text) ->
    ?async_void_wrapper(textarea_add_text, Obj, Text).

-spec set_accepted_chars(lv:textarea(), lv:buffer()) -> ok | lv:error().
set_accepted_chars(Obj, Buffer) ->
    ?async_void_wrapper(textarea_set_accepted_chars, Obj, Buffer).

-spec set_max_length(lv:textarea(), integer()) -> ok | lv:error().
set_max_length(Obj, MaxLen) ->
    ?async_void_wrapper(textarea_set_max_length, Obj, MaxLen).

-spec set_text_selection(lv:textarea(), boolean()) -> ok | lv:error().
set_text_selection(Obj, State) ->
    ?async_void_wrapper(textarea_set_text_selection, Obj, State).

-spec set_password_show_time(lv:textarea(), msec()) -> ok | lv:error().
set_password_show_time(Obj, Time) ->
    ?async_void_wrapper(textarea_set_password_show_time, Obj, Time).

-spec get_text(lv:textarea()) -> {ok, binary()} | lv:error().
get_text(Obj) ->
    ?async_wrapper(textarea_get_text, Obj).

-spec clear_selection(lv:textarea()) -> ok | lv:error().
clear_selection(Obj) ->
    ?async_void_wrapper(textarea_clear_selection, Obj).

-spec set_cursor_click_pos(lv:textarea(), boolean()) -> ok | lv:error().
set_cursor_click_pos(Obj, State) ->
    ?async_void_wrapper(textarea_set_cursor_click_pos, Obj, State).

-spec set_cursor_pos(lv:textarea(), integer()) -> ok | lv:error().
set_cursor_pos(Obj, Offset) ->
    ?async_void_wrapper(textarea_set_cursor_pos, Obj, Offset).
