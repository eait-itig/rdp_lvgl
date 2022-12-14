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

-module(lv_table).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    ctrl/0
    ]).

-type ctrl() :: merge_right | text_crop.
-type row() :: integer().
-type col() :: integer().

-include("async_wrappers.hrl").

-spec create(lv:object()) -> {ok, lv:table()} | lv:error().
create(Parent) ->
    ?async_wrapper(table_create, Parent).

-spec set_row_cnt(lv:table(), row()) -> ok | lv:error().
set_row_cnt(Table, Rows) ->
    ?async_void_wrapper(table_set_row_cnt, Table, Rows).

-spec set_col_cnt(lv:table(), col()) -> ok | lv:error().
set_col_cnt(Table, Cols) ->
    ?async_void_wrapper(table_set_col_cnt, Table, Cols).

-spec set_cell_value(lv:table(), row(), col(), string()) -> ok | lv:error().
set_cell_value(Table, Row, Col, Text) ->
    ?async_void_wrapper(table_set_cell_value, Table, Row, Col, Text).

-spec add_cell_ctrl(lv:table(), row(), col(), lv:flags(ctrl())) -> ok | lv:error().
add_cell_ctrl(Table, Row, Col, Ctrls) ->
    ?async_void_wrapper(table_add_cell_ctrl, Table, Row, Col, Ctrls).

-spec clear_cell_ctrl(lv:table(), row(), col(), lv:flags(ctrl())) -> ok | lv:error().
clear_cell_ctrl(Table, Row, Col, Ctrls) ->
    ?async_void_wrapper(table_clear_cell_ctrl, Table, Row, Col, Ctrls).

-spec get_selected_cell(lv:table()) -> {ok, {row(), col()}} | lv:error().
get_selected_cell(Table) ->
    ?async_wrapper(table_get_selected_cell_pt, Table).

-spec set_col_width(lv:table(), col(), lv:coord()) -> ok | lv:error().
set_col_width(Table, Col, Width) ->
    ?async_void_wrapper(table_set_col_width, Table, Col, Width).
