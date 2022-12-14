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

-module(lv_menu).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([
    root_back_btn_mode/0, header_mode/0, page/0, item/0
    ]).

-include("async_wrappers.hrl").

-type root_back_btn_mode() :: enabled | disabled.
-type header_mode() :: top_fixed | top_unfixed | bottom_fixed.
-type page() :: lv:object().
-type item() :: lv:object().

-spec create(lv:object()) -> {ok, lv:menu()} | lv:error().
create(Parent) ->
    ?async_wrapper(menu_create, Parent).

-spec page_create(lv:menu(), string()) -> {ok, page()} | lv:error().
page_create(Menu, Title) ->
    ?async_wrapper(menu_page_create, Menu, Title).

-spec cont_create(item() | page()) -> {ok, item()} | lv:error().
cont_create(Parent) ->
    ?async_wrapper(menu_cont_create, Parent).

-spec section_create(item() | page()) -> {ok, item()} | lv:error().
section_create(Parent) ->
    ?async_wrapper(menu_section_create, Parent).

-spec separator_create(item() | page()) -> {ok, item()} | lv:error().
separator_create(Parent) ->
    ?async_wrapper(menu_separator_create, Parent).

-spec set_page(lv:menu(), page()) -> ok | lv:error().
set_page(Menu, Page) ->
    ?async_void_wrapper(menu_set_page, Menu, Page).

-spec set_sidebar_page(lv:menu(), page()) -> ok | lv:error().
set_sidebar_page(Menu, Page) ->
    ?async_void_wrapper(menu_set_sidebar_page, Menu, Page).

-spec set_mode_root_back_btn(lv:menu(), root_back_btn_mode()) -> ok | lv:error().
set_mode_root_back_btn(Menu, Mode) ->
    ?async_void_wrapper(menu_set_mode_root_back_btn, Menu, Mode).

-spec set_mode_header(lv:menu(), header_mode()) -> ok | lv:error().
set_mode_header(Menu, Mode) ->
    ?async_void_wrapper(menu_set_mode_header, Menu, Mode).

-spec set_load_page_event(lv:menu(), item(), page()) -> ok | lv:error().
set_load_page_event(Menu, Item, Page) ->
    ?async_void_wrapper(menu_set_load_page_event, Menu, Item, Page).
