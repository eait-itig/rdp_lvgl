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

%% @doc The <code>lv_checkbox</code> widget, an interactive check box.
%%
%% See [https://docs.lvgl.io/master/widgets/checkbox.html]
-module(lv_checkbox).

-export([
    create/1,
    set_text/2,
    get_text/1,
    check/1,
    uncheck/1,
    is_checked/1
    ]).

-include("async_wrappers.hrl").

%% @doc Creates a new <code>lv_checkbox</code> widget.
-spec create(lv:object()) -> {ok, lv:checkbox()} | lv:error().
create(Parent) ->
    ?async_wrapper(checkbox_create, Parent).

%% @doc Modifies the text label on the checkbox.
-spec set_text(lv:checkbox(), binary()) -> ok | lv:error().
set_text(Checkbox, Text) ->
    ?async_void_wrapper(checkbox_set_text, Checkbox, Text).

%% @doc Retrieves the text label on the checkbox.
-spec get_text(lv:checkbox()) -> {ok, binary()} | lv:error().
get_text(Checkbox) ->
    ?async_wrapper(checkbox_get_text, Checkbox).

%% @doc Sets the checkbox to be checked.
%%
%% Equivalent to calling <code>lv_obj:add_state(Checkbox, checked).</code>
%%
%% @see lv_obj:add_state/2
-spec check(lv:checkbox()) -> ok | lv:error().
check(Checkbox) ->
    lv_obj:add_state(Checkbox, checked).

%% @doc Sets the checkbox to be un-checked.
%%
%% Equivalent to calling <code>lv_obj:clear_state(Checkbox, checked).</code>
%%
%% @see lv_obj:clear_state/2
-spec uncheck(lv:checkbox()) -> ok | lv:error().
uncheck(Checkbox) ->
    lv_obj:clear_state(Checkbox, checked).

%% @doc Tests whether the checkbox is checked.
%%
%% Equivalent to calling <code>lv_obj:has_state(Checkbox, checked).</code>
%%
%% @see lv_obj:has_state/2
-spec is_checked(lv:checkbox()) -> {ok, boolean()} | lv:error().
is_checked(Checkbox) ->
    lv_obj:has_state(Checkbox, checked).
