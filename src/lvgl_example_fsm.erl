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

-module(lvgl_example_fsm).
-behaviour(gen_statem).

-compile([{parse_transform, lager_transform}]).

-export([
    start_link/3
    ]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4,
    loading/3,
    login/3,
    checking_login/3,
    done/3
    ]).

-spec start_link(rdp_server:server(), lv:instance(), lv:point()) ->
    {ok, pid()} | {error, term()}.
start_link(Srv, Inst, Res) ->
    gen_statem:start_link(?MODULE, [Srv, Inst, Res], []).

-record(?MODULE, {
    srv :: rdp_server:server(),
    res :: lv:point(),
    inst :: lv:instance(),
    flowsty :: lv:style(),
    errsty :: lv:style(),
    loginev :: undefined | {lv:event(), reference()},
    acceptev :: undefined | {lv:event(), reference()},
    login_inp :: undefined | lv:textarea(),
    pw_inp :: undefined | lv:textarea(),
    errmsg :: undefined | string(),
    login :: undefined | string(),
    password :: undefined | string()
    }).

init([Srv, Inst, {W, H}]) ->
    FlowDir = if
        (W > H) -> column;
        true -> row
    end,

    {ok, FlowStyle} = lv_style:create(Inst),
    ok = lv_style:set_flex_flow(FlowStyle, FlowDir),
    ok = lv_style:set_flex_align(FlowStyle, center, start, center),
    ok = lv_style:set_bg_opacity(FlowStyle, 0),
    ok = lv_style:set_border_post(FlowStyle, false),

    {ok, ErrMsgStyle} = lv_style:create(Inst),
    ok = lv_style:set_text_color(ErrMsgStyle, lv_color:make(16#FF6060)),

    S0 = #?MODULE{srv = Srv, inst = Inst, flowsty = FlowStyle,
                  errsty = ErrMsgStyle, res = {W, H}},

    {ok, loading, S0}.

callback_mode() -> [state_functions, state_enter].

terminate(_Why, _State, #?MODULE{}) ->
    ok.

code_change(_OldVsn, OldState, S0, _Extra) ->
    {ok, OldState, S0}.


loading(enter, _PrevState, S0 = #?MODULE{inst = Inst}) ->
    {ok, Screen} = lv_scr:create(Inst),
    {ok, Spinner} = lv_spinner:create(Screen, 1000, 90),
    ok = lv_obj:center(Spinner),
    ok = lv_scr:load_anim(Inst, Screen, fade_in, 100, 0, true),
    {keep_state_and_data, [{state_timeout, 1000, advance}]};
loading(state_timeout, advance, S0 = #?MODULE{}) ->
    {next_state, login, S0}.


login(enter, _PrevState, S0 = #?MODULE{inst = Inst,
                                       flowsty = FlowStyle,
                                       errsty = ErrStyle,
                                       res = {W, H}}) ->
    {ok, Screen} = lv_scr:create(Inst),
    {ok, Group} = lv_group:create(Inst),

    {ok, Logo} = lv_img:create(Screen),
    ok = lv_img:set_src(Logo,
        rdp_lvgl_server:find_image_path("uq-logo.png")),

    {ok, Flex} = lv_obj:create(Inst, Screen),
    ok = lv_obj:add_style(Flex, FlowStyle),

    if
        (W > H) ->
            ok = lv_obj:set_size(Flex, {W div 3, H}),
            ok = lv_obj:align(Logo, center, {-1 * W div 6, 0}),
            ok = lv_obj:align(Flex, center, {W div 6, 0});
        true ->
            ok = lv_obj:set_size(Flex, {W, 2 * (H div 3)}),
            ok = lv_obj:align(Logo, center, {0, -1 * H div 3}),
            ok = lv_obj:align(Flex, bottom_mid)
    end,

    {ok, Lbl} = lv_label:create(Flex),
    ok = lv_label:set_text(Lbl, "Welcome!"),

    {ok, Text} = lv_textarea:create(Flex),
    ok = lv_textarea:set_one_line(Text, true),
    ok = lv_textarea:set_text_selection(Text, true),
    ok = lv_textarea:set_placeholder_text(Text, "Username"),
    ok = lv_group:add_obj(Group, Text),

    {ok, PwText} = lv_textarea:create(Flex),
    ok = lv_textarea:set_one_line(PwText, true),
    ok = lv_textarea:set_password_mode(PwText, true),
    ok = lv_textarea:set_text_selection(PwText, true),
    ok = lv_textarea:set_placeholder_text(PwText, "Password"),
    ok = lv_group:add_obj(Group, PwText),

    case S0#?MODULE.errmsg of
        undefined -> ok;
        ErrMsg ->
            {ok, ErrLbl} = lv_label:create(Flex),
            ok = lv_label:set_text(ErrLbl, ErrMsg),
            ok = lv_obj:add_style(ErrLbl, ErrStyle)
    end,

    {ok, Btn} = lv_btn:create(Flex),
    {ok, BtnLbl} = lv_label:create(Btn),
    ok = lv_label:set_text(BtnLbl, "Login"),

    {ok, Event, EvMsgRef} = lv_event:setup(Btn, pressed),
    {ok, AcEvent, AcMsgRef} = lv_event:setup(PwText, ready),

    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),

    ok = lv_indev:set_group(Inst, keyboard, Group),

    {keep_state, S0#?MODULE{login_inp = Text,
                            pw_inp = PwText,
                            loginev = {Event, EvMsgRef},
                            acceptev = {AcEvent, AcMsgRef}}};

login(info, {R, event, pressed, _, _}, S0 = #?MODULE{loginev = {_, R}}) ->
    login(event, login, S0);
login(info, {R, event, ready, _, _}, S0 = #?MODULE{acceptev = {_, R}}) ->
    login(event, login, S0);

login(event, login, S0 = #?MODULE{login_inp = LoginInp,
                                  pw_inp = PwInp}) ->
    {ok, Login} = lv_textarea:get_text(LoginInp),
    {ok, Pw} = lv_textarea:get_text(PwInp),
    lager:debug("logging in with ~p/~p", [Login, Pw]),
    {next_state, checking_login, S0#?MODULE{login = Login, password = Pw}}.


checking_login(enter, _PrevState, S0 = #?MODULE{inst = Inst,
                                                flowsty = FlowStyle,
                                                res = {W, H}}) ->
    {ok, Screen} = lv_scr:create(Inst),

    {ok, Flex} = lv_obj:create(Inst, Screen),
    ok = lv_obj:add_style(Flex, FlowStyle),
    ok = lv_obj:set_size(Flex, {W, H}),
    ok = lv_obj:center(Flex),

    {ok, Logo} = lv_img:create(Flex),
    ok = lv_img:set_src(Logo,
        rdp_lvgl_server:find_image_path("uq-logo.png")),

    {ok, Spinner} = lv_spinner:create(Flex, 1000, 90),
    ok = lv_obj:add_flags(Spinner, [flex_in_new_track]),

    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),

    {keep_state_and_data, [{state_timeout, 1000, advance}]};
checking_login(state_timeout, advance, S0 = #?MODULE{login = L, password = P}) ->
    case {L, P} of
        {<<"user">>, <<"password">>} ->
            {next_state, done, S0};
        {<<>>, _} ->
            {next_state, login, S0#?MODULE{errmsg = "Username required"}};
        {_, <<>>} ->
            {next_state, login, S0#?MODULE{errmsg = "Password required"}};
        _ ->
            {next_state, login, S0#?MODULE{errmsg = "Invalid username or password"}}
    end.


done(enter, _PrevState, S0 = #?MODULE{inst = Inst}) ->
    {ok, Screen} = lv_scr:create(Inst),
    {ok, Lbl} = lv_label:create(Screen),
    ok = lv_label:set_text(Lbl, "End of demo!"),
    ok = lv_obj:center(Lbl),
    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),
    keep_state_and_data.
