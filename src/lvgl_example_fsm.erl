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

%% @doc UI state machine for {@link lvgl_example}
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

%% @doc Called by {@link lvgl_example:init_ui/2}
-spec start_link(rdp_server:server(), lv:instance(), lv:point()) ->
    {ok, pid()} | {error, term()}.
start_link(Srv, Inst, Res) ->
    gen_statem:start_link(?MODULE, [Srv, Inst, Res], []).

-record(?MODULE, {
    srv :: rdp_server:server(),
    mref :: reference(),
    res :: lv:point(),
    inst :: lv:instance(),
    ssty :: lv:style(),
    flowsty :: lv:style(),
    apsty :: lv:style(),
    rlsty :: lv:style(),
    chars :: lv:buffer(),
    evs = [] :: [lv:event()],
    errmsg :: undefined | string(),
    login :: undefined | binary(),
    password :: undefined | binary()
    }).

%% @private
init([Srv, Inst, {W, H}]) ->
    {Pid, _} = Srv,
    MRef = monitor(process, Pid),

    {ok, ScreenStyle} = lv_style:create(Inst),
    ok = lv_style:set_flex_flow(ScreenStyle,
        if (W > H) -> row; true -> column end),
    ok = lv_style:set_flex_align(ScreenStyle, center, center, center),
    ok = lv_style:set_bg_color(ScreenStyle, lv_color:make(16#48206c)),

    {ok, FlowStyle} = lv_style:create(Inst),
    ok = lv_style:set_flex_flow(FlowStyle, column),
    ok = lv_style:set_flex_align(FlowStyle, center, start,
        if (W > H) -> start; true -> center end),
    ok = lv_style:set_bg_opa(FlowStyle, 0),
    ok = lv_style:set_border_opa(FlowStyle, 0),

    {ok, APStyle} = lv_style:create(Inst),
    ok = lv_style:set_bg_opa(APStyle, 0.7),
    ok = lv_style:set_border_opa(APStyle, 0),

    {ok, RLStyle} = lv_style:create(Inst),
    ok = lv_style:set_border_side(RLStyle, [left]),
    ok = lv_style:set_border_color(RLStyle, lv_color:palette(black)),
    ok = lv_style:set_border_opa(RLStyle, 0.5),
    ok = lv_style:set_pad_left(RLStyle, 10),
    ok = lv_style:set_pad_top(RLStyle, 0),
    ok = lv_style:set_pad_bottom(RLStyle, 0),
    ok = lv_style:set_radius(RLStyle, 0),

    {ok, Chars} = lv:make_buffer(Inst, <<"0123456789", 0>>),

    S0 = #?MODULE{srv = Srv, mref = MRef, inst = Inst, ssty = ScreenStyle,
                  flowsty = FlowStyle, apsty = APStyle, res = {W, H},
                  chars = Chars, rlsty = RLStyle},

    {ok, loading, S0}.

%% @private
callback_mode() -> [state_functions, state_enter].

%% @private
terminate(_Why, _State, #?MODULE{}) ->
    ok.

%% @private
code_change(_OldVsn, OldState, S0, _Extra) ->
    {ok, OldState, S0}.

make_flex(#?MODULE{inst = Inst, flowsty = FlowStyle, ssty = ScreenStyle, res = {W, H}}) ->
    {ok, Screen} = lv_scr:create(Inst),
    ok = lv_obj:add_style(Screen, ScreenStyle),

    {ok, Logo} = lv_img:create(Screen),
    ok = lv_img:set_src(Logo,
        rdp_lvgl_server:find_image_path("uq-logo.png")),
    {ok, {LogoW, _LogoH}} = lv_obj:get_size(Logo),
    {ok, Flex} = lv_obj:create(Inst, Screen),
    ok = lv_obj:add_style(Flex, FlowStyle),

    if
        (W > H) ->
            FlexW = if (W div 3 < 500) -> 500; true -> W div 3 end,
            ok = lv_obj:set_size(Flex, {FlexW, {percent, 100}});
        true ->
            ok = lv_obj:set_size(Flex, {{percent, 80}, {percent, 66}})
    end,
    {Screen, Flex}.

%% @private
loading(enter, _PrevState, S0 = #?MODULE{inst = Inst}) ->
    {Screen, Flex} = make_flex(S0),
    {ok, _Spinner} = lv_spinner:create(Flex, 1000, 90),
    ok = lv_scr:load_anim(Inst, Screen, fade_in, 100, 0, true),
    {keep_state_and_data, [{state_timeout, 1000, advance}]};
loading(info, {'DOWN', MRef, process, _, _}, S0 = #?MODULE{mref = MRef}) ->
    {stop, normal, S0};
loading(state_timeout, advance, S0 = #?MODULE{}) ->
    {next_state, login, S0}.

make_auth_method_flex(TopLevel, Symbol, #?MODULE{inst = Inst,
                                                 flowsty = FlowStyle,
                                                 apsty = APStyle,
                                                 rlsty = RLStyle}) ->
    {ok, Outer} = lv_obj:create(Inst, TopLevel),
    ok = lv_obj:add_style(Outer, APStyle),
    ok = lv_obj:set_size(Outer, {{percent, 100}, content}),
    ok = lv_obj:set_scrollbar_mode(Outer, off),

    {ok, Sym} = lv_label:create(Outer),
    ok = lv_obj:set_style_text_font(Sym, {"lineawesome", regular, 20}),
    ok = lv_label:set_text(Sym, unicode:characters_to_binary([Symbol], utf8)),
    ok = lv_obj:align(Sym, left_mid),

    {ok, InnerFlex} = lv_obj:create(Inst, Outer),
    ok = lv_obj:add_style(InnerFlex, FlowStyle),
    ok = lv_obj:set_size(InnerFlex, {{percent, 100}, content}),
    ok = lv_obj:align(InnerFlex, top_left, {30, 0}),
    ok = lv_obj:add_style(InnerFlex, RLStyle),
    ok = lv_obj:set_scroll_dir(InnerFlex, [right, vertical]),
    InnerFlex.

%% @private
login(enter, _PrevState, S0 = #?MODULE{inst = Inst, chars = Chars}) ->
    {Screen, Flex} = make_flex(S0),
    {ok, Group} = lv_group:create(Inst),

    {ok, SG} = lv_span:create(Flex),
    ok = lv_obj:set_size(SG, {{percent, 100}, content}),
    ok = lv_span:set_mode(SG, break),

    {ok, TitleSty} = lv_style:create(Inst),
    ok = lv_style:set_text_color(TitleSty, lv_color:palette(white)),
    ok = lv_style:set_text_font(TitleSty, {"roboto", bold, 24}),
    {ok, SubtitleSty} = lv_style:create(Inst),
    ok = lv_style:set_text_color(SubtitleSty, lv_color:palette(white)),

    {ok, Title} = lv_span:new_span(SG),
    ok = lv_span:set_text(Title, "Faculty of EAIT"),
    ok = lv_span:set_style(Title, TitleSty),

    {ok, Subtitle} = lv_span:new_span(SG),
    ok = lv_span:set_text(Subtitle, "\nStaff Remote Access"),
    ok = lv_span:set_style(Subtitle, SubtitleSty),

    UPwFlex = make_auth_method_flex(Flex, 16#f11c, S0),

    {ok, Text} = lv_textarea:create(UPwFlex),
    ok = lv_textarea:set_one_line(Text, true),
    ok = lv_textarea:set_text_selection(Text, true),
    ok = lv_textarea:set_placeholder_text(Text, "Username"),
    ok = lv_group:add_obj(Group, Text),

    {ok, PwText} = lv_textarea:create(UPwFlex),
    ok = lv_textarea:set_one_line(PwText, true),
    ok = lv_textarea:set_password_mode(PwText, true),
    ok = lv_textarea:set_text_selection(PwText, true),
    ok = lv_textarea:set_placeholder_text(PwText, "Password"),
    ok = lv_group:add_obj(Group, PwText),

    {ok, Btn} = lv_btn:create(UPwFlex),
    {ok, BtnLbl} = lv_label:create(Btn),
    ok = lv_label:set_text(BtnLbl, "Login"),

    {ok, BtnEvent, _} = lv_event:setup(Btn, pressed, {login, Text, PwText}),
    {ok, AcEvent, _} = lv_event:setup(PwText, ready, {login, Text, PwText}),

    YkFlex = make_auth_method_flex(Flex, 16#f2c2, S0),

    {ok, UserLbl} = lv_label:create(YkFlex),
    ok = lv_label:set_text(UserLbl, "chemlabs@eait.uq.edu.au"),

    {ok, PinText} = lv_textarea:create(YkFlex),
    ok = lv_textarea:set_one_line(PinText, true),
    ok = lv_textarea:set_text_selection(PinText, true),
    ok = lv_textarea:set_placeholder_text(PinText, "PIN"),
    ok = lv_textarea:set_accepted_chars(PinText, Chars),
    ok = lv_textarea:set_password_mode(PinText, true),
    ok = lv_group:add_obj(Group, PinText),

    {ok, YkBtn} = lv_btn:create(YkFlex),
    {ok, YkBtnLbl} = lv_label:create(YkBtn),
    ok = lv_label:set_text(YkBtnLbl, "Login"),

    {ok, YkBtnEvent, _} = lv_event:setup(YkBtn, pressed,
        {login_pin, "chemlabs", PinText}),
    {ok, YkAcEvent, _} = lv_event:setup(PinText, ready,
        {login_pin, "chemlabs", PinText}),

    {ok, List} = lv_list:create(Flex),
    ok = lv_obj:set_size(List, {{percent, 100}, {percent, 20}}),

    lists:foreach(fun (I) ->
        {ok, Opt} = lv_list:add_btn(List, none, none),
        {ok, Icon} = lv_label:create(Opt),
        ok = lv_obj:set_style_text_font(Icon, {"lineawesome", regular, 16}),
        ok = lv_label:set_text(Icon, unicode:characters_to_binary([16#f108], utf8)),
        ok = lv_obj:set_size(Icon, {{percent, 3}, content}),

        {ok, Label} = lv_label:create(Opt),
        ok = lv_obj:set_size(Label, {{percent, 80}, content}),
        ok = lv_label:set_text(Label, ["List option ", integer_to_list(I)])
    end, lists:seq(1, 100)),

    case S0#?MODULE.errmsg of
        undefined -> ok;
        ErrMsg ->
            {ok, ErrLbl} = lv_label:create(Flex),
            ok = lv_label:set_text(ErrLbl, ErrMsg),
            ok = lv_obj:set_style_text_color(ErrLbl, lv_color:make(16#FF6060))
    end,

    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),

    ok = lv_indev:set_group(Inst, keyboard, Group),

    {keep_state, S0#?MODULE{evs = [BtnEvent, AcEvent, YkBtnEvent, YkAcEvent]}};

login(info, {'DOWN', MRef, process, _, _}, S0 = #?MODULE{mref = MRef}) ->
    {stop, normal, S0};

login(info, {_Ref, {login, LoginInp, PwInp}}, S0 = #?MODULE{}) ->
    {ok, Login} = lv_textarea:get_text(LoginInp),
    {ok, Pw} = lv_textarea:get_text(PwInp),
    lager:debug("logging in with ~p/~p", [Login, Pw]),
    {next_state, checking_login, S0#?MODULE{login = Login, password = Pw}};

login(info, {_Ref, {login_pin, Login, PinInp}}, S0 = #?MODULE{}) ->
    {ok, Pin} = lv_textarea:get_text(PinInp),
    lager:debug("logging in with account ~p, PIN ~p", [Login, Pin]),
    {next_state, checking_login, S0#?MODULE{login = Login, password = Pin}}.

%% @private
checking_login(enter, _PrevState, S0 = #?MODULE{inst = Inst}) ->
    ok = lv_indev:reset_keyboard(Inst, null),

    {Screen, Flex} = make_flex(S0),
    {ok, _Spinner} = lv_spinner:create(Flex, 1000, 90),
    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),

    {keep_state_and_data, [{state_timeout, 1000, advance}]};

checking_login(info, {'DOWN', MRef, process, _, _}, S0 = #?MODULE{mref = MRef}) ->
    {stop, normal, S0};

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

%% @private
done(enter, _PrevState, #?MODULE{inst = Inst}) ->
    {ok, Screen} = lv_scr:create(Inst),
    {ok, Lbl} = lv_label:create(Screen),
    ok = lv_label:set_text(Lbl, "End of demo!"),
    ok = lv_obj:center(Lbl),
    ok = lv_scr:load_anim(Inst, Screen, fade_in, 500, 0, true),
    keep_state_and_data;

done(info, {'DOWN', MRef, process, _, _}, S0 = #?MODULE{mref = MRef}) ->
    {stop, normal, S0}.
