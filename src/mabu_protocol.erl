%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2015 16:45
%%%-------------------------------------------------------------------
-module(mabu_protocol).
-author("yanghaicheng").

-include("mabu_record.hrl").

%% API
-callback on_receive(Pid::pid(), Data::any()) ->
  ok.

-callback on_connect(Pid::pid(), _State::any()) ->
  ok.

-callback on_disconnect(Pid::pid(), State::any()) ->
  ok.