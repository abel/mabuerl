%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十二月 2014 21:14
%%%-------------------------------------------------------------------
-module(mabu_echo).
-author("yanghaicheng").
-include("mabu_record.hrl").
-behaviour(mabu_protocol).

%% API
-export([on_connect/2, on_receive/2, on_disconnect/2]).

on_receive(Pid, Data) ->
  mabu_client:send(Pid, Data).

on_connect(Pid, #client_state{socket = _Socket, addr = Addr, module = _Module} = _State) ->
  error_logger:info_msg("~p Echo Client ~p connected.\n", [Pid, Addr]),
  ok.

on_disconnect(Pid, #client_state{socket = _Socket, addr = Addr, module = _Module} = _State) ->
  error_logger:info_msg("~p Echo Client ~p disconnected.\n", [Pid, Addr]),
  ok.