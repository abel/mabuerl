%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十二月 2014 13:17
%%%-------------------------------------------------------------------
-module(mabu_server).
-author("yanghaicheng").

%% API
-export([start_listen/2, stop_listen/1]).

%% 开始监听
start_listen(Port, Module) when is_integer(Port), is_atom(Module) ->
  application:ensure_started(mabu_server),
  supervisor:start_child(mabu_listener_sup, [Port, Module]).

stop_listen(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(mabu_listener_sup, Pid).
