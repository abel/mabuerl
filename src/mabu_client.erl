%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十二月 2014 11:24
%%%-------------------------------------------------------------------
-module(mabu_client).
-author("yanghaicheng").

-include("mabu_record.hrl").
-export([start_link/1, start_client/2, send/2, close/1]).

-define(TIMEOUT, 30000).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
start_link(Module) when is_atom(Module) ->
  case spawn_link(fun() -> init(Module) end) of
    Pid when is_pid(Pid) ->
      {ok, Pid};
    Error ->
      {error, Error}
  end.

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  Pid ! {socket_ready, Socket}.

send(Pid, Data) when is_pid(Pid) ->
  Pid ! {send_data, Data}.

close(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(mabu_client_sup, Pid).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init(Module) when is_atom(Module) ->
  process_flag(trap_exit, true),
  wait_for_socket(#client_state{module = Module}).

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------

wait_for_socket(State) ->
  receive
    {socket_ready, Socket} when is_port(Socket) ->
      inet:setopts(Socket, [{active, true}, {packet, 2}, binary]),
      {ok, Addr} = inet:peername(Socket),
      error_logger:info_msg("~p Client ~p connected.\n", [self(), Addr]),

      %NewState = #client_state{module = Module, socket = Socket, addr = Addr},
      NewState = State#client_state{socket = Socket, addr = Addr},
      % #client_state{module = Module} = State,
      Module = State#client_state.module,

      Module:on_connect(self(), NewState),
      wait_for_data(NewState);
    Other ->
      error_logger:error_msg("State: wait_for_socket. Unexpected message: ~p\n", [Other]),
      %% Allow to receive async messages
      wait_for_socket(State)
  end.

wait_for_data(#client_state{socket = Socket, addr = Addr, module = Module} = State) ->
  receive
    {tcp, Socket, Data} ->
      Module:on_receive(self(), Data),
      %% inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
      wait_for_data(State);
    {send_data, Data} ->
      ok = gen_tcp:send(Socket, Data),
      wait_for_data(State);
    {tcp_closed, Socket} ->
      error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
      Module:on_disconnect(self(), State),
      exit(normal);
    Data ->
      io:format("~p Ignoring data: ~p\n", [self(), Data]),
      wait_for_data(State)
  after ?TIMEOUT ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    %%wait_for_close(State)
    gen_tcp:close(Socket),
    Module:on_disconnect(self(), Addr),
    exit(normal)
  end.

wait_for_close(#client_state{socket = Socket, addr = Addr, module = Module} = State) ->
  receive
    {send_data, Data} ->
      ok = gen_tcp:send(Socket, Data),
      wait_for_close(State);
    {tcp_closed, Socket} ->
      error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
      Module:on_disconnect(self(), Addr),
      (catch gen_tcp:close(Socket)),
      exit(normal);
    Data ->
      io:format("~p Ignoring data: ~p\n", [self(), Data]),
      wait_for_close(State)
  after ?TIMEOUT ->
    Module:on_disconnect(self(), Addr),
    (catch gen_tcp:close(Socket)),
    exit(normal)
  end.
