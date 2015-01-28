%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十二月 2014 11:22
%%%-------------------------------------------------------------------
-module(mabu_server_sup).
-author("yanghaicheng").

-behaviour(application).
-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) ->
  ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([]) ->
  SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},

  % TCP Listener
  Listener = {mabu_listener_sup,             % Id       = internal id
    {supervisor, start_link, [{local, mabu_listener_sup}, ?MODULE, [listener]]},  % StartFun = {M, F, A}
    permanent,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []
  },

  % Client instance supervisor
  Client = {mabu_client_sup,                 % Id       = internal id
    {supervisor, start_link, [{local, mabu_client_sup}, ?MODULE, [client]]}, % StartFun = {M, F, A}
    permanent,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
  },

  {ok, {SupFlags, [Listener, Client]}
  };

init([listener]) ->
  SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
  % TCP Listener
  Server =
    {undefined,                                % Id       = internal id
      {mabu_listener, start_link, []},         % StartFun = {M, F, A}
      temporary,                               % Restart  = permanent | transient | temporary
      2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                                  % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
  {ok, {SupFlags, [Server]}};

init([client]) ->
  SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
  % TCP Client
  Server =
    {undefined,                                % Id       = internal id
      {mabu_client, start_link, []},           % StartFun = {M, F, A}
      temporary,                               % Restart  = permanent | transient | temporary
      2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                                  % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
  {ok, {SupFlags, [Server]}}.
