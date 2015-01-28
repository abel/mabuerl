%%%-------------------------------------------------------------------
%%% @author yanghaicheng
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2014 11:08
%%%-------------------------------------------------------------------
-author("yanghaicheng").


-record(client_state, {
  socket,    % client socket
  addr,      % client address
  module     % 消息外理模块
}).