%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(eredis_pool).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

%% API
-export([start/0, start/2, stop/0, stop/1]).
-export([q/2, q/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, eredis_sup}, ?MODULE, []).

stop(_State) -> 
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%%
%% WorkerはcheckoutしてPidを得た後にすぐさまcheckinしています。
%% eredisがnon-blockingなクエリ処理をする仕組みを活かす為です。
%%
%% @end
%%--------------------------------------------------------------------
-spec q(Client::pid(), Command::iolist()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

q(PoolName, Command) ->
    q(PoolName, Command, ?TIMEOUT).

-spec q(Client::pid(), Command::iolist(), Timeout::integer()) ->
               {ok, binary() | [binary()]} | {error, Reason::binary()}.

q(PoolName, Command, Timeout) ->
    Worker = poolboy:checkout(PoolName),
    poolboy:checkin(PoolName, Worker),
    Reply = eredis:q(Worker, Command, Timeout),
    Reply.
