%%%-------------------------------------------------------------------
%%% File    : account.erl
%%% Author  : Richard Outten
%%% Description : 
%%%-------------------------------------------------------------------
-module(account).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  Users = emongo_storage:all("users", [{"twitter", [{exists, true}]}, 
                                       {"prowl",   [{exists, true}]}]),
  UserPids = [ {storage_utils:login(U), U, twitter_dm_poller:start(U)} || U <- Users ],
  {ok, UserPids}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  [ twitter_dm_poller:cancel(Pid) || {_Login, _User, Pid} <- State ],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
