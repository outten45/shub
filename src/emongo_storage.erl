%%%-------------------------------------------------------------------
%%% File    : emongo_storage.erl
%%% Author  : Richard Outten 
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(emongo_storage).

-behaviour(gen_server).

-import(storage_utils, [connection_info/1]).

%% API
-export([start_link/0, all/1, all/2, all/3, first/2, user_by_login/1, 
         update/3, update/4, twitter_tokens/1, exists/2, add/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all(Collection) ->
  all(Collection, [], []).

all(Collection, Selector) ->
  all(Collection, Selector, []).

all(Collection, Selector, Options) ->
  gen_server:call(?MODULE, {all, Collection, Selector, Options}).

first(Collection, Id) ->
  gen_server:call(?MODULE, {first, Collection, Id}).

user_by_login(Login) ->
  gen_server:call(?MODULE, {login, "users", Login}).

update(Collection, Selector, Rec) ->
  update(Collection, Selector, Rec, false).

update(Collection, Selector, Rec, Upsert) ->
  gen_server:call(?MODULE, {update, Collection, Selector, Rec, Upsert}).

exists(Collection, Identifier) ->
  gen_server:call(?MODULE, {exists, Collection, Identifier}).

add(Collection, Rec) ->
  gen_server:call(?MODULE, {add, Collection, Rec}).


%%====================================================================
%% query twitter info
%%====================================================================
twitter_tokens({login, Login}) ->
  [User] = user_by_login(Login),
  storage_utils:twitter_tokens(User).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init(ServerConfig) when length(ServerConfig) =:= 0 ->
  io:format("~p starting~n",[?MODULE]),
  {ok, storage_utils:pool_name()};
init(ServerConfig) ->
  {ok, ServerConfig}.  

handle_call({all, Collection, Selector, Options}, _From, State) ->
  Pool = connection_info(State),
  Result = emongo:find(Pool, Collection, Selector, Options),
  {reply, Result, State};
handle_call({first, Collection, Id}, _From, State) ->
  Pool = connection_info(State),
  Result = emongo:find_one(Pool, Collection, [{"_id", {oid, Id} }]),
  {reply, Result, State};
handle_call({login, Collection, Login}, _From, State) ->
  Pool= connection_info(State),
  Result = emongo:find_one(Pool, Collection, [{"login", Login}]),
  {reply, Result, State};
handle_call({update, Collection, Selector, Rec, Upsert}, _From, State) ->
  Pool = connection_info(State),
  Result = emongo:update(Pool, Collection, Selector, Rec, Upsert),
  {reply, Result, State};
handle_call({exists, Collection, Identifier}, _From, State) ->
  Reply = does_exists(Collection, Identifier, State),
  {reply, Reply, State};
handle_call({add, Collection, Rec}, _From, State) ->
  Pool = connection_info(State),
  Result = emongo:insert(Pool, Collection, Rec),
  {reply, Result, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p stopping~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
does_exists(Collection, Identifier, State) ->
  Pool = connection_info(State),
  Result = emongo:find_one(Pool, Collection, [Identifier]),
  does_exists(Result).


does_exists([]) ->
  false;
does_exists(_Result) ->
  true.

