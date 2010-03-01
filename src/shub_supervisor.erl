%%%-------------------------------------------------------------------
%%% File    : shub_supervisor.erl
%%% Author  : Richard Outten
%%% Description : 
%%%-------------------------------------------------------------------
-module(shub_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0, start_in_shell_for_testing/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?SERVER}, ?MODULE, _Arg = []),
    unlink(Pid).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  application:start(emongo),
  {ok,{{one_for_all,3,10}, 
       [{prowl,
         {prowl, start_link, []},
         permanent,
         10000,
         worker,
         [prowl]},
        {emongo_storage,
         {emongo_storage, start_link, []},
         permanent,
         10000,
         worker,
         [emongo_storage]},
        {account,
         {account, start_link, []},
         permanent,
         10000,
         worker,
         [account]}
       ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
