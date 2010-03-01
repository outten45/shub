%% "prowl" provide basic handling of notifications sent to prowl.
%%  
%% Sending a message to prowl:
%%  
%%   prowl:start().
%% 
%%   Params = {"API_KEY", 1, "Test", "Erlang", "testing message from erlang"}.
%%
%%   prowl:add(Params).
%%
%% Verifing call to prowl api:
%%
%%   prowl:start().
%% 
%%   Params = {"API_KEY"}.
%%  
%%   prowl:verify(Params).
%%
-module (prowl).

-behaviour(gen_server).

%% API
-export([start_link/0, add/1, verify/1, add_async/1]).

-export([add_param_string/1, verify_params_string/1, check_return/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("include/prowl_records.hrl").


%%====================================================================
%% API
%%====================================================================
start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Params) ->
  gen_server:call(?MODULE, {add, Params}).

verify(Params) ->
  gen_server:call(?MODULE, {verify, Params}).

add_async(Params) -> 
  gen_server:cast(?MODULE, {send, Params}).

%%============================================================================
%% gen_server callbacks
%%============================================================================
init([]) ->
  process_flag(trap_exit, true),
  io:format("~p starting~n",[?MODULE]),
  {ok, []}.

handle_call({add, Params}, _From, State) ->
%%   ParamsString = add_param_string(Params),
%%   Url = string:join([?prowl_api_url("add"), "?", ParamsString], ""),
%%   Req = {Url, []},
%%   {Status, Result} = check_return(basic_http_get(Req)),
  {Status, Result} = send_message(Params),
  {reply, {Status, Result}, State};
handle_call({verify, Params}, _From, State) ->
  ParamsString = verify_params_string(Params),
  Url = string:join([?prowl_api_url("verify"),"?",ParamsString], ""),
  {Status, Headers, Body} = basic_http_get({Url, []}),
  {reply, {Status, Headers, Body}, State}.

handle_cast({send, Params}, State) ->
  send_message(Params),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("~p stopping~n",[?MODULE]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%============================================================================
%% Prowl parameter helper methods
%%============================================================================
send_message(Params) ->
  ParamsString = add_param_string(Params),
  Url = string:join([?prowl_api_url("add"), "?", ParamsString], ""),
  Req = {Url, []},
  check_return(basic_http_get(Req)).

add_param_string({ApiKey, Priority, Application, Event, Description}) ->
  add_param_string({ApiKey, "", Priority, Application, Event, Description});
add_param_string({ApiKey, ProviderKey, Priority, Application, Event, Description}) ->
  params_to_string([{"apikey", ApiKey}, {"priority", Priority},
                              {"application", Application}, {"event", Event},
                              {"providerkey", ProviderKey}, {"description", Description}]).

verify_params_string({ApiKey}) ->
  verify_params_string({ApiKey, ""});
verify_params_string({ApiKey, ProviderKey}) ->
  params_to_string([{"apikey", ApiKey}, {"providerkey", ProviderKey}]).


%%============================================================================
%% Parsing results
%%============================================================================
check_return({Status, _Headers, Body}) when Status =:= 200 ->
  {XML, []} = xmerl_scan:string(Body),
  parse_result(XML).

parse_result(XML) ->
  ErrorBlock = xmerl_xpath:string("/prowl/error", XML),
  case ErrorBlock of
    [] ->
      parse_result(success, XML);
    _ ->
      parse_result(error, XML)
  end.

parse_result(success, XML) ->
  [ #xmlAttribute{value=Code} ] = xmerl_xpath:string("/prowl/success/@code", XML),
  [ #xmlAttribute{value=Remaining} ] = xmerl_xpath:string("/prowl/success/@remaining", XML),
  [ #xmlAttribute{value=ResetDate} ] = xmerl_xpath:string("/prowl/success/@resetdate", XML),
  {ok, #prowl_success{code=Code, remaining=Remaining, resetdate=ResetDate}};
parse_result(error, XML) ->
  [ #xmlAttribute{value=Code} ] = xmerl_xpath:string("/prowl/error/@code", XML),
  [ #xmlText{value=Message} ] = xmerl_xpath:string("/prowl/error/text()", XML),
  {error, #prowl_error{code=Code, message=Message}}.

  
%%============================================================================
%% Helpers
%%============================================================================
basic_http_get(Req) ->
  {ok, {{_, Status, _}, Headers, Body}} = http:request(get, Req, [], []),
  {Status, Headers, Body}.

params_to_string(Params) ->
  Parts = [ param_set(Key,Val) || {Key, Val}  <- Params ],
  string:join(Parts, "&").

param_set(K,V) when is_number(V) ->
  param_set(K, integer_to_list(V));
param_set(K,V) ->
  string:join([K, edoc_lib:escape_uri(V)], "=").

