%% Twitter client that uses the erlang-oauth library.  It based on
%% erlang-oauth-examples.
%% 

-module (twitter_client).
-compile(export_all).

-define (REQUEST_TOKEN_URL, "http://twitter.com/oauth/request_token").
-define (AUTH_URL, "http://twitter.com/oauth/authorize").
-define (ACCESS_TOKEN_URL, "http://twitter.com/oauth/access_token").
-define (direct_messages_url(Format), "http://twitter.com/direct_messages." ++ Format).
-define (RATE_LIMIT_STATUS_URL, "http://twitter.com/account/rate_limit_status.json").
-define (SERVICE, "twitter").

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("include/posts.hrl").

start({ConsumerKey, ConsumerSecret}) ->
  {ok, Client} = oauth_start({ConsumerKey, ConsumerSecret, hmac_sha1}),
  get_pin_and_access_token(Client),
  Client;
start({ConsumerKey, ConsumerSecret, Token, Secret}) ->
  {ok, Client} = oauth_start({ConsumerKey, ConsumerSecret, hmac_sha1}),
  set_access_token(Client, Token, Secret),
  Client.


%%============================================================================
%% oauth client calls
%%============================================================================
oauth_start(Consumer) ->
  oauth_client:start(Consumer).

set_access_token(Client, Token, Secret) ->
  oauth_client:set_access_token_params(Client, Token, Secret).

get_pin_and_access_token(Client) ->
  {ok, Token} = get_request_token(Client),
  {ok, Verifier} = get_pin(Token),
  get_access_token(Client, lists:concat(Verifier)).

get_pin(Token) ->
  AuthorizeURL = authorize_url(Token),
  io:format("Go to the following URL and get PIN:~n  ~p~n", [AuthorizeURL]),
  io:fread("Enter PIN> ","~s").

get_request_token(Client) ->
  oauth_client:get_request_token(Client, ?REQUEST_TOKEN_URL).

authorize_url(Token) ->
  oauth:uri(?AUTH_URL, [{"oauth_token", Token}]).

get_access_token(Client, Verifier) ->
  oauth_client:get_access_token(Client, ?ACCESS_TOKEN_URL, [{"oauth_verifier", Verifier}]).

access_token_params(Client) ->
  oauth_client:access_token_params(Client).

get_direct_messages({xml, Client, Params}) ->
  oauth_client:get(Client, ?direct_messages_url("xml"), Params);
get_direct_messages({json, Client, Params}) ->
  oauth_client:get(Client, ?direct_messages_url("json"), Params);
get_direct_messages({mochijson2, Client, Params}) ->
  get_direct_messages({json, Client, Params});
get_direct_messages({Client, Params}) ->
  get_direct_messages({json, Client, Params}).

get_rate_limit_status(Client) ->
  oauth_client:get(Client, ?RATE_LIMIT_STATUS_URL, []).

%%============================================================================
%% twitter API calls
%%============================================================================
%%
%%   returns a list of direct_message records populated based on format
%% 
direct_message_list_since({Format, Client, SinceId}) ->
  direct_message_list({Format, Client, [{"since_id", SinceId}]}).

direct_message_list({Format, Client}) ->
  direct_message_list({Format, Client, []});
direct_message_list({Format, Client, Params}) ->
  {ok, _Headers, Result} = get_direct_messages({Format, Client, Params}),
  DirectMessages = parse_direct_message({Format, Result}),
  [parse_direct_message_record({Format, Dm}) || Dm <- DirectMessages].

rate_limit_status(Client) ->
  {ok, _Headers, Json} = get_rate_limit_status(Client),
  mochijson2:decode(Json).

%%============================================================================
%% Parsing
%%============================================================================
parse_direct_message({xml, Result}) ->
  xmerl_xpath:string("//direct-messages/direct_message", Result);
parse_direct_message({json, Result}) ->
  jsonerl:decode(Result);
parse_direct_message({mochijson2, Result}) ->
  mochijson2:decode(Result).

parse_direct_message_record({xml, Dm}) ->
  [ #xmlText{value=Id} ]  = xmerl_xpath:string("id/text()", Dm),
  [ #xmlText{value=CreatedAt} ]  = xmerl_xpath:string("created_at/text()", Dm),
  [ #xmlText{value=SenderScreenName} ]  = xmerl_xpath:string("sender_screen_name/text()", Dm),
  [ #xmlText{value=RecipientScreenName} ]  = xmerl_xpath:string("recipient_screen_name/text()", Dm),
  Text = get_xml_test_value(xmerl_xpath:string("text/text()", Dm)),
  #post{identifier=list_to_integer(Id), 
        created_at=CreatedAt, 
        sender_name=SenderScreenName, 
        recipient_name=RecipientScreenName, 
        text=Text, 
        service=?SERVICE, 
        type="direct_message"};
parse_direct_message_record({json, Dms}) ->
  {{_,CreatedAt},_sid,{_,SenderScreenName},{_,RecipientScreenName},
   _rid,_rep,_sdr,{_,Id},{_,Text}} = Dms,
  #post{identifier=Id, 
        created_at=binary_to_list(CreatedAt), 
        sender_name=binary_to_list(SenderScreenName), 
        recipient_name=binary_to_list(RecipientScreenName), 
        text=binary_to_list(Text),
        service=?SERVICE,
        type="direct_message"};
parse_direct_message_record({mochijson2, Dms}) ->
  {struct, Dm} = Dms,
  Id = proplists:get_value(<<"id">>, Dm),
  CreatedAt = proplists:get_value(<<"created_at">>, Dm),
  SenderScreenName = proplists:get_value(<<"sender_screen_name">>, Dm),
  RecipientScreenName = proplists:get_value(<<"recipient_screen_name">>, Dm),
  Text = proplists:get_value(<<"text">>, Dm),
  #post{identifier=Id,
        created_at=binary_to_list(CreatedAt), 
        sender_name=binary_to_list(SenderScreenName), 
        recipient_name=binary_to_list(RecipientScreenName), 
        text=binary_to_list(Text),
        service=?SERVICE,
        type="direct_message"}.

get_xml_test_value([ #xmlText{value=Val} ]) ->
  Val;
get_xml_test_value(#xmlText{value=Val}) ->
  Val;
get_xml_test_value([H|T]) ->
  get_xml_test_value(H) ++ get_xml_test_value(T).

