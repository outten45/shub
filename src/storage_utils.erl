%%% File    : storage_utils.erl
%%% Author  : Richard Outten 
%%% Description : Helper methods for working the storage meduim.
-module(storage_utils).

-export([connection_info/1, get_list_value/2, convert_to_list/1, pool_name/0]).

-export([add_user_to_post/2, post_to_tuple_list/1, twitter_tokens/1,
         twitter_token_parts/1, object_id/1, login/1, 
         update_since_id_on_user/2, prowl_api_key/1]).

-include_lib("include/posts.hrl").

%%====================================================================
%% list manipulations
%%====================================================================
get_list_value(Key, PL) ->
  convert_to_list(proplists:get_value(Key, PL)).

convert_to_list(V) when is_integer(V) ->
  V;
convert_to_list(V) when is_binary(V) ->
  binary_to_list(V);
convert_to_list(V) ->
  V.


%%====================================================================
%% storage pool and connection information
%%====================================================================
connection_info(State) ->
  proplists:get_value(pool, State).

pool_name() ->
  [{pool, shub}].


%%====================================================================
%% record and list functions for working with users and posts
%%====================================================================

%% Added user information to the post (user and login)
add_user_to_post(User, Post) ->
  UserId = get_list_value(<<"_id">>, User),
  Login = get_list_value(<<"login">>, User),
  Post#post{user=UserId,login=Login}.

update_since_id_on_user(User, SinceId) ->
  UserDict = dict:from_list(User),
  NewUserDict = dict:store(<<"since_id">>, SinceId, UserDict),
  dict:to_list(NewUserDict).

post_to_tuple_list(#post{identifier=Id, 
                         created_at=Ca, 
                         sender_name=Sn, 
                         recipient_name=Rn, 
                         subject=S, 
                         text=T, 
                         service=Sv,
                         type=Ty,
                         user=U,
                         login=L}) ->
  [{identifier,Id}, {created_at, Ca}, {sender_name, Sn},
   {recipient_name, Rn}, {subject, S}, {text, T},
   {service, Sv}, {type, Ty}, {user,U}, {login, L}].


twitter_tokens(User) ->
  Tw = proplists:get_value(<<"twitter">>, User),
  twitter_token_parts(Tw).

twitter_token_parts(Tw) when length(Tw) =:= 4 ->
  Token = get_list_value(<<"token">>, Tw),
  Secret = get_list_value(<<"secret">>, Tw),
  AToken = get_list_value(<<"atoken">>, Tw),
  ASecret = get_list_value(<<"asecret">>, Tw),
  {Token, Secret, AToken, ASecret};
twitter_token_parts(Tw) ->
  Token = get_list_value(<<"token">>, Tw),
  Secret = get_list_value(<<"secret">>, Tw),
  {Token, Secret}.

object_id(Obj) ->
  proplists:get_value(<<"_id">>, Obj).

login(User) ->
  get_list_value(<<"login">>, User).

prowl_api_key(User) ->
  Prowl = proplists:get_value(<<"prowl">>, User),
  get_list_value(<<"apikey">>, Prowl).

