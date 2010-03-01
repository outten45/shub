%%% File    : twitter_dm_poller.erl
%%% Author  : Richard Outten
%%% Description : 
-module(twitter_dm_poller).

-import(storage_utils, [get_list_value/2, object_id/1, add_user_to_post/2, 
                       post_to_tuple_list/1, update_since_id_on_user/2,
                       prowl_api_key/1]).

-export([start/1, start/2, check_new_messages/1, cancel/1]).

-include_lib("include/posts.hrl").

%%====================================================================
%% process creator
%%====================================================================
start(User) ->
  start(User, 90000).

start(User, Time) -> 
  spawn(fun() -> loop(Time, {User}) end).

cancel(Pid) -> Pid ! cancel.

loop(Time, ClientUser) ->
  io:format("starting loop ... "),
  receive
    cancel ->
      io:format("stopping!~n"),
	    void
  after Time ->
      io:format("check_new_messages!!~n"),
	    NewClientUser = check_new_messages(ClientUser),
      loop(Time, NewClientUser)
  end.

%%====================================================================
%% result processing functions
%%====================================================================
check_new_messages({Client, User}) ->
  SinceId = get_list_value(<<"since_id">>, User),
  Dms = twitter_client:direct_message_list_since({mochijson2, Client, SinceId}),
  add_posts(User, Dms),
  MaxFun = fun(P, IdentifierPost) -> identifier_max(P, IdentifierPost) end,
  {_MaxIdentifier, Dm} = lists:foldl(MaxFun, {0,#post{}}, Dms),
  NewUser = update_user(User, Dm),
  emongo_storage:update("users", [{"_id", object_id(NewUser)}], NewUser), 
  {Client, NewUser};
check_new_messages({User}) ->
  Tw = storage_utils:twitter_tokens(User),
  Client = twitter_client:start(Tw),
  check_new_messages({Client, User}).

identifier_max(P=#post{identifier=Id}, {Identifier, _Post}) when Id > Identifier ->
  {Id, P};
identifier_max(_Post, IdentifierPost) ->
  IdentifierPost.

add_posts(_User, []) ->
  [];
add_posts(User, [H|T]) ->
  Id = H#post.identifier,
  case emongo_storage:exists("posts", {"identifier",Id}) of
    true ->
      io:format("already posted: ~p~n", [Id]);
    _ ->
      save_and_send(User, H)
  end,
  add_posts(User, T).

save_and_send(User, Post) ->
  PostWithUser = add_user_to_post(User, Post),
  io:format("~p~n", [PostWithUser]),
  emongo_storage:add("posts", post_to_tuple_list(PostWithUser)),
  ProwlApi = prowl_api_key(User),
  prowl:add_async({ProwlApi, "1", "Shub", 
                   "Twitter DM (" ++ Post#post.sender_name ++ ")", 
                   Post#post.text}).


update_user(User, #post{identifier=Id}) when Id =:= undefined ->
  User;
update_user(User, Dm) ->
  update_since_id_on_user(User, Dm#post.identifier).


