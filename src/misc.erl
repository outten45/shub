%%% File    : misc.erl
%%% Author  : Richard Outten
%%% Description : 
-module(misc).

-export([sleep/1, parse_utc/1]).

sleep(T) ->
  receive
  after T ->
      true
  end.

%% "Wed Feb 24 09:50:41 +0000 2010"
parse_utc(Val) ->
  {ok, [_WeekDay, Month, Day, Hour, Min, Sec, _Offset, Year], []} = 
    io_lib:fread("~3s ~3s ~2d ~2d:~2d:~2d ~5s ~4d",Val),
  MonthNum = http_util:convert_month(Month),
  {ok,{{Year,MonthNum,Day},{Hour,Min,Sec}}}.

