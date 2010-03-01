-module(misc_tests).

%% include the test module
-include_lib("eunit/include/eunit.hrl").

utc_parse_feb_24_test() ->
  {ok, {Date, Time}} = misc:parse_utc("Wed Feb 24 09:50:41 +0000 2010"),
  ?assert(Date =:= {2010, 2, 24}),
  ?assert(Time =:= {9,50,41}).
  
