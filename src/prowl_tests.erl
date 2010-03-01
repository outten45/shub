-module(prowl_tests).

%% include the test module
-include_lib("eunit/include/eunit.hrl").


%% add_params_without_provider_key_test() ->
%%   ?assert("apikey=123KEY&priority=1&application=Test%20App&event=Send&providerkey=&description=Send%20description" =:= 
%%           prowl:add_param_string({"123KEY", 1, "Test App", "Send", "Send description"})).

%% verify_params_without_provider_key_test() ->
%%   ?assert("apikey=123KEY&providerkey=" =:= prowl:verify_params_string({"123KEY"})).


%% check_error_return_test() ->
%%   Body = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
%% <prowl>
%% 	<error code=\"ERRORCODE\">ERRORMESSAGE</error>
%% </prowl>",
%%   {Status, Rec} = prowl:check_return({200, [], Body}),
%%   ?assert(Status =:= error),
%%   %% a little hackish, records are really tuples 
%%   ?assert(Rec =:= {prowl_error, "ERRORCODE","ERRORMESSAGE"}). 


%% check_success_return_test() ->
%%   Body = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
%% <prowl>
%% 	<success code=\"200\" remaining=\"400\" resetdate=\"1265774040\" />
%% </prowl>",
%%   {Status, Rec} = prowl:check_return({200, [], Body}),
%%   ?assert(Status =:= ok),
%%   %% a little hackish, records are really tuples 
%%   ?assert(Rec =:= {prowl_success, "200", "400", "1265774040"}).


