-define(prowl_api_url(Action), "https://prowl.weks.net/publicapi/" ++ Action).

-record(prowl_success, {code, remaining, resetdate}).
-record(prowl_error,{code, message}).

