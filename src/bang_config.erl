-module(bang_config).

-export([authInterval/0, sessionInterval/0]).

%%timeouts in seconds
authInterval() -> 60.
sessionInterval() -> 3600.