-module(bang_config).

-export([authInterval/0, sessionInterval/0]).

%%timeouts in milliseconds
authInterval() -> 600000.
sessionInterval() -> 3600000.