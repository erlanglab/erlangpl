-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), epl:log(debug, Str, Args)).
-define(INFO(Str, Args),  epl:log(info, Str, Args)).
-define(WARN(Str, Args),  epl:log(warn, Str, Args)).
-define(ERROR(Str, Args), epl:log(error, Str, Args)).
