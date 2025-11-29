-module(chat_util).
-export([timestamp/0, pretty/1]).

timestamp() ->
    {{Y,M,D},{H,Min,S}} = erlang:localtime(),
    io_lib:format("~p-~p-~p ~p:~p:~p", [Y,M,D,H,Min,S]).

pretty(Text) when is_list(Text) ->
    lists:flatten(Text);
pretty(Other) ->
    io_lib:format("~p", [Other]).
