%%% =========================================================
%%% chat_user.erl
%%% Each user process handles interactive commands
%%% =========================================================

-module(chat_user).
-export([start/2]).

start(Name, ServerPid) ->
    Response = chat_server:register(ServerPid, Name, self()),
    case Response of
        {ok, registered} ->
            spawn(fun() -> loop(Name, ServerPid) end);
        {error, username_taken} ->
            io:format("Username ~p is already taken.~n", [Name]),
            error
    end.

%% Main user loop
loop(Name, ServerPid) ->
    %% Start async receiver
    spawn(fun() -> receive_loop(Name) end),
    command_loop(Name, ServerPid).

%% Command loop for user input
command_loop(Name, ServerPid) ->
    io:format("~p> ", [Name]),
    Input = io:get_line(""),
    Command = string:trim(Input),
    case string:tokens(Command, " ") of
        ["broadcast" | MsgParts] ->
            Msg = string:join(MsgParts, " "),
            chat_server:broadcast(ServerPid, Name, Msg),
            command_loop(Name, ServerPid);

        ["private", To | MsgParts] ->
            Msg = string:join(MsgParts, " "),
            chat_server:send_private(ServerPid, Name, To, Msg),
            command_loop(Name, ServerPid);

        ["list"] ->
            {user_list, Users} = chat_server:list_users(ServerPid),
            io:format("Users online: ~p~n", [Users]),
            command_loop(Name, ServerPid);

        ["logout"] ->
            chat_server:logout(ServerPid, Name),
            io:format("You logged out.~n"),
            ok;

        _ ->
            io:format("Unknown command.~n"),
            command_loop(Name, ServerPid)
    end.

%% Receive loop for incoming messages
receive_loop(Name) ->
    receive
        {incoming_private, From, Msg} ->
            io:format("[PRIVATE] ~p -> ~p: ~p~n", [From, Name, Msg]),
            receive_loop(Name);

        {incoming_broadcast, From, Msg} ->
            io:format("[BROADCAST] ~p says: ~p~n", [From, Msg]),
            receive_loop(Name)
    end.
