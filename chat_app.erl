%%% =========================================================
%%% chat_app.erl
%%% Single-shell interactive chat
%%% =========================================================

-module(chat_app).
-export([start/0]).

start() ->
    Server = chat_server:start(),

    %% Start users
    chat_user:start(alex, Server),
    chat_user:start(sam, Server),
    chat_user:start(rae, Server),

    io:format("Chat server running! Users: alex, sam, rae~n"),
    command_loop(Server).

%% =========================
%% Main interactive loop
%% =========================
command_loop(Server) ->
    io:format("Enter command (format: username> command):~n"),
    Input = io:get_line(""),
    case string:trim(Input) of
        "" ->
            command_loop(Server);
        Line ->
            process_line(Line, Server),
            command_loop(Server)
    end.

process_line(Line, Server) ->
    case string:tokens(Line, ">") of
        [UserStr, CmdStr] ->
            User = string:trim(UserStr),
            Cmd = string:trim(CmdStr),
            execute_command(User, Cmd, Server);
        _ ->
            io:format("Invalid input format. Use username> command~n")
    end.

execute_command(User, Cmd, Server) ->
    case string:tokens(Cmd, " ") of
        ["broadcast" | MsgParts] ->
            Msg = string:join(MsgParts, " "),
            chat_server:broadcast(Server, User, Msg);

        ["private", To | MsgParts] ->
            Msg = string:join(MsgParts, " "),
            chat_server:send_private(Server, User, To, Msg);

        ["list"] ->
            {user_list, Users} = chat_server:list_users(Server),
            io:format("Users online: ~p~n", [Users]);

        ["logout"] ->
            chat_server:logout(Server, User),
            io:format("~p logged out.~n", [User]);

        _ ->
            io:format("Unknown command.~n")
    end.
