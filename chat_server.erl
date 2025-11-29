%%% =========================================================
%%% chat_server.erl
%%% Central chat server: manages users, broadcasts, private messages
%%% =========================================================

-module(chat_server).

%% exported functions
-export([start/0, register/3, logout/2, send_private/4, broadcast/3, list_users/1]).

-record(user, {name, pid}).

%% =========================
%% Start server
%% =========================
start() ->
    spawn(fun() -> loop([], running) end).

%% =========================
%% Top-level wrappers
%% =========================

register(ServerPid, Name, UserPid) ->
    ServerPid ! {register, Name, UserPid, self()},
    receive
        Msg -> Msg
    end.

logout(ServerPid, Name) ->
    ServerPid ! {logout, Name},
    ok.

send_private(ServerPid, From, To, Text) ->
    ServerPid ! {private_msg, From, To, Text},
    ok.

broadcast(ServerPid, From, Text) ->
    ServerPid ! {broadcast, From, Text},
    ok.

list_users(ServerPid) ->
    ServerPid ! {list_users, self()},
    receive
        Msg -> Msg
    end.

%% =========================
%% Main loop
%% =========================
loop(Users, running) ->
    receive
        {register, Name, UserPid, From} ->
            case lists:keyfind(Name, #user.name, Users) of
                false ->
                    NewUser = #user{name=Name, pid=UserPid},
                    From ! {ok, registered},
                    io:format("~p connected.~n", [Name]),
                    loop([NewUser | Users], running);
                _ ->
                    From ! {error, username_taken},
                    loop(Users, running)
            end;

        {logout, Name} ->
            Filtered = lists:filter(fun(U) -> U#user.name =/= Name end, Users),
            io:format("~p logged out.~n", [Name]),
            loop(Filtered, running);

        {private_msg, FromUser, ToUser, Text} ->
            case lists:keyfind(ToUser, #user.name, Users) of
                false ->
                    io:format("Failed: ~p tried to send to unknown user ~p.~n", [FromUser, ToUser]),
                    loop(Users, running);
                #user{pid=Pid} ->
                    Pid ! {incoming_private, FromUser, Text},
                    loop(Users, running)
            end;

        {broadcast, FromUser, Text} ->
            lists:foreach(
                fun(#user{name=Name, pid=Pid}) ->
                    if Name =:= FromUser -> ok;
                       true -> Pid ! {incoming_broadcast, FromUser, Text}
                    end
                end,
                Users),
            loop(Users, running);

        {list_users, From} ->
            Names = [U#user.name || U <- Users],
            From ! {user_list, Names},
            loop(Users, running)
    end.
