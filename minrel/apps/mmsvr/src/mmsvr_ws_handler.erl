-module(mmsvr_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("records.hrl").


%Upgrade to websocket
init(Req, State) ->
    {cowboy_websocket, Req, State}.

%Init ws
websocket_init(_State) ->
    {reply, {text, <<"Hello, welcome to the matchmaking service!">>}, not_logged}.

%Handle client messages
websocket_handle({text, Data}, not_logged) ->
    case Data of 
        <<"Login:", D/bitstring>> ->
            case players:simple_login(D) of 
                {ok,Player} ->
                    {reply, {text, jiffy:encode({[{<<"state">>,<<"Logged_in">>},{<<"user_id">>,Player#dbplayer.user_id}]})},Player#dbplayer.user_id};
                not_found -> 
                    {reply, {text, jiffy:encode({[{<<"state">>,<<"Not_Logged_in">>}]})},not_logged}

            end;
        <<"Register:", D/bitstring>> ->
            players:simple_register(D),
            {reply, {text, jiffy:encode({[{<<"state">>,<<"Not_Logged_in">>}]})},not_logged};
        _ ->
            {reply, {text, jiffy:encode({[{<<"state">>,<<"Not_Logged_in">>}]})},not_logged}
    end;

websocket_handle({text, Data}, UserId) ->
    case Data of 
        <<"Enter Queue">> -> 
            {_, User} = players:get_player(UserId),
            match_queue:put_player_in_queue(User,self()),
            QueueSize = match_queue:get_queue_length() -1,
            {reply, {text, jiffy:encode({[{<<"user_id">>,UserId},{<<"queue_size">>,QueueSize}]})},UserId};
        <<"Exit Queue">> ->
            match_queue:remove_player_in_queue(UserId),
            {reply, {text, jiffy:encode({[{<<"user_id">>,UserId},{<<"state">>,<<"Exited">>}]})},UserId};
        <<"Queue Size">> -> 
            QueueSize = match_queue:get_queue_length() -1,
            {reply, {text, jiffy:encode({[{<<"user_id">>,UserId},{<<"queue_size">>,QueueSize}]})},UserId}
    end;


websocket_handle(_Info, State) ->
    {ok, State}.


%Handle erlang messages

websocket_info({found_match, Match},State) ->
    Doc ={[{<<"match_id">>,Match#match.id}]},
    {reply, {text,jiffy:encode(Doc)},State};

websocket_info(_Info, State) ->
    {ok, State}.


%terminate
terminate(_Reason, _Req, UserId) ->
    match_queue:remove_player_in_queue(UserId),
    ok;

terminate(_Reason, Req, _State) ->
    io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
ok.