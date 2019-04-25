-module(match_queue).

-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, put_player_in_queue/2, remove_player_in_queue/1, look_players_in_queue/1]).
-export([start_matchmaker/0, get_queue_length/0]).

%%@doc
%%Start Queue process by creating both players and matches tables on RAM  and 
%%creating a periodic timer to start the matchmaking between players 
%%@end
start() ->
    ets:new(players_in_queue, [public, named_table, ordered_set, {keypos, 2}]),
    ets:new(matches_in_progress,[public, named_table, ordered_set, {keypos, 2}]),
    timer:apply_interval(3000,?MODULE, start_matchmaker,[]),
    {ok, self()}.


%%@doc
%%convert and put Player from dets in players table with socket pid   
%%@end
put_player_in_queue(User,SocketPid) ->
    Player= #qplayer{   user_id = User#dbplayer.user_id,
                        username = User#dbplayer.username,
                        mmr =  User#dbplayer.mmr,
                        socket_pid = SocketPid},
    ets:insert(players_in_queue, Player).

%%@doc
%%Remove player of players table
%%@end
remove_player_in_queue(User_id) ->
    ets:delete(players_in_queue, User_id).

%%@doc
%%Notify Player socket handler and remove him of players table   
%%@end
found_match(Player,Match) ->
    Player#qplayer.socket_pid ! {found_match, Match},
    remove_player_in_queue(Player#qplayer.user_id).


%%@doc
%%Create and run a query to look for a matching Opponent according to 
%%Player's mmr and retries   
%%@end
look_players_in_queue(#qplayer{user_id=User_id, mmr=Mmr}) ->
    QH= qlc:q([P#qplayer.user_id ||  P <- ets:table(players_in_queue),
                                    P#qplayer.mmr >= Mmr - 100 - (40*P#qplayer.retries),
                                    P#qplayer.mmr =< Mmr + 100 + (40*P#qplayer.retries),
                                    P#qplayer.user_id =/= User_id]),
    QC = qlc:cursor(QH),
    Result=qlc:next_answers(QC, 1),
    qlc:delete_cursor(QC),
    Result.

%%@doc
%%Get Number of Players in Queue   
%%@end
get_queue_length() ->
    ets:info(players_in_queue,size).

%%@doc
%%Start the matchmaker if there is at least 2 players in queue  
%%@end
start_matchmaker() ->
    Size = get_queue_length(),
    if
        Size>1 ->
            PlayerId = ets:first(players_in_queue),
            matchmaker(PlayerId);
        true ->
            {ok}
    end.

%%@doc
%%Create a match between 2 players  
%%@end
create_match(P1_id,P2_id) ->
    Match= #match{  id = erlang:system_time(),
                    players = [P1_id,P2_id]},
    ets:insert(matches_in_progress, Match),
    Match.

%%@doc
%%Run through all the players in table to matchmaking with someone   
%%@end
matchmaker(PlayerId) ->
    [Player] = ets:lookup(players_in_queue, PlayerId),
    Retries = Player#qplayer.retries,
    case look_players_in_queue(Player) of
        [OpponentId] ->
            Match = create_match(PlayerId,OpponentId),
            [Opponent] = ets:lookup(players_in_queue, OpponentId),
            found_match(Opponent,Match),
            NextPlayer = ets:next(players_in_queue,PlayerId),
            found_match(Player,Match),
            io:format("~s vs ~s~n",[Player#qplayer.username,Opponent#qplayer.username]);
        []  ->
            ets:update_element(players_in_queue, PlayerId, {#qplayer.retries,Retries+1}),
            NextPlayer = ets:next(players_in_queue,PlayerId)
    end,
    case NextPlayer of 
        '$end_of_table' ->
            {ok};
        _ ->
            matchmaker(NextPlayer)
    end.


%%test_users() ->
%%    lists:foreach(
%%        fun(A) ->
 %%           Username = "user" ++ erlang:integer_to_list(A),
 %%           Mmr = rand:uniform(1500),
 %%           put_player_in_queue(#qplayer{user_id=A, username= Username, mmr=Mmr, retries=0})
%%        end,
 %%       lists:seq(1, 5000)).
