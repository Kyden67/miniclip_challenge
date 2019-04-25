-module(players).

-include("records.hrl").

-export([start/0, create_new_player/2, simple_login/1, get_player/1,test_users/0,simple_register/1]).

%%@doc
%%Start players process by opening the disk table for Users 
%%@end
start() ->
    dets:open_file(players_table, [{keypos, 2}]),
    {ok, self()}.

%%@doc
%%Create new player by Nameand Pass
%%@end
create_new_player(Name, Password) ->
    UserId = dets:info(players_table,size) + 1,
	User = #dbplayer{
        user_id = UserId,
		username = Name,
		pass = Password
	},
	dets:insert(players_table, User),
    User.

%%@doc
%%Simple check pass without encryption 
%%@end
check_pass(Player, P) ->
	Pass = Player#dbplayer.pass,
	if
		Pass == P ->
			ok;
		true ->
			not_ok	
end.

%%@doc
%%A simple Register for binary data with username and password  
%%@end
simple_register(D) ->
	{Comma_pos, _} = binary:match(D, [<<",">>]),
	Username = binary_to_list(binary:part(D, {0, Comma_pos})),
	Pass = binary_to_list(binary:part(D, {Comma_pos+1, byte_size(D) - (Comma_pos+1)})),
	create_new_player(Username, Pass).

%%@doc
%%A simple Login for binary data with userid and password  
%%@end
simple_login(D) ->
	{Comma_pos, _} = binary:match(D, [<<",">>]),
	UserId = list_to_integer(binary_to_list(binary:part(D, {0, Comma_pos}))),
	Pass = binary_to_list(binary:part(D, {Comma_pos+1, byte_size(D) - (Comma_pos+1)})),
	case players:get_player(UserId) of
		{ok, Player} ->		%% user found, check pass
			case check_pass(Player, Pass) of
				ok ->
					{ok, Player};
				not_ok ->
					not_found
			end;

		not_found ->
			not_found;
		error ->
			not_found
end.


%%@doc
%%Look for player in disk table by User ID  
%%@end
get_player(UserId) ->
	case dets:lookup(players_table, UserId) of 
		[] ->
			not_found;
		{error, Reason} ->
			io:format("ERROR: ~p~n", [Reason]),
			error;
        [User] ->
			{ok, User}
end.

%%@doc
%%Create new player by Name, Pass and mmr  
%%@end
create_new_player(Name, Password,Mmr) ->
    UserId = dets:info(players_table,size) + 1,
	User = #dbplayer{
        user_id = UserId,
		username = Name,
		pass = Password,
        mmr = Mmr
	},
	dets:insert(players_table, User),
    User.

%%@doc
%%create a set of test Users for disk table with Password="1234"  and random mmr 
%%@end
test_users() ->
    lists:foreach(
        fun(A) ->
            Username = "user" ++ erlang:integer_to_list(A),
            Mmr = rand:uniform(1500),
            create_new_player(Username,"1234",Mmr)
        end,
        lists:seq(1, 5000)).
