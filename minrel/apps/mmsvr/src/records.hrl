-record(dbplayer, {
    user_id         :: pos_integer(),
    username        :: bitstring(),
    pass            :: bitstring(),
    mmr = 600       :: pos_integer()
}).

-record(qplayer,{
    user_id         :: pos_integer(),
    username        :: bitstring(),
    mmr             :: pos_integer(),
    retries = 0     :: pos_integer(),
    socket_pid      :: undefined | pid()     
   }).
-record(match, {
    id              :: integer(),
    players=[]      :: [pos_integer()]
    }).