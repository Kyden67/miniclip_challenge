# miniclip_challenge
Miniclip Challenge

Make sure you have erlang installed.
Make sure you have rebar3 installed and on location "~/.config/rebar3/rebar.config" have this line "{plugins, [rebar3_auto]}." or just add to rebar3_auto to your plugins.

Steps:
- 1 - open terminal on minrel directory
- 2 - type "rebar3 auto" (or "rebar3 compile" and "rebar3 shell")
- 3 - if you want to create a user in dets use "players:create_new_player(Name,Pass)." function on shell
- 4 - to test you can either use the client for one user that you have to type the text-binary yourself (see binaries below) or the many users client that use the test users in dets.


Binaries (for socket) :
- "Login:"<<Name>>","<<Pass>>
- "Register:"<<Name>>","<<Pass>>
- "Enter Queue"
- "Exit Queue"
- "Queue Size"
