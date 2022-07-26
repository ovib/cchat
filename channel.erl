-module(channel).
-export([new_channel/2]).

new_channel(Atom, FirstMember) ->
    Pid = genserver:start(Atom, [FirstMember], fun channel_loop_function/2), %second argument is the State = List of members!
    io:format("A new channel with atom ~p and pid ~p was created~n ", [Atom, Pid])
.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state
channel_loop_function(Members, {join, Member}) ->
    case lists:member(Member, Members) of
        true -> {reply, {error, user_already_joined, "You are already a member of this channel!"}, Members};
        false -> 
            io:format("New list of member of channel ~p: ~p~n ", [self(), [Member|Members]]),
            {reply, ok, [Member | Members]}
    end
.


