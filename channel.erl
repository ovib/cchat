-module(channel).
-export([new_channel/3]).

new_channel(Atom, ChannelName, FirstMemberPid) ->
    % second argument is the STATE OF THE CHANNEL
    % = a list with channel name as first element and the list of client pids as second element
    % = ["#test", [<0.87.0>]] where <0.87.0> is the pid of the first client (e.g. client_4123)
    Pid = genserver:start(Atom, [ChannelName, [FirstMemberPid]], fun channel_loop_function/2), 
    io:format("A new channel with atom ~p and pid ~p was created~n ", [Atom, Pid])
.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state
channel_loop_function([ChannelName, MemberPids], {try_join, Member}) ->
    case lists:member(Member, MemberPids) of
        true -> 
            {reply, {error, user_already_joined, "You are already a member of this channel!"}, [ChannelName, MemberPids]};
        false -> 
            {reply, ok, [ChannelName, [Member | MemberPids]]}
    end
;

channel_loop_function([ChannelName, Members], {message_send, MemberPid, Nick, Msg}) ->
    case lists:member(MemberPid, Members) of 
        true -> 
            % notify other members of channel that a new message has arrived so they can call the GUI and display it
            % possibile optimization: spawning a new process to send every request (or a new process to send all requests). 
            %                         If channel has a lot of clients dispatching all requests may be costly! channel unresponsive (?)
            lists:map(fun(OtherPid) ->
                genserver:request(OtherPid, {message_receive, ChannelName, Nick, Msg}) end, 
                lists:delete(MemberPid, Members) %remove current client from list to avoid double messages!
                ),
            {reply, ok, [ChannelName, Members]}; % recieve message on current client
        false -> {reply, {error, user_not_joined, "You have to be a member of the channel to send messages"}, [ChannelName, Members]}
    end
.
