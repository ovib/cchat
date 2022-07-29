-module(channel).
-export([new_channel/2]).

new_channel(ChannelName, FirstMemberPid) ->
    % second argument is the STATE OF THE CHANNEL
    % = a list with channel name as first element and the list of client pids as second element
    % = ["#test", [<0.87.0>]] where <0.87.0> is the pid of the first client (e.g. client_4123)
    genserver:start(list_to_atom(ChannelName), [ChannelName, [FirstMemberPid]], fun channel_loop_function/2) 
.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state
channel_loop_function([ChannelName, MemberPids], {try_join, Member}) ->
    case lists:member(Member, MemberPids) of
        true -> {reply, {error, user_already_joined, "You are already a member of this channel!"}, [ChannelName, MemberPids]};
        false -> {reply, ok, [ChannelName, [Member | MemberPids]]}
    end
;

channel_loop_function([ChannelName, MemberPids], {message_send, MemberPid, Nick, Msg}) ->
    case lists:member(MemberPid, MemberPids) of 
        true ->
            % notify other members of channel that a new message has arrived so they can call the GUI and display it
            lists:map(fun(OtherPid) ->
                spawn(fun () -> genserver:request(OtherPid, {message_receive, ChannelName, Nick, Msg}) end)
            end, MemberPids -- [MemberPid]), %remove current client from list to avoid double messages!
            {reply, ok, [ChannelName, MemberPids]}; % respond to current client that message was recieved (so it can be shown on the GUI)
        false -> {reply, {error, user_not_joined, "You have to be a member of the channel to send messages"}, [ChannelName, MemberPids]}
    end
;

channel_loop_function([ChannelName, MemberPids], {leave, MemberPid}) ->
    case lists:member(MemberPid, MemberPids) of
        true -> {reply, ok, [ChannelName, MemberPids -- [MemberPid]]}; % remove calling client from the list in state!
        false -> {reply, {error, user_not_joined, "You are not a member of this channel"}, [ChannelName, MemberPids]}    
    end
.
