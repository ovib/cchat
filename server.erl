-module(server).
-export([start/1,stop/1]).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, [], fun server_loop_function/2). % Second argument is State = list of channel atoms!

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state

server_loop_function(Channels, {join, ClientPid, Nick, Channel}) -> 
     io:format("Called server loop function. Channels: ~p~n ", [Channels]),

    %remove '#' from the name of the channel and convert it to atom for convenience 
    ChannelAtom = list_to_atom(string:slice(Channel, 1)), 

    case lists:member(ChannelAtom, Channels) of 
        true -> 
            io:format("Channel ~p iS a member of Channels ~p. Trying to join existing channel with Nick=~p~n", [ChannelAtom, Channels, Nick]),
            Response = genserver:request(ChannelAtom, {try_join, ClientPid}),
            {reply, Response, Channels}; %forward response from Channel to to Client!
        false -> 
            io:format("Channel ~p IS NOT a member of Channels ~p~n ", [ChannelAtom, Channels]),
            channel:new_channel(ChannelAtom, Channel, ClientPid),
            {reply, ok, [ChannelAtom | Channels]} 
        end 
.