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
    % Return ok
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom),
    ok.

% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state
server_loop_function(Channels, {join, ClientPid, Channel}) -> 
    case lists:member(Channel, Channels) of 
        true -> 
            Response = genserver:request(list_to_atom(Channel), {try_join, ClientPid}),
            {reply, Response, Channels}; %forward response from Channel to to Client!
        false -> 
            channel:new_channel(Channel, ClientPid),
            {reply, ok, [Channel | Channels]} 
        end 
;

server_loop_function(Channels, stop_channels) -> 
    lists:map(fun (Channel) ->
        genserver:stop(list_to_atom(Channel)) end, 
    Channels),
    {reply, ok, Channels}
.