-module(server).
-export([start/1,stop/1]).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, #{}, fun server_loop_function/2). %state in an empty map

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state

% TODO: MAKE SURE user_already_joined IS RECOVERABLE
server_loop_function(State, {join, Nick, Channel}) -> 

    %remove '#' from the name of the channel and convert it to atom for convenience 
    ChannelAtom = list_to_atom(string:slice(Channel, 1)), 


    io:format("SERVER STATE BEFORE LOOP: ~p~n", [State]),

    case channel_exists(ChannelAtom, State) of
        true -> 
            case(user_in_channel(Nick, ChannelAtom, State)) of 
                true ->  {reply, {error, user_already_joined, "You are already a member of this channel!"}, State};
                false -> {reply, ok, add_user(Nick, ChannelAtom, State)}  % update list of nicks 
            end;
        false -> 
            channel:new_channel(ChannelAtom, Nick),
            {reply, ok, add_channel(ChannelAtom, Nick, State)} %reply with updated server state (the map with chanel=>members[])!
    end
.

channel_exists(Channel, Map) -> 
    maps:is_key(Channel, Map)
.

user_in_channel(User, Channel, Map) -> 
    lists:member(User, maps:get(Channel, Map))
.

% adds nickname to Channel and returns the updated map
add_user(Nick, Channel, Map) -> 
    maps:put(Channel, [Nick | maps:get(Channel, Map)], Map)
.

% add a new key-value pair Channel => Value to the map and returns an updated copy 
% value is usually the name of the first user when implicitly creating a new channel
add_channel(Channel, Value, Map) ->
    maps:put(Channel, [Value], Map)
.
