-module(server).
-export([start/1,stop/1, test/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, 0, fun server_loop_function/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.


% - This function is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: response message, new state
server_loop_function(Counter, echo) -> {reply, io:format("Received request #~p~n", [Counter]), Counter + 1}.


test(Server) -> genserver:request(Server, echo).

% TO TEST IF SERVER IS RESPONGING RUN THIS IN ERL SHELL:
% Pid = cchat:server().
% server:test(Pid). 