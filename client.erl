-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        nick = Nick,
        gui = GUIAtom,
        server = ServerAtom
    }.

% wrapper of function genserver:request/2
% needed to handle errors more elegantly: no need to repeat error handling code at every call!
send_request(Pid, Data) ->
    try genserver:request(Pid, Data) of
         Response -> Response
        % {'EXIT', Reason} ->
        %     % io:format("REQUEST FAILED. RETURNED ERROR SERVER_NOT_REACHED ~n ", []),
        %             {error, server_not_reached, Reason};
    % catch TypeOfError:ExceptionPattern ->  {error, server_not_reached, string:join(["Server cant be reached.", "Type of error: ", TypeOfError, "Exception Pattern: ", ExceptionPattern], "")}   
    catch Type:Pattern ->  {error, server_not_reached,  io_lib:format("Server can't be reached. Error Type: ~p, ErrorPattern: ~p",[Type, Pattern])}   
    end
.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    %{reply, {error, not_implemented, "join not implemented"}, St} ;
    Response = send_request(St#client_st.server, {join, self(), Channel}), %RESPONSE FROM SERVER
    case Response of 
        ok -> {reply, ok, St}; % RESPOND TO GUI
        Error -> {reply, Error, St}
    end
;

% Leave channel
handle(St, {leave, Channel}) ->
    Response = send_request(list_to_atom(Channel), {leave, self()}),
    case Response of
        ok -> {reply, ok, St};
        Error -> {reply, Error, St}
    end
;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->  
     Response = send_request(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg}),
     case Response of
        ok -> {reply, Response, St};
        Error -> {reply, Error, St}
    end
;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
