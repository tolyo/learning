%%%-------------------------------------------------------------------
%%% @author anatoly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%%
%%% @end
%%% Created : 20. Sep 2017 02:25
%%%-------------------------------------------------------------------
-module(tr_server).
-author("anatoly").

-behaviour(gen_server).

%% API
-export([start_link/0,
  start_link/1,
  get_count/0,
  stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%fsdfasdf--------------------------------------------------------------------
-spec(start_link(Port :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Calls 'start_link(Port)' using the default port.
-spec(start_link() -> {ok, Pid :: pid() }).
start_link() ->
  start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc
%% Returns the number of requests made to the server
%% See: https://github.com/erlware/Erlang-and-OTP-in-Action-Source/issues/4
%% for details
%% @end
%%--------------------------------------------------------------------
get_count() ->
  gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% @doc
%% Shuts down the server
%%
%% @end
%%--------------------------------------------------------------------
stop() ->
  gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Port]) ->
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  {ok, #state{port = Port, lsock = LSock}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_count, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State) ->
  {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Rawdata}, State) ->
  do_rpc(Socket, Rawdata),
  RequestCount = State#state.request_count,
  {noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
  gen_tcp:accept(LSock),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc

%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


do_rpc(Socket, Rawdata) ->
  try
    {M, F, A} = split_out_mfa(Rawdata),
    Result = apply(M, F, A),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
    _Class:Err  ->
      gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
  end .

split_out_mfa(RawData) ->
  MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
  {match, [M, F, A]} =
    re:run( MFA,
      "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
      [{capture, [1,2,3], list}, ungreedy]),
  {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
  {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  {ok, Args} = erl_parse:parse_term(Toks),
  Args.


%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("eunit/include/eunit.hrl").

start_test() ->
  tr_server:start_link(1095).
