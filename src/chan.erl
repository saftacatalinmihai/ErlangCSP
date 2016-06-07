%%%-------------------------------------------------------------------
%%% @author casafta
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2016 12:57
%%%-------------------------------------------------------------------
-module(chan).
-author("casafta").
-behavior(gen_server).
%% API
-export([new/0, new/1, take/1, put/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(queue, {
  msgQ  = queue:new(),
  takeQ = queue:new(),
  putQ  = queue:new(),
  maxBufSize = 1}
).

putQueueLen(State) ->
  queue:len(State#queue.putQ).

takeQueueLen(State) ->
  queue:len(State#queue.takeQ).

msgQueueLen(State) ->
  queue:len(State#queue.msgQ).

putInQueue(msg, State, Item) ->
  State#queue{msgQ = queue:in(Item, State#queue.msgQ)};

putInQueue(take, State, Item) ->
  State#queue{takeQ = queue:in(Item, State#queue.takeQ)};

putInQueue(put, State, Item) ->
  State#queue{putQ = queue:in(Item, State#queue.putQ)}.

takeFromQueue(msg, State) ->
  {{value, Msg}, MsgQ} = queue:out(State#queue.msgQ),
  {Msg, MsgQ};

takeFromQueue(take, State) ->
  {{value, TakeWaiting}, TakeQ} = queue:out(State#queue.takeQ),
  {TakeWaiting, TakeQ};

takeFromQueue(put, State) ->
  {{value, {PutWaiting, Msg}}, PutQ} = queue:out(State#queue.putQ),
  {{PutWaiting, Msg}, PutQ}.

new() -> new(1).
new(BufferSize) ->
  {ok, Chan} = gen_server:start(?MODULE, BufferSize, []),
  Chan.

take(Chan) ->
  gen_server:call(Chan, take).

put(Chan, Msg) ->
  gen_server:call(Chan, {put, Msg}).

init(BufferSize) ->
  {ok, #queue{maxBufSize = BufferSize}}.

handle_call(take, From, State) ->

  case putQueueLen(State) of
    Waiting when Waiting > 0 ->
      {{PutWaiting, Msg}, PutQ} = takeFromQueue(put, State),
      gen_server:reply(PutWaiting, {true, Msg}),
      {Msg, MsgQ} = takeFromQueue(msg, State),
      {reply, Msg, State#queue{putQ = PutQ, msgQ = MsgQ}};

    _ ->
      Max = State#queue.maxBufSize,
      case msgQueueLen(State) of
        0   ->
          {noreply, putInQueue(take,State, From), infinity};
        Len when Len =< Max  ->
          {Msg, MsgQ} = takeFromQueue(msg, State),
          {reply, Msg, MsgQ};
        _ ->
          {stop, 'index_out_of_bounds', State}
      end
  end;


handle_call({put, Msg}, From, State) ->
  case takeQueueLen(State) of
    Waiting when Waiting > 0 ->
      {TakeWaiting, TakeQ} = takeFromQueue(take, State),
      gen_server:reply(TakeWaiting, Msg),
      {reply, {true, Msg}, State#queue{takeQ = TakeQ}};

    _ ->
      Max = State#queue.maxBufSize,
      case msgQueueLen(State) of
        Len when Len < Max ->
          {reply, {true, Msg}, putInQueue(msg, State, Msg)};
        _ ->
          {noreply, putInQueue(put, State, {From, Msg}), infinity}
      end
  end.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info({Pid, echo}, State) ->
  Pid ! echo,
  {noreply,State};

handle_info({Pid, debug}, State) ->
  Pid ! State,
  {noreply,State};

handle_info(_, State) ->
  {noreply,State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
