%%%-------------------------------------------------------------------
%%% @author casafta
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2016 16:01
%%%-------------------------------------------------------------------
-module(chan_test).
-author("casafta").

-include_lib("eunit/include/eunit.hrl").

chan_core_async_example_test() ->
  UpperCaser = fun(In) ->
    Out = chan:new(),
    spawn(fun F() ->
          chan:put(Out, string:to_upper(chan:take(In))),
          F() end ),
    Out
    end,

  Reverser = fun(In) ->
    Out = chan:new(),
    spawn(fun F() ->
      chan:put(Out, lists:reverse(chan:take(In))),
      F() end ),
    Out
    end,

  Printer = fun (In) ->
    spawn( fun F() ->
      io:format("~p~n", [chan:take(In)]),
      F() end )
    end,

  InChan = chan:new(),
  UpperCaserOut = UpperCaser(InChan),
  ReverserOut = Reverser(UpperCaserOut),
  Printer(ReverserOut),

  chan:put(InChan, "redrum"),
  chan:put(InChan, "repaid"),
  1 = 1,
  ok.

chan_simple_test() ->
  C = chan:new(),
  spawn(
    fun() ->
      chan:put(C, 1)
    end),

  M = chan:take(C),
  M = 1,
  ok.

chan_complex_test() ->
  C = chan:new(),
  spawn(
    fun() ->
      chan:take(C)
    end),
  spawn(
    fun() ->
      chan:take(C)
    end),
  spawn(
    fun() ->
      chan:take(C)
    end),
  spawn(
    fun() ->
      chan:put(C, 1)
    end),
  spawn(
    fun() ->
      chan:put(C, 2)
    end),
  spawn(
    fun() ->
      chan:put(C, 3)
    end),
  spawn(
    fun() ->
      chan:put(C, 4)
    end),
  M = chan:take(C),
  io:format("~p~n", [M]),
  ok.

simple_test() ->
  1 = 1,
  _C = chan:new(),
  ok.
