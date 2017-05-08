%%%-------------------------------------------------------------------
%%% @author Madzia
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. maj 2017 21:42
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Madzia").

%% API
-import(pollution,[createMonitor/0,addStation/3, addValue/5, removeValue/4,
getOneValue/4, getStationMean/3, getDailyMean/3,getMaximumGradientStation/2]).
-export([start/0, stop/0, loop/1]).
start() ->
  ServerPid = spawn(fun() -> loop(pollution:createMonitor()) end),
  register(server, ServerPid).
stop() ->
  server ! quit.

loop(Monitor) ->
  receive
    {addValue,{X,Y},{Day,Hour},Type,Value} ->
      io:format("ok~n"),
      loop(addValue({X,Y},{Day,Hour},Type,Value,Monitor));
    {addValue,Name,{Day,Hour},Type,Value} ->
      io:format("ok~n"),
      loop(addValue(Name,{Day,Hour},Type,Value,Monitor));
    {addStation,Name,{X,Y}} ->
      io:format("ok~n"),
      loop(addStation(Name,{X,Y},Monitor));
    {getDailyMean,Day,Type} ->
      io:format("ok~n"),
      getDailyMean(Day, Type, Monitor),
      loop(Monitor);
    {getStationMean,Name,Type} ->
      io:format("ok~n"),
      {N}=getStationMean(Name, Type, Monitor),
      io:fwrite("~p ~n",[N]),
      loop(Monitor);
    {getOneValue,Name,Type,{Day,Hour}} ->
      io:format("ok~n"),
      getOneValue(Name, Type, {Day,Hour}, Monitor),
      loop(Monitor);
    {removeValue,Name,{Day,Hour},Type} ->
      io:format("ok~n"),
      loop(removeValue(Name, {Day,Hour}, Type, Monitor));
    {getMaximumGradientStation,Type} ->
      io:format("ok~n"),
      MaxGradient=getMaximumGradientStation(Monitor,Type),
      io:fwrite("station with maximum gradient:~p ",[MaxGradient]),
      loop(Monitor);
    printMonitor ->
      io:format("ok~n"),
      [io:format("~p ~n",[X])|| X <- Monitor],
      loop(Monitor);
    quit -> io:format("quit")
  end.