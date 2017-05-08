%%%-------------------------------------------------------------------
%%% @author Madzia
%%% Created : 08. kwi 2017 19:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("Madzia").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).
-export([getMaximumGradientStation/2]).
-export([average/1,makeValueLFromMeasurementsL/1,getSomeMeas/3, getStation2/2, test/0, lengthOfList/1, getList/2, getStation/2, average/1]).
-record(station, {name, coordinates, list}).
-record(measurement, { date, hour, type, value}).

createMonitor() -> [].

addStation(Name, {X,Y}, Monitor) ->
  case ((lists:any(fun(#station{name = A}) -> A =:= Name end,Monitor))
    or (lists:any(fun(#station{coordinates = A}) -> A =:= {X,Y} end,Monitor)))of
    false -> [#station{ name = Name, coordinates ={X, Y}, list = [] } | Monitor];
    true -> {error,"There is station like this."}
  end.

%---------------------------------------------------------

addValue({X,Y}, {D,T}, Type, Value, Monitor) ->
  case (
      (lists:any(fun(#station{coordinates = A}) -> A =:= {X,Y} end,Monitor)== true)
        and (lists:any(fun(#station{coordinates = A, list = B}) -> (A =:= {X,Y} andalso
        noSuchMeasurement(D,T,Type,Value,B) == false) end,Monitor) == true)) of
    true ->
      replaceMonitor2({X,Y}, {D,T}, Type, Value, Monitor);
    false ->{error,"You can not add such measurement"}
  end;
addValue(Name, {D,T}, Type, Value, Monitor) ->
  case (
      (lists:any(fun(#station{name = A}) -> A =:= Name end,Monitor)== true)
        and (lists:any(fun(#station{name = A, list = B}) -> (A =:= Name andalso
        noSuchMeasurement(D,T,Type,Value,B) == false) end,Monitor) == true)) of
    true ->
      replaceMonitor(Name, {D,T}, Type, Value, Monitor);
    false -> {error,"You can not add such measurement"}
  end.

noSuchMeasurement(D,T,Type,Value,B) ->
  (lists:any(fun(#measurement{date = C, hour = H, type = E, value = F}) ->
    (C =:= D) andalso (H =:= T) andalso (E =:= Type) andalso (F =:= Value) end,B)).

replaceMonitor2({X,Y}, {D,T}, Type, Value, Monitor) ->
  lists:keyreplace({X,Y}, 3, Monitor, changeStation(getStation2({X,Y}, Monitor), {D,T}, Type, Value)).

replaceMonitor(Name, {D,T}, Type, Value, Monitor) ->
  lists:keyreplace(Name, 2, Monitor, changeStation(getStation(Name, Monitor), {D,T}, Type, Value)).

getStation(Name, Monitor) -> lists:keyfind(Name, 2, Monitor).
getStation2({X,Y}, Monitor) -> lists:keyfind({X,Y}, 3, Monitor).

changeStation(#station{name = N, coordinates = C, list = L}, {D,T}, Type, Value) ->
  #station{name = N,coordinates = C,list = [#measurement{date = D, hour = T, type = Type, value = Value} | L]}.
%----------------------------------------

removeValue(Name, {D,T}, Type, Monitor) -> replaceMonitor3(Name,{D,T}, Type, Monitor).

replaceMonitor3(Name, {D,T}, Type, Monitor) ->
  lists:keyreplace(Name, 2, Monitor, deleteMeasurement(getStation(Name, Monitor), {D,T}, Type)).

deleteMeasurement(#station{name = N, coordinates = C, list = L}, {D,T}, Type) ->
  #station{name = N,coordinates = C,list =
  lists:filter(fun(#measurement{date=D1,hour=T1,type = Type1}) -> ((D1=/=D) or (T1=/=T) or (Type1=/=Type)) end,L)}.

getMeasurement(#station{name = _, coordinates = _, list = L}, {D,T}, Type) ->
  lists:last(lists:filter(fun(#measurement{date=D1,hour=T1,type = Type1}) -> ((D1=:=D) and (T1=:=T) and (Type1=:=Type)) end,L)).

%-------------------------------------------------------

getOneValue(Name, Type, {D,T}, Monitor) -> getOne(getMeasurement(getStation(Name,Monitor),{D,T}, Type)).

getOne(#measurement{date=_,hour=_,type = _, value = V}) -> V.

%-------------------------------------------------------
getStationMean({X,Y}, Type, Monitor) -> average(getList(getStation2({X,Y}, Monitor), Type));
getStationMean(Name, Type, Monitor) -> average(getList(getStation(Name, Monitor), Type)).

getList(false,_) -> 0.0;
getList(#station{name = _, coordinates = _, list = L}, Type) ->
  lists:map(fun(#measurement{date=_,hour=_,type = _, value = V}) -> V end,
    lists:filter(fun(#measurement{type = Type1}) -> (Type1=:=Type) end,L)).

lengthOfList(L) -> lists:foldl(fun(_, Acc) -> 1 + Acc end, 0, L).

average([])-> 0.0;
average(0.0) -> 0.0;
average(L) -> lists:sum(L)/lengthOfList(L).

getSomeMeas(Type, Day, Monitor) ->
  lists:filter(fun(#measurement{type = Type1, date = Date1}) -> ((Type1=:=Type) and (Date1=:=Day))end, makeAllMeasList(Monitor)).

makeAllMeasList(Monitor) ->
  zipper(lists:map(fun(#station{name=_, coordinates = _, list=L}) -> L end ,Monitor)).

zipper([]) -> [];
zipper([H|Tail]) -> H++zipper(Tail).

getDailyMean(Day, Type, Monitor) -> average(makeValueLFromMeasurementsL(getSomeMeas(Type,Day,Monitor))).

makeValueLFromMeasurementsL(List) ->
  lists:map(fun(#measurement{date=_,hour=_,type = _, value = V}) -> V end, List).
%-----------------------------------------------------------

getMaximumGradientStation(Monitor, Type) ->
  case getStationWithMaxGradient(Monitor,Type) of
    {_,0.0} -> {exit,"No measurement of this type"};
    _ -> extractName(getStationWithMaxGradient(Monitor,Type))
  end.

getStationWithMaxGradient(Monitor,Type) ->
  case getListOfStationsWithGradients(Monitor,Type) of
    [] -> {exit,"No measurement of this type"};
    _ -> lists:last(getListOfStationsWithGradients(Monitor,Type))
  end.

getListOfStationsWithGradients(Monitor,Type) -> lists:keysort(2,lists:map(
  fun(#station{name = N, coordinates = _, list = _}) -> {N, forOneStation(N,Monitor,Type)} end, Monitor)).
extractName({Name,_}) -> Name.

forOneStation(Name, Monitor,Type) -> forOneStationAvCount(filterList(getAllMeasurementForStation(getStation(Name, Monitor)),Type)).

filterList(List, Type) ->
  lists:filter(fun(#measurement{type = Type1}) -> (Type1=:=Type)end, List).

forOneStationAvCount(L) -> av(changeDatesToSec(reverseMeasurements(L))).

getAllMeasurementForStation(#station{name = _, coordinates = _, list = L}) -> L.

changeDatesToSec(L) ->
  lists:map(fun(#measurement{date=D,hour=H,type = _, value = V}) ->
    {calendar:datetime_to_gregorian_seconds({D,H}), V} end,L).

reverseMeasurements(L) -> lists:reverse(L).

diff([]) -> 0;%error('Empty list');
diff([{_,_}]) -> 0;
diff([{S1,V1},{S2,V2}]) -> abs(V2-V1)/(S2-S1);
diff([{S1,V1},{S2,V2}|Tail]) -> (abs(V2-V1)/(S2-S1)) + diff([{S2,V2}|Tail]).

av(L) -> diff(L)/(lengthOfList(L)-1).
%-----------------------------------------------------------

test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  Date1 = {{2017, 4, 1}, {1, 0, 51}},
  Date2 = {{2017, 4, 1}, {2, 0, 51}},
  Date3 = {{2017, 4, 1}, {3, 0, 51}},
  P2 = pollution:addValue({50.2345, 18.3445}, Date1, "PM10", 10, P1),
  P3 = pollution:addValue({50.2345, 18.3445}, Date1, "PM2.5", 15, P2),
  P4 = pollution:addValue({50.2345, 18.3445}, Date2, "PM10", 20, P3),
  P5 = pollution:addValue({50.2345, 18.3445}, Date2, "PM2.5", 12, P4),
  P6 = pollution:addValue({50.2345, 18.3445}, Date3, "PM10", 30, P5),
  P7 = pollution:addValue({50.2345, 18.3445}, Date3, "PM2.5", 3, P6),
  P8 = pollution:addStation("Aleja Mickiewicza", {5, 1}, P7).