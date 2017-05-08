%%%-------------------------------------------------------------------
%%% @author Madzia
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. maj 2017 23:12
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Madzia").

-include_lib("eunit/include/eunit.hrl").


addValue1_test() ->
  {X,Y} = calendar:local_time(),
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Krakow",{19,50},P),
  P2 = pollution:addStation("Tarnow", {20, 50}, P1),
  P3 = pollution:addStation("Rzeszow", {22, 50}, P2),
  P4 = pollution:addStation("Warszawa", {21, 52}, P3),
  %---------P4 includes 4 station------------
  %--------------test addValue---------------
  P5 = pollution:addValue({19,50},{X,Y}, "temp", 20, P4),

  ?assertEqual(P5,[{station,"Warszawa",{21,52},[]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[]},
    {station,"Krakow",{19,50},[{measurement, X, Y, "temp", 20}]}]),
  P6 = pollution:addValue({19,50},{X,Y}, "temp", 30, P5),

  ?assertEqual(P6,[{station,"Warszawa",{21,52},[]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[]},
    {station,"Krakow",{19,50},[{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P7 = pollution:addValue({19,50},{X,Y}, "temp", 10, P6),
  ?assertEqual(P7,[{station,"Warszawa",{21,52},[]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P8 = pollution:addValue({20,50},{X,Y}, "temp", 10, P7),
  ?assertEqual(P8,[{station,"Warszawa",{21,52},[]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[{measurement, X, Y, "temp", 10}]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P9 = pollution:addValue({20,50},{X,Y}, "pressure", 10, P8),
  ?assertEqual(P9,[{station,"Warszawa",{21,52},[]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[{measurement, X, Y, "pressure", 10},{measurement, X, Y, "temp", 10}]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P10 = pollution:addValue({21,52},{X,Y}, "PM10", 22.5, P9),
  ?assertEqual(P10,[{station,"Warszawa",{21,52},[{measurement, X, Y, "PM10", 22.5 }]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[{measurement, X, Y, "pressure", 10},{measurement, X, Y, "temp", 10}]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P11 = pollution:addValue("Krakow",{X,Y}, "PM10", 13.70, P10),
  ?assertEqual(P11,[{station,"Warszawa",{21,52},[{measurement, X, Y, "PM10", 22.5 }]},
    {station,"Rzeszow",{22,50},[]},
    {station,"Tarnow",{20,50},[{measurement, X, Y, "pressure", 10},{measurement, X, Y, "temp", 10}]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "PM10", 13.70},{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),
  P12 = pollution:addValue("Rzeszow",{X,Y}, "PM1", 3.5, P11),
  ?assertEqual(P12,[{station,"Warszawa",{21,52},[{measurement, X, Y, "PM10", 22.5 }]},
    {station,"Rzeszow",{22,50},[{measurement, X, Y, "PM1", 3.5 }]},
    {station,"Tarnow",{20,50},[{measurement, X, Y, "pressure", 10},{measurement, X, Y, "temp", 10}]},
    {station,"Krakow",{19,50},
      [{measurement, X, Y, "PM10", 13.70},{measurement, X, Y, "temp", 10},{measurement, X, Y, "temp", 30},{measurement, X, Y, "temp", 20}]}]),

  %--------enable adding measurements with the same coordinates, date and hour and type-------------
  ?assertMatch({error,_},pollution:addValue("Rzeszow",{X,Y}, "PM1", 3.5, P12)),
  ?assertMatch({error,_},pollution:addValue("Warszawa",{X,Y}, "PM10", 22.5, P12)),
  ?assertMatch({error,_},pollution:addValue("Krakow",{X,Y}, "temp", 30, P12)),
  ?assertMatch({error,_},pollution:addValue({19,50},{X,Y}, "temp", 30, P12)),
  ?assertMatch({error,_},pollution:addValue({19,50},{X,Y}, "temp", 10, P12)),

  %--------enable adding measurements to non-existent station-------------
  ?assertMatch({error,_},pollution:addValue("Bydgoszcz",{X,Y}, "PM1", 3.5, P12)),
  ?assertMatch({error,_},pollution:addValue("Wroclaw",{X,Y}, "PM2", 3.5, P12)),
  ?assertMatch({error,_},pollution:addValue({1,2},{X,Y}, "PM3", 3.5, P12)),
  ?assertMatch({error,_},pollution:addValue({0,1},{X,Y}, "PM3", 3.5, P12)),
  ?assertMatch({error,_},pollution:addValue({-19,-50},{X,Y}, "PM3", 3.5, P12)).

getMaximumGradientStation_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Krakow",{19,50},P),
  P2 = pollution:addStation("Tarnow", {20, 50}, P1),
  P3 = pollution:addStation("Rzeszow", {22, 50}, P2),
  P4 = pollution:addStation("Warszawa", {21, 52}, P3),

  %---------dates-------------------
  {X1,Y1} = {{2017,5,1},{23,49,44}},
  {X2,Y2} = {{2017,5,2},{23,49,44}},
  {X3,Y3} = {{2017,5,3},{23,49,44}},
  {X4,Y4} = {{2017,5,4},{23,49,44}},
  {X5,Y5} = {{2017,5,5},{23,49,44}},

 %-------addValues pollutions measurements type PM1--------
G1 = pollution:addValue("Warszawa",{X1,Y1},"PM1",1,P4),
  G2 = pollution:addValue("Warszawa",{X2,Y2},"PM1",2,G1),
  G3 = pollution:addValue("Warszawa",{X3,Y3},"PM1",3,G2),
  G4 = pollution:addValue("Warszawa",{X4,Y4},"PM1",4,G3),

  G5 = pollution:addValue("Rzeszow",{X1,Y1},"PM1",1,G4),
  G6 = pollution:addValue("Rzeszow",{X2,Y2},"PM1",3,G5),
  G7 = pollution:addValue("Rzeszow",{X3,Y3},"PM1",5,G6),
  G8 = pollution:addValue("Rzeszow",{X4,Y4},"PM1",7,G7),


  ?assertEqual(pollution:getMaximumGradientStation(G8,"PM1"),"Rzeszow"),
G11 = pollution:addValue("Warszawa",{X1,Y1},"PM1",1,P4),
G12 = pollution:addValue("Warszawa",{X2,Y2},"PM1",2,G11),
G13 = pollution:addValue("Warszawa",{X3,Y3},"PM1",3,G12),

G14 = pollution:addValue("Rzeszow",{X1,Y1},"PM1",1,G13),
G15 = pollution:addValue("Rzeszow",{X3,Y3},"PM1",2,G14),
G16 = pollution:addValue("Rzeszow",{X5,Y5},"PM1",3,G15),

  ?assertEqual(pollution:getMaximumGradientStation(G16,"PM1"),"Warszawa"),

G21 = pollution:addValue("Warszawa",{X1,Y1},"PM1",1,P4),
G22 = pollution:addValue("Warszawa",{X2,Y2},"PM1",2,G21),
G23 = pollution:addValue("Warszawa",{X3,Y3},"PM2",3,G22), %--different type
G24 = pollution:addValue("Warszawa",{X3,Y3},"PM1",3,G23),

G25 = pollution:addValue("Rzeszow",{X1,Y1},"PM1",1,G24),
G26 = pollution:addValue("Rzeszow",{X3,Y3},"PM1",2,G25),
G27 = pollution:addValue("Rzeszow",{X5,Y5},"PM1",3,G26),

  ?assertEqual(pollution:getMaximumGradientStation(G27,"PM1"),"Warszawa"),

  ?assertMatch({exit,_},pollution:getMaximumGradientStation(G27,"Non-existent Type")),
  ?assertMatch({exit,_},pollution:getMaximumGradientStation(G27,"PM")).
  %----------empty Monitor---------
  %?assertMatch({exit,_},pollution:getMaximumGradientStation(P,"PM")).

addStation_test() ->
  % Initialising monitor:
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Aleja Kochanowskiego", {18.3445, 00.0000}, P1),

  % Test cases:
  % adding station to an empty monitor
  ?assertEqual(pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),P1),
  ?assertNotMatch({error,_}, P1),

  % adding station to a non empty monitor
  ?assertEqual(pollution:addStation("Aleja Kochanowskiego", {18.3445, 00.0000}, P1),P2),
  ?assertNotMatch({error,_}, P2),

  % adding already existing station
  P3 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P1),
  ?assertMatch({error,_},pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P1)),

  % adding station with different coordinates but the same name
  P4 = pollution:addStation("Aleja Slowackiego", {50.0000, 18.0000}, P1),
  ?assertMatch({error,_}, P4),

  % adding station with different name but the same coordinates
  P5 = pollution:addStation("Aleja Mickiewicza", {50.2345, 18.3445}, P1),
  ?assertMatch({error,_}, P5).

addValue_test() ->
  Name = "Test station",
  Pos = {1.0, 2.0},
  DateTime = {{2017, 5, 6}, {1, 2, 3}},
  M0 = pollution:createMonitor(),
  M1 = pollution:addStation(Name, Pos, M0),
  M2 = pollution:addStation("Another station", {3.0, 4.0}, M1),
  M3 = pollution:addValue(Pos, DateTime, "P10", 1, M2),

  % can't add 2 times value to the same station and with the same time
  %?assertMatch({error, _}, pollution:addValue(Pos, DateTime, "P10", 100, M3)),
  %?assertMatch({error, _}, pollution:addValue(Name, DateTime, "P10", 10, M3)),

  % can't add to not exist station
  ?assertMatch({error, _}, pollution:addValue({100.0, 200.0}, DateTime, "P10", 10, M3)),
  ?assertMatch({error, _}, pollution:addValue("not exist", DateTime, "P10", 10, M3)),

  %add value with new type
  M4 = pollution:addValue(Pos, DateTime, "temperature", 15, M3),
  ?assertNotMatch({error, _}, M4),
  ?assertEqual(M4, pollution:addValue(Name, DateTime, "temperature", 15, M3)),

  %add value with new time
  NewTime1 = {{2017, 5, 6}, {10, 20, 30}},
  M5 = pollution:addValue(Pos, NewTime1, "P10", 1, M3),
  ?assertNotMatch({error, _}, M5),
  ?assertEqual(M5, pollution:addValue(Name, NewTime1, "P10", 1, M3)),

  %add value with new date
  NewTime2 = {{2017, 1, 1}, {1, 2, 3}},
  M6 = pollution:addValue(Pos, NewTime2, "P10", 1, M3),
  ?assertNotMatch({error, _}, M6),
  ?assertEqual(M6, pollution:addValue(Name, NewTime2, "P10", 1, M3)),

  %add default value to another station
  M7 = pollution:addValue("Another station", DateTime, "P10", 1, M3),
  ?assertNotMatch({error, _}, M7),
  ?assertEqual(M7, pollution:addValue({3.0, 4.0}, DateTime, "P10", 1, M3)).

getDailyMean_test()->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Aleja Mickiewicza", {18.3445, 00.0000}, P1),
  P3 = pollution:addStation("Aleja Krasickiego", {18.3445, 01.0000}, P2),
  Date1 = {{2017, 4, 1},{1,1,1}},
  Date2 = {{2017, 4, 2},{1,1,1}},
  Date11={2017, 4, 1},
  Date21={2017, 4, 2},
  P4 = pollution:addValue("Aleja Slowackiego", Date1, "PM10", 10, P3),
  P5 = pollution:addValue("Aleja Slowackiego", Date2, "PM10", 100, P4),
  P6 = pollution:addValue("Aleja Krasickiego", Date1, "PM10", 20, P5),
  P7 = pollution:addValue("Aleja Mickiewicza", Date1, "PM2.5", 30, P6),
  P8 = pollution:addValue("Aleja Mickiewicza", Date2, "PM10", 101, P7),
  ?assert(pollution:getDailyMean(Date11,"PM10",P8)==15),
  ?assert(pollution:getDailyMean(Date11,"PM2.5",P8)==30),
  ?assert(pollution:getDailyMean(Date21,"PM10",P8)==100.5).

getStationMean_test() ->
%  Set up
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
  P8 = pollution:addStation("Aleja Mickiewicza", {5, 1}, P7),
  M = pollution:createMonitor(),


% Test cases
% Get a PM10 mean of an existing station
  Mean1 = pollution:getStationMean({50.2345, 18.3445}, "PM10", P8),
  ?assertEqual(20.0, Mean1),

% Get a PM2.5 mean of an existing station
  Mean2 = pollution:getStationMean({50.2345, 18.3445}, "PM2.5", P8),
  ?assertEqual(10.0, Mean2),

% Get a mean of an empty station
  Mean3 = pollution:getStationMean("Aleja Mickiewicza", "PM10", P8),
  ?assertEqual(0.0, Mean3),

% Get a mean of an empty monitor
  Mean4 = pollution:getStationMean("Aleja Slowackiego", "PM10", M),
  ?assertEqual(0.0, Mean4),

% Get a mean of a nonexistent station
  Mean5 = pollution:getStationMean("Aleja", "PM10", P8),
  ?assertEqual(0.0, Mean5),

% Get a mean of a wrong pollution type
  Mean6 = pollution:getStationMean("Aleja Mickiewicza", "PM100", P8),
  ?assertEqual(0.0, Mean6).











