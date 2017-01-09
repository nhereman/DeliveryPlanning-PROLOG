:- module(pretty_print, [pretty_print/1]).

:- use_module(auxiliary).
:- use_module(utility).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		  PRETTY PRINT           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pretty_print(+P) - Display plan P.
pretty_print(plan(Schedules)) :- 
			findall(Day,working_day(Day,_,_),Days),
			print_days(Days,Schedules).

% print_days(+Days,+Schedules) - Print all Schedules of all Days.
print_days([],_) :- format("\n").
print_days([H|T],Schedules) :-
			print_day(H,Schedules),
			print_days(T,Schedules).


% print_day(+Day, +Schedules) - Print all Schedules in Day.
print_day(Day,Schedules) :-
			format("*** Schedule for Day ~p ***\n\n", [Day]),
			findall(Vid,vehicle(Vid,_,_,_,_,_),Vs),
			print_vehicles(Vs,Day,Schedules).

% print(+Vs, +Day, +Schedules) - Print all Schedules in Day with all vehicles in Vs.
print_vehicles([],_,_).
print_vehicles([H|T],Day,Schedules) :-
			print_vehicle(H,Day,Schedules),
			print_vehicles(T,Day,Schedules).


% print_vehicle(+Vid,+Day,+Schedules) - Print the schedule of Vid on Day in Schedules.
print_vehicle(Vid,Day,Schedules) :-
			format("< Vehicle ~s >\n\n",[Vid]),
			is_day_vehicle_schedule(Day,Vid,Schedules,Schedule),
			print_schedule(Schedule,Schedules).

% print_schedule(+Schedule, +Schedules) - Print Schedule in Schedules.
print_schedule(schedule(Vid,Day,Route),Schedules) :-
			format("time\tLoc.\tload\taction\n"),
			working_day(Day,StartTime,_),
			previous_depot(Day,Vid,Schedules,StartPos),
			following_orders_in_route(Route,Os), % Picking up first orders
			load(Os,NewLoad),
			length(Os,Length),
			NewTime is StartTime + (Length*5),
			(	Length \= 0, !,
				print_route_action_time_pos_load(StartTime,0.0,StartPos),
				print_pickup_action(StartPos,Os),
				print_route_action(Route,Vid,Day,NewTime,NewLoad,StartPos,Schedules);
				Length = 0, !,
				print_route_action(Route,Vid,Day,NewTime,NewLoad,StartPos,Schedules) ).

%print_route_action(+Route,+Vid,+Day,+Time,+Load,+PosId,+Schedules) - Print the action taken on Route by Vid on Day.
print_route_action([],_,_,Time,Load,PosId,_) :- print_route_action_time_pos_load(Time,Load,PosId),format("Park at depot ~s.\n\n",[PosId]).
print_route_action([H|T],Vid,Day,Time,Load,PosId,Schedules) :-
			order(H,_,_,_),
			!,
			is_location(H,ToLoc),
			is_location(PosId,FromLoc),
			distance(FromLoc,ToLoc,Dist),							%driving
			print_route_action_time_pos_load(Time,Load,PosId),
			print_driving_action(Dist,ToLoc),
			driving_duration(Vid,PosId,H,DrivingTime),
			OrderTime is Time + DrivingTime,
			print_route_action_time_pos_load(OrderTime,Load,H), % Deliver
			format("Deliver order ~s.\n",[H]),
			NewTime is OrderTime + 5,
			load([H],OrderLoad),
			NewLoad is Load - OrderLoad,
			print_route_action(T,Vid,Day,NewTime,NewLoad,H,Schedules).
print_route_action([H|T],Vid,Day,Time,Load,PosId,Schedules) :-
			depot(H,_,_),
			!,
			is_location(H,ToLoc),
			is_location(PosId,FromLoc),
			distance(FromLoc,ToLoc,Dist),                           % driving
			print_route_action_time_pos_load(Time,Load,PosId),
			print_driving_action(Dist,ToLoc),
			driving_duration(Vid,PosId,H,DrivingTime),
			PickTime is Time + DrivingTime,
			following_orders_in_route(T,Os), % Picking
			load(Os,NewLoad),
			length(Os,Length),
			NewTime is Time + (Length*5),
			(	Length\=0,
				print_route_action_time_pos_load(PickTime,Load,H),
				print_pickup_action(H,Os);
				Length=0),
			print_route_action(T,Vid,Day,NewTime,NewLoad,H,Schedules).


% print_route_action_time_pos_load(+Time, +Load, +PosId) - Print Time, Load, PosId
print_route_action_time_pos_load(Time,Load,PosId) :-
			time_to_hours_minutes(Time,Hours,Minutes),
			is_location(PosId,Loc),
			print_time(Hours,Minutes),
			print_location(Loc),
			format("~2fkg\t",[Load]).


% print_driving_action(+Dist, +Location) - Print a comment on a driving action.
print_driving_action(Dist,location(X,Y)) :- format("Drive ~pkm to the location (~p,~p)\n",[Dist,X,Y]).

% print_pickup_action(+Did, +Orders) - Print a comment on a pickup action.
print_pickup_action(Did,Orders) :-
			format("pick up order(s) "),
			print_orders(Orders),
			format(" from depot ~s\n",[Did]).

% print(+Orders) :- Print Orders
print_orders([H]) :- !, format("~s",[H]).
print_orders([H|T]) :- T \= [], !, format("~s,",[H]), print_orders(T).

% time_to_hours_minutes(+Time,-Hours,-Minutes) - Transfor Time in a Hours:Minutes format.
time_to_hours_minutes(Time,Hours,Minutes) :-
			TempHours is Time / 60,
			Hours is floor(TempHours),
			TempMin is Time - (60*Hours),
			round(TempMin,Minutes).

% print_time(+Hours,+Minutes) - Print Hours:Minutes
print_time(Hours,Minutes) :-
		Minutes < 10,
		!,
		format("~p:0~p\t",[Hours,Minutes]).
print_time(Hours,Minutes) :-
		Minutes >= 10,
		!,
		format("~p:~p\t",[Hours,Minutes]).

% print_location(+Location) - Print a location (X,Y).
print_location(location(X,Y)) :- format("(~p,~p)\t",[X,Y]).
