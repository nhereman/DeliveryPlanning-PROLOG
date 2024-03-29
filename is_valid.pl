:- module(is_valid,[is_valid/1]).

:- use_module(auxiliary,[driving_duration/4, distance/3, is_location/2, load/2,update_inventory/3]).
:- use_module(utility).

%%%%%%%%%%%%%%%%
%   IS_VALID   %
%%%%%%%%%%%%%%%%

%is_valid(+P) - True if plan P has the correct format and respect the hard-constraint.
is_valid(P) :-
			is_valid_format(P),
			schedule_every_vehicle_every_day(P),
			order_at_most_once(P),
			no_overtime(P),
			enough_inventory(P),
			plan_no_overload(P).


%%% CORRECT FORMAT %%%


% is_valid_format(+P) - True if plan has the correct format
is_valid_format(plan(X)) :- is_valid_schedulelist_format(X).

% is_valid_schedulelist_format(+Schedules) - True if Schedules is a list of schedule having correct format.
is_valid_schedulelist_format([]).
is_valid_schedulelist_format([H|T]) :-
			is_valid_schedule_format(H),
			is_valid_schedulelist_format(T).
% is_valid_schedule_format(+Schedule) - True if Schedule has the correct format.
is_valid_schedule_format(schedule(VID,Day,Route)) :-
			is_valid_vehicle(VID),
			is_working_day(Day),
			is_valid_route_format(Route).

% is_valid_route_format(+Route) - True if each element of the list Route is the id of an order or a depot.
is_valid_route_format([]).
is_valid_route_format([H|T]) :-
			((is_valid_order(H),!);(is_valid_depot(H),!)),
			is_valid_route_format(T).

% Hard Constraint 1 : Exactly one schedule for each vehicle for each working_day

% schedule_every_vehicle_every_day(+P) -True if in plan P every vehicle is scheduled exactly once every working day.
schedule_every_vehicle_every_day(plan(Schedules)) :-
			findall(D,working_day(D,_,_),Days),
			findall(V,is_valid_vehicle(V),Vs),
			schedules_every_day_check(Schedules,Days,Vs).

% schedules_every_day_check(+Schedules, +Days, +Vs) - True if every vehicle of Vs is scheduled exactly once every day of Days.
schedules_every_day_check(_,[],_):-!.
schedules_every_day_check(Schedules, [Day|T], Vs) :-
			schedules_every_vehicle_on_day(Schedules,Day,Vs),
			schedules_every_day_check(Schedules,T,Vs).

% schedules_every_vehicle_on_day(+Schedules, +Day, +Vs) - True if every vehicle of Vs is scheduled exactly once on working day Day.
schedules_every_vehicle_on_day([],_,[]).
schedules_every_vehicle_on_day([schedule(VID,Day,_)|T],Day,Vs) :-
			!,
			member(VID,Vs), !,
			delete(Vs,VID,NewVs),
			schedules_every_vehicle_on_day(T,Day,NewVs).
schedules_every_vehicle_on_day([schedule(_,D,_)|T],Day,Vs) :-
			D \= Day,
			!,
			schedules_every_vehicle_on_day(T,Day,Vs).



% Hard Constraint 2 : Each order at most once.


% order_at_most_once(+P) - True if in plan P each order is shipped at most once.
order_at_most_once(plan(Schedules)) :- no_duplicate_order_schedule(Schedules,[]).

% no_duplicate_order_schedule(+Schedules, +Delivered) - True if orders are shipped at most once in Schedules and if the shipped order are not in Delivered.
no_duplicate_order_schedule([],_).
no_duplicate_order_schedule([schedule(_,_,R)|T],Delivered) :-
			order_list_route_once(R,RouteDelivered),
			intersection(Delivered,RouteDelivered,X),
			length(X,0),
			append(Delivered,RouteDelivered,NewDelivered),
			no_duplicate_order_schedule(T,NewDelivered).


% Hard Constraint 3 : Only during working day.


% no_overtime(+P) - True if there is no vehicle working after the end of a working day.
no_overtime(plan(Schedules)) :- no_overtime(Schedules,Schedules).

% no_overtime(+Schedules1, +Schedules) - True if there no vehicle working after the end of a working day in Schedules1 where Schedules1
%										 is included in Schedules
no_overtime([],_).
no_overtime([H|T],Schedules) :- 
			schedule_no_overtime(H,Schedules),
			no_overtime(T,Schedules).

% schedule_no_overtime(+Schedule, +Schedules) - True if the Schedule finish before the end of the day, where Schedule is included in Schedules.
schedule_no_overtime(schedule(Vid,Day,R),Schedules) :-
			previous_depot(Day,Vid,Schedules,StartId),
			route_duration(Vid,R,StartId,Duration),
			working_day(Day,StartTime,EndTime),
			End is StartTime + Duration,
			End =< EndTime.

% Hard Constraint 4 : Only visit depot when empty
% => Implemented in Hard Constraint 5.

% Hard Constraint 5 : Item only taken if it is still available.

% enough_inventory(+P) - True if in plan P every order are loaded only if there is enough inventory. Orders are only loaded in the last depot
%						 visited before shipping.
enough_inventory(plan(Schedules)) :-
			findall(Did,depot(Did,_,_),Ds),
			enough_inventory_all_depots(Ds,Schedules).

% enough_inventory_all_depots(+Ds, +Schedules) - True if there is enough inventory in all depots of Ds to load designed orders in Schedules.
enough_inventory_all_depots([],_).
enough_inventory_all_depots([H|T],Schedules) :-
			depot(H,Inventory,_),
			findall(Oid,order_taken_in_depot(H,Schedules,Schedules,Oid),Os),
			enough_inventory_all_orders(Inventory,Os),
			enough_inventory_all_depots(T,Schedules).

% enough_inventory_all_orders(+Inventory, +Orders) - True if there is enough product in Inventory to load all Orders.
enough_inventory_all_orders(_,[]):- !.
enough_inventory_all_orders(Inventory, [H|T]) :-
			!,
			update_inventory(Inventory,H,NewInventory),!,
			enough_inventory_all_orders(NewInventory,T).

% order_taken_in_depot(+Did,+Schedules1,+Schedules,?Oid) - Oid is an order taken in depot Did in the Schedules1 where Schedules1 is included
%															in Scheduls.
order_taken_in_depot(Did,[H|T],Schedules,Oid) :-
			order_taken_in_depot_schedule(Did,H,Schedules,Oid);
			order_taken_in_depot(Did,T,Schedules,Oid).

% order_taken_in_depot(+Did,+Schedule,+Schedules,?Oid) - Oid is an order taken in depot Did in Schedule in the context of Schedules.
order_taken_in_depot_schedule(Did,schedule(Vid,Day,R),Schedules,Oid) :-
			previous_depot(Day,Vid,Schedules,StartId),
			order_taken_in_depot_route(Did,R,StartId,Oid).

% Hard Constraint 6 : Vehicles can not take more weight than their capacity.

% plan_no_overload(+P) - True if in plan P, vehicle never carry more than their capacity.
plan_no_overload(plan(Schedules)) :- schedule_not_overload(Schedules).

% schedule_not_overload(+Schedules) - True if in Schedules, vehicle never carry more than their capacity.
schedule_not_overload([]).
schedule_not_overload([schedule(Vid,_,R)|T]) :-
			route_no_overload(R,Vid),
			schedule_not_overload(T).

% route_no_overload(+Route,+Vid)
route_no_overload([],_) :- !.
route_no_overload(Route,Vid) :-
			Route \= [],
			!,
			vehicle(Vid,_,C,_,_,_),
			following_orders_in_route(Route,Orders),
			load(Orders,Load),
			Load =< C,
			append(Orders,TempRoute,Route),
			[_|NewRoute] = TempRoute,
			route_no_overload(NewRoute,Vid).
