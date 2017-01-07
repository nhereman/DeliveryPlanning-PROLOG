:- module(is_valid,[is_valid/1,previous_day/2,last_pos_id/2,is_day_vehicle_schedule/4 ,order_list_route_once/2,order_taken_in_depot_route/4]).

:- use_module(auxiliary,[driving_duration/4, distance/3, is_location/2, load/2,update_inventory/3]).

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

% is_valid_vehicle(+VID) - true if VID is the id of a vehicle.
is_valid_vehicle(VID) :- vehicle(VID,_,_,_,_,_).
% is_working_day(+Day) - True if Day is a working day.
is_working_day(Day) :-  working_day(Day,_,_).
% is_valid_order(+OID) - True if OID is the id of an order.
is_valid_order(OID) :- order(OID,_,_,_).
% is_valid_depot(+DID) - True if DID is the id of a depot.
is_valid_depot(DID) :- depot(DID,_,_).


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

% order_list_route_once(+Schedules, +Delivered) - True if orders are shipped at most once in Schedules and if the shipped order are not in Delivered.
no_duplicate_order_schedule([],_).
no_duplicate_order_schedule([schedule(_,_,R)|T],Delivered) :-
			order_list_route_once(R,RouteDelivered),
			intersection(Delivered,RouteDelivered,X),
			length(X,0),
			append(Delivered,RouteDelivered,NewDelivered),
			no_duplicate_order_schedule(T,NewDelivered).


% order_list_route_once(+Route,-Orders) - Orders is the list of order id shipped during Route. Fail if an order is shipped more than once.
order_list_route_once([],[]).
order_list_route_once([H|T],Orders) :-
			is_valid_order(H),
			!,
			order_list_route_once(T,NewOrders),
			not(member(H,NewOrders)),
			Orders = [H|NewOrders].
order_list_route_once([H|T],Orders) :-
			is_valid_depot(H),
			!,
			order_list_route_once(T,Orders).



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
schedule_no_overtime(schedule(Vid,Day,R),_) :-
			not(previous_day(Day,_)),
			!,
			vehicle(Vid,StartId,_,_,_,_),
			route_duration(Vid,R,StartId,Duration),
			working_day(Day,StartTime,EndTime),
			End is StartTime + Duration,
			End =< EndTime.
schedule_no_overtime(schedule(Vid,Day,R),Schedules) :-
			previous_day(Day,NewDay),
			!,
			is_day_vehicle_schedule(NewDay,Vid,Schedules,S),
			last_pos_id(S,StartId),
			route_duration(Vid,R,StartId,Duration),
			working_day(Day,StartTime,EndTime),
			End is StartTime + Duration,
			End =< EndTime.


% route_duration(+Vid, +R, +StartId, -Duration) - Duration is the duration of the route R done by vehicle Vid starting at StartId.
route_duration(Vid,R,StartId,Duration) :-
			route_driving_duration(Vid,R,StartId,D1),
			route_orders_duration(R,D2),
			Duration is D1+D2.

% route_driving_duration(+Vid, +R, +PrecId, -Duration) - Duration is the driving duration of the route R bye vehicle Vid starting at PrecId.
route_driving_duration(_,[],_,0.0) :- !.
route_driving_duration(Vid,[H|T],PrecId,Duration) :-
			route_driving_duration(Vid,T,H,D1),
			driving_duration(Vid,PrecId,H,D2),
			Duration is D1 + D2.

% route_orders_duration(+R, -Duration) - Duration is the duration taken in route R to load and unload orders.
route_orders_duration(R,Duration) :-
			order_list_route_once(R,Orders),
			length(Orders,Nb),
			Duration is 10.0 * Nb.

% previous_day(+Day, -PrevDay) - PrevDay is the working day preceding Day.
previous_day(2,1) :- working_day(1,_,_).
previous_day(X,Y) :- X > 2, Y is X - 1, working_day(Y,_,_), !.
previous_day(X,Z) :- X > 2, Y is X - 1, previous_day(Y,Z), !.

% is_day_vehicle_schedule(+Day, +Vid, +Schedules, -Schedule) - Schedule is the schedule corresponding to vehicle Vid and day Day in Schedules.
is_day_vehicle_schedule(_,_,[],_) :- fail.
is_day_vehicle_schedule(Day,Vid,[schedule(Vid,Day,R)|_],schedule(Vid,Day,R)) :- !.
is_day_vehicle_schedule(Day,Vid,[schedule(_,D,_)|T],Schedule) :-
			Day \= D,
			!,
			is_day_vehicle_schedule(Day,Vid,T,Schedule).
is_day_vehicle_schedule(Day,Vid,[schedule(Id,_,_)|T],Schedule) :-
			Vid \= Id,
			!,
			is_day_vehicle_schedule(Day,Vid,T,Schedule).

% last_pos_id(+Schedule, -ID) - ID is the last position of the vehicle on Schedule.
last_pos_id(schedule(Vid,_,R),ID) :- last_pos_id(R,Vid,ID).

% last_pos_id(+Route, +Vid, -ID) - ID is the last position of the vehicle Vid on Route. 
last_pos_id([],Vid,ID) :- vehicle(Vid,ID,_,_,_,_).
last_pos_id([H|[]],_,H) :- !.
last_pos_id([_|T],Vid,ID) :-
			T \= [],
			!,
			last_pos_id(T,Vid,ID).


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
			update_inventory(Inventory,H,NewInventory),!,%%%%%%%% !!!! MOVE THIS CUT !!!!
			enough_inventory_all_orders(NewInventory,T).

% order_taken_in_depot(+Did,+Schedules1,+Schedules,?Oid) - Oid is an order taken in depot Did in the Schedules1 where Schedules1 is included
%															in Scheduls.
order_taken_in_depot(Did,[H|T],Schedules,Oid) :-
			order_taken_in_depot_schedule(Did,H,Schedules,Oid);
			order_taken_in_depot(Did,T,Schedules,Oid).

% order_taken_in_depot(+Did,+Schedule,+Schedules,?Oid) - Oid is an order taken in depot Did in Schedule in the context of Schedules.
order_taken_in_depot_schedule(Did,schedule(Vid,Day,R),_,Oid) :-
			not(previous_day(Day,_)),
			!,
			vehicle(Vid,StartId,_,_,_,_),
			order_taken_in_depot_route(Did,R,StartId,Oid).
order_taken_in_depot_schedule(Did,schedule(Vid,Day,R),Schedules,Oid) :-
			previous_day(Day,PrevDay),
			!,
			is_day_vehicle_schedule(PrevDay,Vid,Schedules,S),
			last_pos_id(S,StartId),
			order_taken_in_depot_route(Did,R,StartId,Oid).

% order_taken_in_depot_route(+Did,+Route,+LastDid,?Oid) -Oid is an order taken in depot Did in Route, where LastDid is the last depot visited.
order_taken_in_depot_route(_,[],_,_) :- !,fail.
order_taken_in_depot_route(_,_,LastDid,_) :- not(is_valid_depot(LastDid)),!,fail.
order_taken_in_depot_route(Did,[Oid|_],Did,Oid) :- is_valid_order(Oid).
order_taken_in_depot_route(Did,[H|T],LastDid,Oid) :- 
			is_valid_order(H),
			order_taken_in_depot_route(Did,T,LastDid,Oid).
order_taken_in_depot_route(Did,[H|T],_,Oid) :- 
			is_valid_depot(H),
			order_taken_in_depot_route(Did,T,H,Oid).



% Hard Constraint 6 : Vehicles can not take more weight than their capacity.

% plan_no_overload(+P) - True if in plan P, vehicle never carry more than their capacity.
plan_no_overload(plan(Schedules)) :- schedule_not_overload(Schedules).

% schedule_not_overload(+Schedules) - True if in Schedules, vehicle never carry more than their capacity.
schedule_not_overload([]).
schedule_not_overload([schedule(Vid,_,R)|T]) :-
			vehicle(Vid,_,C,_,_,_),
			consecutive_orders_route(R,Orders),
			max_load_orders(Orders,Max),
			Max < C,
			schedule_not_overload(T).

% max_load_orders(+Orders,-Max) - Max is the maximum load between the load of all Orders.
max_load_orders([],0.0).
max_load_orders([H|T],Max) :-
			max_load_orders(T,OldMax),
			load(H,Load),
			Load > OldMax,
			!,
			Max = Load.
max_load_orders([H|T],Max) :-
			max_load_orders(T,OldMax),
			load(H,Load),
			Load =< OldMax,
			!,
			Max = OldMax.


% consecutive_orders_route(+R, -Orders) - Orders is the list of all list of consecutive orders shipped.
consecutive_orders_route([],[[]]).
consecutive_orders_route([H|T],Orders) :-
			is_valid_depot(H),
			!,
			consecutive_orders_route(T,NewOrders),
			Orders = [[]|NewOrders].
consecutive_orders_route([H|T],Orders) :-
			is_valid_order(H),
			!,
			consecutive_orders_route(T,NewOrders),
			[NO1|NO2] = NewOrders,
			Orders2 = [H|NO1],
			Orders = [Orders2|NO2].