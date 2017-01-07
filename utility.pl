:- module(utility,[previous_depot/4,previous_day/2,last_pos_id/2,is_day_vehicle_schedule/4,order_taken_in_depot_route/4,is_valid_vehicle/1,
					is_working_day/1,is_valid_order/1,is_valid_depot/1,order_list_route_once/2]).

% Some usefull predicates.


% is_valid_vehicle(+VID) - true if VID is the id of a vehicle.
is_valid_vehicle(VID) :- vehicle(VID,_,_,_,_,_).
% is_working_day(+Day) - True if Day is a working day.
is_working_day(Day) :-  working_day(Day,_,_).
% is_valid_order(+OID) - True if OID is the id of an order.
is_valid_order(OID) :- order(OID,_,_,_).
% is_valid_depot(+DID) - True if DID is the id of a depot.
is_valid_depot(DID) :- depot(DID,_,_).


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

% previous_depot(+Day, +Vid, +Schedules, -Did) - Did is the depot ID where the vehicle Vid start his Day in the context Schedules.
previous_depot(Day,Vid,_,Did) :-
			working_day(Day,_,_),
			not(previous_day(Day,_)),
			!,
			vehicle(Vid,Did,_,_,_,_).
previous_depot(Day,Vid,Schedules,Did) :-
			previous_day(Day,PrevDay),
			!,
			is_day_vehicle_schedule(PrevDay,Vid,Schedules,S),
			last_pos_id(S,Did).


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