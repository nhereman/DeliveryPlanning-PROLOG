%%% DRIVING_DURATION %%%

%driving_duration(+VID,+FromID,+ToID,-Duration)
driving_duration(VID,FromID,ToID,Duration) :-
			vehicle(VID,_,_,Pace,_,_),
			is_location(FromID,Floc),
			is_location(ToID,Tloc),
		    distance(Floc,Tloc,Dist),
		    Duration is Pace * Dist.

distance(location(X1,Y1),location(X2,Y2), Dist) :- Dist is abs(X1-X2) + abs(Y1-Y2).

is_location(ID,Location) :- 
			depot(ID,_,Location), !;
			order(ID,_,Location,_). 

%%% EARNING %%%

%earning(+OID,+Day,-Value)
earning(OID,Day,Value) :-
			working_day(Day,_,_),
			order(OID, OrderDetails, _, Deadline),
			(Day =< Deadline, !, order_detail_value(OrderDetails,Value);
			Day > Deadline, !, order_detail_value(OrderDetails,T), Value is T / 2).

order_detail_value([], 0.0).
order_detail_value([Pid/Amount|R],Value) :-
			product(Pid,Pval,_),
			order_detail_value(R,NewValue),
			Value is NewValue + (Pval * Amount).


%%% LOAD %%%

%load(+Os,-Weight)
load([],0.0).
load([OID|R],Weight) :-
			order(OID,OrderDetails,_,_),
			order_detail_weight(OrderDetails, OrderWeight),
			load(R,NewWeight),
			Weight is NewWeight + OrderWeight.

order_detail_weight([],0.0).
order_detail_weight([Pid/Amount|R],Weight) :-
			product(Pid,_,Pweight),
			order_detail_weight(R,NewWeight),
			Weight is NewWeight + (Pweight * Amount).

%%% UPDATE_INVENTORY %%%

%update_inventory(+Inventory, ?OID, ?NewInventory)
update_inventory(Inventory,OID,NewInventory) :-
			order(OID,OrderDetails,_,_),
			update_inventory_order_detail(Inventory,OrderDetails,NewInventory).

update_inventory_order_detail(Inventory,[],NewInventory) :- permutation(Inventory,NewInventory), !.
update_inventory_order_detail(Inventory, [PID/Amount|R], NewInventory) :-
							remove_product_inventory(Inventory,PID,Amount,T),
							update_inventory_order_detail(T,R,NewInventory).


%remove_product_inventory(Inventory,PID,Amount,NewInventory).
remove_product_inventory([],_,_,_) :- fail.
remove_product_inventory([PID/AmountI|T], PID, Amount, NewInventory) :-
						NewAmount is AmountI - Amount,
						NewAmount > 0,
						!,
						NewInventory = [PID/NewAmount|T].
remove_product_inventory([PID/AmountI|T], PID, Amount, NewInventory) :-
						AmountI = Amount,
						!,
						(NewInventory = T ; 
							NewInventory = [PID/0|T]).					
remove_product_inventory([Iid/AmountI|T],PID,Amount,NewInventory) :-
						Iid \= PID,
						!,
						remove_product_inventory(T,PID,Amount,TempInv),
						NewInventory = [Iid/AmountI|TempInv].




%%% PLAN FORMAT VALID %%%

is_valid_vehicle(VID) :- vehicle(VID,_,_,_,_,_).
is_working_day(Day) :-  working_day(Day,_,_).
is_valid_order(OID) :- order(OID,_,_,_).
is_valid_depot(DID) :- depot(DID,_,_).

is_valid_route_format([]).
is_valid_route_format([H|T]) :-
			((is_valid_order(H),!);(is_valid_depot(H),!)),
			is_valid_route_format(T).

is_valid_schedule_format(schedule(VID,Day,Route)) :-
			is_valid_vehicle(VID),
			is_working_day(Day),
			is_valid_route_format(Route).

is_valid_schedulelist_format([]).
is_valid_schedulelist_format([H|T]) :-
			is_valid_schedule_format(H),
			is_valid_schedulelist_format(T).

is_valid_format(plan(X)) :- is_valid_schedulelist_format(X).


%%% IS_VALID %%%

%is_valid(+P).
is_valid(P) :-
			is_valid_format(P), % Add Hard Constraints 4,5
			schedule_every_vehicle_every_day(P),
			order_at_most_once(P),
			no_overtime(P),
			enough_inventory(P),
			plan_no_overload(P).


% Hard Constraint 1 : Exactly one schedule for each vehicle for each working_day

schedule_every_vehicle_every_day(plan(Schedules)) :-
			findall(D,working_day(D,_,_),Days),
			findall(V,is_valid_vehicle(V),Vs),
			schedules_every_day_check(Schedules,Days,Vs).

schedules_every_day_check(_,[],_):-!.
schedules_every_day_check(Schedules, [Day|T], Vs) :-
			schedules_every_vehicle_on_day(Schedules,Day,Vs),
			schedules_every_day_check(Schedules,T,Vs).

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

order_at_most_once(plan(Schedules)) :- no_duplicate_order_schedule(Schedules,[]).

no_duplicate_order_schedule([],_).
no_duplicate_order_schedule([schedule(_,_,R)|T],Delivered) :-
			order_list_route_once(R,RouteDelivered),
			intersection(Delivered,RouteDelivered,X),
			length(X,0),
			append(Delivered,RouteDelivered,NewDelivered),
			no_duplicate_order_schedule(T,NewDelivered).


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

no_overtime(plan(Schedules)) :- no_overtime(Schedules,Schedules).

no_overtime([],_).
no_overtime([H|T],Schedules) :- 
			schedule_no_overtime(H,Schedules),
			no_overtime(T,Schedules).

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


route_duration(Vid,R,StartId,Duration) :-
			route_driving_duration(Vid,R,StartId,D1),
			route_orders_duration(R,D2),
			Duration is D1+D2.

route_driving_duration(_,[],_,0.0) :- !.
route_driving_duration(Vid,[H|T],PrecId,Duration) :-
			route_driving_duration(Vid,T,H,D1),
			driving_duration(Vid,PrecId,H,D2),
			Duration is D1 + D2.

route_orders_duration(R,Duration) :-
			order_list_route_once(R,Orders),
			length(Orders,Nb),
			Duration is 10.0 * Nb.


previous_day(2,1) :- working_day(1,_,_).
previous_day(X,Y) :- X > 2, Y is X - 1, working_day(Y,_,_), !.
previous_day(X,Z) :- X > 2, Y is X - 1, previous_day(Y,Z), !.


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


last_pos_id(schedule(Vid,_,R),ID) :- last_pos_id(R,Vid,ID).

last_pos_id([],Vid,ID) :- vehicle(Vid,ID,_,_,_,_).
last_pos_id([H|[]],_,H) :- !.
last_pos_id([_|T],Vid,ID) :-
			T \= [],
			!,
			last_pos_id(T,Vid,ID).




% Hard Constraint 4 : Only visit depot when empty

% Hard Constraint 5 : Item only taken if it is still available.
enough_inventory(plan(Schedules)) :-
			findall(Did,depot(Did,_,_),Ds),
			enough_inventory_all_depots(Ds,Schedules).

enough_inventory_all_depots([],_).
enough_inventory_all_depots([H|T],Schedules) :-
			depot(H,Inventory,_),
			findall(Oid,order_taken_in_depot(H,Schedules,Schedules,Oid),Os),
			enough_inventory_all_orders(Inventory,Os),
			enough_inventory_all_depots(T,Schedules).


enough_inventory_all_orders(_,[]):- !.
enough_inventory_all_orders(Inventory, [H|T]) :-
			!,
			update_inventory(Inventory,H,NewInventory),!,%%%%%%%% !!!! MOVE THIS CUT !!!!
			enough_inventory_all_orders(NewInventory,T).


order_taken_in_depot(Did,[H|T],Schedules,Oid) :-
			order_taken_in_depot_schedule(Did,H,Schedules,Oid);
			order_taken_in_depot(Did,T,Schedules,Oid).


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
plan_no_overload(plan(Schedules)) :- schedule_not_overload(Schedules).

schedule_not_overload([]).
schedule_not_overload([schedule(Vid,_,R)|T]) :-
			vehicle(Vid,_,C,_,_,_),
			consecutive_orders_route(R,Orders),
			max_load_orders(Orders,Max),
			Max < C,
			schedule_not_overload(T).

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


