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
			is_valid_format(P), % Add Hard Constraints 2,3,4,5,6
			schedule_every_vehicle_every_day(P),
			order_at_most_once(P).


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

