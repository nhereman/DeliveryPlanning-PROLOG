:- module(find_heuristically,[find_heuristically/1]).

:- use_module(utility,[previous_depot/4,is_valid_order/1,route_duration/4]).
:- use_module(auxiliary,[update_inventory/3,load/2,driving_duration/4]).
:- use_module(profit,[route_expenses/4,order_list_revenue/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%   FIND HEURISTICALLY  %
%%%%%%%%%%%%%%%%%%%%%%%%%



% find_heuristically(-P) :- Find a valid plan approximately maximizing profit.
find_heuristically(plan(Schedules)) :-
			findall(Day,working_day(Day,_,_),Days),
			sort(Days,SortedDays),
			findall(Oid,order(Oid,_,_,_),Orders),
			startingInventories(InvState),
			heurist_schedule_days(SortedDays,Orders,InvState,[],Schedules),
			is_valid(plan(Schedules)).



% heurist_schedule_days(+Days,+Orders,+InvState,+Scheduled ,-Schedules)
% Find a Schedules for days approximately maximizing profit in the context Scheduled.
heurist_schedule_days([],_,_,Scheduled,Scheduled).
heurist_schedule_days([H|T],Orders,InvState,Scheduled,Schedules) :-
			findall(Vid,vehicle(Vid,_,_,_,_,_),Vs),
			heurist_schedule_vehicles(Vs,H,Orders,InvState,Scheduled,NewScheduled,NewOrders,NewInvState),
			heurist_schedule_days(T,NewOrders,NewInvState,NewScheduled,Schedules).

% heurist_schedule_vehicles(+Vehicles,+Day,+Orders,+InvState,+Scheduled,-Schedules,-RemainingOrders, -NewInvState) -
% Find a Schedules for Vehicles in Day approximately maximizing profit.
% RemainingOrders are the orders remaining after Schedules.
heurist_schedule_vehicles([],_,Orders,InvState,Scheduled,Scheduled,Orders,InvState).
heurist_schedule_vehicles([H|T],Day,Orders,InvState,Scheduled,Schedules,RemainingOrders,NewInvState) :-
			heurist_schedule_vehicle(H,Day,Orders,InvState,Scheduled,NewScheduled,NewOrders,TempInvState),
			heurist_schedule_vehicles(T,Day,NewOrders,TempInvState,NewScheduled,Schedules,RemainingOrders,NewInvState).



% heurist_schedule_vehicle(+Vid,+Day,+Orders,+InvState,+Scheduled,-Schedules,-RemainingOrders,-NewInvState)
% Schedules is Scheduled with a schedule for the vehicle Vid on Day.
heurist_schedule_vehicle(Vid,Day,Orders,InvState,Scheduled,Schedules,RemainingOrders,NewInvState) :-
			working_day(Day,Time,_),
			previous_depot(Day,Vid,Scheduled,LastDepot),
			heurist_route(Vid,Day,Orders,InvState,[],LastDepot,Time,RemainingOrders,NewInvState,NewRoute),
			NewSchedule = schedule(Vid,Day,NewRoute),
			Schedules = [NewSchedule|Scheduled].



% heurist_route(+Vid,+Day,+Orders,+InvState,+Route,+LastDepot,+Time,-RemainingOrders,NewInvState,-NewRoute)
% NewRoute is Route extended in order to approximatly maximize profit
heurist_route(Vid,Day,Orders,InvState,Route,LastDepot,Time,RemainingOrders,NewInvState,NewRoute) :-
			findall(O,available_orders_depot(InvState,Orders,LastDepot,O),OLists),
			filter_overload_orders(OLists,Vid,FOLists),
			select_best_orderlist(FOLists,LastDepot,Vid,Day,Route,Time,[],BestOrders),
			BestOrders = [],
			!,
			RemainingOrders = Orders,
			NewInvState = InvState,
			NewRoute = Route.
heurist_route(Vid,Day,Orders,InvState,Route,LastDepot,Time,RemainingOrders,NewInvState,NewRoute) :-
			findall(O,available_orders_depot(InvState,Orders,LastDepot,O),OLists),
			filter_overload_orders(OLists,Vid,FOLists),
			select_best_orderlist(FOLists,LastDepot,Vid,Day,Route,Time,[],BestOrders),
			BestOrders \= [],
			!,
			append(Route,BestOrders,TempRoute),
			route_duration(Vid,BestOrders,LastDepot,OrderDuration),
			OrderTime is Time + OrderDuration,
			last(BestOrders,LastOrder),
			update_inventory_state_multiple(InvState,LastDepot,BestOrders,TempInvState),
			update_remaining_orders(Orders,BestOrders,TempOrders),
			findall(D,depot_reachable_on_time(OrderTime,Vid,Day,LastOrder,D),ReachableDepots),
			select_best_depot(ReachableDepots,TempInvState,TempOrders,-1,nil,NewDepot),
			driving_duration(Vid,LastOrder,NewDepot,ToDepotDuration),
			NewTime is OrderTime + ToDepotDuration,
			append(TempRoute,[NewDepot],TempRoute2),
			heurist_route(Vid,Day,TempOrders,TempInvState,TempRoute2,NewDepot,NewTime,RemainingOrders,NewInvState,NewRoute).


% select_best_orderlist(+OLists,+LastDepot,+Vid,+Day,+PrecRoute,+Time,+CurrMaxList,-MaxList) - Select the order list maximizing profit.
select_best_orderlist([],_,_,_,_,_,MaxList,MaxList).
select_best_orderlist([H|T],LastDepot,Vid,Day,PrecRoute,Time,CurrMaxList,MaxList) :-
			not(enough_time_to_join_a_depot(H,Vid,Time,LastDepot,Day)),
			!,
			select_best_orderlist(T,LastDepot,Vid,Day,PrecRoute,Time,CurrMaxList,MaxList).
select_best_orderlist([H|T],LastDepot,Vid,Day,PrecRoute,Time,CurrMaxList,MaxList) :-
			enough_time_to_join_a_depot(H,Vid,Time,LastDepot,Day),
			orderlist_profit(H,LastDepot,Vid,Day,PrecRoute,Profit),
			orderlist_profit(CurrMaxList,LastDepot,Vid,Day,PrecRoute,MaxProfit),
			Profit >= MaxProfit,
			!,
			select_best_orderlist(T,LastDepot,Vid,Day,PrecRoute,Time,H,MaxList).
select_best_orderlist([H|T],LastDepot,Vid,Day,PrecRoute,Time,CurrMaxList,MaxList) :-
			enough_time_to_join_a_depot(H,Vid,Time,LastDepot,Day),
			orderlist_profit(H,LastDepot,Vid,Day,PrecRoute,Profit),
			orderlist_profit(CurrMaxList,LastDepot,Vid,Day,PrecRoute,MaxProfit),
			Profit < MaxProfit,
			!,
			select_best_orderlist(T,LastDepot,Vid,Day,PrecRoute,Time,CurrMaxList,MaxList).

% select_best_depot(+Ds,+InvState,+RemainingOrders,+CurrNbOrderBest,+CurrBest,-Best)
select_best_depot([],_,_,_,Best,Best) :- Best \= nil.
select_best_depot([H|T],InvState,RemainingOrders,CurrNbOrderBest,_,Best) :-
			findall(O,available_orders_depot(InvState,RemainingOrders,H,O),OLists),
			length(OLists,Length),
			Length >= CurrNbOrderBest,
			!,
			select_best_depot(T,InvState,RemainingOrders,Length,H,Best).
select_best_depot([H|T],InvState,RemainingOrders,CurrNbOrderBest,CurrBest,Best) :-
			findall(O,available_orders_depot(InvState,RemainingOrders,H,O),OLists),
			length(OLists,Length),
			Length < CurrNbOrderBest,
			!,
			select_best_depot(T,InvState,RemainingOrders,CurrNbOrderBest,CurrBest,Best).

%%%%%%%%%%%%%%%%%%%%%%
% Usefull predicates %
%%%%%%%%%%%%%%%%%%%%%%


% update_remaining_orders(+Orders,+ToRemove,-RemainingOrders)
update_remaining_orders(Orders,[],Orders) :- !.
update_remaining_orders(Orders,[H|T],RemainingOrders) :-
			select(H,Orders,TempsOrders), !,
			update_remaining_orders(TempsOrders,T,RemainingOrders).

% depot_reachable_on_time(+Time,+Vid,+Day,+LastOrder,?Did) - True if depot Did is reachable before the end of Day.
depot_reachable_on_time(Time,Vid,Day,LastOrder,Did) :-
			depot(Did,_,_),
			driving_duration(Vid,LastOrder,Did,DrivingDuration),
			working_day(Day,_,EndTime),
			NewTime is Time + DrivingDuration,
			NewTime =< EndTime.

% enough_time_to_join_depot(+Orders,+Vid,+Time,+LastDepot,+Day) - True if Vid has enough tim to join a depot.
enough_time_to_join_a_depot([],_,_,_,_) :- !.
enough_time_to_join_a_depot(Orders,Vid,Time,LastDepot,Day) :-
			Orders \= [],
			!,
			working_day(Day,_,EndTime),
			findall(D,depot(D,_,_),Ds),
			last(Orders,LastId),
			shortest_time_to_depot(Ds,LastId,Vid,10000,DepotTime),
			route_duration(Vid,Orders,LastDepot,RouteDuration),
			TotalTime is Time + RouteDuration + DepotTime,
			TotalTime =< EndTime.


% shortest_time_to_depot(+Ds,+CurrId,+Vid,+CurrBestTime,-BestTime) - Give the shortest time to join a depot from currId.
shortest_time_to_depot([],_,_,BestTime,BestTime).
shortest_time_to_depot([H|T],CurrId,Vid,CurrBestTime,BestTime) :-
			driving_duration(Vid,CurrId,H,Time),
			Time =< CurrBestTime,
			!,
			shortest_time_to_depot(T,CurrId,Vid,Time,BestTime).
shortest_time_to_depot([H|T],CurrId,Vid,CurrBestTime,BestTime) :-
			driving_duration(Vid,CurrId,H,Time),
			Time > CurrBestTime,
			!,
			shortest_time_to_depot(T,CurrId,Vid,CurrBestTime,BestTime).
			

% orderlist_profit(+Orders,+Did,+Vid,+Day,+PrecRoute,-Profit) - Compute profit of Orders.
orderlist_profit(Orders,Did,Vid,Day,PrecRoute,Profit) :-
			PrecRoute = [],
			!,
			order_list_revenue(Orders,Day,Revenue),
			route_expenses(Orders,Vid,Did,Exp1),
			vehicle(Vid,_,_,_,UsageCost,_),
			Expenses is UsageCost + Exp1,
			Profit is Revenue - Expenses.
orderlist_profit(Orders,Did,Vid,Day,PrecRoute,Profit) :-
			PrecRoute \= [],
			!,
			order_list_revenue(Orders,Day,Revenue),
			route_expenses(Orders,Vid,Did,Expenses),
			Profit is Revenue - Expenses.

% filter(+Orders,+Vid,-NewList) :- NewList is Orders without the overloading order lists.
filter_overload_orders([],_,[]).
filter_overload_orders([H|T],Vid,NewList) :-
			vehicle(Vid,_,Cap,_,_,_),
			load(H,Load),
			Load =< Cap,
			!,
			filter_overload_orders(T,Vid,Temp),
			NewList = [H|Temp].
filter_overload_orders([H|T],Vid,NewList) :-
			vehicle(Vid,_,Cap,_,_,_),
			load(H,Load),
			Load > Cap,
			!,
			filter_overload_orders(T,Vid,NewList).



%%%%%%%%%%%%%%%%%%%%%%%
%  Inventories State  %
%%%%%%%%%%%%%%%%%%%%%%%


% startingInventories(-Inventories) - Inventories the state of the inventory of all depot at the start. Format : [Did/Inv,...]
startingInventories(Inventories) :- findall(Did/Inv,depot(Did,Inv,_),Inventories).

% update_inv_state(+InvState,?Did,?Oid,?NewState) - Update the state of inventories by removing order Oid from depot Did.
update_inventory_state(InvState,Did,Oid,NewState) :-
			member(Did/Inv,InvState),
			update_inventory(Inv,Oid,NewInv), !,
			select(Did/Inv,InvState,TempState), !,
			NewState = [Did/NewInv|TempState].

% update_inventory_state_multiple(+InvState,+Did,+Orders,?NewState) - Update the state of inventories by removing all Orders from depot Did.
update_inventory_state_multiple(InvState,_,[],InvState) :- !.
update_inventory_state_multiple(InvState,Did,[H|T],NewState) :-
			update_inventory_state(InvState,Did,H,TempState),
			update_inventory_state_multiple(TempState,Did,T,NewState).


% available_orders_depot(+InvState,+RemainingOrders,?Did,?Orders)
% Orders is a list of remaining order available in depot Did. The maximum size of Orders is 3.
available_orders_depot(InvState,RemainingOrders,Did,Orders) :-
			available_orders_depot(InvState,RemainingOrders,Did,[],Orders).

% available_orders_depot(+InvState,+RemainingOrders,?Did,+Acc,?Orders) - Orders is a list of order available in depot Did. The maximum size of Orders is 2.
available_orders_depot(_,_,_,Acc,Acc).
available_orders_depot(InvState,RemainingOrders,Did,Acc,Orders) :-
			length(Acc,L),
			L =< 1,
			member(O,RemainingOrders),
			not(member(O,Acc)),
			update_inventory_state(InvState,Did,O,NewState),
			select(O,NewAcc,Acc),
			available_orders_depot(NewState,RemainingOrders,Did,NewAcc,Orders).