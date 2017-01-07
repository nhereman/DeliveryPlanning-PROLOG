:- module(auxiliary,[driving_duration/4, distance/3, is_location/2, earning/3, load/2,update_inventory/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    %%
%%       AUXILIARY PREDICATES         %%
%%                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%
%   DRIVING_DURATION   %
%%%%%%%%%%%%%%%%%%%%%%%%

% driving_duration(+VID,+FromID,+ToID,-Duration) - Duration is the driving duration between FromId to ToID with vehicle VID.
driving_duration(VID,FromID,ToID,Duration) :-
			vehicle(VID,_,_,Pace,_,_),
			is_location(FromID,Floc),
			is_location(ToID,Tloc),
		    distance(Floc,Tloc,Dist),
		    Duration is Pace * Dist.

% distance(+Location1, +Location2, -Dist) - Dist is the distance between the two location.
distance(location(X1,Y1),location(X2,Y2), Dist) :- Dist is abs(X1-X2) + abs(Y1-Y2).

% is_location(+ID, -Location) - Location is the location associated to the ID.
is_location(ID,Location) :- 
			depot(ID,_,Location), !;
			order(ID,_,Location,_). 

%%%%%%%%%%%%%%%
%   EARNING   %
%%%%%%%%%%%%%%%

% earning(+OID,+Day,-Value) - Value is the amount of $ earned by shipping order OID on working_day Day.
earning(OID,Day,Value) :-
			working_day(Day,_,_),
			order(OID, OrderDetails, _, Deadline),
			(Day =< Deadline, !, order_detail_value(OrderDetails,Value);
			Day > Deadline, !, order_detail_value(OrderDetails,T), Value is T / 2).

% order_detail_value(+OrderDetail, -Value) - Value is the value of the product in OrderDetail.
order_detail_value([], 0.0).
order_detail_value([Pid/Amount|R],Value) :-
			product(Pid,Pval,_),
			order_detail_value(R,NewValue),
			Value is NewValue + (Pval * Amount).

%%%%%%%%%%%%
%   LOAD   %
%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%
%   UPDATE_INVENTORY   %
%%%%%%%%%%%%%%%%%%%%%%%%

% update_inventory(+Inventory, ?OID, ?NewInventory) - NewInventory is the state of Inventory after the order OID is removed.
update_inventory(Inventory,OID,NewInventory) :-
			order(OID,OrderDetails,_,_),
			update_inventory_order_detail(Inventory,OrderDetails,NewInventory).

% update_inventory(+Inventory, ?OrderDetails, ?NewInventory) - NewInventory is the state of Inventory after the products for OrderDetails are removed.
update_inventory_order_detail(Inventory,[],NewInventory) :- permutation(Inventory,NewInventory), !.
update_inventory_order_detail(Inventory, [PID/Amount|R], NewInventory) :-
							remove_product_inventory(Inventory,PID,Amount,T),
							update_inventory_order_detail(T,R,NewInventory).


%remove_product_inventory(+Inventory,?PID, ?Amount, ?NewInventory)  - NewInventory is the state of Inventory after an Amount of PID is removed.
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

