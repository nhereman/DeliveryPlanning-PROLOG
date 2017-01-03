distance(location(X1,Y1),location(X2,Y2), Dist) :- Dist is abs(X1-X2) + abs(Y1-Y2).

driving_duration(VID,FromID,ToID,Duration) :-
			vehicle(VID,_,_,Pace,_,_),
			depot(FromID,_,Floc),
			depot(ToID,_,Tloc),
		    distance(Floc,Tloc,Dist),
		    Duration is Dist * Pace.


earning(OID,Day,Value) :-
			working_day(Day,_,_),
			order(OID, OrderDetails, _, _),
			order_detail_value(OrderDetails,Value).

order_detail_value([], 0).
order_detail_value([Pid/Amount|R],Value) :-
			product(Pid,Pval,_),
			order_detail_value(R,NewValue),
			Value is NewValue + (Pval * Amount).


load([],0).
load([order(_,OrderDetails,_,_)|R],Weight) :-
			order_detail_weight(OrderDetails, OrderWeight),
			load(R,NewWeight),
			Weight is NewWeight + OrderWeight.

order_detail_weight([],0).
order_detail_weight([Pid/Amount|R],Weight) :-
			product(Pid,_,Pweight),
			order_detail_weight(R,NewWeight),
			Weight is NewWeight + (Pweight * Amount).


update_inventory(Inventory,OID,NewInventory) :-
			order(OID,OrderDetails,_,_),
			update_inventory_order_detail(Inventory,OrderDetails,NewInventory).


update_inventory_order_detail(Inventory,[Pid/Amount],NewInventory) :- remove_product_inventory(Inventory,Pid,Amount,NewInventory).
update_inventory_order_detail(Inventory,[Pid/Amount|Rest],NewInventory) :-
			update_inventory_order_detail(Inventory,Rest,MidInventory),
			remove_product_inventory(MidInventory,Pid,Amount,NewInventory).

remove_product_inventory(Inventory,PID,Amount,NewInventory) :- remove_product_inventory(Inventory,PID,Amount,[],NewInventory).
remove_product_inventory([],_,_,NewInventory,NewInventory).
remove_product_inventory([PID/Iamount|Irest],PID,Amount,Acc,NewInventory) :-
			NewAmount is Iamount - Amount,
			((Iamount = Amount,NewAcc = Acc);(Iamount > Amount,append(Acc,[PID/NewAmount],NewAcc))),
			remove_product_inventory(Irest,PID,Amount,NewAcc,NewInventory).
remove_product_inventory([Ipid/Iamount|Irest],PID,Amount,Acc,NewInventory) :-
			Ipid \= PID,
			append(Acc,[Ipid/Iamount],NewAcc),
			remove_product_inventory(Irest,PID,Amount,NewAcc,NewInventory).