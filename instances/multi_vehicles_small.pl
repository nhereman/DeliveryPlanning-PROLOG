%products
product(p1,1,0.1).
product(p2,25,20).
product(p3,50,0.1).

%orders
order(o1,[p3/7],location(8,172),1).
order(o2,[p2/10],location(75,100),1).
order(o3,[p1/10,p2/8],location(41,166),1).
order(o4,[p3/2],location(70,30),1).
order(o5,[p1/214],location(21,172),1).
order(o6,[p1/100,p2/3],location(34,55),1).
order(o7,[p1/46],location(23,88),1).
order(o8,[p3/1],location(84,136),1).
order(o9,[p2/10],location(111,162),1).
order(o10,[p1/79,p2/9],location(105,124),1).

%warehouses
depot(d1,[p1/500,p2/50,p3/15],location(100,100)).

%vehicles
vehicle(v1,d1,500,1.5,150,0.5).%slow, large capacity
vehicle(v2,d1,100,1,150,0.2).%normal, low capacity
vehicle(v3,d1,200,0.75,200,0.5).%fast, low capacity

%working days
working_day(1,600,960).