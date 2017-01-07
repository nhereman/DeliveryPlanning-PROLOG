:- module(find_optimal,[find_optimal/1]).

:- use_module(is_valid,[is_valid/1]).
:- use_module(profit,[profit/2]).

%%%%%%% FIND OPTIMAL %%%%%%%

:- dynamic best/2.

% find_optimal(-X) -X is a valid plan maximizing profit.
find_optimal(_) :-
			assert(best(nul,-1.0)),
			is_valid(X),
			profit(X,ValueX),
			update_best(X,ValueX),
			fail.
find_optimal(X) :-
			best(X,_),
			retract(best(_,_)).

% update_best(+X,+ValueX) - Update the best solution to find_optimal if ValueX is bigger than the best.
update_best(X,ValueX) :-
			best(_,ValueBest),
			ValueX > ValueBest,
			!,
			retract(best(_,_)),
			assert(best(X,ValueX)).
update_best(_,_).