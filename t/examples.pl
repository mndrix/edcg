:- use_module(library(edcg)).

% Declare accumulators
edcg:acc_info(castor,_,_,_,true).
edcg:acc_info(fwd, T, In, Out, Out=[T|In]). % forward accumulator
edcg:acc_info(rev, T, Out, In, Out=[T|In]). % reverse accumulator
edcg:acc_info(add, I, In, Out, plus(I,In,Out)).  % adder

% Declare passed arguments
edcg:pass_info(pollux).

% Declare predicates using these hidden arguments
edcg:pred_info(p,1,[castor,pollux]).
edcg:pred_info(q,1,[castor,pollux]).
edcg:pred_info(r,1,[castor,pollux]).
edcg:pred_info(flist,1,[fwd]).
edcg:pred_info(rlist,1,[rev]).
edcg:pred_info(sum_first_n,1,[add]).

% The program
:- dynamic q/4, r/4.  % eliminate "not defined" warnings
p(X) -->>
    Y is X+1,
    q(Y),
    r(Y).


% flist(N,[],List) creates the list [1,2,...,N]
flist(0) -->>
    !,
    [].
flist(N) -->>
    N>0,
    [N]:fwd,
    N1 is N-1,
    flist(N1).

% rlist(N,List,[]) creates the list [N,...,2,1]
rlist(0) -->>
    !,
    [].
rlist(N) -->>
    N>0,
    [N]:rev,
    N1 is N-1,
    rlist(N1).


% sum(N,0,Sum) adds the numbers 1,2,...,N
sum_first_n(0) -->>
    !,
    [].
sum_first_n(N) -->>
    N>0,
    [N]:add,
    N1 is N-1,
    sum_first_n(N1).


:- use_module(library(tap)).

'flist solutions' :-
    flist(7,[],L),
    L == [1,2,3,4,5,6,7].


'rlist solutions' :-
    rlist(7,L,[]),
    L == [7,6,5,4,3,2,1].

'trivial sum' :-
    sum_first_n(0,0,Sum),
    Sum == 0.

'real sum' :-
    sum_first_n(4,0,Sum),
    Sum is 4+3+2+1.
