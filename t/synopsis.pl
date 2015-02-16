:- use_module(library(edcg)).

% Declare accumulators
edcg:acc_info(adder, X, In, Out, plus(X,In,Out)).

% Declare predicates using these hidden arguments
edcg:pred_info(len,0,[adder,dcg]).
edcg:pred_info(increment,0,[adder]).

increment -->>
    [1]:adder.


len(Xs,N) :-
    len(0,N,Xs,[]).

len -->>
    [_],
    !,
    increment,
    len.
len -->>
    [].


:- use_module(library(tap)).

len([],0).
len([a],1).
len([a,b,a],3).
