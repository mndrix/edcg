% Operations on the Acc and Pass data structures:

:- ['generic-util.pl'].
:- ['special-util.pl'].

% Create the Acc and Pass data structures:
% Acc contains terms of the form acc(A,LeftA,RightA) where A is the name of an
% accumulator, and RightA and LeftA are the accumulating parameters.
% Pass contains terms of the form pass(A,Arg) where A is the name of a passed
% argument, and Arg is the argument.
'_create_acc_pass'([], _, _, [], []).
'_create_acc_pass'([A|AList], Index, TGoal, [acc(A,LeftA,RightA)|Acc], Pass) :-
    '_is_acc'(A), !,
    Index1 is Index+1,
    arg(Index1, TGoal, LeftA),
    Index2 is Index+2,
    arg(Index2, TGoal, RightA),
    '_create_acc_pass'(AList, Index2, TGoal, Acc, Pass).
'_create_acc_pass'([A|AList], Index, TGoal, Acc, [pass(A,Arg)|Pass]) :-
    '_is_pass'(A), !,
    Index1 is Index+1,
    arg(Index1, TGoal, Arg),
    '_create_acc_pass'(AList, Index1, TGoal, Acc, Pass).
'_create_acc_pass'([A|_AList], _Index, _TGoal, _Acc, _Pass) :-
    \+'_is_acc'(A),
    \+'_is_pass'(A),
    print_message(error,not_a_hidden_param(A)).


% Use the Acc and Pass data structures to create the arguments of a body goal:
% Add the hidden parameters named in GList to the goal.
'_use_acc_pass'([], _, _, Acc, Acc, _).
% 1a. The accumulator A is used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
    '_replace_acc'(A, LeftA, RightA, MidA, RightA, Acc, MidAcc), !,
    Index1 is Index+1,
    arg(Index1, TGoal, LeftA),
    Index2 is Index+2,
    arg(Index2, TGoal, MidA),
    '_use_acc_pass'(GList, Index2, TGoal, MidAcc, NewAcc, Pass).
% 1b. The accumulator A is not used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
    '_acc_info'(A, LStart, RStart), !,
    Index1 is Index+1,
    arg(Index1, TGoal, LStart),
    Index2 is Index+2,
    arg(Index2, TGoal, RStart),
    '_use_acc_pass'(GList, Index2, TGoal, Acc, NewAcc, Pass).
% 2a. The passed argument A is used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
    '_is_pass'(A),
    member(pass(A,Arg), Pass), !,
    Index1 is Index+1,
    arg(Index1, TGoal, Arg),
    '_use_acc_pass'(GList, Index1, TGoal, Acc, NewAcc, Pass).
% 2b. The passed argument A is not used in the head:
'_use_acc_pass'([A|GList], Index, TGoal, Acc, NewAcc, Pass) :-
    '_pass_info'(A, AStart), !,
    Index1 is Index+1,
    arg(Index1, TGoal, AStart),
    '_use_acc_pass'(GList, Index1, TGoal, Acc, NewAcc, Pass).
% 3. Defaulty case when A does not exist:
'_use_acc_pass'([A|_GList], _Index, _TGoal, Acc, Acc, _Pass) :-
    print_message(error,not_a_hidden_param(A)).

% Finish the Acc data structure:
% Link its Left and Right accumulation variables together in pairs:
'_finish_acc'([]).
'_finish_acc'([acc(_,Link,Link)|Acc]) :- '_finish_acc'(Acc).

% Replace elements in the Acc data structure:
% Succeeds iff replacement is successful.
'_replace_acc'(A, L1, R1, L2, R2, Acc, NewAcc) :-
    member(acc(A,L1,R1), Acc), !,
    '_replace'(acc(A,_,_), acc(A,L2,R2), Acc, NewAcc).

% Combine two accumulator lists ('or'ing their values)
'_merge_acc'([], [], G1, G1, [], G2, G2, []) :- !.
'_merge_acc'([acc(Acc,OL,R)|Accs], [acc(Acc,L1,R)|Accs1], G1, NG1,
         [acc(Acc,L2,R)|Accs2], G2, NG2, [acc(Acc,NL,R)|NewAccs]) :- !,
    ( ( OL == L1, OL \== L2 ) ->
      MG1 = (G1,L1=L2), MG2 = G2, NL = L2
        ; ( OL == L2, OL \== L1 ) ->
      MG2 = (G2,L2=L1), MG1 = G1, NL = L1
        ; MG1 = G1, MG2 = G2, L1 = L2, L2 = NL ),
    '_merge_acc'(Accs, Accs1, MG1, NG1, Accs2, MG2, NG2, NewAccs).
