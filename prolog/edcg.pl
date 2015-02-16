:- op(1200, xfx, ['-->>']).   % Same as ':-'.

% The predicate term_expansion/2 implements the extended translation.
% If loaded into Prolog along with the appropriate acc_info, pass_info,
% and pred_info facts it will be used automatically when consulting programs.

term_expansion((H-->>B), (TH:-TB)) :-
    functor(H, Na, Ar),
    '_has_hidden'(H, HList),
    '_new_goal'(H, HList, HArity, TH),
    '_create_acc_pass'(HList, HArity, TH, Acc, Pass),
    '_expand_goal'(B, TB, Na/Ar, HList, Acc, NewAcc, Pass),
    '_finish_acc'(NewAcc), !.

% Expand a goal:
'_expand_goal'((G1,G2), (TG1,TG2), NaAr, HList, Acc, NewAcc, Pass) :-
    '_expand_goal'(G1, TG1, NaAr, HList, Acc, MidAcc, Pass),
    '_expand_goal'(G2, TG2, NaAr, HList, MidAcc, NewAcc, Pass).
'_expand_goal'((G1->G2;G3), (TG1->TG2;TG3), NaAr, HList, Acc, NewAcc, Pass) :-
    '_expand_goal'(G1, TG1, NaAr, HList, Acc, MidAcc, Pass),
    '_expand_goal'(G2, MG2, NaAr, HList, MidAcc, Acc1, Pass),
    '_expand_goal'(G3, MG3, NaAr, HList, Acc, Acc2, Pass),
    '_merge_acc'(Acc, Acc1, MG2, TG2, Acc2, MG3, TG3, NewAcc).
'_expand_goal'((G1;G2), (TG1;TG2), NaAr, HList, Acc, NewAcc, Pass) :-
    '_expand_goal'(G1, MG1, NaAr, HList, Acc, Acc1, Pass),
    '_expand_goal'(G2, MG2, NaAr, HList, Acc, Acc2, Pass),
    '_merge_acc'(Acc, Acc1, MG1, TG1, Acc2, MG2, TG2, NewAcc).
'_expand_goal'((G1->G2), (TG1->TG2), NaAr, HList, Acc, NewAcc, Pass) :-
    '_expand_goal'(G1, TG1, NaAr, HList, Acc, MidAcc, Pass),
    '_expand_goal'(G2, TG2, NaAr, HList, MidAcc, NewAcc, Pass).
'_expand_goal'((\+G), (\+TG), NaAr, HList, Acc, Acc, Pass) :-
    '_expand_goal'(G, TG, NaAr, HList, Acc, _TempAcc, Pass).

'_expand_goal'({G}, G, _, _, Acc, Acc, _) :- !.
'_expand_goal'(insert(X,Y), LeftA=X, _, _, Acc, NewAcc, _) :-
    '_replace_acc'(dcg, LeftA, RightA, Y, RightA, Acc, NewAcc), !.
'_expand_goal'(insert(X,Y):A, LeftA=X, _, _, Acc, NewAcc, _) :-
    '_replace_acc'(A, LeftA, RightA, Y, RightA, Acc, NewAcc), !.
% Force hidden arguments in L to be appended to G:
'_expand_goal'((G:A), TG, _, _HList, Acc, NewAcc, Pass) :-
    \+'_list'(G),
    '_has_hidden'(G, []), !,
    '_make_list'(A, AList),
    '_new_goal'(G, AList, GArity, TG),
    '_use_acc_pass'(AList, GArity, TG, Acc, NewAcc, Pass).
% Use G's regular hidden arguments & override defaults for those arguments
% not in the head:
'_expand_goal'((G:A), TG, _, _HList, Acc, NewAcc, Pass) :-
    \+'_list'(G),
    '_has_hidden'(G, GList), GList\==[], !,
    '_make_list'(A, L),
    '_new_goal'(G, GList, GArity, TG),
    '_replace_defaults'(GList, NGList, L),
    '_use_acc_pass'(NGList, GArity, TG, Acc, NewAcc, Pass).
'_expand_goal'((L:A), Joiner, NaAr, _, Acc, NewAcc, _) :-
    '_list'(L), !,
    '_joiner'(L, A, NaAr, Joiner, Acc, NewAcc).
'_expand_goal'(L, Joiner, NaAr, _, Acc, NewAcc, _) :-
    '_list'(L), !,
    '_joiner'(L, dcg, NaAr, Joiner, Acc, NewAcc).
'_expand_goal'((X/A), true, _, _, Acc, Acc, _) :-
    atomic(A),
    '_member'(acc(A,X,_), Acc), !.
'_expand_goal'((X/A), true, _, _, Acc, Acc, Pass) :-
    atomic(A),
    '_member'(pass(A,X), Pass), !.
'_expand_goal'((A/X), true, _, _, Acc, Acc, _) :-
    atomic(A),
    '_member'(acc(A,_,X), Acc), !.
'_expand_goal'((X/A/Y), true, _, _, Acc, Acc, _) :-
    var(X), var(Y), atomic(A),
    '_member'(acc(A,X,Y), Acc), !.
'_expand_goal'((X/Y), true, NaAr, _, Acc, Acc, _) :-
    write('*** Warning: in '),write(NaAr),write(' the term '),write(X/Y),
    write(' uses a non-existent hidden parameter.'),nl.
% Defaulty cases:
'_expand_goal'(G, TG, _HList, _, Acc, NewAcc, Pass) :-
    '_has_hidden'(G, GList), !,
    '_new_goal'(G, GList, GArity, TG),
    '_use_acc_pass'(GList, GArity, TG, Acc, NewAcc, Pass).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Operations on the Acc and Pass data structures:

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
    write('*** Error: '),write(A),
    write(' is not a hidden parameter.'),nl.

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
    '_member'(pass(A,Arg), Pass), !,
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
    write('*** Error: the hidden parameter '),write(A),
    write(' does not exist.'),nl.

% Finish the Acc data structure:
% Link its Left and Right accumulation variables together in pairs:
'_finish_acc'([]).
'_finish_acc'([acc(_,Link,Link)|Acc]) :- '_finish_acc'(Acc).

% Replace elements in the Acc data structure:
% Succeeds iff replacement is successful.
'_replace_acc'(A, L1, R1, L2, R2, Acc, NewAcc) :-
    '_member'(acc(A,L1,R1), Acc), !,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specialized utilities:

% Given a goal Goal and a list of hidden parameters GList
% create a new goal TGoal with the correct number of arguments.
% Also return the arity of the original goal.
'_new_goal'(Goal, GList, GArity, TGoal) :-
    functor(Goal, Name, GArity),
    '_number_args'(GList, GArity, TArity),
    functor(TGoal, Name, TArity),
    '_match'(1, GArity, Goal, TGoal).

% Add the number of arguments needed for the hidden parameters:
'_number_args'([], N, N).
'_number_args'([A|List], N, M) :-
    '_is_acc'(A), !,
    N2 is N+2,
    '_number_args'(List, N2, M).
'_number_args'([A|List], N, M) :-
    '_is_pass'(A), !,
    N1 is N+1,
    '_number_args'(List, N1, M).
'_number_args'([_|List], N, M) :- !,
    % error caught elsewhere
    '_number_args'(List, N, M).

% Give a list of G's hidden parameters:
'_has_hidden'(G, GList) :-
    functor(G, GName, GArity),
    pred_info(GName, GArity, GList).
'_has_hidden'(G, []) :-
    functor(G, GName, GArity),
    \+pred_info(GName, GArity, _).

% Succeeds if A is an accumulator:
'_is_acc'(A)  :- atomic(A), !, '_acc_info'(A, _, _, _, _, _, _).
'_is_acc'(A)  :- functor(A, N, 2), !, '_acc_info'(N, _, _, _, _, _, _).

% Succeeds if A is a passed argument:
'_is_pass'(A) :- atomic(A), !, '_pass_info'(A, _).
'_is_pass'(A) :- functor(A, N, 1), !, '_pass_info'(N, _).

% Get initial values for the accumulator:
'_acc_info'(AccParams, LStart, RStart) :-
    functor(AccParams, Acc, 2),
    '_is_acc'(Acc), !,
    arg(1, AccParams, LStart),
    arg(2, AccParams, RStart).
'_acc_info'(Acc, LStart, RStart) :-
    '_acc_info'(Acc, _, _, _, _, LStart, RStart).

% Isolate the internal database from the user database:
'_acc_info'(Acc, Term, Left, Right, Joiner, LStart, RStart) :-
    acc_info(Acc, Term, Left, Right, Joiner, LStart, RStart).
'_acc_info'(Acc, Term, Left, Right, Joiner, _, _) :-
    acc_info(Acc, Term, Left, Right, Joiner).
'_acc_info'(dcg, Term, Left, Right, Left=[Term|Right], _, []).

% Get initial value for the passed argument:
% Also, isolate the internal database from the user database.
'_pass_info'(PassParam, PStart) :-
    functor(PassParam, Pass, 1),
    '_is_pass'(Pass), !,
    arg(1, PassParam, PStart).
'_pass_info'(Pass, PStart) :-
    pass_info(Pass, PStart).
'_pass_info'(Pass, _) :-
    pass_info(Pass).

% Calculate the joiner for an accumulator A:
'_joiner'([], _, _, true, Acc, Acc).
'_joiner'([Term|List], A, NaAr, (Joiner,LJoiner), Acc, NewAcc) :-
    '_replace_acc'(A, LeftA, RightA, MidA, RightA, Acc, MidAcc),
    '_acc_info'(A, Term, LeftA, MidA, Joiner, _, _), !,
    '_joiner'(List, A, NaAr, LJoiner, MidAcc, NewAcc).
% Defaulty case:
'_joiner'([_Term|List], A, NaAr, Joiner, Acc, NewAcc) :-
    write('*** Warning: in '),write(NaAr),
    write(' the accumulator '),write(A),
    write(' does not exist.'),nl,
    '_joiner'(List, A, NaAr, Joiner, Acc, NewAcc).

% Replace hidden parameters with ones containing initial values:
'_replace_defaults'([], [], _).
'_replace_defaults'([A|GList], [NA|NGList], AList) :-
    '_replace_default'(A, NA, AList),
    '_replace_defaults'(GList, NGList, AList).

'_replace_default'(A, NewA, AList) :-  % New initial values for accumulator.
    functor(NewA, A, 2),
    '_member'(NewA, AList), !.
'_replace_default'(A, NewA, AList) :-  % New initial values for passed argument.
    functor(NewA, A, 1),
    '_member'(NewA, AList), !.
'_replace_default'(A, NewA, _) :-      % Use default initial values.
    A=NewA.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generic utilities:

% Match arguments L, L+1, ..., H of the predicates P and Q:
'_match'(L, H, _, _) :- L>H, !.
'_match'(L, H, P, Q) :- L=<H, !,
    arg(L, P, A),
    arg(L, Q, A),
    L1 is L+1,
    '_match'(L1, H, P, Q).

% Flatten a conjunction and terminate it with 'true':
'_flat_conj'(Conj, FConj) :- '_flat_conj'(Conj, FConj, true).

'_flat_conj'(true, X, X).
'_flat_conj'((A,B), X1, X3) :-
    '_flat_conj'(A, X1, X2),
    '_flat_conj'(B, X2, X3).
'_flat_conj'(G, (G,X), X) :-
    \+G=true,
    \+G=(_,_).

'_member'(X, [X|_]).
'_member'(X, [_|L]) :- '_member'(X, L).

'_list'(L) :- nonvar(L), L=[_|_], !.
'_list'(L) :- L==[], !.

'_append'([], L, L).
'_append'([X|L1], L2, [X|L3]) :- '_append'(L1, L2, L3).

'_make_list'(A, [A]) :- \+'_list'(A), !.
'_make_list'(L,   L) :-   '_list'(L), !.

% replace(Elem, RepElem, List, RepList)
'_replace'(_, _, [], []).
'_replace'(A, B, [A|L], [B|R]) :- !,
    '_replace'(A, B, L, R).
'_replace'(A, B, [C|L], [C|R]) :-
    \+C=A, !,
    '_replace'(A, B, L, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
