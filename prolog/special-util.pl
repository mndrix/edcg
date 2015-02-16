% Specialized utilities:

:- ['generic-util.pl'].
:- ['messages.pl'].

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
    print_message(warning, missing_accumulator(NaAr,A)),
    '_joiner'(List, A, NaAr, Joiner, Acc, NewAcc).

% Replace hidden parameters with ones containing initial values:
'_replace_defaults'([], [], _).
'_replace_defaults'([A|GList], [NA|NGList], AList) :-
    '_replace_default'(A, NA, AList),
    '_replace_defaults'(GList, NGList, AList).

'_replace_default'(A, NewA, AList) :-  % New initial values for accumulator.
    functor(NewA, A, 2),
    member(NewA, AList), !.
'_replace_default'(A, NewA, AList) :-  % New initial values for passed argument.
    functor(NewA, A, 1),
    member(NewA, AList), !.
'_replace_default'(A, NewA, _) :-      % Use default initial values.
    A=NewA.
