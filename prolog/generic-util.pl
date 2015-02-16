% Generic utilities

% Match arguments L, L+1, ..., H of the predicates P and Q:
'_match'(L, H, _, _) :- L>H, !.
'_match'(L, H, P, Q) :- L=<H, !,
    arg(L, P, A),
    arg(L, Q, A),
    L1 is L+1,
    '_match'(L1, H, P, Q).


'_list'(L) :- nonvar(L), L=[_|_], !.
'_list'(L) :- L==[], !.

'_make_list'(A, [A]) :- \+'_list'(A), !.
'_make_list'(L,   L) :-   '_list'(L), !.

% replace(Elem, RepElem, List, RepList)
'_replace'(_, _, [], []).
'_replace'(A, B, [A|L], [B|R]) :- !,
    '_replace'(A, B, L, R).
'_replace'(A, B, [C|L], [C|R]) :-
    \+C=A, !,
    '_replace'(A, B, L, R).
