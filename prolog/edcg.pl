:- module( edcg, [
    op(1200, xfx, '-->>'),   % Similar to '-->'
    edcg_import_sentinel/0
]).

:- use_module(library(lists), [member/2]).

:- ['acc-pass.pl'].
:- ['special-util.pl'].
:- ['generic-util.pl'].


% these predicates define extra arguments
:- multifile
    acc_info/5,
    acc_info/7,
    pass_info/1,
    pass_info/2.


% True if the module being read has opted-in to EDCG macro expansion.
wants_edcg_expansion :-
    prolog_load_context(module, Module),
    Module \== edcg,  % don't expand macros in our own library
    predicate_property(Module:edcg_import_sentinel, imported_from(edcg)).

% dummy predicate exported to detect which modules want EDCG expansion
edcg_import_sentinel.


% Perform EDCG macro expansion
user:term_expansion((H-->>B), (TH:-TB)) :-
    wants_edcg_expansion,
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
    member(acc(A,X,_), Acc), !.
'_expand_goal'((X/A), true, _, _, Acc, Acc, Pass) :-
    atomic(A),
    member(pass(A,X), Pass), !.
'_expand_goal'((A/X), true, _, _, Acc, Acc, _) :-
    atomic(A),
    member(acc(A,_,X), Acc), !.
'_expand_goal'((X/A/Y), true, _, _, Acc, Acc, _) :-
    var(X), var(Y), atomic(A),
    member(acc(A,X,Y), Acc), !.
'_expand_goal'((X/Y), true, NaAr, _, Acc, Acc, _) :-
    write('*** Warning: in '),write(NaAr),write(' the term '),write(X/Y),
    write(' uses a non-existent hidden parameter.'),nl.
% Defaulty cases:
'_expand_goal'(G, TG, _HList, _, Acc, NewAcc, Pass) :-
    '_has_hidden'(G, GList), !,
    '_new_goal'(G, GList, GArity, TG),
    '_use_acc_pass'(GList, GArity, TG, Acc, NewAcc, Pass).
