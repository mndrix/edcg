:- multifile prolog:message//1.

prolog:message(missing_accumulator(Predicate,Accumulator)) -->
    ['In ~w the accumulator ''~w'' does not exist'-[Predicate,Accumulator]].
prolog:message(missing_hidden_parameter(Predicate,Term)) -->
    ['In ~w the term ''~w'' uses a non-existent hidden parameter.'-[Predicate,Term]].
prolog:message(not_a_hidden_param(Name)) -->
    ['~w is not a hidden parameter'-[Name]].
