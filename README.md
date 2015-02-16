# Synopsis

```prolog
:- use_module(library(edcg)).

% Declare accumulators
edcg:acc_info(adder, X, In, Out, plus(X,In,Out)).

% Declare predicates using these hidden arguments
edcg:pred_info(len,0,[adder,dcg]).
edcg:pred_info(increment,0,[adder]).

increment -->>
    [1]:adder.  % add one to the accumulator


len(Xs,N) :-
    len(0,N,Xs,[]).

len -->>
    [_],  % 'dcg' accumulator has an element
    !,
    increment,  % increment the 'adder' accumulator
    len.
len -->>
    [].
```

# Introduction

DCG notation gives us a single, hidden accumulator.  Extended DCG notation (implemented by this library) lets predicates have arbitrarily many hidden accumulators. As demonstrated by the Synopsis above, those accumulators can be implemented with arbitrary goals (like plus/3).

Benefits of this library:

  * avoid tedium and errors from manually threading accumulators through your predicates
  * add or remove accumulators with a single declaration
  * change accumulator implementation with a single declaration (ex, switching from ordsets to rbtrees)

# Syntax

Extended DCG syntax is very similar to DCG notation.  An EDCG is created with clauses whose neck is the `-->>` operator.  The following syntax is supported inside an EDCG clause:

  * `{Goal}` - don't expand any hidden arguments of `Goal`
  * `Goal` - expand all hidden arguments of Goal that are also in the head. Those hidden arguments not in the head are given default values.
  * `Goal:L` - If `Goal` has no hidden arguments then force the expansion of all arguments in `L` in the order given. If `Goal` has hidden arguments then expand all of them, using the contents of `L` to override the expansion. `L` is either a term of the form `Acc`, `Acc(Left,Right)`, `Pass`, `Pass(Value)`, or a list of such terms. When present, the arguments `Left`, `Right`, and `Value` override the default values of arguments not in the head.
  * `List:Acc` - Accumulate a list of terms in the accumulator `Acc`
  * `List` - Accumulate a list of terms in the accumulator `dcg`
  * `X/Acc` - Unify `X` with the left term for the accumulator `Acc`
  * `Acc/X` - Unify `X` with the right term for the accumulator `Acc`
  * `X/Acc/Y` - Unify `X` with the left and `Y` with the right term for the accumulator `Acc`
  * `insert(X,Y):Acc` - Insert the arguments `X` and `Y` into the chain implementing the accumulator `Acc`. This is useful when the value of the accumulator changes radically because `X` and `Y` may be the arguments of an arbitrary relation
  * `insert(X,Y):Acc` - Insert the arguments `X` and `Y` into the chain implementing the accumulator `dcg`. This inserts the difference list X-Y into the accumulated list

# Declarations

## Declaration of Predicates

Predicates are declared with facts of the following form:

```prolog
pred_info(Name, Arity, List)
```

The predicate `Name/Arity` has the hidden parameters given in `List`. The parameters are added in the order given by `List` and their names must be atoms.

## Declaration of Accumulators

Accumulators are declared with facts in one of two forms. The short form is:

```prolog
acc_info(Acc, Term, Left, Right, Joiner)
```

The long form is:

```
acc_info(Acc, Term, Left, Right, Joiner, LStart, RStart)
```

In most cases the short form gives sufficient information. It declares the accumulator `Acc`, which must be an atom, along with the accumulating function, `Joiner`, and its arguments `Term`, the term to be accumulated, and `Left` & `Right`, the variables used in chaining.

The long form of `acc_info` is useful in more complex programs. It contains two additional arguments, `LStart` and `RStart`, that are used to give default starting values for an accumulator occurring in a body goal that does not occur in the head. The starting values are given to the unused accumulator to ensure that it will execute correctly even though its value is not used. Care is needed to give correct values for `LStart` and `RStart`. For DCG-like list accumulation both may remain unbound.

Two conventions are used for the two variables used in chaining depending on which direction the accumulation is done. For forward accumulation, `Left` is the input and `Right` is the output. For reverse accumulation, `Right` is the input and `Left` is the output.

## Declaration of Passed Arguments

Passed arguments are conceptually the same as accumulators with `=/2` as the joiner function.  Passed arguments are declared as facts in one of two forms. The short form is:

```prolog
pass_info(Pass)
```

The long form is:

```prolog
pass_info(Pass, PStart)
```

In most cases the short form is sufficient. It declares a passed argument `Pass`, that must be an atom. The long form also contains the starting value `PStart` that is used to give a default value for a passed argument in a body goal that does not occur in the head. Most of the time this situation does not occur.

# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(edcg).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/edcg

@license mit
