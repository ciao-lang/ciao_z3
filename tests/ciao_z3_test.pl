:- module(ciao_z3_test, [], [assertions]).

:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(format)).
:- use_module(ciao_z3(ciao_z3)).

% Expected:
%   true
%   X = 0.0
%   Y = 0.0
%   X (int) = 0
%   Y (int) = 0
%   TF1 = true
%   TF2 = false

:- test test1/0.
:- export(test1/0).
test1 :-
    z3_init,
    z3_mk_context(Ctx),
    z3_mk_solver(Ctx, Solver),
    z3_declare_real(Ctx, "x"),
    z3_declare_real(Ctx, "y"),
    Xv = '$VAR'('x'),
    Yv = '$VAR'('y'),
    z3_expr(Ctx, (Xv + Yv = 0), T1),
    z3_expr(Ctx, (Yv >= 0), T2),
    z3_expr(Ctx, (Xv >= 0), T3),
%   z3_status(Ctx,S0),
%   write(S0),nl,
    z3_solver_assert(Ctx,Solver,T1),
    z3_solver_assert(Ctx,Solver,T2),
    z3_solver_assert(Ctx,Solver,T3),
    z3_solver_check(Ctx,Solver,LBool),
    write(LBool),nl,
    ( LBool == true -> % satisfiable
        z3_solver_get_model(Ctx,Solver,Model),
        z3_expr(Ctx, Xv, TX),
        z3_expr(Ctx, Yv, TY),
        z3_model_eval(Ctx,Model,TX,0,ValX,_),
        z3_model_eval(Ctx,Model,TY,0,ValY,_),
        z3_term_to_string(Ctx, ValX, ValXStr),
        format("X = ~s~n", [ValXStr]),
        z3_term_to_string(Ctx, ValY, ValYStr),
        format("Y = ~s~n", [ValYStr]),
        expr_from_z3(Ctx, ValX, ValXInt),
        format("X (int) = ~d~n", [ValXInt]),
        expr_from_z3(Ctx, ValY, ValYInt),
        format("Y (int) = ~d~n", [ValYInt]),
        %
        z3_expr(Ctx, (Yv = 0), T4),
        z3_expr(Ctx, (Xv = 1), T5),
        %z3_solver_check_assumptions1(Ctx, Solver,T4,TF1),
        z3_model_eval(Ctx,Model,T4,0,V4,_TF1),
        z3_model_eval(Ctx,Model,T5,0,V5,_TF2),
        %z3_formula_true_in_model(Model,T5,TF2),
        z3_term_to_string(Ctx, V4, V4Str),
        format("TF1 = ~s~n", [V4Str]),
        z3_term_to_string(Ctx, V5, V5Str),
        format("TF2 = ~s~n", [V5Str])
    ; write('No model')
    ),
    z3_del_context(Ctx),
    z3_exit.

% (incomplete)
expr_from_z3(Ctx, V, X) :-
    z3_get_numeral_int(Ctx, V, Int),
    !,
    X = Int.
expr_from_z3(Ctx, V, X) :-
    % (for rational -- fix)
    z3_term_to_string(Ctx, V, Str),
    number_codes(Str, X).

:- test test2/0.
:- export(test2/0).
% Expected:
%   T1 = (= (+ x y) (to_real 0))
test2 :-
    z3_init,
    z3_mk_context(Ctx),
    z3_declare_real(Ctx, "x"),
    z3_declare_real(Ctx, "y"),
    Xv = '$VAR'('x'),
    Yv = '$VAR'('y'),
    z3_expr(Ctx, (Xv + Yv = 0), T1),
    z3_term_to_string(Ctx, T1, T1Str),
    format("T1 = ~s~n", T1Str),
    z3_del_context(Ctx),
    z3_exit.
