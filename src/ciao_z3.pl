:- module(ciao_z3, [], [assertions, basicmodes, regtypes, foreign_interface]).

:- use_module(library(lists), [append/3]).

% Ciao foreign language interface for Z3
% See https://z3prover.github.io/api/html/group__capi.html

% TODO: Add documentation
% TODO: Use Z3 def_API decls, e.g., def_API('Z3_get_version', VOID, (_out(UINT), _out(UINT), _out(UINT), _out(UINT)))

:- export(z3_get_version/4).
:- true pred z3_get_version(go(A),go(B),go(C),go(D)) :: c_uint * c_uint * c_uint * c_uint + foreign('Z3_get_version').

:- export(z3_init/0).
z3_init :-
	z3_reset_memory. % TODO: right?

:- export(z3_exit/0).
z3_exit :-
	z3_finalize_memory.

%:- export(z3_finalize_memory/0).
:- true pred z3_finalize_memory + foreign('Z3_finalize_memory').

:- export(z3_reset_memory/0).
z3_reset_memory :-
	clean_dic,
	z3_reset_memory_.

:- true pred z3_reset_memory_ + foreign('Z3_reset_memory').

:- export(z3_mk_context/1).
% TODO: missing version with config
:- true pred z3_mk_context(go(Ctx)) :: 
	address + (foreign('mk_context'), returns(Ctx)).

:- export(z3_del_context/1).
:- true pred z3_del_context(in(Ctx)) :: 
	address + foreign('Z3_del_context').

:- export(z3_mk_solver/2).
:- true pred z3_mk_solver(in(Ctx), go(Solver)) :: 
	address * address + (foreign('Z3_mk_solver'), returns(Solver)).

:- export(z3_solver_assert/3).
:- true pred z3_solver_assert(in(Ctx), in(Solver), in(T)) :: 
	address * address * address + (foreign('Z3_solver_assert')).

:- export(z3_solver_check/3).
z3_solver_check(Ctx, Solver, LBool) :-
	z3_solver_check_(Ctx, Solver, LBool0),
	z3_lbool(LBool0, LBool).

:- true pred z3_solver_check_(in(Ctx), in(Solver), go(LBool)) :: 
	address * address * c_int + (foreign('Z3_solver_check'), returns(LBool)).

z3_lbool(-1, false).
z3_lbool(0, undef).
z3_lbool(1, true).

:- export(z3_solver_check_assumptions1/4).
% TODO: add version with many assumptions
z3_solver_check_assumptions1(Ctx, Solver, Assumption, LBool) :-
	z3_solver_check_assumptions1_(Ctx, Solver, Assumption, LBool0),
	z3_lbool(LBool0, LBool).

:- true pred z3_solver_check_assumptions1_(in(Ctx), in(Solver), in(Assumption), go(LBool)) :: 
	address * address * address * c_int + (foreign('solver_check_assumptions1'), returns(LBool)).

:- export(z3_model_eval/6).
:- true pred z3_model_eval(in(Ctx), in(Model), in(X), in(ModelComplete), go(V), go(Success)) :: 
	address * address * address * c_int * address * c_int + (returns(Success), foreign('Z3_model_eval')).

:- export(z3_declare_real/2).
z3_declare_real(Ctx, Name) :-
	z3_declare(Ctx, real, Name, _).

:- export(z3_declare_int/2).
z3_declare_int(Ctx, Name) :-
	z3_declare(Ctx, int, Name, _).

:- export(z3_declare_bool/2).
z3_declare_bool(Ctx, Name) :-
	z3_declare(Ctx, bool, Name, _).
 
:- export(z3_declare_array/4).
z3_declare_array(Ctx, Domain, Range, Name) :-
	z3_type(Domain, Ctx, DomainT),
	z3_type(Range, Ctx, RangeT),
	z3_declare(Ctx, array(DomainT, RangeT), Name, _).

% ---------------------------------------------------------------------------

% Cached declared consts (for z3_expr/3)
% NOTE:
%   Z3 calls returns the same object for mk_symbol/4 when the name is
%   repeated, however it does not possible to retrieve the const declared
%   by this symbol.
:- data declare_dic/2.

clean_dic :-
	retractall_fact(declare_dic(_,_)).

% ---------------------------------------------------------------------------

:- export(z3_declare/4).
z3_declare(Ctx, Tau, Name, T) :-
	name_to_atomic(Name, K),
	( current_fact(declare_dic(K, T0)) ->
	    T = T0
	; z3_type(Tau, Ctx, TauT),
	  mk_symbol(Ctx, Name, Symbol),
	  z3_mk_const(Ctx, Symbol, TauT, T),
	  assertz_fact(declare_dic(K, T))
	).

name_to_atomic(Name, K) :-
	( int(Name) -> K = Name
	; atom(Name) -> K = Name
	; atom_codes(K, Name)
	).

:- true pred z3_mk_int_symbol(in(Ctx), in(Int), go(Symbol)) ::
	address * c_int * address + (foreign('Z3_mk_int_symbol'), returns(Symbol)).
:- true pred z3_mk_string_symbol(in(Ctx), in(String), go(Symbol)) ::
	address * string * address + (foreign('Z3_mk_string_symbol'), returns(Symbol)).

mk_symbol(Ctx, X, Symbol) :- int(X), !, z3_mk_int_symbol(Ctx, X, Symbol).
mk_symbol(Ctx, X, Symbol) :- z3_mk_string_symbol(Ctx, X, Symbol).

:- true pred z3_mk_const(in(Ctx), in(Symbol), in(Sort), go(T)) ::
	address * address * address * address + (foreign('Z3_mk_const'), returns(T)).

z3_type(bool, Ctx, Tau) :- z3_mk_bool_sort(Ctx, Tau).
z3_type(int, Ctx, Tau) :- z3_mk_int_sort(Ctx, Tau).
z3_type(real, Ctx, Tau) :- z3_mk_real_sort(Ctx, Tau).
% TODO: use select/store as constraints?
% TODO: add bitvec?
z3_type(array(Domain, Range), Ctx, Tau) :- z3_mk_array_sort(Ctx, Domain, Range, Tau).

:- true pred z3_mk_real_sort(in(Ctx), go(Sort)) ::
	address * address + (foreign('Z3_mk_real_sort'), returns(Sort)).
:- true pred z3_mk_int_sort(in(Ctx), go(Sort)) ::
	address * address + (foreign('Z3_mk_int_sort'), returns(Sort)).
:- true pred z3_mk_bool_sort(in(Ctx), go(Sort)) ::
	address * address + (foreign('Z3_mk_bool_sort'), returns(Sort)).
:- true pred z3_mk_array_sort(in(Ctx), in(Domain), in(Range), go(Sort)) ::
	address * address * address * address + (foreign('Z3_mk_array_sort'), returns(Sort)).

:- export(z3_expr/3).
% Construct a Z3 ast
z3_expr(Ctx, Expr, R) :-
	exp(Expr, Ctx, R).

exp([], Ctx, R) --> !, exp(true, Ctx, R).
exp([X], Ctx, R) :- !, exp(X, Ctx, R).
exp([X|Ys], Ctx, R) :- !, exp((X,Ys), Ctx, R).
exp((X,Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_and2(Ctx, Xr, Yr, R).
exp((X;Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_or2(Ctx, Xr, Yr, R).
exp(false, Ctx, R) :- !, z3_mk_false(Ctx, R).
exp(true, Ctx, R) :- !, z3_mk_true(Ctx, R).
exp((X->Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_implies(Ctx, Xr, Yr, R).
% TODO: neg, not? (this is not unary minus!)
exp(neg(X), Ctx, R) :- !, exp(X, Ctx, Xr), z3_mk_not(Ctx, Xr, R).
% Equality
exp((X=Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_eq(Ctx, Xr, Yr, R).
% TODO: Add pairwise distinct
% exp(...)
% Constraints for arithmetic (reals and integers)
exp((X<Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_lt(Ctx, Xr, Yr, R).
exp((X>Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_gt(Ctx, Xr, Yr, R).
exp((X=<Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_le(Ctx, Xr, Yr, R).
exp((X>=Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_ge(Ctx, Xr, Yr, R).
% Constraints for arrays
% % TODO: apply1, update1 avoids temporary variables (is it better?)
exp(read(A,I,V), Ctx, R) :- !,
	exp((V = select(A,I)), Ctx, R).
exp(write(A,I,V,A2), Ctx, R) :- !,
	exp((A2 = store(A,I,V)), Ctx, R).
% Expressions for arrays
exp(store(A,I,V), Ctx, R) :- !, exp(A, Ctx, Ar), exp(I, Ctx, Ir), exp(V, Ctx, Vr), z3_mk_store(Ctx, Ar, Ir, Vr, R).
exp(select(A,I), Ctx, R) :- !, exp(A, Ctx, Ar), exp(I, Ctx, Ir), z3_mk_select(Ctx, Ar, Ir, R).
% Expressions for arithmetic (reals and integers)
exp((X*Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_mul(Ctx, Xr, Yr, R).
exp((X+Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_add(Ctx, Xr, Yr, R).
exp((X-Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_sub(Ctx, Xr, Yr, R).
exp(-(X), Ctx, R) :- !, exp(X, Ctx, Xr), z3_mk_unary_minus(Ctx, Xr, R).
exp(xor(X,Y), Ctx, R) :- !, exp(X, Ctx, Xr), exp(Y, Ctx, Yr), z3_mk_xor(Ctx, Xr, Yr, R).
% Variable (uninterpreted constants in Z3)
% TODO: do not confuse with quantifier bounded variables (Z3_mk_bound)
exp('$VAR'(N), _Ctx, R) :- !,
	( declare_dic(N, R0) ->
	    R = R0
	; throw(error(undeclared_var(N), exp/3))
	).
	    % name(N,I),
	    % Tau = real,
	    % Name = "x"||I,
	    % z3_declare(Ctx, Tau, Name, R).
% Numerals
% TODO: add more, use direct interface (not string); int->32; int64->64
exp(A, Ctx, R) :- int(A), !,
	number_codes(A,S),
	Ty = int,
	z3_type(Ty, Ctx, Ty2),
	z3_mk_numeral(Ctx, S, Ty2, R).
exp(A/B, Ctx, R) :- int(A), int(B), !,
	number_codes(A,Sa),
	number_codes(B,Sb),
	append(Sa, "/"||Sb, S),
	Ty = real,
	z3_type(Ty, Ctx, Ty2),
	z3_mk_numeral(Ctx, S, Ty2, R).
exp(A, Ctx, R) :- flt(A), !,
	number_codes(A,S),
	Ty = real,
	z3_type(Ty, Ctx, Ty2),
	z3_mk_numeral(Ctx, S, Ty2, R).
% TODO: case for other atoms?
exp(A, _Ctx, _R) :-
	throw(error(wrong_expr(A), exp/3)).

%:- export(z3_mk_and2/4).
:- true pred z3_mk_and2(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('mk_and2')).
%:- export(z3_mk_or2/4).
:- true pred z3_mk_or2(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('mk_or2')).
%:- export(z3_mk_false/2).
:- true pred z3_mk_false(in(Ctx), go(R)) :: address * address + (returns(R), foreign('Z3_mk_false')).
%:- export(z3_mk_true/2).
:- true pred z3_mk_true(in(Ctx), go(R)) :: address * address + (returns(R), foreign('Z3_mk_true')).
%:- export(z3_mk_implies/4).
:- true pred z3_mk_implies(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_implies')).
%:- export(z3_mk_not/3).
:- true pred z3_mk_not(in(Ctx), in(X), go(R)) :: address * address * address + (returns(R), foreign('Z3_mk_not')).
%:- export(z3_mk_eq/4).
:- true pred z3_mk_eq(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_eq')).
%:- export(z3_mk_lt/4).
:- true pred z3_mk_lt(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_lt')).
%:- export(z3_mk_gt/4).
:- true pred z3_mk_gt(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_gt')).
%:- export(z3_mk_le/4).
:- true pred z3_mk_le(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_le')).
%:- export(z3_mk_ge/4).
:- true pred z3_mk_ge(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_ge')).
%:- export(z3_mk_store/5).
:- true pred z3_mk_store(in(Ctx), in(A), in(I), in(V), go(R)) :: address * address * address * address * address + (returns(R), foreign('Z3_mk_store')).
%:- export(z3_mk_select/4).
:- true pred z3_mk_select(in(Ctx), in(A), in(I), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_select')).
%:- export(z3_mk_mul/4).
:- true pred z3_mk_mul(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('mk_mul2')).
%:- export(z3_mk_add/4).
:- true pred z3_mk_add(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('mk_add2')).
%:- export(z3_mk_sub/4).
:- true pred z3_mk_sub(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('mk_sub2')).
%:- export(z3_mk_unary_minus/3).
:- true pred z3_mk_unary_minus(in(Ctx), in(X), go(R)) :: address * address * address + (returns(R), foreign('Z3_mk_unary_minus')).
%:- export(z3_mk_xor/4).
:- true pred z3_mk_xor(in(Ctx), in(X), in(Y), go(R)) :: address * address * address * address + (returns(R), foreign('Z3_mk_xor')).
%:- export(z3_mk_numeral/4).
:- true pred z3_mk_numeral(in(Ctx), in(S), in(Ty), go(R)) :: address * string * address * address + (returns(R), foreign('Z3_mk_numeral')).

%% :- true pred yices_new_variable(in(Tau),go(V)) ::
%% 		c_int32 * c_int32 + (foreign, returns(V)).
% TODO: Z3_mk_bound(ctx, idx, s)

:- export(z3_term_to_string/3).
:- true pred z3_term_to_string(in(Ctx), in(Term), go(String)) ::
	address * address * string + (foreign('Z3_ast_to_string'), do_not_free(String), returns(String))
   # "Converts a term to a string".

:- export(z3_get_numeral_int/3).
z3_get_numeral_int(Ctx, Term, V) :-
	z3_get_numeral_int_(Ctx, Term, V, 1).

:- true pred z3_get_numeral_int_(in(Ctx), in(Term), go(V), go(Success)) ::
	address * address * c_int * c_int + (foreign('Z3_get_numeral_int'), returns(Success)).

:- export(z3_get_numeral_int64/3).
z3_get_numeral_int64(Ctx, Term, V) :-
	z3_get_numeral_int64_(Ctx, Term, V, 1).

:- true pred z3_get_numeral_int64_(in(Ctx), in(Term), go(V), go(Success)) ::
	address * address * c_int64 * c_int + (foreign('Z3_get_numeral_int64'), returns(Success)).

:- export(z3_get_numeral_uint64/3).
z3_get_numeral_uint64(Ctx, Term, V) :-
	z3_get_numeral_uint64_(Ctx, Term, V, 1).

:- true pred z3_get_numeral_uint64_(in(Ctx), in(Term), go(V), go(Success)) ::
	address * address * c_uint * c_int + (foreign('Z3_get_numeral_uint64'), returns(Success)).

:- export(z3_get_error_msg_ex/3).
:- true pred z3_get_error_msg_ex(in(Ctx), in(ErrorCode), go(String)) ::
	address * c_int * string + (foreign('Z3_get_error_msg_ex'), do_not_free(String), returns(String)).

:- export(z3_solver_get_model/3).
% Call after z3_solver_check or z3_solver_check_assumptions
% NOTE: Needs enabled model construction
:- true pred z3_solver_get_model(in(Ctx), in(Solver), go(Model)) ::
	address * address * address + (foreign('Z3_solver_get_model'), returns(Model)).

:- include(.(ciao_z3_config_auto)).

:- use_foreign_library(z3).

% ---------------------------------------------------------------------------

:- export(smt2clp/2).
% Translate from SMT2 to CLP format
:- true pred smt2clp(in(In), in(Out)) :: atm * atm + foreign.

:- use_foreign_source(ciao_z3_aux).

