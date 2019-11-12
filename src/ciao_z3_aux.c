#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <memory.h>
#include <assert.h>
#include <setjmp.h>
#include <z3.h>

typedef int bool_t;
#define FALSE 0
#define TRUE 1

void panic(char *msg) {
  fprintf(stderr, "PANIC: %s\n", msg);
  exit(1);
}

void error_handler(Z3_context c, Z3_error_code e) {
  printf("Error code: %d\n", e);
  fprintf(stderr,"ERROR: incorrect use of Z3\n");
  exit(1);
}

Z3_context mk_context_custom(Z3_config cfg, Z3_error_handler err) {
  Z3_context ctx;

  // model=true is needed for Z3_solver_get_model
  Z3_set_param_value(cfg, "model", "true");
  ctx = Z3_mk_context(cfg);
  Z3_set_error_handler(ctx, err);
    
  return ctx;
}

Z3_context mk_context() {
  Z3_config  cfg;
  Z3_context ctx;
  cfg = Z3_mk_config();
  ctx = mk_context_custom(cfg, error_handler);
  Z3_del_config(cfg);
  return ctx;
}

/* -------------------------------------------------------------------------------------- */

Z3_ast mk_and2(Z3_context ctx, Z3_ast x, Z3_ast y) {
  Z3_ast args[2];
  args[0] = x;
  args[1] = y;
  return Z3_mk_and(ctx, 2, args);
}

Z3_ast mk_or2(Z3_context ctx, Z3_ast x, Z3_ast y) {
  Z3_ast args[2];
  args[0] = x;
  args[1] = y;
  return Z3_mk_or(ctx, 2, args);
}

Z3_ast mk_add2(Z3_context ctx, Z3_ast x, Z3_ast y) {
  Z3_ast args[2];
  args[0] = x;
  args[1] = y;
  return Z3_mk_add(ctx, 2, args);
}

Z3_ast mk_sub2(Z3_context ctx, Z3_ast x, Z3_ast y) {
  Z3_ast args[2];
  args[0] = x;
  args[1] = y;
  return Z3_mk_sub(ctx, 2, args);
}

Z3_ast mk_mul2(Z3_context ctx, Z3_ast x, Z3_ast y) {
  Z3_ast args[2];
  args[0] = x;
  args[1] = y;
  return Z3_mk_mul(ctx, 2, args);
}

/* -------------------------------------------------------------------------------------- */

Z3_lbool solver_check_assumptions(Z3_context ctx, Z3_solver s, Z3_ast assumption) {
  Z3_ast assumptions[1];
  assumptions[0] = assumption;
  return Z3_solver_check_assumptions(ctx, s, 1, assumptions);
}

/* -------------------------------------------------------------------------------------- */

Z3_ast parse_smtlib2_string(Z3_context ctx, char *str) {
  return Z3_parse_smtlib2_string(ctx, str, 0, 0, 0, 0, 0, 0);
}

/* -------------------------------------------------------------------------------------- */
/* Simple checks */

static inline bool_t is_app_of(Z3_context ctx, Z3_ast a, Z3_decl_kind kind) {
  if (Z3_get_ast_kind(ctx, a) != Z3_APP_AST) return FALSE;
  Z3_app b = Z3_to_app(ctx, a);
  return (Z3_get_decl_kind(ctx, Z3_get_app_decl(ctx, b)) == kind);
}

static inline bool_t is_var(Z3_context c, Z3_ast a) {
  return Z3_get_ast_kind(c, a) == Z3_VAR_AST;
}

static inline bool_t is_false(Z3_context c, Z3_ast a) { return is_app_of(c, a, Z3_OP_FALSE); }
static inline bool_t is_true(Z3_context c, Z3_ast a)  { return is_app_of(c, a, Z3_OP_TRUE); }
static inline bool_t is_not(Z3_context c, Z3_ast a)   { return is_app_of(c, a, Z3_OP_NOT); }
static inline bool_t is_or(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_OR); }
static inline bool_t is_add(Z3_context c, Z3_ast a)   { return is_app_of(c, a, Z3_OP_ADD); }
static inline bool_t is_and(Z3_context c, Z3_ast a)   { return is_app_of(c, a, Z3_OP_AND); }
static inline bool_t is_sub(Z3_context c, Z3_ast a)   { return is_app_of(c, a, Z3_OP_SUB); }
static inline bool_t is_mul(Z3_context c, Z3_ast a)   { return is_app_of(c, a, Z3_OP_MUL); }
static inline bool_t is_eq(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_EQ); }
static inline bool_t is_le(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_LE); }
static inline bool_t is_ge(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_GE); }
static inline bool_t is_lt(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_LT); }
static inline bool_t is_gt(Z3_context c, Z3_ast a)    { return is_app_of(c, a, Z3_OP_GT); }

static inline bool_t is_quantifier(Z3_context c, Z3_ast a) {
  return Z3_get_ast_kind(c, a) == Z3_QUANTIFIER_AST;
}

static inline bool_t is_rational_value(Z3_context c, Z3_ast a) {
  return (Z3_get_sort_kind(c, Z3_get_sort(c, a)) == Z3_REAL_SORT &&
          Z3_is_numeral_ast(c, a));
}

static inline bool_t is_int_value(Z3_context c, Z3_ast a) {
  return (Z3_get_sort_kind(c, Z3_get_sort(c, a)) == Z3_INT_SORT &&
          Z3_is_numeral_ast(c, a));
}

/* -------------------------------------------------------------------------------------- */
/* Translate clauses */ 

void translate_term(Z3_context ctx, FILE *out, Z3_ast p);

/* assume num_args of a == 2 */
void translate_infix(Z3_context ctx, FILE *out, const char *n, Z3_ast a) {
  assert(Z3_get_ast_kind(ctx, a) == Z3_APP_AST);
  Z3_app b = Z3_to_app(ctx, a);
  assert(Z3_get_app_num_args(ctx, b) == 2);
  //
  fprintf(out, "(");
  translate_term(ctx, out, Z3_get_app_arg(ctx, b, 0));
  fprintf(out, " %s ", n);
  translate_term(ctx, out, Z3_get_app_arg(ctx, b, 1));
  fprintf(out, ")");
}

/* assume num_args of a > 0 */
void translate_connective(Z3_context ctx, FILE *out, const char *n, Z3_ast a) {
  assert(Z3_get_ast_kind(ctx, a) == Z3_APP_AST);
  Z3_app b = Z3_to_app(ctx, a);
  int len = Z3_get_app_num_args(ctx, b);
  assert(len > 0);
  translate_term(ctx, out, Z3_get_app_arg(ctx, b, 0));
  for (int i = 1; i < len; i++) {
    fprintf(out, "%s ", n);
    translate_term(ctx, out, Z3_get_app_arg(ctx, b, i));
  }
}

void translate_term(Z3_context ctx, FILE *out, Z3_ast p) {
  // Skip quantifiers
  while (Z3_get_ast_kind(ctx, p) == Z3_QUANTIFIER_AST) {
    assert(Z3_is_quantifier_forall(ctx, p));
    p = Z3_get_quantifier_body(ctx, p);
  }
  switch(Z3_get_ast_kind(ctx, p)) {
  case Z3_VAR_AST:
    fprintf(out, "X%d", Z3_get_index_value(ctx, p));
    break;
  case Z3_QUANTIFIER_AST:
    // TODO: Not supported
    assert(0);
    break;
  case Z3_NUMERAL_AST: /* both real and int */
    /* TODO: treat differently the accept num/dem case */
    //    fprintf(out, "%s", Z3_get_numeral_string(ctx, p));
    fprintf(out, "%s", Z3_get_numeral_string(ctx, p));
    break;
  case Z3_APP_AST:
    goto app;
  default:
    // TODO: Not supported
    assert(0);
  }
  goto end;

 app:
  {
    Z3_app b = Z3_to_app(ctx, p);
    const char *n = "";
    switch(Z3_get_decl_kind(ctx, Z3_get_app_decl(ctx, b))) {
    case Z3_OP_AND:     n = ","; goto conn;
    case Z3_OP_OR:      n = ";"; goto conn;
    case Z3_OP_ADD:     n = "+"; goto infix;
    case Z3_OP_SUB:     n = "-"; goto infix;
    case Z3_OP_MUL:     n = "*"; goto infix;
    case Z3_OP_EQ:      n = "="; goto infix;
    case Z3_OP_LE:      n = "=<"; goto infix;
    case Z3_OP_GE:      n = ">="; goto infix;
    case Z3_OP_LT:      n = "<"; goto infix;
    case Z3_OP_GT:      n = ">"; goto infix;
    case Z3_OP_IMPLIES: n = "->"; goto infix;
    case Z3_OP_NOT:     n = "\\+"; goto str;
    case Z3_OP_FALSE:   n = "false"; goto cons;
    case Z3_OP_TRUE:    n = "true"; goto cons;
    default:
      {
        Z3_func_decl decl = Z3_get_app_decl(ctx, b);
        Z3_symbol name = Z3_get_decl_name(ctx, decl);
        n = Z3_get_symbol_string(ctx, name);
      }
      goto str;
    }
  conn:
    translate_connective(ctx, out, n, p);
    goto end;
  infix:
    translate_infix(ctx, out, n, p);
    goto end;
  cons:
    fprintf(out, "%s", n);
    goto end;
  str:
    {
      fprintf(out, "%s", n);
      int len = Z3_get_app_num_args(ctx, b);
      if (len > 0) {
        fprintf(out, "(");
        translate_term(ctx, out, Z3_get_app_arg(ctx, b, 0));
        for (int i = 1; i < len; i++) {
          fprintf(out, ", ");
          translate_term(ctx, out, Z3_get_app_arg(ctx, b, i));
        }
        fprintf(out, ")");
      }
    goto end;
    }
  }

 end:
  return;
}

void translate_clause(Z3_context ctx, FILE *out, Z3_ast c) {
  while (Z3_get_ast_kind(ctx, c) == Z3_QUANTIFIER_AST) {
    assert(Z3_is_quantifier_forall(ctx, c));
    c = Z3_get_quantifier_body(ctx, c);
  }
  //printf("CLAUSE: %s\n", Z3_ast_to_string(ctx, c));
  translate_term(ctx, out, c);
  fprintf(out, ".\n");
}

void smt2clp(char *file, char *outFile) {
  Z3_context ctx;
  Z3_ast clauses;

  ctx = mk_context();
  clauses  = Z3_parse_smtlib2_file(ctx, file, 0, 0, 0, 0, 0, 0);

  FILE *out;
  out = fopen(outFile, "wb");
  if (out == NULL) {
    fprintf(stderr,"ERROR: could not open output %s\n", outFile);
    return;
  }
  if (is_app_of(ctx, clauses, Z3_OP_AND)) {
    Z3_app app = Z3_to_app(ctx, clauses);
    int len = Z3_get_app_num_args(ctx, app);
    for (int idx = 0; idx < len; idx++) {
      Z3_ast clause;
      clause = Z3_get_app_arg(ctx, app, idx);
      translate_clause(ctx, out, clause);
    }
  } else {
    translate_clause(ctx, out, clauses);
  }
  fclose(out);

  Z3_del_context(ctx);
}
