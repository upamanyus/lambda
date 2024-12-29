#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum extern_op {
  EXTERN_PUT0,
  EXTERN_PUT1,
  EXTERN_GET,
};

enum term_kind {
  TERM_VAR,
  TERM_EXTERN,  // put0, put1, get, unit
  TERM_AP,
  TERM_ABS,
};

const char *extern_string[] = {
    "put0",
    "put1",
    "get",
};

struct term {
  union {
    uint64_t var;
    enum extern_op ext;
    struct {
      struct term *f;
      struct term *v;
    } ap;
    struct {
      uint64_t x;
      struct term *e;
    } abs;
  };
  enum term_kind kind;
};

struct term *term_new() { return malloc(sizeof(struct term)); }

void term_free(struct term *t) {
  switch (t->kind) {
    case TERM_AP:
      term_free(t->ap.f);
      term_free(t->ap.v);
      break;
    case TERM_ABS:
      term_free(t->abs.e);
      break;
    default:
      break;
  }
  return free(t);
}

struct term *term_ext(enum extern_op ext) {
  struct term *t = term_new();
  t->kind = TERM_EXTERN;
  t->ext = ext;
  return t;
}

struct term *term_var(uint64_t name) {
  struct term *t = term_new();
  t->kind = TERM_VAR;
  t->var = name;
  return t;
}

struct term *term_ap(struct term *f, struct term *v) {
  struct term *t = term_new();
  t->kind = TERM_AP;
  t->ap.f = f;
  t->ap.v = v;
  return t;
}

struct term *term_abs(uint64_t x, struct term *e) {
  struct term *t = term_new();
  t->kind = TERM_ABS;
  t->abs.x = x;
  t->abs.e = e;
  return t;
}

struct term *term_unit() { return term_abs(0, term_var(0)); }

struct term *term_true() { return term_abs(0, term_abs(1, term_var(0))); }

struct term *term_false() { return term_abs(0, term_abs(1, term_var(1))); }

struct term *term_let(uint64_t x, struct term *v, struct term *e) {
  return term_ap(term_abs(x, e), v);
}

struct term *term_seq(struct term *e1, struct term *e2) {
    return term_let(UINT64_MAX, e1, e2);
}

struct term *term_Z() {
    uint64_t f = 0, x = 1, v = 2;
    return term_abs(
        f, term_ap(term_abs(x, term_ap(term_var(f),
                                       term_abs(v, term_ap(term_ap(term_var(x),
                                                                   term_var(x)),
                                                           term_var(v))))),
                   term_abs(x, term_ap(term_var(f),
                                       term_abs(v, term_ap(term_ap(term_var(x),
                                                                   term_var(x)),
                                                           term_var(v)))))));
}

struct term *term_clone(struct term *t) {
  struct term *tp = term_new();
  tp->kind = t->kind;
  switch (t->kind) {
    case TERM_VAR:
      tp->var = t->var;
      break;
    case TERM_EXTERN:
      tp->ext = t->ext;
      break;
    case TERM_AP:
      tp->ap.f = term_clone(t->ap.f);
      tp->ap.v = term_clone(t->ap.v);
      break;
    case TERM_ABS:
      tp->abs.x = t->abs.x;
      tp->abs.e = term_clone(t->abs.e);
      break;
  }
  return tp;
}

struct term *term_subst_aux(struct term *t, uint64_t name, struct term *v,
                            bool *used) {
  switch (t->kind) {
    case TERM_ABS:
      if (t->abs.x != name) {
        t->abs.e = term_subst_aux(t->abs.e, name, v, used);
      }
      return t;
    case TERM_EXTERN:
      return t;
    case TERM_VAR:
      if (t->var == name) {
        term_free(t);
        if (!used) {
          return v;
        } else {
          return term_clone(v);
        }
      } else {
        return t;
      }
    case TERM_AP:
      t->ap.f = term_subst_aux(t->ap.f, name, v, used);
      t->ap.v = term_subst_aux(t->ap.v, name, v, used);
      return t;
  }
}

struct term *term_subst(struct term *t, uint64_t name, struct term *v) {
  bool used = false;
  t = term_subst_aux(t, name, v, &used);
  if (!used) {
    term_free(v);
  }
  return t;
}

struct term *term_eval(struct term *t) {
  switch (t->kind) {
    case TERM_ABS:
      return t;
    case TERM_EXTERN:
      return t;
    case TERM_VAR:
      return t;
    case TERM_AP:
      struct term *f = t->ap.f;
      struct term *v = t->ap.v;
      f = term_eval(f);
      v = term_eval(v);
      switch (f->kind) {
        case TERM_ABS:
          free(t);
          struct term *e = f->abs.e;
          uint64_t x = f->abs.x;
          free(f);
          return term_eval(term_subst(e, x, v));
        case TERM_EXTERN:
          free(t);
          enum extern_op ext = f->ext;
          term_free(f);
          term_free(v);
          switch (ext) {
            case EXTERN_GET:
              if ((getchar() & 1) == 0) {
                return term_false();
              } else {
                return term_true();
              }
            case EXTERN_PUT0:
              putchar('0');
              fflush(stdout);
              return term_unit();
            case EXTERN_PUT1:
              putchar('1');
              fflush(stdout);
              return term_unit();
          }
        default:
          t->ap.f = f;
          t->ap.v = v;
          return t;
      }
  }
}

void term_print_aux(struct term *t, bool needs_parens) {
  switch (t->kind) {
    case TERM_EXTERN:
      printf("@%s", extern_string[t->ext]);
      break;
    case TERM_VAR:
      printf("%lu", t->var);
      break;
    case TERM_AP:
      if (needs_parens) {
        fputs("(", stdout);
      }
      term_print_aux(t->ap.f, true);
      fputs(" ", stdout);
      term_print_aux(t->ap.v, true);
      if (needs_parens) {
        fputs(")", stdout);
      }
      break;
    case TERM_ABS:
      if (needs_parens) {
        fputs("(", stdout);
      }
      printf("\\%lu.", t->abs.x);
      term_print_aux(t->ap.v, true);
      if (needs_parens) {
        fputs(")", stdout);
      }
      break;
  }
}

void term_print(struct term *t) { term_print_aux(t, false); }

void test(struct term *t) {
  fputs("Starting: ", stdout);
  term_print(t);
  puts("");
  t = term_eval(t);
  puts("");
  fputs("Ending with: ", stdout);
  term_print(t);
  puts("");
  term_free(t);
}

int main() {
  uint64_t a = 0;
  uint64_t x = 1;
  uint64_t f = 2;

  struct term *t = term_true();
  test(t);

  t = term_ap(term_abs(a, term_ap(term_var(a), term_var(a))), term_var(37));
  test(t);

  t = term_ap(term_abs(f, term_abs(x, term_ap(term_var(f), term_var(x)))),
              term_abs(f, term_abs(x, term_ap(term_var(f), term_var(x)))));
  test(t);

  t = term_seq(term_ap(term_ext(EXTERN_PUT0), term_unit()),
               term_ap(term_ext(EXTERN_PUT1), term_unit()));
  test(t);

  t = term_ap(
      term_ap(
          term_Z(),
          term_abs(
              f,
              term_abs(
                  UINT64_MAX,
                  term_seq(term_ap(term_ap(term_ap(term_ap(term_ext(EXTERN_GET),
                                                           term_unit()),
                                                   term_ext(EXTERN_PUT1)),
                                           term_ext(EXTERN_PUT0)),
                                   term_unit()),
                           term_ap(term_var(f), term_unit()))))),
      term_unit());
  test(t);
}
