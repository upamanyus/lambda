type term =
  Extern of string | Var of string | Ap of (term * term) | Abs of (string * term)
