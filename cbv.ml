type term =
  Extern of string | Var of string | Ap of (term * term) | Abs of (string * term)

let t_true = Abs("mt", Abs("mf", Var("mt")))
let t_false = Abs("mt", Abs("mf", Var("mf")))
let t_id = Abs("x", Var("x"))
let t_let (x, v, e) = Ap(Abs(x, e), v)
let t_seq (e1, e2) = t_let("_let", e1, e2)
let t_put0 = Ap(Extern("put0"), t_id)
let t_put1 = Ap(Extern("put1"), t_id)
let t_get = Ap(Extern("get"), t_id)
let t_ite(cond, t, e) = Ap(Ap(Ap(cond, Abs("_unit", t)), Abs("_unit", e)), t_id)
let t_Z = Abs("f",
              Ap(
                Abs("x", Ap(Var "f", Abs("v", Ap(Ap(Var "x", Var "x"), Var "v")))),
                Abs("x", Ap(Var "f", Abs("v", Ap(Ap(Var "x", Var "x"), Var "v"))))
              )
             )

let print_term (t : term) =
  let rec print_aux (t : term) (needs_parens : bool) =
      match t with
      | Extern(c) -> Printf.printf "@%s" c
      | Var(x) -> print_string x
      | Ap(f, v) ->
        if needs_parens then
          print_char '(';
        print_aux f true; print_char ' '; print_aux v true;
        if needs_parens then
          print_char ')';
      | Abs(x, e) ->
        if needs_parens then
          print_char '(';
        print_string "λ"; print_string x; print_char '.'; print_aux e false;
        if needs_parens then
          print_char ')';
  in
  print_aux t false

let parse_term (x : string) =
  let lex (x : string) =
    let rec lex_aux (x : string) (start : int) =
    for pos = 0 to String.length x do
      ()
    done
    in

  let pos = ref 0 in
  let get_char () = String.get pos in
  let parse_arg () =
    let arg_start = pos in
    let arg_end = String.index_from x arg_start '.' in
    let arg_len = (arg_end - arg_start) + 1
    pos := arg_end;
    String.sub arg_start arg_end
  in
  if String.get x 0 = 'λ' then


let rec subst (t : term) (x : string) (v : term) =
  match t with
  | Var y -> if x = y then v else t
  | Ap(f, v') -> Ap(subst f x v, subst v' x v)
  | Abs(y, e) -> Abs(y, if x = y then e else subst e x v)
  | _ -> t

let rec eval_term (t : term) =
  match t with
  | Ap (f, v) ->
    let f = eval_term f in
    let v = eval_term v in
    (match f with
     | Abs(x, e) -> eval_term (subst e x v)
     | Extern("get") -> let i = (input_char stdin) in
       if ((Char.code i land 1) = 1) then t_true else t_false
     | Extern("put0") -> print_char '0'; flush stdout; Extern("postput0")
     | Extern("put1") -> print_char '1'; flush stdout; Extern("postput1")
     | _ -> Ap(f,v)
    )
  | _ -> t

let e1 = Ap(Abs("a", (Ap(Var "a", Var "a"))), Extern "C")
let e2 = Ap(Abs("f", Abs("x", Ap(Var "f", Var "x"))),
            Abs("f", Abs("x", Ap(Var "f", Var "x"))))
let e3 = t_seq(t_put0, t_put1)
let e3 = t_seq(t_put0, t_put1)
let e4 = Ap(Ap(Ap(t_get, Extern("put1")), Extern("put0")), t_id)

let e5 = Ap(Ap(t_Z,
               Abs("fZ",
                   Abs("iter_again",
                       t_ite(Var "iter_again",
                             t_seq(t_put0, Ap(Var "fZ", t_false)),
                             t_id
                            )
                      )
                  )
              ),
            t_true)

let e6 = Ap(Ap(t_Z,
               Abs("fZ",
                   Abs("_unit",
                       t_seq(Ap(Ap(Ap(t_get, Extern("put1")), Extern("put0")), t_id),
                             Ap(Var "fZ", t_id))
                      )
                  )
              ),
            t_id)

let test e =
  let ep = eval_term e in
  print_newline ();
  print_string "Trace:";
  print_term e;
  print_string " -> ";
  print_term ep;
  print_newline ()

let () =
  test e1;
  test e2;
  test e3;
  test e4;
  test e5;
  test e6;
