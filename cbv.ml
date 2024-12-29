open Term

let parse (s : string) =
  Parser.main Lexer.token (Lexing.from_string s)

let t_true = parse "λ mt. λ mf. mt"
let t_false = parse "λ mt. λ mf. mf"

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

let wrapper = parse {|
    λ f.
    let tt := (λx.x) in
    let true := (λ mt. λ mf. mt) in
    let false := (λ mt. λ mf. mf) in
    let put := (λ b. b @put1 @put0 tt) in
    let fix := λ f.(λx. f(λv. x x v)) (λx. f(λv. x x v)) in
    f tt
|}

let parse' (s : string) = Ap(wrapper, (parse s))

let eval_term' (s : string) =
  let t = parse' s in
  print_term t;
  print_newline ();
  let t' = eval_term t in
  print_newline ();
  print_term t'

let e1 = parse "(λ a. (a a)) @C"
let e2 = parse "let y := (λ f. λ x. f x) in (y y)"
let e3 = parse {|
let tt := (λx. x) in
@put0 tt;
@put1 tt
|}
let e4 = parse "@get (λ x. x) @put1 @put0 (λx. x)"

let e5 = parse' {| λ _.
  fix (λ recur. λ iter_again.
       iter_again (λ _. put true; recur false) (λ _. tt) tt
  ) true
|}

let e6 = parse' {| λ _.
    fix (λ recur. λ _.put (@get ()); recur ()) ()
|}

let test e =
  print_newline ();
  print_string "Starting: "; print_term e;
  print_newline ();
  let ep = eval_term e in
  print_newline ();
  print_string "Ending with: ";
  print_term ep;
  print_newline ()

let () =
  print_term (parse "λ _ . a b; c d");
  print_newline ();
  eval_term' "λ _ . put false; put true; put true; put true";
  test e1;
  test e2;
  test e3;
  test e4;
  test e5;
  test e6;
