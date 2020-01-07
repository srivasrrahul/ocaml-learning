open Core
open Lexer
open Lexing




let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);;

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    exit (-1);;






let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  Some(v) ->
    begin
      printf "parsed_value";
    (* Manual_token_type.output_value v; *)
      let evaluated_value = Expr_ast.eval_expr v in
      Printf.printf "evaluated value = %d\n" evaluated_value;
    end
  | None -> ();;

let loop filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx;;

loop "/Users/rasrivastava/OCAML/ARITH_EXPR_PARSER/test_input.txt";;