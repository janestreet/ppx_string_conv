open! Base
open! Ppxlib
module A = Ast_builder.Default

let ppx_name = "ppx_string_conv"

let error_ext ~loc message : extension =
  Location.error_extensionf ~loc "%s: %s" ppx_name message
;;

module Nested_type = struct
  type t =
    | Identifier of
        { name : longident
        ; arguments : t Loc.t list
        }
    | Type_variable of string

  let rec of_core_type (type_ : core_type) =
    let loc = type_.ptyp_loc in
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_.ptyp_desc with
    | Ptyp_constr (name, arguments) ->
      let%map.Result arguments = List.map arguments ~f:of_core_type |> Result.all in
      { loc; txt = Identifier { name = name.txt; arguments } }
    | Ptyp_var (name, _) -> Ok { loc; txt = Type_variable name }
    | Ptyp_variant _ ->
      Error (error_ext ~loc "polymorphic variants are only supported for named types")
    | _ -> Error (error_ext ~loc "type not supported")
  ;;
end

module Fn_name = struct
  type t =
    | To_string
    | Of_string

  let arg_type ~base_type ~loc = function
    | To_string -> base_type
    | Of_string -> [%type: string]
  ;;

  let return_type ~base_type ~loc = function
    | To_string -> [%type: string]
    | Of_string -> base_type
  ;;

  let for_type_variable t ~name =
    match t with
    | To_string -> [%string "__string_of_'%{name}__"]
    | Of_string -> [%string "__'%{name}_of_string__"]
  ;;

  let for_type t ~type_name =
    match t, type_name with
    | To_string, "t" -> "to_string"
    | Of_string, "t" -> "of_string"
    (* to work with things like [int_of_string] and [string_of_int] *)
    | To_string, _ -> [%string "string_of_%{type_name}"]
    | Of_string, _ -> [%string "%{type_name}_of_string"]
  ;;

  let rec call' t ~type_:{ loc; txt } ~(arg : expression option) =
    match (txt : Nested_type.t) with
    | Identifier { name = Lident "string"; arguments = [] } ->
      (* Special case for the type 'string' -- there's no 'string_of_string' function, so
         we use the identity function instead *)
      (match arg with
       | Some expr -> expr
       | None -> [%expr fun s -> s])
    | Identifier { name; arguments } ->
      (* Normal case -- follow naming convention *)
      A.type_constr_conv
        { loc; txt = name }
        ~loc
        ~f:(fun type_name -> for_type t ~type_name)
        (List.map ~f:(fun type_ -> call' t ~arg:None ~type_) arguments
         @ Option.to_list arg)
    | Type_variable name ->
      let expression = for_type_variable t ~name |> A.evar ~loc in
      A.eapply ~loc expression (Option.to_list arg)
  ;;

  let call t arg ~type_ = call' t ~type_ ~arg:(Some arg)
  let get_function t ~type_ = call' t ~type_ ~arg:None
end

module What_to_generate = struct
  type t =
    | Only of Fn_name.t (** [to_string] or [of_string] *)
    | Both (** just [string] *)

  let build t ~f =
    let fns =
      match t with
      | Only fn -> [ fn ]
      | Both -> [ To_string; Of_string ]
    in
    List.map fns ~f
  ;;
end

let value_binding_item ~loc ~portable pat expr =
  Ppxlib_jane.Ast_builder.Default.value_binding
    ~pat
    ~modes:(if portable then [ { loc; txt = Mode "portable" } ] else [])
    ~expr
    ~loc
;;

let type_of_decl (decl : type_declaration) : core_type =
  let loc = decl.ptype_name.loc in
  A.ptyp_constr
    ~loc
    (Loc.make ~loc (Lident decl.ptype_name.txt))
    (List.map decl.ptype_params ~f:fst)
;;

let pexp_error ~loc message = A.pexp_extension ~loc (error_ext ~loc message)

module String_literal_or_variable = struct
  type t =
    | Literal of string Loc.t
    | Variable of expression

  let to_expression t =
    match t with
    | Literal s -> A.estring ~loc:s.loc s.txt
    | Variable expr -> expr
  ;;

  let length_expression t ~loc =
    match t with
    | Literal s -> A.eint ~loc (String.length s.txt)
    | Variable expr -> [%expr Ppx_string_conv_runtime.String.length [%e expr]]
  ;;
end

let make_stringable_sig_for_type
  ~loc
  ~what_to_generate
  (decl : type_declaration)
  ~portable
  : signature_item list
  =
  match name_type_params_in_td_res decl with
  | Error (error, rest_of_errors) ->
    let loc = decl.ptype_loc in
    List.map (error :: rest_of_errors) ~f:(fun error ->
      A.psig_extension ~loc (Location.Error.to_extension error) [])
  | Ok decl ->
    let t = type_of_decl decl in
    let make_val fn_name sig_ =
      A.psig_value
        ~loc
        (Ppxlib_jane.Ast_builder.Default.value_description
           ~loc
           ~name:
             (Loc.map decl.ptype_name ~f:(fun type_name ->
                Fn_name.for_type fn_name ~type_name))
           ~prim:[]
           ~modalities:
             (if portable then Ppxlib_jane.Shim.Modalities.portable ~loc else [])
           ~type_:(sig_ t ~fn_name))
    in
    let type_ base_type ~fn_name =
      let ret_type = Fn_name.return_type fn_name ~base_type ~loc in
      let arg_type = Fn_name.arg_type fn_name ~base_type ~loc in
      List.fold_right
        decl.ptype_params
        ~init:[%type: [%t arg_type] -> [%t ret_type]]
        ~f:(fun (base_type, _) rest ->
          let ret_type = Fn_name.return_type fn_name ~base_type ~loc in
          let arg_type = Fn_name.arg_type fn_name ~base_type ~loc in
          [%type: ([%t arg_type] -> [%t ret_type]) -> [%t rest]])
    in
    What_to_generate.build what_to_generate ~f:(fun fn_name -> make_val fn_name type_)
;;

let generate_intf ~what_to_generate =
  Deriving.Generator.V2.make
    Deriving.Args.(empty +> flag "portable")
    (fun ~ctxt (_rec_flag, types) portable ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      List.concat_map
        types
        ~f:(make_stringable_sig_for_type ~loc ~what_to_generate ~portable))
;;

module Constructor_kind = struct
  type t =
    | Base_string of { name : string Loc.t option }
    | Fallback
    | Nested of { prefix : String_literal_or_variable.t option }
    | Nested_auto_prefix

  module Attr = struct
    let fallback' context =
      Attribute.declare_with_attr_loc
        "stringable.fallback"
        context
        Ast_pattern.(pstr nil)
        (fun ~attr_loc:loc -> loc)
    ;;

    let nested' context =
      Attribute.declare_with_attr_loc
        "stringable.nested"
        context
        Ast_pattern.(
          alt
            (map1 ~f:(fun prefix_expr -> Some prefix_expr) (single_expr_payload __))
            (map0 ~f:None (pstr nil)))
        (fun ~attr_loc:loc prefix_expr_opt -> loc, prefix_expr_opt)
    ;;

    let rename' context =
      Attribute.declare
        "stringable.rename"
        context
        Ast_pattern.(single_expr_payload (estring __'))
        (fun s -> s)
    ;;

    let fallback = fallback' Attribute.Context.constructor_declaration
    let nested = nested' Attribute.Context.constructor_declaration
    let rename = rename' Attribute.Context.constructor_declaration
    let fallback_poly = fallback' Attribute.Context.rtag
    let nested_poly = nested' Attribute.Context.rtag
    let rename_poly = rename' Attribute.Context.rtag
  end

  let get' ~fallback ~nested ~rename constr =
    let fallback = Attribute.get fallback constr in
    let nested = Attribute.get nested constr in
    let rename = Attribute.get rename constr in
    let multiple_attribute_error ~loc =
      Error
        (error_ext ~loc "at most one of 'rename', 'nested' and 'fallback' may be used")
    in
    match fallback, nested, rename with
    | Some _, None, None -> Ok Fallback
    | None, Some (_, prefix_expr_opt), None ->
      (match prefix_expr_opt with
       | Some prefix_expr ->
         (match prefix_expr.pexp_desc with
          | Pexp_constant (Pconst_string (txt, _, _)) ->
            let prefix =
              if String.is_empty txt
              then None
              else
                Some
                  (String_literal_or_variable.Literal { txt; loc = prefix_expr.pexp_loc })
            in
            Ok (Nested { prefix })
          | _ -> Ok (Nested { prefix = Some (Variable prefix_expr) }))
       | None -> Ok Nested_auto_prefix)
    | None, None, name -> Ok (Base_string { name })
    | Some loc, _, _ | _, Some (loc, _), _ -> multiple_attribute_error ~loc
  ;;

  let get constr =
    get' constr ~fallback:Attr.fallback ~nested:Attr.nested ~rename:Attr.rename
  ;;

  let get_poly constr =
    get'
      constr
      ~fallback:Attr.fallback_poly
      ~nested:Attr.nested_poly
      ~rename:Attr.rename_poly
  ;;
end

let maybe_capitalize ~capitalization name =
  match capitalization with
  | None -> name
  | Some capitalization ->
    Loc.map
      name
      ~f:(Capitalization_ppx_configuration.apply_to_snake_case_exn capitalization)
;;

let generate_auto_prefix ~capitalization ~variant_name ~nested_separator =
  (* Generate prefix from variant name + separator from capitalization or nested_separator *)
  let transformed_name = maybe_capitalize ~capitalization variant_name in
  let separator =
    match nested_separator with
    | Some sep -> sep
    | None ->
      (match capitalization with
       | None -> "_"
       | Some (Multi_word cap) ->
         (match Capitalization.separator cap with
          (* Absence of the separator means that words are just concatenated. *)
          | None -> ""
          | Some sep_char -> String.make 1 sep_char)
       | Some (Single_word single_word) ->
         Capitalization_ppx_configuration.Single_word.raise_incompatible
           single_word
           "single word capitalizations are not compatible with [@nested];@ either \
            specify the ~nested_separator argument or use a multiple word capitalization")
  in
  String_literal_or_variable.Literal
    (Loc.map transformed_name ~f:(fun name -> name ^ separator))
;;

let build_value_binding_impl ~loc ~decl ~fn_name ~type_variables ~body ~portable =
  let fn_name_pattern =
    A.pvar ~loc (Fn_name.for_type fn_name ~type_name:decl.ptype_name.txt)
  in
  let variables =
    List.map type_variables ~f:(fun { txt = name; loc } ->
      let pattern = Fn_name.for_type_variable fn_name ~name |> A.pvar ~loc in
      let base_type = A.ptyp_var ~loc name in
      let arg = Fn_name.arg_type fn_name ~loc ~base_type in
      let ret = Fn_name.return_type fn_name ~loc ~base_type in
      [%pat? ([%p pattern] : [%t arg] -> [%t ret])])
  in
  let body = A.eabstract variables body ~loc in
  value_binding_item ~loc ~portable fn_name_pattern body
;;

let build_function ~loc ~decl ~fn_name ~type_variables ~arg_name ~match_ ~cases ~portable =
  (* builds [let [fn_name] ([arg_name] : [arg]): [ret] = match [match_] with [cases]] *)
  let base_type = type_of_decl decl in
  let arg = Fn_name.arg_type fn_name ~loc ~base_type in
  let ret = Fn_name.return_type fn_name ~loc ~base_type in
  let body =
    [%expr
      fun ([%p arg_name] : [%t arg]) : [%t ret] -> [%e A.pexp_match ~loc match_ cases]]
  in
  build_value_binding_impl ~loc ~decl ~fn_name ~type_variables ~body ~portable
;;

module Generic_variant_renderer = struct
  module Basic_case = struct
    type t =
      { expression : expression
      ; pattern : pattern
      ; string : string Loc.t
      }
  end

  module Nested_case = struct
    type t =
      { pattern_with_x : pattern
      ; expression_of_x : expression
      ; nested_type : Nested_type.t Loc.t
      ; prefix : String_literal_or_variable.t option
      }
  end

  module Fallback_case = struct
    type t =
      { pattern_with_x : pattern
      ; expression_of_x : expression
      ; nested_type : Nested_type.t Loc.t
      }
  end

  type t =
    { loc : Location.t
    ; code_path : Code_path.t
    ; case_insensitive : bool
    ; capitalization : Capitalization_ppx_configuration.t option
    ; decl : type_declaration
    ; errors : extension list
    ; basic_cases : Basic_case.t list
    ; nested_cases : Nested_case.t list
    ; maybe_fallback_case : Fallback_case.t option
    ; list_options_on_error : bool
    ; portable : bool
    ; type_variables : string Loc.t list
    }

  let maybe_error_wildcard ~loc ~errors =
    (* If there are constructors with invalid parameters, we don't bother generate a
       [to_string] for them, making the match non-exhaustive

       To avoid generating a confusing error about that that's shown to the user, we need
       to add a dummy wildcard case if there are any errors to make the match exhaustive. *)
    match errors with
    | [] -> []
    | _ :: _ -> [ A.case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false] ]
  ;;

  let render_to_string
    { loc
    ; code_path = _
    ; case_insensitive = _
    ; capitalization = _
    ; decl
    ; errors
    ; basic_cases
    ; nested_cases
    ; maybe_fallback_case
    ; list_options_on_error = _
    ; portable
    ; type_variables
    }
    =
    let build_case ~pattern ~expr = A.case ~lhs:pattern ~guard:None ~rhs:expr in
    let build_basic ({ pattern; string; expression = _ } : Basic_case.t) =
      let string_lit = A.estring ~loc:string.loc string.txt in
      build_case ~pattern ~expr:string_lit
    in
    let build_nested_or_fallback ~pattern_with_x ~nested_type ~prefix =
      let body =
        let unprefixed = Fn_name.call ~type_:nested_type To_string [%expr x] in
        match prefix with
        | None -> unprefixed
        | Some prefix ->
          [%expr
            Ppx_string_conv_runtime.String.( ^ )
              [%e String_literal_or_variable.to_expression prefix]
              [%e unprefixed]]
      in
      build_case ~pattern:pattern_with_x ~expr:body
    in
    let build_nested
      ({ pattern_with_x; nested_type; prefix; expression_of_x = _ } : Nested_case.t)
      =
      build_nested_or_fallback ~pattern_with_x ~nested_type ~prefix
    in
    let build_fallback
      ({ pattern_with_x; nested_type; expression_of_x = _ } : Fallback_case.t)
      =
      build_nested_or_fallback ~pattern_with_x ~nested_type ~prefix:None
    in
    let cases =
      [ List.map basic_cases ~f:build_basic
      ; List.map nested_cases ~f:build_nested
      ; Option.to_list maybe_fallback_case |> List.map ~f:build_fallback
      ; maybe_error_wildcard ~loc ~errors
      ]
      |> List.concat
    in
    build_function
      ~loc
      ~decl
      ~fn_name:To_string
      ~arg_name:[%pat? t]
      ~match_:[%expr t]
      ~cases
      ~portable
      ~type_variables
  ;;

  let render_of_string
    { loc
    ; code_path
    ; case_insensitive
    ; capitalization = _
    ; decl
    ; errors = _
    ; basic_cases
    ; nested_cases
    ; maybe_fallback_case
    ; list_options_on_error
    ; portable
    ; type_variables
    }
    =
    let build_basic ({ expression; string; pattern = _ } : Basic_case.t) =
      (* Builds case for [ | "text" -> Constr]

         Note we convert to lowercase if case-insensitive. Later on we also convert the
         input for the case-insensitive ocaml pattern match. *)
      let string_pat =
        A.pstring
          ~loc:string.loc
          (if case_insensitive then String.lowercase string.txt else string.txt)
      in
      A.case ~lhs:string_pat ~guard:None ~rhs:expression
    in
    let call_nested ~expression_of_x ~nested_type expr =
      let x = Fn_name.call ~type_:nested_type Of_string expr in
      [%expr
        let x = [%e x] in
        [%e expression_of_x]]
    in
    let raise_or_fallback expr =
      match maybe_fallback_case with
      | None ->
        let fn_name = Fn_name.for_type Of_string ~type_name:decl.ptype_name.txt in
        let path = Code_path.fully_qualified_path code_path in
        let message = A.estring ~loc [%string "%{path}.%{fn_name}: invalid string"] in
        (match list_options_on_error with
         | false ->
           [%expr
             Ppx_string_conv_runtime.raise_s
               (List [ Atom [%e message]; List [ Atom "value"; Atom [%e expr] ] ])]
         | true ->
           (match nested_cases with
            | _ :: _ ->
              pexp_error
                ~loc:expr.pexp_loc
                "Using [list_options_on_error] is incompatible with [nested]."
            | [] ->
              let valid_strings =
                A.elist
                  ~loc
                  (List.map basic_cases ~f:(fun basic_case ->
                     [%expr Atom [%e A.estring ~loc basic_case.string.txt]]))
              in
              [%expr
                Ppx_string_conv_runtime.raise_s
                  (List
                     [ Atom [%e message]
                     ; List [ Atom "value"; Atom [%e expr] ]
                     ; List [ Atom "valid_options"; List [%e valid_strings] ]
                     ])]))
      | Some ({ expression_of_x; nested_type; pattern_with_x = _ } : Fallback_case.t) ->
        call_nested ~expression_of_x ~nested_type expr
    in
    let build_nested_unprefixed ~expression_of_x ~nested_type ~expr ~or_else =
      (* Handler for [| Constr of nested_type [@nested]] *)
      [%expr
        try [%e call_nested ~expression_of_x ~nested_type expr] with
        | _ -> [%e or_else]]
    in
    let build_nested_prefixed ~expression_of_x ~nested_type ~expr:s ~or_else ~prefix =
      (* Handler for [| Constr of nested_type [@nested "prefix"]] *)
      let prefix_expr = String_literal_or_variable.to_expression prefix in
      match case_insensitive with
      | false ->
        [%expr
          match
            Ppx_string_conv_runtime.String.chop_prefix [%e s] ~prefix:[%e prefix_expr]
          with
          | Some x -> [%e call_nested ~expression_of_x ~nested_type [%expr x]]
          | None -> [%e or_else]]
      | true ->
        (* There's no [Ppx_string_conv_runtime.String.Caseless.chop_prefix] but we can
           make one ourselves with [is_prefix] and [drop_prefix]. *)
        let prefix_length = String_literal_or_variable.length_expression prefix ~loc in
        [%expr
          match
            Ppx_string_conv_runtime.String.Caseless.is_prefix
              [%e s]
              ~prefix:[%e prefix_expr]
          with
          | true ->
            [%e
              call_nested
                ~expression_of_x
                ~nested_type
                [%expr
                  Ppx_string_conv_runtime.String.drop_prefix [%e s] [%e prefix_length]]]
          | false -> [%e or_else]]
    in
    let build_nested
      ({ expression_of_x; nested_type; prefix; pattern_with_x = _ } : Nested_case.t)
      ~or_else
      expr
      =
      (* General handler for [@nested] *)
      match prefix with
      | None -> build_nested_unprefixed ~expression_of_x ~nested_type ~expr ~or_else
      | Some prefix ->
        build_nested_prefixed ~expression_of_x ~nested_type ~expr ~or_else ~prefix
    in
    let fallback_case_body expr =
      (* The body of the fallback case -- it's the final fallback at the end, followed by
         each of the nesteds with the earlier variants taking priority.

         That's done by a [List.fold_right] with each nested case falling back to cases
         after it in the list, with the final one falling back to the fallback case or
         raise. *)
      List.fold_right
        nested_cases
        ~f:(fun nested_case or_else -> build_nested nested_case ~or_else expr)
        ~init:(raise_or_fallback expr)
    in
    let match_ =
      match case_insensitive with
      | false -> [%expr s]
      | true -> [%expr Ppx_string_conv_runtime.String.lowercase s]
    in
    let cases =
      let fallback_case =
        A.case ~lhs:[%pat? _] ~guard:None ~rhs:(fallback_case_body [%expr s])
      in
      List.map basic_cases ~f:build_basic @ [ fallback_case ]
    in
    build_function
    (* Main body. Most interesting thing is we instead match the result of the
       [String.Lowercase] call if case-insensitive passed to line up with the
       pre-lowercased base string matches *)
      ~loc
      ~decl
      ~fn_name:Of_string
      ~type_variables
      ~arg_name:[%pat? s]
      ~match_
      ~cases
      ~portable
  ;;

  let render_errors
    { loc
    ; code_path = _
    ; case_insensitive = _
    ; capitalization = _
    ; decl = _
    ; errors
    ; basic_cases = _
    ; nested_cases = _
    ; maybe_fallback_case = _
    ; list_options_on_error = _
    ; portable = _
    ; type_variables = _
    }
    =
    (* Renders errors, specifically of bad settings on constructors. Note despite their
       location after the generated functions in the AST, the locations on the errors are
       actually tied to the variant definitions that triggered them, so the red line goes
       in the right place in editors *)
    List.map errors ~f:(fun error -> A.pstr_extension ~loc error [])
  ;;

  let render t ~fn_name =
    let generated =
      match (fn_name : Fn_name.t) with
      | To_string -> render_to_string t
      | Of_string -> render_of_string t
    in
    generated, render_errors t
  ;;
end

module Variant_impl = struct
  (* A helper type for validating & storing data about constructors, which gets rendered
     to the implementation in ml files *)

  type t =
    { loc : Location.t
    ; code_path : Code_path.t
    ; case_insensitive : bool
    ; capitalization : Capitalization_ppx_configuration.t option
    ; nested_separator : string option
    ; decl : type_declaration
    ; base_string_variants : (constructor_declaration * string Loc.t) Queue.t
    ; nested_variants :
        (constructor_declaration
        * Nested_type.t Loc.t
        * String_literal_or_variable.t option)
          Queue.t
    ; mutable fallback_variant : (constructor_declaration * Nested_type.t Loc.t) option
    ; errors : extension Queue.t
    ; list_options_on_error : bool
    ; portable : bool
    ; type_variables : string Loc.t list
    }

  let parse_base_string t ~constr ~name =
    match (constr : constructor_declaration) with
    | { pcd_args = Pcstr_tuple []; pcd_vars = []; pcd_res = None; pcd_name; _ } ->
      Ok
        (Option.value_or_thunk name ~default:(fun () ->
           maybe_capitalize ~capitalization:t.capitalization pcd_name))
    | { pcd_args = Pcstr_tuple [ _arg ]; pcd_vars = []; pcd_res = None; pcd_name = _; _ }
      ->
      Error
        (error_ext
           ~loc:constr.pcd_loc
           "expected variant without arguments: to take an argument, add [@nested] or \
            [@nested \"your_prefix_here\"]")
    | { pcd_args = _; pcd_vars = _; pcd_res = Some _; pcd_name = _; _ } ->
      Error (error_ext ~loc:constr.pcd_loc "GADTs not supported")
    | _ -> Error (error_ext ~loc:constr.pcd_loc "expected variant without arguments")
  ;;

  let get_nested_type constr =
    match (constr : constructor_declaration) with
    | { pcd_args = Pcstr_tuple [ arg ]; pcd_vars = []; pcd_res = None; _ } ->
      let ty = Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg in
      Nested_type.of_core_type ty
    | { pcd_args = _; pcd_vars = _; pcd_res = Some _; pcd_name = _; _ } ->
      Error (error_ext ~loc:constr.pcd_loc "GADTs not supported")
    | _ ->
      Error
        (error_ext
           ~loc:constr.pcd_loc
           [%string "expected exactly one argument ([| %{constr.pcd_name.txt} of ...])"])
  ;;

  let add_variant t constr =
    match%bind.Result Constructor_kind.get constr with
    | Base_string { name } ->
      let%map.Result name_to_use = parse_base_string t ~constr ~name in
      Queue.enqueue t.base_string_variants (constr, name_to_use)
    | Fallback ->
      let%bind.Result type_ = get_nested_type constr in
      (match t.fallback_variant with
       | None ->
         t.fallback_variant <- Some (constr, type_);
         Ok ()
       | Some _ ->
         Error
           (error_ext ~loc:constr.pcd_loc "'fallback' can only be used on one variant"))
    | Nested { prefix } ->
      let%map.Result nested_type = get_nested_type constr in
      Queue.enqueue t.nested_variants (constr, nested_type, prefix)
    | Nested_auto_prefix ->
      let%map.Result nested_type = get_nested_type constr in
      let auto_prefix =
        generate_auto_prefix
          ~capitalization:t.capitalization
          ~variant_name:constr.pcd_name
          ~nested_separator:t.nested_separator
      in
      Queue.enqueue t.nested_variants (constr, nested_type, Some auto_prefix)
  ;;

  let create
    ~loc
    ~code_path
    ~case_insensitive
    ~capitalization
    ~list_options_on_error
    ~nested_separator
    ~decl
    ~constructors
    ~portable
    ~type_variables
    =
    let t =
      { loc
      ; code_path
      ; case_insensitive
      ; capitalization
      ; nested_separator
      ; decl
      ; base_string_variants = Queue.create ()
      ; nested_variants = Queue.create ()
      ; fallback_variant = None
      ; errors = Queue.create ()
      ; list_options_on_error
      ; portable
      ; type_variables
      }
    in
    List.iter constructors ~f:(fun constructor ->
      match add_variant t constructor with
      | Ok () -> ()
      | Error error -> Queue.enqueue t.errors error);
    t
  ;;

  let render
    { loc
    ; code_path
    ; case_insensitive
    ; capitalization
    ; nested_separator = _
    ; decl
    ; base_string_variants
    ; nested_variants
    ; fallback_variant
    ; errors
    ; list_options_on_error
    ; portable
    ; type_variables
    }
    ~fn_name
    =
    let errors = Queue.to_list errors in
    let basic_cases =
      Queue.to_list base_string_variants
      |> List.map ~f:(fun (constr, string) ->
        let expression = A.econstruct constr None in
        let pattern = A.pconstruct constr None in
        ({ expression; pattern; string } : Generic_variant_renderer.Basic_case.t))
    in
    let nested_cases =
      Queue.to_list nested_variants
      |> List.map ~f:(fun (constr, nested_type, prefix) ->
        let expression_of_x = A.econstruct constr (Some [%expr x]) in
        let pattern_with_x = A.pconstruct constr (Some [%pat? x]) in
        ({ expression_of_x; pattern_with_x; nested_type; prefix }
         : Generic_variant_renderer.Nested_case.t))
    in
    let maybe_fallback_case =
      Option.map fallback_variant ~f:(fun (constr, nested_type) ->
        let expression_of_x = A.econstruct constr (Some [%expr x]) in
        let pattern_with_x = A.pconstruct constr (Some [%pat? x]) in
        ({ expression_of_x; pattern_with_x; nested_type }
         : Generic_variant_renderer.Fallback_case.t))
    in
    let gvr : Generic_variant_renderer.t =
      { loc
      ; code_path
      ; case_insensitive
      ; capitalization
      ; decl
      ; errors
      ; basic_cases
      ; nested_cases
      ; maybe_fallback_case
      ; list_options_on_error
      ; portable
      ; type_variables
      }
    in
    Generic_variant_renderer.render gvr ~fn_name
  ;;
end

module Poly_variant_impl = struct
  let parse_row_field ~loc ~capitalization ~nested_separator ~decl (rtag : row_field) =
    let this_type = type_of_decl decl in
    let%bind.Result constructor_kind = Constructor_kind.get_poly rtag in
    let error txt = Error (error_ext ~loc txt) in
    let make_basic ~name ~original_name =
      let string =
        match name with
        | None -> maybe_capitalize ~capitalization original_name
        | Some name -> name
      in
      let expression = A.pexp_variant ~loc:original_name.loc original_name.txt None in
      let pattern = A.ppat_variant ~loc:original_name.loc original_name.txt None in
      let case =
        ({ expression; pattern; string } : Generic_variant_renderer.Basic_case.t)
      in
      Ok (`basic_case case)
    in
    let make_nested ~prefix ~name ~inner_type =
      let%bind.Result nested_type = Nested_type.of_core_type inner_type in
      let expression_of_x = A.pexp_variant ~loc:name.loc name.txt (Some [%expr x]) in
      let pattern_with_x = A.ppat_variant ~loc:name.loc name.txt (Some [%pat? x]) in
      let case =
        ({ expression_of_x; pattern_with_x; nested_type; prefix }
         : Generic_variant_renderer.Nested_case.t)
      in
      Ok (`nested_case case)
    in
    let make_fallback ~name ~inner_type =
      let%bind.Result nested_type = Nested_type.of_core_type inner_type in
      let expression_of_x = A.pexp_variant ~loc:name.loc name.txt (Some [%expr x]) in
      let pattern_with_x = A.ppat_variant ~loc:name.loc name.txt (Some [%pat? x]) in
      let case =
        ({ expression_of_x; pattern_with_x; nested_type }
         : Generic_variant_renderer.Fallback_case.t)
      in
      Ok (`fallback_case case)
    in
    let make_subvariant ~prefix (subvariant : core_type) =
      let%bind.Result nested_type = Nested_type.of_core_type subvariant in
      let%bind.Result nested_type_name =
        match nested_type.txt with
        | Identifier { name; arguments = _ } ->
          (* If you inherit the same name twice, they're either equal or it's a type
             error, so we can (and actually must) just use the name. See:
             {[
               (* If you enter this in utop *)
               type 'a t = [ `T of 'a ]]

               type ('a, 'b) x =
                 [ 'a t
                 | 'b t
                 ]
                 constraint 'a = [> `A ] constraint 'b = [> `B ]

               (* You get back this *)
               type ('a, 'b) x = [ `T of 'a ]
                 constraint 'a = [> `A | `B ] constraint 'b = 'a
             ]}
          *)
          Ok { nested_type with txt = name }
        | Type_variable _ ->
          (* This is actually (currently) a type error *)
          Error
            (error_ext
               ~loc
               "polymorphic variant inheriting from type variable not supported")
      in
      let pattern_with_x =
        let type_constraint = Ast_helper.Pat.type_ ~loc nested_type_name in
        Ast_helper.Pat.alias type_constraint { txt = "x"; loc }
      in
      let expression_of_x = [%expr (x :> [%t this_type])] in
      let case =
        ({ pattern_with_x; expression_of_x; nested_type; prefix }
         : Generic_variant_renderer.Nested_case.t)
      in
      Ok (`nested_case case)
    in
    match rtag.prf_desc, constructor_kind with
    | Rtag (original_name, true, []), Base_string { name } ->
      make_basic ~name ~original_name
    | Rinherit subvariant, Base_string { name = None } ->
      make_subvariant subvariant ~prefix:None
    | Rinherit subvariant, Nested { prefix } -> make_subvariant ~prefix subvariant
    | Rinherit _, Nested_auto_prefix ->
      error
        [%string
          "You used [@nested] on a polymorphic subvariant; most likely you will achieve \
           the desired behavior by dropping it."]
    | Rtag (name, false, [ inner_type ]), Nested { prefix } ->
      make_nested ~prefix ~name ~inner_type
    | Rtag (name, false, [ inner_type ]), Nested_auto_prefix ->
      let auto_prefix =
        generate_auto_prefix ~capitalization ~variant_name:name ~nested_separator
      in
      make_nested ~prefix:(Some auto_prefix) ~name ~inner_type
    | Rtag (name, false, [ inner_type ]), Fallback -> make_fallback ~name ~inner_type
    | Rtag (name, false, [ _ ]), Base_string _ ->
      error
        [%string
          "expected poly variant with no inner types: [%{name.txt}]; maybe you forgot \
           [@nested] or [@fallback]?"]
    | Rtag (name, false, []), _ ->
      error
        [%string
          "invalid state: no empty constructor, no non-empty constructors: [%{name.txt}]"]
    | Rtag (name, true, []), Fallback ->
      error [%string "can't use [@fallback] on a regular case: [%{name.txt}]"]
    | Rtag (name, true, []), (Nested _ | Nested_auto_prefix) ->
      error [%string "can't use [@nested] on a regular case: [%{name.txt}]"]
    | Rtag (name, true, _ :: _), _ | Rtag (name, false, _ :: _ :: _), _ ->
      error
        [%string
          "can't handle poly variant with same name and multiple inner types: \
           [%{name.txt}]"]
    | Rinherit _, Base_string { name = Some _ } ->
      error "can't use [@rename] on a subvariant of polymorphic variant"
    | Rinherit _, Fallback ->
      error "can't use [@fallback] on a subvariant of polymorphic variant"
  ;;

  let parse
    ~loc
    ~code_path
    ~case_insensitive
    ~capitalization
    ~list_options_on_error
    ~nested_separator
    ~decl
    ~row_fields
    ~portable
    ~type_variables
    =
    let cases, errors =
      List.fold_right row_fields ~init:([], []) ~f:(fun row_field (rows, errors) ->
        match parse_row_field ~loc ~capitalization ~nested_separator ~decl row_field with
        | Ok row -> row :: rows, errors
        | Error error -> rows, error :: errors)
    in
    let basic_cases, nested_cases, fallback_cases =
      List.partition3_map cases ~f:(function
        | `basic_case case -> `Fst case
        | `nested_case case -> `Snd case
        | `fallback_case case -> `Trd case)
    in
    let%bind.Result maybe_fallback_case =
      match fallback_cases with
      | [] -> Ok None
      | [ case ] -> Ok (Some case)
      | _ :: _ :: _ -> Error "multiple [@fallback] cases, which is not allowed"
    in
    Ok
      ({ loc
       ; code_path
       ; case_insensitive
       ; capitalization
       ; decl
       ; errors
       ; basic_cases
       ; nested_cases
       ; maybe_fallback_case
       ; list_options_on_error
       ; portable
       ; type_variables
       }
       : Generic_variant_renderer.t)
  ;;
end

module Argument_names = struct
  let case_insensitive = "case_insensitive"
  let capitalize = Capitalization_ppx_configuration.argument_name
  let list_options_on_error = "list_options_on_error"
  let nested_separator = "nested_separator"
end

let build_variant_impl
  ~loc
  ~code_path
  ~case_insensitive
  ~capitalization
  ~list_options_on_error
  ~nested_separator
  ~fn_name
  ~type_variables
  ~decl
  ~constructors
  ~portable
  =
  Variant_impl.create
    ~loc
    ~code_path
    ~case_insensitive
    ~capitalization
    ~list_options_on_error
    ~nested_separator
    ~decl
    ~constructors
    ~type_variables
    ~portable
  |> Variant_impl.render ~fn_name
;;

let build_poly_variant_impl
  ~loc
  ~code_path
  ~case_insensitive
  ~capitalization
  ~list_options_on_error
  ~nested_separator
  ~fn_name
  ~type_variables
  ~decl
  ~row_fields
  ~portable
  =
  let%map.Result gvr =
    Poly_variant_impl.parse
      ~loc
      ~code_path
      ~case_insensitive
      ~capitalization
      ~list_options_on_error
      ~nested_separator
      ~type_variables
      ~decl
      ~row_fields
      ~portable
  in
  Generic_variant_renderer.render gvr ~fn_name
;;

let build_alias_impl
  ~loc
  ~case_insensitive
  ~capitalization
  ~nested_separator
  ~fn_name
  ~decl
  ~alias_to
  ~portable
  ~type_variables
  =
  let invalid_argument_names =
    [ Option.some_if case_insensitive Argument_names.case_insensitive
    ; Option.map capitalization ~f:(Fn.const Argument_names.capitalize)
    ; Option.map nested_separator ~f:(Fn.const Argument_names.nested_separator)
    ]
    |> List.filter_opt
  in
  let%bind.Result () =
    match invalid_argument_names with
    | [] -> Ok ()
    | invalid_argument_names ->
      let invalid_argument_names =
        List.map invalid_argument_names ~f:(fun invalid_argument_name ->
          [%string "[%{invalid_argument_name}]"])
        |> String.concat ~sep:", "
      in
      Error
        [%string
          "following arguments not supported when used with abstract type aliases: \
           %{invalid_argument_names}"]
  in
  Ok
    (build_value_binding_impl
       ~loc
       ~decl
       ~fn_name
       ~type_variables
       ~body:(Fn_name.get_function fn_name ~type_:alias_to)
       ~portable)
;;

let build_impl_or_error
  ~loc
  ~code_path
  ~case_insensitive
  ~capitalization
  ~list_options_on_error
  ~nested_separator
  ~fn_name
  ~portable
  (decl : type_declaration)
  =
  let%bind.Result decl =
    name_type_params_in_td_res decl
    |> Result.map_error ~f:(fun (error, _) -> Location.Error.to_extension error)
  in
  let type_variables = List.map decl.ptype_params ~f:get_type_param_name in
  match Ppxlib_jane.Shim.Type_kind.of_parsetree decl.ptype_kind, decl.ptype_manifest with
  | Ptype_record _, _ -> Error (error_ext ~loc "records not supported")
  | Ptype_record_unboxed_product _, _ ->
    Error (error_ext ~loc "unboxed records not supported")
  | Ptype_abstract, None -> Error (error_ext ~loc "abstract types not supported")
  | Ptype_open, _ ->
    (* [type t = ..] *) Error (error_ext ~loc "extensible variants not supported")
  | Ptype_variant constructors, _ ->
    (* [t] is a normal variant *)
    Ok
      (build_variant_impl
         ~loc
         ~code_path
         ~case_insensitive
         ~capitalization
         ~list_options_on_error
         ~type_variables
         ~nested_separator
         ~fn_name
         ~decl
         ~constructors
         ~portable)
  | Ptype_abstract, Some type_ ->
    (match type_.ptyp_desc with
     | Ptyp_variant (row_fields, Closed, None) ->
       (* [t] is a polymorphic variant *)
       build_poly_variant_impl
         ~loc
         ~code_path
         ~case_insensitive
         ~capitalization
         ~list_options_on_error
         ~type_variables
         ~nested_separator
         ~fn_name
         ~decl
         ~row_fields
         ~portable
       |> Result.map_error ~f:(error_ext ~loc)
     | _ ->
       let result =
         let%bind.Result alias_to = Nested_type.of_core_type type_ in
         build_alias_impl
           ~loc
           ~case_insensitive
           ~capitalization
           ~nested_separator
           ~type_variables
           ~fn_name
           ~decl
           ~alias_to
           ~portable
         |> Result.map_error ~f:(error_ext ~loc)
       in
       Result.map result ~f:(fun bindings -> bindings, []))
;;

let build_impl
  ~loc
  ~code_path
  ~case_insensitive
  ~capitalization
  ~list_options_on_error
  ~nested_separator
  ~what_to_generate
  ~portable
  ~rec_flag
  decls
  =
  let rec_flag = really_recursive rec_flag decls in
  let validate_list_options_on_error =
    match (what_to_generate : What_to_generate.t) with
    | Only Of_string | Both -> []
    | Only To_string ->
      if list_options_on_error
      then
        [ A.pstr_extension
            ~loc
            (error_ext
               ~loc
               [%string
                 "[%{Argument_names.list_options_on_error}] is not meaningful when only \
                  deriving [to_string]"])
            []
        ]
      else []
  in
  let generated =
    What_to_generate.build what_to_generate ~f:(fun fn_name ->
      let bindings, errors =
        List.map decls ~f:(fun decl ->
          match
            build_impl_or_error
              ~loc
              ~code_path
              ~case_insensitive
              ~capitalization
              ~list_options_on_error
              ~nested_separator
              ~fn_name
              ~portable
              decl
          with
          | Error message -> None, [ A.pstr_extension ~loc message [] ]
          | Ok (binding, errors) -> Some binding, errors)
        |> List.unzip
      in
      A.pstr_value_list ~loc rec_flag (List.filter_opt bindings) @ List.concat errors)
    |> List.concat
  in
  validate_list_options_on_error @ generated
;;

let generate_impl ~what_to_generate =
  Deriving.Generator.V2.make
    Deriving.Args.(
      empty
      +> flag Argument_names.case_insensitive
      +> Capitalization_ppx_configuration.argument ~ppx_name
      +> flag Argument_names.list_options_on_error
      +> arg Argument_names.nested_separator (estring __)
      +> flag "portable")
    (fun ~ctxt
      (rec_flag, types)
      case_insensitive
      capitalization
      list_options_on_error
      nested_separator
      portable ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      let code_path = Expansion_context.Deriver.code_path ctxt in
      build_impl
        ~loc
        ~code_path
        ~case_insensitive
        ~capitalization
        ~rec_flag
        ~list_options_on_error
        ~nested_separator
        ~what_to_generate
        ~portable
        types)
;;

class ghostify_locs =
  object
    inherit Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end

let ghostify_locs = new ghostify_locs

let generate_extension ~fn_name ~loc ~path:_ core_type =
  let loc = ghostify_locs#location loc in
  let core_type = ghostify_locs#core_type core_type in
  match Nested_type.of_core_type core_type with
  | Ok type_ -> Fn_name.get_function fn_name ~type_
  | Error error -> A.pexp_extension ~loc error
;;

let add_deriving ~name ~what_to_generate =
  Deriving.add
    name
    ~sig_type_decl:(generate_intf ~what_to_generate)
    ~str_type_decl:(generate_impl ~what_to_generate)
    ?extension:
      (match what_to_generate with
       | Only fn_name -> Some (generate_extension ~fn_name)
       | Both -> None)
  |> Deriving.ignore
;;

let () =
  add_deriving ~name:"string" ~what_to_generate:Both;
  add_deriving ~name:"to_string" ~what_to_generate:(Only To_string);
  add_deriving ~name:"of_string" ~what_to_generate:(Only Of_string)
;;
