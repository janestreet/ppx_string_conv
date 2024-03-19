open! Base
open! Ppxlib
module A = Ast_builder.Default

let ppx_name = "ppx_string_conv"

module Fn_name = struct
  type t =
    | To_string
    | Of_string

  let for_type t ~type_name =
    match t, type_name with
    | To_string, "t" -> "to_string"
    | Of_string, "t" -> "of_string"
    (* to work with things like [int_of_string] and [string_of_int] *)
    | To_string, _ -> [%string "string_of_%{type_name}"]
    | Of_string, _ -> [%string "%{type_name}_of_string"]
  ;;

  let call' t ~(type_ : longident_loc) ~(args : expression list) =
    let loc = type_.loc in
    match type_.txt with
    | Lident "string" ->
      (* Special case for the type 'string' -- there's no 'string_of_string' function,
         so we use the identity function instead *)
      (match args with
       | [ expr ] -> expr
       | _ -> A.eapply ~loc [%expr fun s -> s] args)
    | _ ->
      (* Normal case -- follow naming convention *)
      A.type_constr_conv type_ ~loc ~f:(fun type_name -> for_type t ~type_name) args
  ;;

  let call t arg ~type_ = call' t ~type_ ~args:[ arg ]
  let get_function t ~type_ = call' t ~type_ ~args:[]
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

let type_of_decl (decl : type_declaration) : core_type =
  let loc = decl.ptype_name.loc in
  A.ptyp_constr ~loc (Loc.make ~loc (Lident decl.ptype_name.txt)) []
;;

let error_ext ~loc message : extension =
  ( Loc.make ~loc "ocaml.error"
  , PStr [ A.pstr_eval ~loc (A.estring ~loc [%string "%{ppx_name}: %{message}"]) [] ] )
;;

let psig_error ~loc message = A.psig_extension ~loc (error_ext ~loc message) []
let pstr_error ~loc message = A.pstr_extension ~loc (error_ext ~loc message) []
let lstring (loc : string loc) = A.estring ~loc:loc.loc loc.txt

let make_stringable_sig_for_type ~loc ~what_to_generate (decl : type_declaration)
  : signature
  =
  match List.is_empty decl.ptype_params with
  | false ->
    let loc = decl.ptype_loc in
    [ psig_error ~loc "types with parameters not supported" ]
  | true ->
    let t = type_of_decl decl in
    let make_val fn_name sig_ =
      A.psig_value
        ~loc
        (A.value_description
           ~loc
           ~name:
             (Loc.map decl.ptype_name ~f:(fun type_name ->
                Fn_name.for_type fn_name ~type_name))
           ~prim:[]
           ~type_:(sig_ t))
    in
    let build_fn (fn_name : Fn_name.t) =
      let type_ t =
        match fn_name with
        | To_string -> [%type: [%t t] -> string]
        | Of_string -> [%type: string -> [%t t]]
      in
      make_val fn_name type_
    in
    What_to_generate.build what_to_generate ~f:build_fn
;;

let generate_intf ~what_to_generate =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (_rec_flag, types) ->
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat_map types ~f:(make_stringable_sig_for_type ~loc ~what_to_generate))
;;

module Constructor_kind = struct
  type t =
    | Base_string of { name : string Loc.t option }
    | Fallback
    | Nested of { prefix : string Loc.t option }

  module Attr = struct
    let fallback =
      Attribute.declare_with_attr_loc
        "stringable.fallback"
        Attribute.Context.constructor_declaration
        Ast_pattern.(pstr nil)
        (fun ~attr_loc:loc -> loc)
    ;;

    let nested =
      Attribute.declare_with_attr_loc
        "stringable.nested"
        Attribute.Context.constructor_declaration
        Ast_pattern.(single_expr_payload (estring __'))
        (fun ~attr_loc:loc prefix -> loc, prefix)
    ;;

    let rename =
      Attribute.declare
        "stringable.rename"
        Attribute.Context.constructor_declaration
        Ast_pattern.(single_expr_payload (estring __'))
        (fun s -> s)
    ;;
  end

  let get (constr : constructor_declaration) =
    let fallback = Attribute.get Attr.fallback constr in
    let nested = Attribute.get Attr.nested constr in
    let rename = Attribute.get Attr.rename constr in
    let multiple_attribute_error ~loc =
      Error
        (error_ext ~loc "at most one of 'rename', 'nested' and 'fallback' may be used")
    in
    match fallback, nested, rename with
    | Some _, None, None -> Ok Fallback
    | None, Some (_, prefix), None ->
      let prefix = if String.is_empty prefix.txt then None else Some prefix in
      Ok (Nested { prefix })
    | None, None, name -> Ok (Base_string { name })
    | Some loc, _, _ | _, Some (loc, _), _ -> multiple_attribute_error ~loc
  ;;
end

module Variant_impl = struct
  (* A helper type for validating & storing data about constructors, which gets rendered
     to the implementation in ml files *)

  type t =
    { loc : Location.t
    ; code_path : Code_path.t
    ; case_insensitive : bool
    ; capitalization : Capitalization.t option
    ; decl : type_declaration
    ; base_string_variants : (constructor_declaration * string Loc.t) Queue.t
    ; nested_variants :
        (constructor_declaration * longident_loc * string Loc.t option) Queue.t
    ; mutable fallback_variant : (constructor_declaration * longident_loc) option
    ; errors : extension Queue.t
    }

  let capitalization_of_string deriving_arg =
    match deriving_arg with
    | None -> Ok None
    | Some s ->
      (try Ok (Some (Capitalization.of_string s)) with
       | _ ->
         let can_be = Lazy.force Capitalization.can_be |> String.concat ~sep:", " in
         Error [%string "invalid capitalize argument: (can be: %{can_be})"])
  ;;

  let create' ~loc ~code_path ~case_insensitive ~capitalize_string ~decl =
    let%map.Result capitalization = capitalization_of_string capitalize_string in
    { loc
    ; code_path
    ; case_insensitive
    ; capitalization
    ; decl
    ; base_string_variants = Queue.create ()
    ; nested_variants = Queue.create ()
    ; fallback_variant = None
    ; errors = Queue.create ()
    }
  ;;

  let parse_base_string t ~constr ~name =
    match (constr : constructor_declaration) with
    | { pcd_args = Pcstr_tuple []; pcd_vars = []; pcd_res = None; pcd_name; _ } ->
      Ok
        (Option.value_or_thunk name ~default:(fun () ->
           match t.capitalization with
           | None -> pcd_name
           | Some capitalization ->
             Loc.map pcd_name ~f:(Capitalization.apply_to_snake_case capitalization)))
    | _ -> Error (error_ext ~loc:constr.pcd_loc "expected unit variant without arguments")
  ;;

  let get_nested_type constr =
    match (constr : constructor_declaration) with
    | { pcd_args = Pcstr_tuple [ ty ]; pcd_vars = []; pcd_res = None; _ } ->
      (match ty with
       | { ptyp_desc = Ptyp_constr (nested_type, []); _ } -> Ok nested_type
       | _ -> Error (error_ext ~loc:ty.ptyp_loc "types with parameters not supported"))
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
  ;;

  let create ~loc ~code_path ~case_insensitive ~capitalize_string ~decl ~constructors =
    let%map.Result t =
      create' ~loc ~code_path ~case_insensitive ~capitalize_string ~decl
    in
    List.iter constructors ~f:(fun constructor ->
      match add_variant t constructor with
      | Ok () -> ()
      | Error error -> Queue.enqueue t.errors error);
    t
  ;;

  let build_function t ~fn_name ~types ~arg_name ~match_ ~cases =
    (* with [arg, ret = types]

       builds [let [fn_name] ([arg_name] : [arg]): [ret] = match [match_] with [cases]] *)
    let loc = t.loc in
    let fn_name =
      A.pvar ~loc (Fn_name.for_type fn_name ~type_name:t.decl.ptype_name.txt)
    in
    let base_type = type_of_decl t.decl in
    let arg, ret = types base_type in
    [%stri
      let [%p fn_name] =
        fun ([%p arg_name] : [%t arg]) : [%t ret] -> [%e A.pexp_match ~loc match_ cases]
      ;;]
  ;;

  let render_to_string t =
    (* Builds to_string/string_of_... methods, structured a function that matches its only
       argument and returns a string *)
    let loc = t.loc in
    let build_case ?args constr ~body =
      A.case ~lhs:(A.pconstruct constr args) ~guard:None ~rhs:body
    in
    let build_base_string (constr, text) =
      let string_lit = A.estring ~loc:text.Loc.loc text.txt in
      build_case constr ~body:string_lit
    in
    let build_nested (constr, nested_type, prefix) =
      let body x =
        let unprefixed = Fn_name.call ~type_:nested_type To_string x in
        match prefix with
        | None -> unprefixed
        | Some prefix -> [%expr Base.String.( ^ ) [%e lstring prefix] [%e unprefixed]]
      in
      build_case constr ~args:[%pat? x] ~body:(body [%expr x])
    in
    let build_fallback (constr, nested_type) = build_nested (constr, nested_type, None) in
    let maybe_error_wildcard =
      (* If there are constructors with invalid parameters, we don't bother generate a
         [to_string] for them, making the match non-exhaustiveness

         To avoid generating a confusing error about that that's shown to the user, we
         need to add a dummy wildcard case if there are any errors to make the match
         exhaustive. *)
      match Queue.is_empty t.errors with
      | true -> []
      | false -> [ A.case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false] ]
    in
    build_function
      t
      ~fn_name:To_string
      ~types:(fun t -> t, [%type: string])
      ~arg_name:[%pat? t]
      ~match_:[%expr t]
      ~cases:
        (List.join
           [ Queue.to_list t.base_string_variants |> List.map ~f:build_base_string
           ; Queue.to_list t.nested_variants |> List.map ~f:build_nested
           ; Option.to_list t.fallback_variant |> List.map ~f:build_fallback
           ; maybe_error_wildcard
           ])
  ;;

  let build_raise_for_of_string t ~value =
    let loc = t.loc in
    let fn_name = Fn_name.for_type Of_string ~type_name:t.decl.ptype_name.txt in
    let path = Code_path.fully_qualified_path t.code_path in
    let message = A.estring ~loc [%string "%{path}.%{fn_name}: invalid string"] in
    [%expr
      Base.raise_s (List [ Atom [%e message]; List [ Atom "value"; Atom [%e value] ] ])]
  ;;

  let render_of_string t =
    (* Builds to_string/string_of_... methods, structured a function matching a string and
       raising on errors *)
    let loc = t.loc in
    let build_base_string (constr, text) =
      (* Builds case for [ | "text" -> Constr]

         Note we convert to lowercase if case-insensitive. Later on we also convert the
         input for the case-insensitive ocaml pattern match. *)
      let string_pat =
        A.pstring
          ~loc:text.Loc.loc
          (if t.case_insensitive then String.lowercase text.txt else text.txt)
      in
      A.case ~lhs:string_pat ~guard:None ~rhs:(A.econstruct constr None)
    in
    let call_nested ~constr ~nested_type x =
      (* Calls an of_string function for the type at the [nested_type] longindent_loc *)
      A.econstruct constr (Some (Fn_name.call ~type_:nested_type Of_string x))
    in
    let build_fallback expr =
      (* Handler for [| _ -> ...] *)
      match t.fallback_variant with
      | Some (constr, type_) ->
        (* When there's a [| Constr of type_ [@fallback]] defined: *)
        call_nested expr ~constr ~nested_type:type_
      | None ->
        (* ...otherwise we [raise_s] *)
        build_raise_for_of_string t ~value:expr
    in
    let build_nested_unprefixed ~constr ~nested_type ~expr ~or_else =
      (* Handler for [| Constr of nested_type [@nested]] *)
      [%expr
        try [%e call_nested ~constr ~nested_type expr] with
        | _ -> [%e or_else]]
    in
    let build_nested_prefixed ~constr ~nested_type ~expr:s ~or_else ~prefix =
      (* Handler for [| Constr of nested_type [@nested "prefix"]] *)
      match t.case_insensitive with
      | false ->
        [%expr
          match Base.String.chop_prefix [%e s] ~prefix:[%e lstring prefix] with
          | Some x -> [%e call_nested ~constr ~nested_type [%expr x]]
          | None -> [%e or_else]]
      | true ->
        (* There's no [Base.String.Caseless.chop_prefix] but we can make one ourselves
           with [is_prefix] and [drop_prefix]. *)
        let prefix_length = A.eint ~loc:prefix.loc (String.length prefix.txt) in
        [%expr
          match Base.String.Caseless.is_prefix [%e s] ~prefix:[%e lstring prefix] with
          | true ->
            [%e
              call_nested
                ~constr
                ~nested_type
                [%expr String.drop_prefix [%e s] [%e prefix_length]]]
          | false -> [%e or_else]]
    in
    let build_nested ~constr ~nested_type ~expr ~or_else ~prefix =
      (* General handler for [@nested] *)
      match prefix with
      | None -> build_nested_unprefixed ~constr ~nested_type ~expr ~or_else
      | Some prefix -> build_nested_prefixed ~constr ~nested_type ~expr ~or_else ~prefix
    in
    let fallback_case_body expr =
      (* The body of the fallback case -- it's the final fallback at the end, followed by
         each of the nesteds with the earlier variants taking priority.

         That's done by a [List.fold_right] with each nested case falling back to cases
         after it in the list, with the final one falling back to the fallback case or
         raise. *)
      List.fold_right
        (Queue.to_list t.nested_variants)
        ~f:(fun (constr, nested_type, prefix) or_else ->
          build_nested ~constr ~nested_type ~or_else ~expr ~prefix)
        ~init:(build_fallback expr)
    in
    build_function
      (* Main body. Most interesting thing is we instead match the result of the
         [String.Lowercase] call if case-insensitive passed to line up with the
         pre-lowercased base string matches *)
      t
      ~fn_name:Of_string
      ~types:(fun t -> [%type: string], t)
      ~arg_name:[%pat? s]
      ~match_:
        (match t.case_insensitive with
         | false -> [%expr s]
         | true -> [%expr Base.String.lowercase s])
      ~cases:
        ((Queue.to_list t.base_string_variants |> List.map ~f:build_base_string)
         @ [ A.case ~lhs:[%pat? _] ~guard:None ~rhs:(fallback_case_body [%expr s]) ])
  ;;

  let render_errors t =
    (* Renders errors, specifically of bad settings on constructors. Note despite their
       location after the generated functions in the AST, the locations on the errors are
       actually tied to the variant definitions that triggered them, so the red line goes
       in the right place in editors *)
    let loc = t.loc in
    Queue.to_list t.errors |> List.map ~f:(fun error -> A.pstr_extension ~loc error [])
  ;;

  let render t ~what_to_generate =
    What_to_generate.build what_to_generate ~f:(function
      | To_string -> render_to_string t
      | Of_string -> render_of_string t)
    @ render_errors t
  ;;
end

let build_alias_impl ~(type_name : string loc) ~(to_ : core_type) ~loc ~what_to_generate =
  (* Builds implementations for [type t = Other_type.t] types *)
  match to_ with
  | { ptyp_desc = Ptyp_constr (other_type, []); _ } ->
    let build_fn fn_name =
      let pat_fn_name = A.pvar ~loc (Fn_name.for_type fn_name ~type_name:type_name.txt) in
      let other_fn = Fn_name.get_function fn_name ~type_:other_type in
      [%stri let [%p pat_fn_name] = [%e other_fn]]
    in
    What_to_generate.build what_to_generate ~f:build_fn
  | _ -> [ pstr_error ~loc:to_.ptyp_loc "types with parameters not supported" ]
;;

let build_impl_or_error
  ~loc
  ~code_path
  ~case_insensitive
  ~capitalize_string
  ~what_to_generate
  (decl : type_declaration)
  =
  match List.is_empty decl.ptype_params with
  | false -> Error "types with parameters not supported"
  | true ->
    (match decl.ptype_kind, decl.ptype_manifest with
     | Ptype_record _, _ -> Error "records not supported"
     | Ptype_variant constructors, _ ->
       let%map.Result impl =
         Variant_impl.create
           ~loc
           ~code_path
           ~case_insensitive
           ~capitalize_string
           ~decl
           ~constructors
       in
       Variant_impl.render impl ~what_to_generate
     | Ptype_abstract, Some type_ ->
       (* [type t = Some_other.t] *)
       Ok (build_alias_impl ~loc ~type_name:decl.ptype_name ~to_:type_ ~what_to_generate)
     | Ptype_abstract, None -> Error "abstract types not supported"
     | Ptype_open, _ -> (* [type t = ..] *) Error "open types not supported")
;;

let build_impl ~loc ~code_path ~case_insensitive ~capitalize_string ~what_to_generate decl
  =
  match
    build_impl_or_error
      ~loc
      ~code_path
      ~case_insensitive
      ~capitalize_string
      ~what_to_generate
      decl
  with
  | Error message -> [ pstr_error ~loc message ]
  | Ok x -> x
;;

let generate_impl ~what_to_generate =
  Deriving.Generator.V2.make
    Deriving.Args.(empty +> flag "case_insensitive" +> arg "capitalize" (estring __))
    (fun ~ctxt (_rec_flag, types) case_insensitive capitalize_string ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      let code_path = Expansion_context.Deriver.code_path ctxt in
      List.concat_map
        types
        ~f:
          (build_impl
             ~loc
             ~code_path
             ~case_insensitive
             ~capitalize_string
             ~what_to_generate))
;;

let add_deriving ~name ~what_to_generate =
  Deriving.add
    name
    ~sig_type_decl:(generate_intf ~what_to_generate)
    ~str_type_decl:(generate_impl ~what_to_generate)
  |> Deriving.ignore
;;

let () =
  add_deriving ~name:"string" ~what_to_generate:Both;
  add_deriving ~name:"to_string" ~what_to_generate:(Only To_string);
  add_deriving ~name:"of_string" ~what_to_generate:(Only Of_string)
;;
