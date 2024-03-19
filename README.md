ppx_string_conv
===============

<!--
```ocaml
open Core
```
-->

`ppx_string_conv` is a ppx to help derive `of_string` and `to_string`, primarily for
variant types.

# Non-variant uses

You can use `[@@deriving string]` in mlis & signatures as well as aliases (`type t =
...`) in mls.

```ocaml
module My_id : sig
  type t [@@deriving string]
end = struct
  type t = int [@@deriving string]
end
```

# Choosing what to generate

Using `[@@deriving string]` generates both `of_string` and `to_string`.

If you only want to generate one of the functions, you can use `[@@deriving to_string]` or
`[@@deriving of_string]` instead.

# Variants

The primary use case of the ppx is to generate the stringable functions based on the
variant names.

```ocaml
module Fruit = struct
  type t =
    | Apple
    | Pear
    | Orange
  [@@deriving sexp_of, string]
end
```

```ocaml
# let () = print_s [%sexp (Fruit.of_string "Apple" : Fruit.t)]
Apple
# let () = print_endline (Fruit.to_string Pear)
Pear
# let (_ : Fruit.t) = Fruit.of_string "some-bad-value"
Exception:
("Toplevel.Fruit.of_string: invalid string" (value some-bad-value))
```

## Renaming with `~capitalize:"..."` and `[@rename "..."]`

You can rename all variants systemically by passing the `~capitalize:"..."` deriving argument.

Additionally, you can override the name for any variant using the `[@rename "name"]`
attribute (in which case the `~capitalize` rule won't be applied):

```ocaml
module Chassis_id_subtype = struct
  type t =
    | Chassis_component
    | Interface_alias
    | Port_component
    | Mac_address
    | Network_address
    | Interface_name
    | Local
    | Custom [@rename "something-custom"]
  [@@deriving string ~capitalize:"kebab-case", enumerate]
end
```

```ocaml
# let () = List.map Chassis_id_subtype.all ~f:Chassis_id_subtype.to_string |> List.iter ~f:print_endline
chassis-component
interface-alias
port-component
mac-address
network-address
interface-name
local
something-custom
```

For all the options you can use with `~capitalize`, see this error message:

```ocaml
type t = Show_me_the_options [@@deriving string ~capitalize:"invalid"]
```
```mdx-error
Line 1, characters 1-71:
Error: ppx_string_conv: invalid capitalize argument: (can be: PascalCase,
       camelCase, snake_case, Capitalized_snake_case, Pascal_Snake_Case,
       SCREAMING_SNAKE_CASE, aLtErNaTiNg_sNaKe_cAsE, kebab-case,
       Capitalized-kebab-case, Pascal-Kebab-Case, SCREAMING-KEBAB-CASE,
       aLtErNaTiNg-kEbAb-cAsE, Sentence case, Title Case, lower sentence
       case, UPPER SENTENCE CASE, aLtErNaTiNg sEnTeNcE CaSe)
```

## Fallback cases with `[@fallback]`

You can use `[@fallback]` to define a fallback case for the type used instead of
raising. This allows you to define `to_string` and `of_string` pairs that will never fail
and keep all information:

```ocaml
module Operational_state = struct
  type t =
    | Uninstalled
    | Normal
    | Faulted
    | Unable_to_parse of string [@fallback]
  [@@deriving sexp_of, string ~capitalize:"snake_case"]
end
```
For example,

```ocaml
# let () = print_endline (Operational_state.to_string (Unable_to_parse "hello"))
hello
# let () =
    let state = Operational_state.of_string "something-invalid" in
    print_s [%sexp (state : Operational_state.t)]
(Unable_to_parse something-invalid)
```

### Custom types

If a non-`string` type is used, it will delegate to that type's `of_string` and
`to_string`. If those functions raise, the exception will be propagated.

```ocaml
module Unrecognized = struct
  type t = string [@@deriving string]
end

type t =
  | Uninstalled
  | Normal
  | Faulted
  | Unable_to_parse of Unrecognized.t [@fallback]
[@@deriving string]
```

## Case-insensitivity with `~case_insensitive`

You can make the generated `of_string` functions case-insensitive by using the
`~case_insensitive` deriving argument:

```ocaml
module Case_insensitive_demo = struct
  type t =
    | Apple
    | Pear
    | Orange
  [@@deriving string ~case_insensitive]

  let show s = s |> of_string |> to_string |> print_endline
end
```

```ocaml
# let () = Case_insensitive_demo.show "apple"
Apple
# let () = Case_insensitive_demo.show "oRAngE"
Orange
```

## Nesting with `[@nested "prefix"]`

You can nest types with a prefix to disambiguate

```ocaml
module Nested = struct
  type fruit =
    | Apple
    | Pear
    | Orange
  [@@deriving string ~capitalize:"snake_case", sexp_of]

  type animal =
    | Cat
    | Dog
  [@@deriving string ~capitalize:"snake_case", sexp_of]

  type t =
    | Fruit of fruit [@nested "fruit."]
    | Animal of animal [@nested "animal."]
  [@@deriving string, sexp_of]
end
```

```ocaml
# let () = print_endline (Nested.to_string (Fruit Pear))
fruit.pear
# let () = let t = Nested.of_string "animal.cat" in print_s [%sexp (t : Nested.t)]
(Animal Cat)
```

Any errors from the nested type's `of_string` will be exposed if the prefix matches:

```ocaml
# let (_ : Nested.t) =  Nested.of_string "animal.bad-animal"
Exception:
("Toplevel.Nested.animal_of_string: invalid string" (value bad-animal))
```

Note that `~case_insensitive` works with prefixes too, but the part after the prefix will
only be case-insensitive if the `of_string` implementation of the nested type is too

### Empty prefix support (warning: potentially error-prone)

If you use `[@nested ""]` the behavior changes slightly; if no other case matches it will
attempt to call the nested type's `of_string`, only using the fallback or raising if the
nested call raises.

This is intended to allow you to extend one variant with other extra cases:

```ocaml
module Nested_no_prefix = struct
  type basic_state = On | Off [@@deriving string, sexp_of]
  type t =
    | Basic_state of basic_state [@nested ""]
    | Faulted
  [@@deriving string, sexp_of]
end
```

```ocaml
# let () = print_endline (Nested_no_prefix.to_string (Basic_state Off))
Off
# let () = let t = Nested_no_prefix.of_string "Faulted" in print_s [%sexp (t : Nested_no_prefix.t)]
Faulted
# let () = let t = Nested_no_prefix.of_string "On" in print_s [%sexp (t : Nested_no_prefix.t)]
(Basic_state On)
```

This is potentially error-prone if you have the same name for multiple types, or mix up
case-sensitive and case-insensitive. The rules for which representation 'wins' will be
stable but it's still worth being careful to avoid a situation where this can happen.
