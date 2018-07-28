(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let int64_ns_of_ptime t =
  let d, ps = Ptime.Span.to_d_ps (Ptime.to_span t) in
  Int64.(add (mul (of_int d) 86_400L) (div ps 1000L))

let pp_ptime_ns ppf t =
  Format.fprintf ppf "%Ld" (int64_ns_of_ptime t)

module Field = struct
  type value =
    | Float of float
    | String of string
    | Bool of bool
    | Int of int64

  let pp_value ppf = function
    | Float v -> Format.pp_print_float ppf v
    | String v -> Format.pp_print_string ppf v
    | Bool v -> Format.pp_print_bool ppf v
    | Int v -> Format.fprintf ppf "%Ld" v

  let string_of_value = Fmt.to_to_string pp_value

  type t = { k : string ; v : value }

  let pp ppf { k ; v } =
    Format.fprintf ppf "%s=%a" k pp_value v

  let to_string = Fmt.to_to_string pp

  let create ~k ~v = { k ; v }

  let key { k ; _ } = k
  let value { v ; _ } = v
end

module Point = struct
  type t = {
    measurement: string;
    fields: Field.t list;
    tags: (string * string) list;
    timestamp: Ptime.t
  }

  let create ~measurement ~fields ~tags ~timestamp () =
    { measurement; fields ; tags ; timestamp }

  let measurement { measurement ; _ } = measurement
  let fields { fields ; _ } = fields
  let tags { tags ; _ } = tags
  let timestamp { timestamp ; _ } = timestamp

  let pp_list pp = Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ",") pp

  let pp_print_tag ppf (k, v) =
    Format.fprintf ppf "%s=%s" k v

  let pp ppf { measurement ; fields ; tags ; timestamp } =
    Format.fprintf ppf "%s,%a %a %a"
      measurement
      (pp_list pp_print_tag) tags
      (pp_list Field.pp) fields
      pp_ptime_ns timestamp

  let to_string = Fmt.to_to_string pp
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
