(*---------------------------------------------------------------------------
   Copyright (c) 2018 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Field : sig
  type value =
    | Float of float
    | String of string
    | Bool of bool
    | Int of int64

  val pp_value : Format.formatter -> value -> unit
  val string_of_value : value -> string

  type t = {
    k: string;
    v: value;
  }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val create : k:string -> v:value -> t

  val key : t -> string
  val value : t -> value
end

module Point : sig
  type t = {
    measurement: string;
    fields: Field.t list;
    tags: (string * string) list;
    timestamp: Ptime.t option
  }

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string

  val create :
    ?timestamp:Ptime.t ->
    ?fields:Field.t list ->
    ?tags:(string * string) list ->
    string -> t

  val fields : t -> Field.t list
  val tags : t -> (string * string) list
  val measurement : t -> string
  val timestamp : t -> Ptime.t option
end

module Query : sig
  type 'a t = {
    name : string ;
    columns : string list ;
    values : 'a list ;
  }

  val encoding :
    'a Json_encoding.encoding -> 'a t Json_encoding.encoding
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
