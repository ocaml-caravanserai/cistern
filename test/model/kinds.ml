(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of kinds, written by hand: each scalar, nullable and not, and a key
   with no [@auto]. The source:

   {[ type kinds = { id : int64; [@key] i : int; i_opt : int option; s : string;
   s_opt : string option; b : bool; b_opt : bool option; f : float; f_opt :
   float option; y : bytes; y_opt : bytes option; t : Ptime.t; t_opt : Ptime.t
   option; } ]}

   A Ptime.t is cut to 1 µs (not rounded) in of_draft (the one funnel of create)
   and in each set, as the literal and the server cut it. Thus the record that
   create and update give is the value that the server keeps (D5a of the plan of
   phase 2). *)

type kinds =
  { id: int64
  ; i: int
  ; i_opt: int option
  ; s: string
  ; s_opt: string option
  ; b: bool
  ; b_opt: bool option
  ; f: float
  ; f_opt: float option
  ; y: bytes
  ; y_opt: bytes option
  ; t: Ptime.t
  ; t_opt: Ptime.t option }

module Cistern_fields__kinds = struct
  module S = Cistern.Private.Schema
  module F = Cistern.Private.Field
  module D = Cistern.Private.Decoder
  module V = Cistern.Value

  let field ?(nullable = false) ?(key = false) name scalar : S.field =
    { name
    ; column= name
    ; scalar
    ; nullable
    ; key
    ; auto= false
    ; unique= false
    ; index= false }

  let opt f = function None -> V.Null | Some x -> f x

  let cut t = Ptime.truncate ~frac_s:6 t

  let int64 i = V.Int i

  let int i = V.Int (Int64.of_int i)

  let text s = V.Text s

  let bool b = V.Int (if b then 1L else 0L)

  let float f = V.Float f

  let bytes y = V.Bytes (Bytes.to_string y)

  let ptime t = V.Datetime t

  module Kinds = struct
    let s_id = field "id" Int64 ~key:true

    let s_i = field "i" Int

    let s_i_opt = field "i_opt" Int ~nullable:true

    let s_s = field "s" String

    let s_s_opt = field "s_opt" String ~nullable:true

    let s_b = field "b" Bool

    let s_b_opt = field "b_opt" Bool ~nullable:true

    let s_f = field "f" Float

    let s_f_opt = field "f_opt" Float ~nullable:true

    let s_y = field "y" Bytes

    let s_y_opt = field "y_opt" Bytes ~nullable:true

    let s_t = field "t" Ptime

    let s_t_opt = field "t_opt" Ptime ~nullable:true

    let schema : S.model =
      { name= "kinds"
      ; table= "kinds"
      ; fields=
          [ s_id
          ; s_i
          ; s_i_opt
          ; s_s
          ; s_s_opt
          ; s_b
          ; s_b_opt
          ; s_f
          ; s_f_opt
          ; s_y
          ; s_y_opt
          ; s_t
          ; s_t_opt ]
      ; relations= [] }

    let make field ~encode ~decode ~get ~set =
      F.make ~model:schema ~field ~encode ~decode ~get ~set

    let id =
      make s_id ~encode:int64 ~decode:D.int64
        ~get:(fun r -> r.id)
        ~set:(fun r v -> {r with id= v})

    let i =
      make s_i ~encode:int ~decode:D.int
        ~get:(fun r -> r.i)
        ~set:(fun r v -> {r with i= v})

    let i_opt =
      make s_i_opt ~encode:(opt int) ~decode:(D.option D.int)
        ~get:(fun r -> r.i_opt)
        ~set:(fun r v -> {r with i_opt= v})

    let s =
      make s_s ~encode:text ~decode:D.string
        ~get:(fun r -> r.s)
        ~set:(fun r v -> {r with s= v})

    let s_opt =
      make s_s_opt ~encode:(opt text) ~decode:(D.option D.string)
        ~get:(fun r -> r.s_opt)
        ~set:(fun r v -> {r with s_opt= v})

    let b =
      make s_b ~encode:bool ~decode:D.bool
        ~get:(fun r -> r.b)
        ~set:(fun r v -> {r with b= v})

    let b_opt =
      make s_b_opt ~encode:(opt bool) ~decode:(D.option D.bool)
        ~get:(fun r -> r.b_opt)
        ~set:(fun r v -> {r with b_opt= v})

    let f =
      make s_f ~encode:float ~decode:D.float
        ~get:(fun r -> r.f)
        ~set:(fun r v -> {r with f= v})

    let f_opt =
      make s_f_opt ~encode:(opt float) ~decode:(D.option D.float)
        ~get:(fun r -> r.f_opt)
        ~set:(fun r v -> {r with f_opt= v})

    let y =
      make s_y ~encode:bytes ~decode:D.bytes
        ~get:(fun r -> r.y)
        ~set:(fun r v -> {r with y= v})

    let y_opt =
      make s_y_opt ~encode:(opt bytes) ~decode:(D.option D.bytes)
        ~get:(fun r -> r.y_opt)
        ~set:(fun r v -> {r with y_opt= v})

    let t =
      make s_t ~encode:ptime ~decode:D.ptime
        ~get:(fun r -> r.t)
        ~set:(fun r v -> {r with t= cut v})

    let t_opt =
      make s_t_opt ~encode:(opt ptime) ~decode:(D.option D.ptime)
        ~get:(fun r -> r.t_opt)
        ~set:(fun r v -> {r with t_opt= Option.map cut v})
  end
end

module Cistern_drafts__kinds = struct
  module Fl = Cistern_fields__kinds

  type kinds =
    { id: int64
    ; i: int
    ; i_opt: int option
    ; s: string
    ; s_opt: string option
    ; b: bool
    ; b_opt: bool option
    ; f: float
    ; f_opt: float option
    ; y: bytes
    ; y_opt: bytes option
    ; t: Ptime.t
    ; t_opt: Ptime.t option }

  (* The column and the encoder of each field come from the field. *)
  let encode_kinds (d : kinds) =
    let module K = Fl.Kinds in
    let e f v = (Fl.F.column f, Fl.F.encode f v) in
    [ e K.id d.id
    ; e K.i d.i
    ; e K.i_opt d.i_opt
    ; e K.s d.s
    ; e K.s_opt d.s_opt
    ; e K.b d.b
    ; e K.b_opt d.b_opt
    ; e K.f d.f
    ; e K.f_opt d.f_opt
    ; e K.y d.y
    ; e K.y_opt d.y_opt
    ; e K.t d.t
    ; e K.t_opt d.t_opt ]
end

module Cistern_model__kinds :
  Cistern.MODEL
    with type t = kinds
     and type key = int64
     and type draft = Cistern_drafts__kinds.kinds = struct
  module Fl = Cistern_fields__kinds.Kinds
  module D = Cistern.Private.Decoder
  module F = Cistern.Private.Field

  type t = kinds

  type key = int64

  type draft = Cistern_drafts__kinds.kinds

  let schema = Fl.schema

  let key = Fl.id

  let fields =
    F.
      [ Any Fl.id
      ; Any Fl.i
      ; Any Fl.i_opt
      ; Any Fl.s
      ; Any Fl.s_opt
      ; Any Fl.b
      ; Any Fl.b_opt
      ; Any Fl.f
      ; Any Fl.f_opt
      ; Any Fl.y
      ; Any Fl.y_opt
      ; Any Fl.t
      ; Any Fl.t_opt ]

  let decode c : t =
    let id = D.next c (F.decode Fl.id) in
    let i = D.next c (F.decode Fl.i) in
    let i_opt = D.next c (F.decode Fl.i_opt) in
    let s = D.next c (F.decode Fl.s) in
    let s_opt = D.next c (F.decode Fl.s_opt) in
    let b = D.next c (F.decode Fl.b) in
    let b_opt = D.next c (F.decode Fl.b_opt) in
    let f = D.next c (F.decode Fl.f) in
    let f_opt = D.next c (F.decode Fl.f_opt) in
    let y = D.next c (F.decode Fl.y) in
    let y_opt = D.next c (F.decode Fl.y_opt) in
    let t = D.next c (F.decode Fl.t) in
    let t_opt = D.next c (F.decode Fl.t_opt) in
    {id; i; i_opt; s; s_opt; b; b_opt; f; f_opt; y; y_opt; t; t_opt}

  let encode_draft = Cistern_drafts__kinds.encode_kinds

  (* The key has no [@auto]: it is in the draft. The cut of a Ptime.t is here,
     because each create passes through of_draft. *)
  let of_draft (d : draft) _ : t =
    let cut = Cistern_fields__kinds.cut in
    { id= d.id
    ; i= d.i
    ; i_opt= d.i_opt
    ; s= d.s
    ; s_opt= d.s_opt
    ; b= d.b
    ; b_opt= d.b_opt
    ; f= d.f
    ; f_opt= d.f_opt
    ; y= d.y
    ; y_opt= d.y_opt
    ; t= cut d.t
    ; t_opt= Option.map cut d.t_opt }
end

module Cistern_gen__kinds = struct
  module M = Cistern.Private.Model

  module Kinds = struct
    type t = kinds

    module Q = struct
      module Fl = Cistern_fields__kinds.Kinds

      let id = Fl.id

      let i = Fl.i

      let i_opt = Fl.i_opt

      let s = Fl.s

      let s_opt = Fl.s_opt

      let b = Fl.b

      let b_opt = Fl.b_opt

      let f = Fl.f

      let f_opt = Fl.f_opt

      let y = Fl.y

      let y_opt = Fl.y_opt

      let t = Fl.t

      let t_opt = Fl.t_opt

      include Cistern.Private.Expr.Ops
    end

    let get_by_id k = M.get (module Cistern_model__kinds) k

    let find_by_id k = M.find (module Cistern_model__kinds) k

    let update us r = M.update (module Cistern_model__kinds) us r

    let delete r = M.delete (module Cistern_model__kinds) r

    let draft ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f ?f_opt ~y ?y_opt ~t ?t_opt ()
        : Cistern_drafts__kinds.kinds =
      {id; i; i_opt; s; s_opt; b; b_opt; f; f_opt; y; y_opt; t; t_opt}
  end
end

module Kinds = struct
  include Cistern_gen__kinds.Kinds
  module Drafts = Cistern_drafts__kinds

  type draft = Drafts.kinds

  let model : (module Cistern.MODEL) = (module Cistern_model__kinds)

  let create d = Cistern.Private.Model.create (module Cistern_model__kinds) d
end
