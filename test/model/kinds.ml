(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of kinds, written by hand: each scalar, nullable and not, and a key
   with no [@auto]. The attributes are on the type declaration, as in a source
   of [@@deriving cistern]. The items after the declaration have the form of the
   ppx (the plan of phase 3, D7 to D10): the suite test/ppx checks that the
   Pprintast text of the ppx output for this type declaration is the Pprintast
   text of these items. Thus these items have no doc comment. They came from the
   generator, so the D2 test of test/ppx and the run-time rows give their
   meaning.

   A Ptime.t is cut to 1 µs (not rounded) in of_draft (the one funnel of create)
   and in each set, as the literal and the server cut it. Thus the record that
   create and update give is the value that the server keeps (D5a of the plan of
   phase 2). *)

type kinds =
  { id: int64 [@key]
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

module Cistern__private__kinds = Cistern.Private
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_fields__kinds = struct
  module Kinds = struct
    module Schema = struct
      let id =
        { Cistern__private__kinds.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__kinds.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= false
        ; unique= false
        ; index= false }

      let i =
        { Cistern__private__kinds.Schema.name= "i"
        ; column= "i"
        ; scalar= Cistern__private__kinds.Schema.Int
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let i_opt =
        { Cistern__private__kinds.Schema.name= "i_opt"
        ; column= "i_opt"
        ; scalar= Cistern__private__kinds.Schema.Int
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let s =
        { Cistern__private__kinds.Schema.name= "s"
        ; column= "s"
        ; scalar= Cistern__private__kinds.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let s_opt =
        { Cistern__private__kinds.Schema.name= "s_opt"
        ; column= "s_opt"
        ; scalar= Cistern__private__kinds.Schema.String
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let b =
        { Cistern__private__kinds.Schema.name= "b"
        ; column= "b"
        ; scalar= Cistern__private__kinds.Schema.Bool
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let b_opt =
        { Cistern__private__kinds.Schema.name= "b_opt"
        ; column= "b_opt"
        ; scalar= Cistern__private__kinds.Schema.Bool
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let f =
        { Cistern__private__kinds.Schema.name= "f"
        ; column= "f"
        ; scalar= Cistern__private__kinds.Schema.Float
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let f_opt =
        { Cistern__private__kinds.Schema.name= "f_opt"
        ; column= "f_opt"
        ; scalar= Cistern__private__kinds.Schema.Float
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let y =
        { Cistern__private__kinds.Schema.name= "y"
        ; column= "y"
        ; scalar= Cistern__private__kinds.Schema.Bytes
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let y_opt =
        { Cistern__private__kinds.Schema.name= "y_opt"
        ; column= "y_opt"
        ; scalar= Cistern__private__kinds.Schema.Bytes
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let t =
        { Cistern__private__kinds.Schema.name= "t"
        ; column= "t"
        ; scalar= Cistern__private__kinds.Schema.Ptime
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let t_opt =
        { Cistern__private__kinds.Schema.name= "t_opt"
        ; column= "t_opt"
        ; scalar= Cistern__private__kinds.Schema.Ptime
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__kinds.Schema.name= "kinds"
        ; table= "kinds"
        ; fields=
            [id; i; i_opt; s; s_opt; b; b_opt; f; f_opt; y; y_opt; t; t_opt]
        ; relations= [] }
    end

    let id =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__kinds.Encode.int64
        ~decode:Cistern__private__kinds.Decoder.int64
        ~get:(fun (cistern__r : kinds) -> cistern__r.id)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with id= cistern__v} )

    let i =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.i ~encode:Cistern__private__kinds.Encode.int
        ~decode:Cistern__private__kinds.Decoder.int
        ~get:(fun (cistern__r : kinds) -> cistern__r.i)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with i= cistern__v} )

    let i_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.i_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt Cistern__private__kinds.Encode.int)
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.int )
        ~get:(fun (cistern__r : kinds) -> cistern__r.i_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with i_opt= cistern__v} )

    let s =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.s ~encode:Cistern__private__kinds.Encode.text
        ~decode:Cistern__private__kinds.Decoder.string
        ~get:(fun (cistern__r : kinds) -> cistern__r.s)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with s= cistern__v} )

    let s_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.s_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt
             Cistern__private__kinds.Encode.text )
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.string )
        ~get:(fun (cistern__r : kinds) -> cistern__r.s_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with s_opt= cistern__v} )

    let b =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.b ~encode:Cistern__private__kinds.Encode.bool
        ~decode:Cistern__private__kinds.Decoder.bool
        ~get:(fun (cistern__r : kinds) -> cistern__r.b)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with b= cistern__v} )

    let b_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.b_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt
             Cistern__private__kinds.Encode.bool )
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.bool )
        ~get:(fun (cistern__r : kinds) -> cistern__r.b_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with b_opt= cistern__v} )

    let f =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.f ~encode:Cistern__private__kinds.Encode.float
        ~decode:Cistern__private__kinds.Decoder.float
        ~get:(fun (cistern__r : kinds) -> cistern__r.f)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with f= cistern__v} )

    let f_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.f_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt
             Cistern__private__kinds.Encode.float )
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.float )
        ~get:(fun (cistern__r : kinds) -> cistern__r.f_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with f_opt= cistern__v} )

    let y =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.y ~encode:Cistern__private__kinds.Encode.bytes
        ~decode:Cistern__private__kinds.Decoder.bytes
        ~get:(fun (cistern__r : kinds) -> cistern__r.y)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with y= cistern__v} )

    let y_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.y_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt
             Cistern__private__kinds.Encode.bytes )
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.bytes )
        ~get:(fun (cistern__r : kinds) -> cistern__r.y_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with y_opt= cistern__v} )

    let t =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.t ~encode:Cistern__private__kinds.Encode.ptime
        ~decode:Cistern__private__kinds.Decoder.ptime
        ~get:(fun (cistern__r : kinds) -> cistern__r.t)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          {cistern__r with t= Cistern__private__kinds.Encode.cut cistern__v} )

    let t_opt =
      Cistern__private__kinds.Field.make ~model:Schema.cistern__model
        ~field:Schema.t_opt
        ~encode:
          (Cistern__private__kinds.Encode.opt
             Cistern__private__kinds.Encode.ptime )
        ~decode:
          (Cistern__private__kinds.Decoder.option
             Cistern__private__kinds.Decoder.ptime )
        ~get:(fun (cistern__r : kinds) -> cistern__r.t_opt)
        ~set:(fun (cistern__r : kinds) cistern__v ->
          { cistern__r with
            t_opt= Cistern__private__kinds.Encode.cut_opt cistern__v } )
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_drafts__kinds = struct
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

  let encode_kinds (cistern__d : kinds) =
    [ ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.id
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.id
          cistern__d.id )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.i
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.i
          cistern__d.i )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.i_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.i_opt
          cistern__d.i_opt )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.s
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.s
          cistern__d.s )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.s_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.s_opt
          cistern__d.s_opt )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.b
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.b
          cistern__d.b )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.b_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.b_opt
          cistern__d.b_opt )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.f
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.f
          cistern__d.f )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.f_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.f_opt
          cistern__d.f_opt )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.y
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.y
          cistern__d.y )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.y_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.y_opt
          cistern__d.y_opt )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.t
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.t
          cistern__d.t )
    ; ( Cistern__private__kinds.Field.column Cistern_fields__kinds.Kinds.t_opt
      , Cistern__private__kinds.Field.encode Cistern_fields__kinds.Kinds.t_opt
          cistern__d.t_opt ) ]
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__kinds :
  Cistern.MODEL
    with type t = kinds
     and type key = int64
     and type draft = Cistern_drafts__kinds.kinds = struct
  type nonrec t = kinds

  type key = int64

  type draft = Cistern_drafts__kinds.kinds

  let schema = Cistern_fields__kinds.Kinds.Schema.cistern__model

  let key = Cistern_fields__kinds.Kinds.id

  let fields =
    [ Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.id
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.i
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.i_opt
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.s
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.s_opt
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.b
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.b_opt
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.f
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.f_opt
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.y
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.y_opt
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.t
    ; Cistern__private__kinds.Field.Any Cistern_fields__kinds.Kinds.t_opt ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.id)
    in
    let cistern__1 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.i)
    in
    let cistern__2 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.i_opt)
    in
    let cistern__3 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.s)
    in
    let cistern__4 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.s_opt)
    in
    let cistern__5 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.b)
    in
    let cistern__6 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.b_opt)
    in
    let cistern__7 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.f)
    in
    let cistern__8 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.f_opt)
    in
    let cistern__9 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.y)
    in
    let cistern__10 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.y_opt)
    in
    let cistern__11 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.t)
    in
    let cistern__12 =
      Cistern__private__kinds.Decoder.next cistern__c
        (Cistern__private__kinds.Field.decode Cistern_fields__kinds.Kinds.t_opt)
    in
    { id= cistern__0
    ; i= cistern__1
    ; i_opt= cistern__2
    ; s= cistern__3
    ; s_opt= cistern__4
    ; b= cistern__5
    ; b_opt= cistern__6
    ; f= cistern__7
    ; f_opt= cistern__8
    ; y= cistern__9
    ; y_opt= cistern__10
    ; t= cistern__11
    ; t_opt= cistern__12 }

  let encode_draft = Cistern_drafts__kinds.encode_kinds

  let of_draft (cistern__d : draft) cistern__k : t =
    { id= cistern__d.id
    ; i= cistern__d.i
    ; i_opt= cistern__d.i_opt
    ; s= cistern__d.s
    ; s_opt= cistern__d.s_opt
    ; b= cistern__d.b
    ; b_opt= cistern__d.b_opt
    ; f= cistern__d.f
    ; f_opt= cistern__d.f_opt
    ; y= cistern__d.y
    ; y_opt= cistern__d.y_opt
    ; t= Cistern__private__kinds.Encode.cut cistern__d.t
    ; t_opt= Cistern__private__kinds.Encode.cut_opt cistern__d.t_opt }
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_gen__kinds = struct
  module Kinds = struct
    type nonrec t = kinds

    module Q = struct
      let id = Cistern_fields__kinds.Kinds.id

      let i = Cistern_fields__kinds.Kinds.i

      let i_opt = Cistern_fields__kinds.Kinds.i_opt

      let s = Cistern_fields__kinds.Kinds.s

      let s_opt = Cistern_fields__kinds.Kinds.s_opt

      let b = Cistern_fields__kinds.Kinds.b

      let b_opt = Cistern_fields__kinds.Kinds.b_opt

      let f = Cistern_fields__kinds.Kinds.f

      let f_opt = Cistern_fields__kinds.Kinds.f_opt

      let y = Cistern_fields__kinds.Kinds.y

      let y_opt = Cistern_fields__kinds.Kinds.y_opt

      let t = Cistern_fields__kinds.Kinds.t

      let t_opt = Cistern_fields__kinds.Kinds.t_opt

      include Cistern__private__kinds.Expr.Ops
    end

    let get_by_id cistern__k =
      Cistern__private__kinds.Model.get (module Cistern_model__kinds) cistern__k

    let find_by_id cistern__k =
      Cistern__private__kinds.Model.find
        (module Cistern_model__kinds)
        cistern__k

    let all = Cistern__private__kinds.Model.all (module Cistern_model__kinds)

    let where cistern__e =
      Cistern__private__kinds.Model.where
        (module Cistern_model__kinds)
        cistern__e

    let update ?i ?i_opt ?s ?s_opt ?b ?b_opt ?f ?f_opt ?y ?y_opt ?t ?t_opt
        cistern__r =
      Cistern__private__kinds.Model.update
        (module Cistern_model__kinds)
        (Cistern__private__kinds.Encode.updates
           [ Cistern__private__kinds.Encode.set Q.i i
           ; Cistern__private__kinds.Encode.set Q.i_opt i_opt
           ; Cistern__private__kinds.Encode.set Q.s s
           ; Cistern__private__kinds.Encode.set Q.s_opt s_opt
           ; Cistern__private__kinds.Encode.set Q.b b
           ; Cistern__private__kinds.Encode.set Q.b_opt b_opt
           ; Cistern__private__kinds.Encode.set Q.f f
           ; Cistern__private__kinds.Encode.set Q.f_opt f_opt
           ; Cistern__private__kinds.Encode.set Q.y y
           ; Cistern__private__kinds.Encode.set Q.y_opt y_opt
           ; Cistern__private__kinds.Encode.set Q.t t
           ; Cistern__private__kinds.Encode.set Q.t_opt t_opt ] )
        cistern__r

    let delete cistern__r =
      Cistern__private__kinds.Model.delete
        (module Cistern_model__kinds)
        cistern__r

    let draft ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f ?f_opt ~y ?y_opt ~t ?t_opt ()
        : Cistern_drafts__kinds.kinds =
      {id; i; i_opt; s; s_opt; b; b_opt; f; f_opt; y; y_opt; t; t_opt}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Kinds = struct
  include Cistern_gen__kinds.Kinds
  module Drafts = Cistern_drafts__kinds

  type draft = Drafts.kinds

  let model : (module Cistern.MODEL) = (module Cistern_model__kinds)

  let create ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f ?f_opt ~y ?y_opt ~t ?t_opt ()
      =
    Cistern__private__kinds.Model.create
      (module Cistern_model__kinds)
      (Cistern_gen__kinds.Kinds.draft ~id ~i ?i_opt ~s ?s_opt ~b ?b_opt ~f
         ?f_opt ~y ?y_opt ~t ?t_opt () )
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
