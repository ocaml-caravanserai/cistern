(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of person, written by hand (the plan of phase 3, D16): a single
   table with an [@auto] key, a [@unique] field, a nullable [@unique] field with
   its own column name, an [@index] field, an option field, a [@default], and
   its own table name. Stage 1 has the form of the ppx (see kinds.ml). *)

type person =
  { id: int64 [@key] [@auto]
  ; email: string [@unique]
  ; nick: string option [@unique] [@column "nick_name"]
  ; city: string [@index]
  ; age: int option
  ; active: bool [@default true] }
[@@table "people"]

module Cistern__private__person = Cistern.Private
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

let cistern__default__person__5 () : bool = true
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_fields__person = struct
  module Person = struct
    module Schema = struct
      let id =
        { Cistern__private__person.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__person.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= true
        ; unique= false
        ; index= false }

      let email =
        { Cistern__private__person.Schema.name= "email"
        ; column= "email"
        ; scalar= Cistern__private__person.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= true
        ; index= false }

      let nick =
        { Cistern__private__person.Schema.name= "nick"
        ; column= "nick_name"
        ; scalar= Cistern__private__person.Schema.String
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= true
        ; index= false }

      let city =
        { Cistern__private__person.Schema.name= "city"
        ; column= "city"
        ; scalar= Cistern__private__person.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= true }

      let age =
        { Cistern__private__person.Schema.name= "age"
        ; column= "age"
        ; scalar= Cistern__private__person.Schema.Int
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let active =
        { Cistern__private__person.Schema.name= "active"
        ; column= "active"
        ; scalar= Cistern__private__person.Schema.Bool
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__person.Schema.name= "person"
        ; table= "people"
        ; fields= [id; email; nick; city; age; active]
        ; relations= [] }
    end

    let id =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__person.Encode.int64
        ~decode:Cistern__private__person.Decoder.int64
        ~get:(fun (cistern__r : person) -> cistern__r.id)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with id= cistern__v} )

    let email =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.email ~encode:Cistern__private__person.Encode.text
        ~decode:Cistern__private__person.Decoder.string
        ~get:(fun (cistern__r : person) -> cistern__r.email)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with email= cistern__v} )

    let nick =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.nick
        ~encode:
          (Cistern__private__person.Encode.opt
             Cistern__private__person.Encode.text )
        ~decode:
          (Cistern__private__person.Decoder.option
             Cistern__private__person.Decoder.string )
        ~get:(fun (cistern__r : person) -> cistern__r.nick)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with nick= cistern__v} )

    let city =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.city ~encode:Cistern__private__person.Encode.text
        ~decode:Cistern__private__person.Decoder.string
        ~get:(fun (cistern__r : person) -> cistern__r.city)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with city= cistern__v} )

    let age =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.age
        ~encode:
          (Cistern__private__person.Encode.opt
             Cistern__private__person.Encode.int )
        ~decode:
          (Cistern__private__person.Decoder.option
             Cistern__private__person.Decoder.int )
        ~get:(fun (cistern__r : person) -> cistern__r.age)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with age= cistern__v} )

    let active =
      Cistern__private__person.Field.make ~model:Schema.cistern__model
        ~field:Schema.active ~encode:Cistern__private__person.Encode.bool
        ~decode:Cistern__private__person.Decoder.bool
        ~get:(fun (cistern__r : person) -> cistern__r.active)
        ~set:(fun (cistern__r : person) cistern__v ->
          {cistern__r with active= cistern__v} )
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_drafts__person = struct
  type person =
    { email: string
    ; nick: string option
    ; city: string
    ; age: int option
    ; active: bool }

  let encode_person (cistern__d : person) =
    [ ( Cistern__private__person.Field.column Cistern_fields__person.Person.email
      , Cistern__private__person.Field.encode
          Cistern_fields__person.Person.email cistern__d.email )
    ; ( Cistern__private__person.Field.column Cistern_fields__person.Person.nick
      , Cistern__private__person.Field.encode Cistern_fields__person.Person.nick
          cistern__d.nick )
    ; ( Cistern__private__person.Field.column Cistern_fields__person.Person.city
      , Cistern__private__person.Field.encode Cistern_fields__person.Person.city
          cistern__d.city )
    ; ( Cistern__private__person.Field.column Cistern_fields__person.Person.age
      , Cistern__private__person.Field.encode Cistern_fields__person.Person.age
          cistern__d.age )
    ; ( Cistern__private__person.Field.column
          Cistern_fields__person.Person.active
      , Cistern__private__person.Field.encode
          Cistern_fields__person.Person.active cistern__d.active ) ]
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__person :
  Cistern.MODEL
    with type t = person
     and type key = int64
     and type draft = Cistern_drafts__person.person = struct
  type nonrec t = person

  type key = int64

  type draft = Cistern_drafts__person.person

  let schema = Cistern_fields__person.Person.Schema.cistern__model

  let key = Cistern_fields__person.Person.id

  let fields =
    [ Cistern__private__person.Field.Any Cistern_fields__person.Person.id
    ; Cistern__private__person.Field.Any Cistern_fields__person.Person.email
    ; Cistern__private__person.Field.Any Cistern_fields__person.Person.nick
    ; Cistern__private__person.Field.Any Cistern_fields__person.Person.city
    ; Cistern__private__person.Field.Any Cistern_fields__person.Person.age
    ; Cistern__private__person.Field.Any Cistern_fields__person.Person.active ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode Cistern_fields__person.Person.id)
    in
    let cistern__1 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode
           Cistern_fields__person.Person.email )
    in
    let cistern__2 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode
           Cistern_fields__person.Person.nick )
    in
    let cistern__3 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode
           Cistern_fields__person.Person.city )
    in
    let cistern__4 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode Cistern_fields__person.Person.age)
    in
    let cistern__5 =
      Cistern__private__person.Decoder.next cistern__c
        (Cistern__private__person.Field.decode
           Cistern_fields__person.Person.active )
    in
    { id= cistern__0
    ; email= cistern__1
    ; nick= cistern__2
    ; city= cistern__3
    ; age= cistern__4
    ; active= cistern__5 }

  let encode_draft = Cistern_drafts__person.encode_person

  let of_draft (cistern__d : draft) cistern__k : t =
    { id= Cistern__private__person.Encode.auto cistern__k
    ; email= cistern__d.email
    ; nick= cistern__d.nick
    ; city= cistern__d.city
    ; age= cistern__d.age
    ; active= cistern__d.active }
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_gen__person = struct
  module M = Cistern.Private.Model

  module Person = struct
    type t = person

    module Q = struct
      module Fl = Cistern_fields__person.Person

      let id = Fl.id

      let email = Fl.email

      let nick = Fl.nick

      let city = Fl.city

      let age = Fl.age

      let active = Fl.active

      include Cistern.Private.Expr.Ops
    end

    let get_by_id k = M.get (module Cistern_model__person) k

    let find_by_id k = M.find (module Cistern_model__person) k

    let get_by_email v = M.get_by (module Cistern_model__person) Q.email v

    let find_by_email v = M.find_by (module Cistern_model__person) Q.email v

    (* None names no row: the value has no option. *)
    let get_by_nick v = M.get_by (module Cistern_model__person) Q.nick (Some v)

    let find_by_nick v = M.find_by (module Cistern_model__person) Q.nick (Some v)

    let filter_by_city v = M.where (module Cistern_model__person) Q.(city ==. v)

    let update us r = M.update (module Cistern_model__person) us r

    let delete r = M.delete (module Cistern_model__person) r

    let all = M.all (module Cistern_model__person)

    let where e = M.where (module Cistern_model__person) e

    let draft ~email ?nick ~city ?age ?(active = cistern__default__person__5 ())
        () : Cistern_drafts__person.person =
      {email; nick; city; age; active}
  end
end

module Person = struct
  include Cistern_gen__person.Person
  module Drafts = Cistern_drafts__person

  type draft = Drafts.person

  let model : (module Cistern.MODEL) = (module Cistern_model__person)

  let create d = Cistern.Private.Model.create (module Cistern_model__person) d
end
