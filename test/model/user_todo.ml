(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of user and todo, written by hand (DESIGN.md, "Generated code"; the
   plan of phase 4, E14): the group of DESIGN.md with a has_many and a
   belongs_to. Phase 4g wrote the items after the declaration again from the
   generator: they have the form of the ppx (see kinds.ml). *)

type user =
  { id: int64 [@key] [@auto]
  ; name: string
  ; email: string [@unique]
  ; nick: string option [@unique]
  ; age: int option
  ; active: bool [@default true]
  ; todos: todo list Cistern.Deferred.t [@has_many] }

and todo =
  { id: int64 [@key] [@auto]
  ; user_id: int64
  ; user: user Cistern.Deferred.t [@belongs_to {key= user_id; references= id}]
  ; title: string [@index]
  ; done_: bool [@default false] [@column "done"]
  ; note: string option }

let cistern__default__user__5 () : bool = true
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

let cistern__default__user__9 () : bool = false
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern__private__user = Cistern.Private
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_fields__user = struct
  module User = struct
    module Schema = struct
      let id =
        { Cistern__private__user.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__user.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= true
        ; unique= false
        ; index= false }

      let name =
        { Cistern__private__user.Schema.name= "name"
        ; column= "name"
        ; scalar= Cistern__private__user.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let email =
        { Cistern__private__user.Schema.name= "email"
        ; column= "email"
        ; scalar= Cistern__private__user.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= true
        ; index= false }

      let nick =
        { Cistern__private__user.Schema.name= "nick"
        ; column= "nick"
        ; scalar= Cistern__private__user.Schema.String
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= true
        ; index= false }

      let age =
        { Cistern__private__user.Schema.name= "age"
        ; column= "age"
        ; scalar= Cistern__private__user.Schema.Int
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let active =
        { Cistern__private__user.Schema.name= "active"
        ; column= "active"
        ; scalar= Cistern__private__user.Schema.Bool
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__user.Schema.name= "user"
        ; table= "user"
        ; fields= [id; name; email; nick; age; active]
        ; relations=
            [ Cistern__private__user.Schema.Has_many
                {name= "todos"; target= "todo"} ] }
    end

    let id =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__user.Encode.int64
        ~decode:Cistern__private__user.Decoder.int64
        ~get:(fun (cistern__r : user) -> cistern__r.id)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with id= cistern__v} )

    let name =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.name ~encode:Cistern__private__user.Encode.text
        ~decode:Cistern__private__user.Decoder.string
        ~get:(fun (cistern__r : user) -> cistern__r.name)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with name= cistern__v} )

    let email =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.email ~encode:Cistern__private__user.Encode.text
        ~decode:Cistern__private__user.Decoder.string
        ~get:(fun (cistern__r : user) -> cistern__r.email)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with email= cistern__v} )

    let nick =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.nick
        ~encode:
          (Cistern__private__user.Encode.opt Cistern__private__user.Encode.text)
        ~decode:
          (Cistern__private__user.Decoder.option
             Cistern__private__user.Decoder.string )
        ~get:(fun (cistern__r : user) -> cistern__r.nick)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with nick= cistern__v} )

    let age =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.age
        ~encode:
          (Cistern__private__user.Encode.opt Cistern__private__user.Encode.int)
        ~decode:
          (Cistern__private__user.Decoder.option
             Cistern__private__user.Decoder.int )
        ~get:(fun (cistern__r : user) -> cistern__r.age)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with age= cistern__v} )

    let active =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.active ~encode:Cistern__private__user.Encode.bool
        ~decode:Cistern__private__user.Decoder.bool
        ~get:(fun (cistern__r : user) -> cistern__r.active)
        ~set:(fun (cistern__r : user) cistern__v ->
          {cistern__r with active= cistern__v} )
  end

  module Todo = struct
    module Schema = struct
      let id =
        { Cistern__private__user.Schema.name= "id"
        ; column= "id"
        ; scalar= Cistern__private__user.Schema.Int64
        ; nullable= false
        ; key= true
        ; auto= true
        ; unique= false
        ; index= false }

      let user_id =
        { Cistern__private__user.Schema.name= "user_id"
        ; column= "user_id"
        ; scalar= Cistern__private__user.Schema.Int64
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let title =
        { Cistern__private__user.Schema.name= "title"
        ; column= "title"
        ; scalar= Cistern__private__user.Schema.String
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= true }

      let done_ =
        { Cistern__private__user.Schema.name= "done_"
        ; column= "done"
        ; scalar= Cistern__private__user.Schema.Bool
        ; nullable= false
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let note =
        { Cistern__private__user.Schema.name= "note"
        ; column= "note"
        ; scalar= Cistern__private__user.Schema.String
        ; nullable= true
        ; key= false
        ; auto= false
        ; unique= false
        ; index= false }

      let cistern__model =
        { Cistern__private__user.Schema.name= "todo"
        ; table= "todo"
        ; fields= [id; user_id; title; done_; note]
        ; relations=
            [ Cistern__private__user.Schema.Belongs_to
                {name= "user"; target= "user"; key= "user_id"; references= "id"}
            ] }
    end

    let id =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.id ~encode:Cistern__private__user.Encode.int64
        ~decode:Cistern__private__user.Decoder.int64
        ~get:(fun (cistern__r : todo) -> cistern__r.id)
        ~set:(fun (cistern__r : todo) cistern__v ->
          {cistern__r with id= cistern__v} )

    let user_id =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.user_id ~encode:Cistern__private__user.Encode.int64
        ~decode:Cistern__private__user.Decoder.int64
        ~get:(fun (cistern__r : todo) -> cistern__r.user_id)
        ~set:(fun (cistern__r : todo) cistern__v ->
          { cistern__r with
            user_id= cistern__v
          ; user=
              Cistern__private__user.Encode.reset
                Cistern__private__user.Encode.int64 cistern__r.user_id
                cistern__v cistern__r.user } )

    let title =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.title ~encode:Cistern__private__user.Encode.text
        ~decode:Cistern__private__user.Decoder.string
        ~get:(fun (cistern__r : todo) -> cistern__r.title)
        ~set:(fun (cistern__r : todo) cistern__v ->
          {cistern__r with title= cistern__v} )

    let done_ =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.done_ ~encode:Cistern__private__user.Encode.bool
        ~decode:Cistern__private__user.Decoder.bool
        ~get:(fun (cistern__r : todo) -> cistern__r.done_)
        ~set:(fun (cistern__r : todo) cistern__v ->
          {cistern__r with done_= cistern__v} )

    let note =
      Cistern__private__user.Field.make ~model:Schema.cistern__model
        ~field:Schema.note
        ~encode:
          (Cistern__private__user.Encode.opt Cistern__private__user.Encode.text)
        ~decode:
          (Cistern__private__user.Decoder.option
             Cistern__private__user.Decoder.string )
        ~get:(fun (cistern__r : todo) -> cistern__r.note)
        ~set:(fun (cistern__r : todo) cistern__v ->
          {cistern__r with note= cistern__v} )
  end

  module Set_user = struct
    let todos (cistern__r : user) cistern__v =
      {cistern__r with todos= Cistern__private__user.Deferred.Loaded cistern__v}
  end

  module Set_todo = struct
    let user (cistern__r : todo) cistern__v =
      {cistern__r with user= Cistern__private__user.Deferred.Loaded cistern__v}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_drafts__user = struct
  type user =
    { name: string
    ; email: string
    ; nick: string option
    ; age: int option
    ; active: bool }

  and todo =
    {user_id: int64 option; title: string; done_: bool; note: string option}

  let encode_user (cistern__d : user) =
    [ ( Cistern__private__user.Field.column Cistern_fields__user.User.name
      , Cistern__private__user.Field.encode Cistern_fields__user.User.name
          cistern__d.name )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.User.email
      , Cistern__private__user.Field.encode Cistern_fields__user.User.email
          cistern__d.email )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.User.nick
      , Cistern__private__user.Field.encode Cistern_fields__user.User.nick
          cistern__d.nick )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.User.age
      , Cistern__private__user.Field.encode Cistern_fields__user.User.age
          cistern__d.age )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.User.active
      , Cistern__private__user.Field.encode Cistern_fields__user.User.active
          cistern__d.active ) ]

  let encode_todo (cistern__d : todo) =
    [ ( Cistern__private__user.Field.column Cistern_fields__user.Todo.user_id
      , Cistern__private__user.Encode.opt
          (Cistern__private__user.Field.encode Cistern_fields__user.Todo.user_id)
          cistern__d.user_id )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.Todo.title
      , Cistern__private__user.Field.encode Cistern_fields__user.Todo.title
          cistern__d.title )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.Todo.done_
      , Cistern__private__user.Field.encode Cistern_fields__user.Todo.done_
          cistern__d.done_ )
    ; ( Cistern__private__user.Field.column Cistern_fields__user.Todo.note
      , Cistern__private__user.Field.encode Cistern_fields__user.Todo.note
          cistern__d.note ) ]

  module Set_todo = struct
    let user_id (cistern__d : todo) cistern__k =
      {cistern__d with user_id= Cistern__private__user.Encode.some cistern__k}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__user :
  Cistern.MODEL
    with type t = user
     and type key = int64
     and type draft = Cistern_drafts__user.user = struct
  type nonrec t = user

  type key = int64

  type draft = Cistern_drafts__user.user

  let schema = Cistern_fields__user.User.Schema.cistern__model

  let key = Cistern_fields__user.User.id

  let fields =
    [ Cistern__private__user.Field.Any Cistern_fields__user.User.id
    ; Cistern__private__user.Field.Any Cistern_fields__user.User.name
    ; Cistern__private__user.Field.Any Cistern_fields__user.User.email
    ; Cistern__private__user.Field.Any Cistern_fields__user.User.nick
    ; Cistern__private__user.Field.Any Cistern_fields__user.User.age
    ; Cistern__private__user.Field.Any Cistern_fields__user.User.active ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.id)
    in
    let cistern__1 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.name)
    in
    let cistern__2 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.email)
    in
    let cistern__3 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.nick)
    in
    let cistern__4 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.age)
    in
    let cistern__5 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.User.active)
    in
    { id= cistern__0
    ; name= cistern__1
    ; email= cistern__2
    ; nick= cistern__3
    ; age= cistern__4
    ; active= cistern__5
    ; todos= Cistern__private__user.Deferred.Unloaded }

  let encode_draft = Cistern_drafts__user.encode_user

  let of_draft (cistern__d : draft) cistern__k : t =
    { id= Cistern__private__user.Encode.auto cistern__k
    ; name= cistern__d.name
    ; email= cistern__d.email
    ; nick= cistern__d.nick
    ; age= cistern__d.age
    ; active= cistern__d.active
    ; todos= Cistern__private__user.Deferred.Unloaded }
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_model__todo :
  Cistern.MODEL
    with type t = todo
     and type key = int64
     and type draft = Cistern_drafts__user.todo = struct
  type nonrec t = todo

  type key = int64

  type draft = Cistern_drafts__user.todo

  let schema = Cistern_fields__user.Todo.Schema.cistern__model

  let key = Cistern_fields__user.Todo.id

  let fields =
    [ Cistern__private__user.Field.Any Cistern_fields__user.Todo.id
    ; Cistern__private__user.Field.Any Cistern_fields__user.Todo.user_id
    ; Cistern__private__user.Field.Any Cistern_fields__user.Todo.title
    ; Cistern__private__user.Field.Any Cistern_fields__user.Todo.done_
    ; Cistern__private__user.Field.Any Cistern_fields__user.Todo.note ]

  let decode cistern__c : t =
    let cistern__0 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.Todo.id)
    in
    let cistern__1 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.Todo.user_id)
    in
    let cistern__2 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.Todo.title)
    in
    let cistern__3 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.Todo.done_)
    in
    let cistern__4 =
      Cistern__private__user.Decoder.next cistern__c
        (Cistern__private__user.Field.decode Cistern_fields__user.Todo.note)
    in
    { id= cistern__0
    ; user_id= cistern__1
    ; title= cistern__2
    ; done_= cistern__3
    ; note= cistern__4
    ; user= Cistern__private__user.Deferred.Unloaded }

  let encode_draft = Cistern_drafts__user.encode_todo

  let of_draft (cistern__d : draft) cistern__k : t =
    { id= Cistern__private__user.Encode.auto cistern__k
    ; user_id= Cistern__private__user.Encode.draft_key cistern__d.user_id
    ; title= cistern__d.title
    ; done_= cistern__d.done_
    ; note= cistern__d.note
    ; user= Cistern__private__user.Deferred.Unloaded }
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Cistern_gen__user = struct
  module User = struct
    type nonrec t = user

    module Q = struct
      let id = Cistern_fields__user.User.id

      let name = Cistern_fields__user.User.name

      let email = Cistern_fields__user.User.email

      let nick = Cistern_fields__user.User.nick

      let age = Cistern_fields__user.User.age

      let active = Cistern_fields__user.User.active

      include Cistern__private__user.Expr.Ops
    end

    let get_by_id cistern__k =
      Cistern__private__user.Model.get (module Cistern_model__user) cistern__k

    let find_by_id cistern__k =
      Cistern__private__user.Model.find (module Cistern_model__user) cistern__k

    let get_by_email cistern__v =
      Cistern__private__user.Model.get_by
        (module Cistern_model__user)
        Q.email cistern__v

    let find_by_email cistern__v =
      Cistern__private__user.Model.find_by
        (module Cistern_model__user)
        Q.email cistern__v

    let get_by_nick cistern__v =
      Cistern__private__user.Model.get_by
        (module Cistern_model__user)
        Q.nick
        (Cistern__private__user.Encode.some cistern__v)

    let find_by_nick cistern__v =
      Cistern__private__user.Model.find_by
        (module Cistern_model__user)
        Q.nick
        (Cistern__private__user.Encode.some cistern__v)

    let all = Cistern__private__user.Model.all (module Cistern_model__user)

    let where cistern__e =
      Cistern__private__user.Model.where (module Cistern_model__user) cistern__e

    let update ?name ?email ?nick ?age ?active cistern__r =
      Cistern__private__user.Model.update
        (module Cistern_model__user)
        (Cistern__private__user.Encode.updates
           [ Cistern__private__user.Encode.set Q.name name
           ; Cistern__private__user.Encode.set Q.email email
           ; Cistern__private__user.Encode.set Q.nick nick
           ; Cistern__private__user.Encode.set Q.age age
           ; Cistern__private__user.Encode.set Q.active active ] )
        cistern__r

    let delete cistern__r =
      Cistern__private__user.Model.delete
        (module Cistern_model__user)
        cistern__r

    let draft ~name ~email ?nick ?age ?(active = cistern__default__user__5 ())
        () : Cistern_drafts__user.user =
      {name; email; nick; age; active}
  end

  module Todo = struct
    type nonrec t = todo

    module Q = struct
      let id = Cistern_fields__user.Todo.id

      let user_id = Cistern_fields__user.Todo.user_id

      let title = Cistern_fields__user.Todo.title

      let done_ = Cistern_fields__user.Todo.done_

      let note = Cistern_fields__user.Todo.note

      include Cistern__private__user.Expr.Ops
    end

    let get_by_id cistern__k =
      Cistern__private__user.Model.get (module Cistern_model__todo) cistern__k

    let find_by_id cistern__k =
      Cistern__private__user.Model.find (module Cistern_model__todo) cistern__k

    let filter_by_title cistern__v =
      Cistern__private__user.Model.where
        (module Cistern_model__todo)
        (Cistern__private__user.Expr.Ops.( ==. ) Q.title cistern__v)

    let filter_by_user_id cistern__v =
      Cistern__private__user.Model.where
        (module Cistern_model__todo)
        (Cistern__private__user.Expr.Ops.( ==. ) Q.user_id cistern__v)

    let all = Cistern__private__user.Model.all (module Cistern_model__todo)

    let where cistern__e =
      Cistern__private__user.Model.where (module Cistern_model__todo) cistern__e

    let update ?user_id ?title ?done_ ?note cistern__r =
      Cistern__private__user.Model.update
        (module Cistern_model__todo)
        (Cistern__private__user.Encode.updates
           [ Cistern__private__user.Encode.set Q.user_id user_id
           ; Cistern__private__user.Encode.set Q.title title
           ; Cistern__private__user.Encode.set Q.done_ done_
           ; Cistern__private__user.Encode.set Q.note note ] )
        cistern__r

    let delete cistern__r =
      Cistern__private__user.Model.delete
        (module Cistern_model__todo)
        cistern__r

    let draft ?user_id ~title ?(done_ = cistern__default__user__9 ()) ?note () :
        Cistern_drafts__user.todo =
      {user_id; title; done_; note}
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module User = struct
  include Cistern_gen__user.User
  module Drafts = Cistern_drafts__user

  type draft = Drafts.user

  let model : (module Cistern.MODEL) = (module Cistern_model__user)

  let create ~name ~email ?nick ?age ?active ?todos () =
    Cistern__private__user.Model.create
      (module Cistern_model__user)
      ~children:
        (Cistern__private__user.Encode.somes
           [ Cistern__private__user.Encode.group
               (Cistern__private__user.Model.child
                  (module Cistern_model__todo)
                  ~fk:Cistern_drafts__user.Set_todo.user_id
                  ~set:Cistern_fields__user.Set_user.todos )
               todos ] )
      (Cistern_gen__user.User.draft ~name ~email ?nick ?age ?active ())

  let todos (cistern__r : Cistern_gen__user.User.t) =
    Cistern__private__user.Model.where
      (module Cistern_model__todo)
      (Cistern__private__user.Expr.Ops.( ==. ) Cistern_fields__user.Todo.user_id
         cistern__r.id )

  module Rel = struct
    let todos =
      Cistern__private__user.Relation.has_many ~name:"todos"
        (module Cistern_model__user)
        (module Cistern_model__todo)
        ~key:(fun (cistern__r : Cistern_gen__user.Todo.t) ->
          Cistern__private__user.Encode.some cistern__r.user_id )
        ~set:Cistern_fields__user.Set_user.todos
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]

module Todo = struct
  include Cistern_gen__user.Todo
  module Drafts = Cistern_drafts__user

  type draft = Drafts.todo

  let model : (module Cistern.MODEL) = (module Cistern_model__todo)

  let create ?user_id ~title ?done_ ?note () =
    Cistern__private__user.Model.create
      (module Cistern_model__todo)
      (Cistern_gen__user.Todo.draft ?user_id ~title ?done_ ?note ())

  let user (cistern__r : Cistern_gen__user.Todo.t) =
    Cistern__private__user.Model.get
      (module Cistern_model__user)
      cistern__r.user_id

  module Rel = struct
    let user =
      Cistern__private__user.Relation.belongs_to ~name:"user"
        (module Cistern_model__todo)
        (module Cistern_model__user)
        ~key:(fun (cistern__r : Cistern_gen__user.Todo.t) ->
          Cistern__private__user.Encode.some cistern__r.user_id )
        ~set:
          (Cistern__private__user.Encode.parent
             Cistern_fields__user.Set_todo.user
             (fun (cistern__r : Cistern_gen__user.Todo.t) ->
               {cistern__r with user= Cistern__private__user.Deferred.Unloaded} )
          )
  end
end
[@@ocaml.warning "-23-27-30-32-34-37-40-41-42-60-69"]
