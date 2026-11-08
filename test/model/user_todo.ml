(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The group of user and todo, written by hand in the three stages of DESIGN.md,
   "Generated code". The source is this group:

   {[ type user = { id : int64; [@key] [@auto] name : string; email : string;
   [@unique] nick : string option; [@unique] age : int option; active : bool;
   [@default true] } and todo = { id : int64; [@key] [@auto] user_id : int64;
   user : user Deferred.t; [@belongs_to { key = user_id; references = id }]
   title : string; [@index] done_ : bool; [@default false] [@column "done"] note
   : string option; } ]}

   The user record also has todos : todo list Deferred.t [@has_many]. Phase 4c1
   adds the relation fields (the plan of phase 4, E3, E9, E10): they are not
   columns, decode and of_draft give Unloaded, the setter of user_id resets
   user, and User.create has ?todos. Each call of the library goes through
   Cistern.Private. *)

type user =
  { id: int64
  ; name: string
  ; email: string
  ; nick: string option
  ; age: int option
  ; active: bool
  ; todos: todo list Cistern.Deferred.t }

and todo =
  { id: int64
  ; user_id: int64
  ; user: user Cistern.Deferred.t
  ; title: string
  ; done_: bool
  ; note: string option }

(* Stage 1: the schemas and the fields of the two types. *)
module Cistern_fields__user = struct
  module S = Cistern.Private.Schema
  module F = Cistern.Private.Field
  module D = Cistern.Private.Decoder
  module V = Cistern.Value

  let field ?column ?(nullable = false) ?(key = false) ?(auto = false)
      ?(unique = false) ?(index = false) name scalar : S.field =
    { name
    ; column= Option.value column ~default:name
    ; scalar
    ; nullable
    ; key
    ; auto
    ; unique
    ; index }

  let opt f = function None -> V.Null | Some x -> f x

  let int64 i = V.Int i

  let int i = V.Int (Int64.of_int i)

  let text s = V.Text s

  let bool b = V.Int (if b then 1L else 0L)

  module User = struct
    let s_id = field "id" Int64 ~key:true ~auto:true

    let s_name = field "name" String

    let s_email = field "email" String ~unique:true

    let s_nick = field "nick" String ~nullable:true ~unique:true

    let s_age = field "age" Int ~nullable:true

    let s_active = field "active" Bool

    let schema : S.model =
      { name= "user"
      ; table= "user"
      ; fields= [s_id; s_name; s_email; s_nick; s_age; s_active]
      ; relations= [Has_many {name= "todos"; target= "todo"}] }

    let id : (user, int64) F.t =
      F.make ~model:schema ~field:s_id ~encode:int64 ~decode:D.int64
        ~get:(fun (r : user) -> r.id)
        ~set:(fun (r : user) v -> {r with id= v})

    let name : (user, string) F.t =
      F.make ~model:schema ~field:s_name ~encode:text ~decode:D.string
        ~get:(fun (r : user) -> r.name)
        ~set:(fun (r : user) v -> {r with name= v})

    let email : (user, string) F.t =
      F.make ~model:schema ~field:s_email ~encode:text ~decode:D.string
        ~get:(fun (r : user) -> r.email)
        ~set:(fun (r : user) v -> {r with email= v})

    let nick : (user, string option) F.t =
      F.make ~model:schema ~field:s_nick ~encode:(opt text)
        ~decode:(D.option D.string)
        ~get:(fun (r : user) -> r.nick)
        ~set:(fun (r : user) v -> {r with nick= v})

    let age : (user, int option) F.t =
      F.make ~model:schema ~field:s_age ~encode:(opt int)
        ~decode:(D.option D.int)
        ~get:(fun (r : user) -> r.age)
        ~set:(fun (r : user) v -> {r with age= v})

    let active : (user, bool) F.t =
      F.make ~model:schema ~field:s_active ~encode:bool ~decode:D.bool
        ~get:(fun (r : user) -> r.active)
        ~set:(fun (r : user) v -> {r with active= v})
  end

  module Todo = struct
    let s_id = field "id" Int64 ~key:true ~auto:true

    let s_user_id = field "user_id" Int64

    let s_title = field "title" String ~index:true

    let s_done = field "done_" Bool ~column:"done"

    let s_note = field "note" String ~nullable:true

    let schema : S.model =
      { name= "todo"
      ; table= "todo"
      ; fields= [s_id; s_user_id; s_title; s_done; s_note]
      ; relations=
          [ Belongs_to
              {name= "user"; target= "user"; key= "user_id"; references= "id"}
          ] }

    let id : (todo, int64) F.t =
      F.make ~model:schema ~field:s_id ~encode:int64 ~decode:D.int64
        ~get:(fun (r : todo) -> r.id)
        ~set:(fun (r : todo) v -> {r with id= v})

    (* The setter of the key of a Belongs_to resets the relation for a new key
       (E9). *)
    let user_id : (todo, int64) F.t =
      F.make ~model:schema ~field:s_user_id ~encode:int64 ~decode:D.int64
        ~get:(fun (r : todo) -> r.user_id)
        ~set:(fun (r : todo) v ->
          { r with
            user_id= v
          ; user= Cistern.Private.Encode.reset int64 r.user_id v r.user } )

    let title : (todo, string) F.t =
      F.make ~model:schema ~field:s_title ~encode:text ~decode:D.string
        ~get:(fun (r : todo) -> r.title)
        ~set:(fun (r : todo) v -> {r with title= v})

    let done_ : (todo, bool) F.t =
      F.make ~model:schema ~field:s_done ~encode:bool ~decode:D.bool
        ~get:(fun (r : todo) -> r.done_)
        ~set:(fun (r : todo) v -> {r with done_= v})

    let note : (todo, string option) F.t =
      F.make ~model:schema ~field:s_note ~encode:(opt text)
        ~decode:(D.option D.string)
        ~get:(fun (r : todo) -> r.note)
        ~set:(fun (r : todo) v -> {r with note= v})
  end

  (* The setters of the relation fields (E3): not columns, thus not typed
     fields. *)
  module Set_user = struct
    let todos (cistern__r : user) v =
      {cistern__r with todos= Cistern.Deferred.Loaded v}
  end

  module Set_todo = struct
    let user (cistern__r : todo) v =
      {cistern__r with user= Cistern.Deferred.Loaded v}
  end
end

(* Stage 1: the drafts, the draft encoders and the setters. A draft has only the
   column fields with no [@auto] key. In a todo draft, the key of the Belongs_to
   is optional: a nested create (phase 4) fills it. *)
module Cistern_drafts__user = struct
  module Fl = Cistern_fields__user

  type user =
    { name: string
    ; email: string
    ; nick: string option
    ; age: int option
    ; active: bool }

  and todo =
    {user_id: int64 option; title: string; done_: bool; note: string option}

  (* The column and the encoder of each field come from the field. *)
  let e f v = (Fl.F.column f, Fl.F.encode f v)

  let encode_user (d : user) =
    let module U = Fl.User in
    [ e U.name d.name
    ; e U.email d.email
    ; e U.nick d.nick
    ; e U.age d.age
    ; e U.active d.active ]

  (* The key of the Belongs_to is optional in a draft: None gives NULL. *)
  let encode_todo (d : todo) =
    let module T = Fl.Todo in
    [ ( Fl.F.column T.user_id
      , match d.user_id with
        | None ->
            Fl.V.Null
        | Some k ->
            Fl.F.encode T.user_id k )
    ; e T.title d.title
    ; e T.done_ d.done_
    ; e T.note d.note ]

  (* The setters of the draft keys of stage 1. The setters of the relation
     fields are in Cistern_fields__user (E3). *)
  module Set_todo = struct
    let user_id (d : todo) k = {d with user_id= Some k}
  end
end

(* Stage 1: the model modules. *)
module Cistern_model__user :
  Cistern.MODEL
    with type t = user
     and type key = int64
     and type draft = Cistern_drafts__user.user = struct
  module Fl = Cistern_fields__user.User
  module D = Cistern.Private.Decoder
  module F = Cistern.Private.Field

  type t = user

  type key = int64

  type draft = Cistern_drafts__user.user

  let schema = Fl.schema

  let key = Fl.id

  let fields =
    F.
      [ Any Fl.id
      ; Any Fl.name
      ; Any Fl.email
      ; Any Fl.nick
      ; Any Fl.age
      ; Any Fl.active ]

  let decode c : t =
    let id = D.next c (F.decode Fl.id) in
    let name = D.next c (F.decode Fl.name) in
    let email = D.next c (F.decode Fl.email) in
    let nick = D.next c (F.decode Fl.nick) in
    let age = D.next c (F.decode Fl.age) in
    let active = D.next c (F.decode Fl.active) in
    {id; name; email; nick; age; active; todos= Cistern.Deferred.Unloaded}

  let encode_draft = Cistern_drafts__user.encode_user

  let of_draft (d : draft) k : t =
    { id= Option.value k ~default:0L
    ; name= d.name
    ; email= d.email
    ; nick= d.nick
    ; age= d.age
    ; active= d.active
    ; todos= Cistern.Deferred.Unloaded }
end

module Cistern_model__todo :
  Cistern.MODEL
    with type t = todo
     and type key = int64
     and type draft = Cistern_drafts__user.todo = struct
  module Fl = Cistern_fields__user.Todo
  module D = Cistern.Private.Decoder
  module F = Cistern.Private.Field

  type t = todo

  type key = int64

  type draft = Cistern_drafts__user.todo

  let schema = Fl.schema

  let key = Fl.id

  let fields =
    F.[Any Fl.id; Any Fl.user_id; Any Fl.title; Any Fl.done_; Any Fl.note]

  let decode c : t =
    let id = D.next c (F.decode Fl.id) in
    let user_id = D.next c (F.decode Fl.user_id) in
    let title = D.next c (F.decode Fl.title) in
    let done_ = D.next c (F.decode Fl.done_) in
    let note = D.next c (F.decode Fl.note) in
    {id; user_id; user= Cistern.Deferred.Unloaded; title; done_; note}

  let encode_draft = Cistern_drafts__user.encode_todo

  (* A draft with no user_id fails on the server (Not_null) before this. *)
  let of_draft (d : draft) k : t =
    { id= Option.value k ~default:0L
    ; user_id= Option.value d.user_id ~default:0L
    ; user= Cistern.Deferred.Unloaded
    ; title= d.title
    ; done_= d.done_
    ; note= d.note }
end

(* Stage 2: the namespace module. *)
module Cistern_gen__user = struct
  module M = Cistern.Private.Model
  module U = Cistern.Update

  module User = struct
    type t = user

    module Q = struct
      let id = Cistern_fields__user.User.id

      let name = Cistern_fields__user.User.name

      let email = Cistern_fields__user.User.email

      let nick = Cistern_fields__user.User.nick

      let age = Cistern_fields__user.User.age

      let active = Cistern_fields__user.User.active

      include Cistern.Private.Expr.Ops
    end

    let get_by_id k = M.get (module Cistern_model__user) k

    let find_by_id k = M.find (module Cistern_model__user) k

    let get_by_email v = M.get_by (module Cistern_model__user) Q.email v

    let find_by_email v = M.find_by (module Cistern_model__user) Q.email v

    (* A nullable unique field: the value with no option (None names no row). *)
    let get_by_nick v = M.get_by (module Cistern_model__user) Q.nick (Some v)

    let find_by_nick v = M.find_by (module Cistern_model__user) Q.nick (Some v)

    let update ?name ?email ?nick ?age ?active r =
      M.update
        (module Cistern_model__user)
        (List.filter_map Fun.id
           [ Option.map (U.set Q.name) name
           ; Option.map (U.set Q.email) email
           ; Option.map (U.set Q.nick) nick
           ; Option.map (U.set Q.age) age
           ; Option.map (U.set Q.active) active ] )
        r

    let delete r = M.delete (module Cistern_model__user) r

    let all = M.all (module Cistern_model__user)

    let where e = M.where (module Cistern_model__user) e

    let draft ~name ~email ?nick ?age ?(active = true) () :
        Cistern_drafts__user.user =
      {name; email; nick; age; active}
  end

  module Todo = struct
    type t = todo

    module Q = struct
      let id = Cistern_fields__user.Todo.id

      let user_id = Cistern_fields__user.Todo.user_id

      let title = Cistern_fields__user.Todo.title

      let done_ = Cistern_fields__user.Todo.done_

      let note = Cistern_fields__user.Todo.note

      include Cistern.Private.Expr.Ops
    end

    let get_by_id k = M.get (module Cistern_model__todo) k

    let find_by_id k = M.find (module Cistern_model__todo) k

    let update ?user_id ?title ?done_ ?note r =
      M.update
        (module Cistern_model__todo)
        (List.filter_map Fun.id
           [ Option.map (U.set Q.user_id) user_id
           ; Option.map (U.set Q.title) title
           ; Option.map (U.set Q.done_) done_
           ; Option.map (U.set Q.note) note ] )
        r

    let delete r = M.delete (module Cistern_model__todo) r

    let all = M.all (module Cistern_model__todo)

    let where e = M.where (module Cistern_model__todo) e

    (* An [@index] field, and the key of a Belongs_to (which acts as one). *)
    let filter_by_title v = where Q.(title ==. v)

    let filter_by_user_id v = where Q.(user_id ==. v)

    let draft ?user_id ~title ?(done_ = false) ?note () :
        Cistern_drafts__user.todo =
      {user_id; title; done_; note}
  end
end

(* Stage 3: create (with the children of each has_many, E10), model, Drafts, the
   relation scopes (E11) and Rel (E2). *)
module User = struct
  include Cistern_gen__user.User
  module Drafts = Cistern_drafts__user

  type draft = Drafts.user

  let model : (module Cistern.MODEL) = (module Cistern_model__user)

  (* A group only for a ?todos that the caller gave: then todos is Loaded, also
     for an empty list. With no ?todos, todos stays Unloaded. *)
  let create ~name ~email ?nick ?age ?active ?todos () =
    Cistern.Private.Model.create
      (module Cistern_model__user)
      ~children:
        ( match todos with
        | None ->
            []
        | Some cistern__ds ->
            [ Cistern.Private.Model.child
                (module Cistern_model__todo)
                ~fk:Drafts.Set_todo.user_id
                ~set:Cistern_fields__user.Set_user.todos cistern__ds ] )
      (draft ~name ~email ?nick ?age ?active ())

  (* The scope of the has_many: the todos with the key of this user. *)
  let todos (cistern__r : user) =
    Cistern_gen__user.Todo.where
      Cistern_gen__user.Todo.Q.(user_id ==. cistern__r.id)

  module Rel = struct
    let todos : (user, todo) Cistern.Relation.t =
      Cistern.Private.Relation.has_many ~name:"todos"
        (module Cistern_model__user)
        (module Cistern_model__todo)
        ~key:(fun (cistern__r : todo) ->
          Cistern.Private.Encode.some cistern__r.user_id )
        ~set:Cistern_fields__user.Set_user.todos
  end
end

module Todo = struct
  include Cistern_gen__user.Todo
  module Drafts = Cistern_drafts__user

  type draft = Drafts.todo

  let model : (module Cistern.MODEL) = (module Cistern_model__todo)

  let create ?user_id ~title ?done_ ?note () =
    Cistern.Private.Model.create
      (module Cistern_model__todo)
      (draft ?user_id ~title ?done_ ?note ())

  (* The scope of the belongs_to: get of the key (Not_found if the row is
     gone). *)
  let user (cistern__r : todo) =
    Cistern.Private.Model.get (module Cistern_model__user) cistern__r.user_id

  module Rel = struct
    (* The key is not nullable: no parent leaves the relation Unloaded (the
       notes of the review of 4a). *)
    let user : (todo, user) Cistern.Relation.t =
      Cistern.Private.Relation.belongs_to ~name:"user"
        (module Cistern_model__todo)
        (module Cistern_model__user)
        ~key:(fun (cistern__r : todo) ->
          Cistern.Private.Encode.some cistern__r.user_id )
        ~set:(fun (cistern__r : todo) -> function
          | Some cistern__p ->
              Cistern_fields__user.Set_todo.user cistern__r cistern__p
          | None ->
              {cistern__r with user= Cistern.Deferred.Unloaded} )
  end
end
