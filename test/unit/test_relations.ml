(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Deferred and Relation (the plan of phase 4, E1 and E2), with the relations of
   the hand-written user and todo. *)

open Windtrap
open Cistern_test_models
module Deferred = Cistern.Deferred
module Relation = Cistern.Private.Relation
module U = User_todo.Cistern_model__user
module T = User_todo.Cistern_model__todo

let todos () =
  Relation.has_many ~name:"todos"
    (module U)
    (module T)
    ~key:(fun (t : User_todo.todo) -> Some t.user_id)
    ~set:(fun u _ -> u)

let user () =
  Relation.belongs_to ~name:"user"
    (module T)
    (module U)
    ~key:(fun (t : User_todo.todo) -> Some t.user_id)
    ~set:(fun t _ -> t)

(* Models whose schema is real, for the checks of the constructors: the rest of
   each module is never read (the review of 4a, F2). *)
module Stubs = struct
  module S = Cistern.Private.Schema

  let fld ?(nullable = false) ?(key = false) name scalar : S.field =
    { name
    ; column= name
    ; scalar
    ; nullable
    ; key
    ; auto= key
    ; unique= false
    ; index= false }

  (* A tree: a has_many of children and a belongs_to of a parent (a nullable
     key). *)
  let node : S.model =
    { name= "node"
    ; table= "node"
    ; fields= [fld ~key:true "id" Int64; fld ~nullable:true "parent_id" Int64]
    ; relations=
        [ Has_many {name= "children"; target= "node"}
        ; Belongs_to
            {name= "parent"; target= "node"; key= "parent_id"; references= "id"}
        ] }

  let person : S.model =
    { name= "person"
    ; table= "person"
    ; fields= [fld ~key:true "id" Int64]
    ; relations= [] }

  (* Two Belongs_to to one model, the second with a nullable key. *)
  let msg : S.model =
    { name= "msg"
    ; table= "msg"
    ; fields=
        [ fld ~key:true "id" Int64
        ; fld "from_id" Int64
        ; fld ~nullable:true "to_id" Int64 ]
    ; relations=
        [ Belongs_to
            {name= "sender"; target= "person"; key= "from_id"; references= "id"}
        ; Belongs_to
            {name= "recipient"; target= "person"; key= "to_id"; references= "id"}
        ] }

  module Stub (X : sig
    type t

    val schema : S.model
  end) =
  struct
    type t = X.t

    type key = int64

    type draft = unit

    let schema = X.schema

    let key : (t, key) Cistern.Private.Field.t =
      Cistern.Private.Field.make ~model:X.schema
        ~field:(List.hd X.schema.fields) ~encode:Cistern.Private.Encode.int64
        ~decode:Cistern.Private.Decoder.int64
        ~get:(fun _ -> 0L)
        ~set:(fun r _ -> r)

    let fields = []

    let decode _ = failwith "a stub"

    let encode_draft () = []

    let of_draft () _ = failwith "a stub"
  end

  type n

  type p

  type m

  module N = Stub (struct
    type t = n

    let schema = node
  end)

  module P = Stub (struct
    type t = p

    let schema = person
  end)

  module M = Stub (struct
    type t = m

    let schema = msg
  end)
end

let refused f =
  match f () with _ -> "no error" | exception Invalid_argument m -> m

let tests =
  group "Relations"
    [ test "Deferred.get and get_exn" (fun () ->
          equal (option int) (Some 3) (Deferred.get (Deferred.Loaded 3)) ;
          equal (option int) None (Deferred.get Deferred.Unloaded) ;
          equal int 3 (Deferred.get_exn (Deferred.Loaded 3)) ;
          raises
            (Invalid_argument "Deferred.get_exn: the relation is not loaded")
            (fun () -> Deferred.get_exn (Deferred.Unloaded : int Deferred.t) ) )
    ; test "Deferred values compare with =" (fun () ->
          equal bool true (Deferred.Loaded [1; 2] = Deferred.Loaded [1; 2]) ;
          equal bool false (Deferred.Loaded [1] = Deferred.Unloaded) )
    ; test "the name and the kind of a relation" (fun () ->
          equal string "todos" (Cistern.Relation.name (todos ())) ;
          equal bool true (Cistern.Relation.kind (todos ()) = `Has_many) ;
          equal string "user" (Cistern.Relation.name (user ())) ;
          equal bool true (Cistern.Relation.kind (user ()) = `Belongs_to) )
    ; test "a relation that the schema does not have, or has in another form"
        (fun () ->
          equal string
            "Relation.has_many: the model \"user\" has no relation \"notes\""
            (refused (fun () ->
                 Relation.has_many ~name:"notes"
                   (module U)
                   (module T)
                   ~key:(fun (t : User_todo.todo) -> Some t.user_id)
                   ~set:(fun u _ -> u) ) ) ;
          equal string
            "Relation.has_many: the model \"todo\" has a belongs_to \"user\" \
             to \"user\", not a has_many"
            (refused (fun () ->
                 Relation.has_many ~name:"user"
                   (module T)
                   (module U)
                   ~key:(fun (_ : User_todo.user) -> None)
                   ~set:(fun t _ -> t) ) ) ;
          equal string
            "Relation.belongs_to: the model \"user\" has a has_many \"todos\" \
             to \"todo\", not a belongs_to"
            (refused (fun () ->
                 Relation.belongs_to ~name:"todos"
                   (module U)
                   (module T)
                   ~key:(fun (_ : User_todo.user) -> None)
                   ~set:(fun u _ -> u) ) ) ;
          equal string
            "Relation.belongs_to: the model \"todo\" has a belongs_to \"user\" \
             to \"user\", not to \"todo\""
            (refused (fun () ->
                 Relation.belongs_to ~name:"user"
                   (module T)
                   (module T)
                   ~key:(fun (_ : User_todo.todo) -> None)
                   ~set:(fun t _ -> t) ) ) )
    ; test "a tree, two Belongs_to to one model, and nullable keys" (fun () ->
          let ok f =
            match f () with _ -> "ok" | exception Invalid_argument m -> m
          in
          let set a _ = a in
          let nokey _ = None in
          equal (list string)
            [ "ok"
            ; "ok"
            ; "ok"
            ; "ok"
            ; "Relation.has_many: the model \"node\" has a has_many \
               \"children\" to \"node\", not to \"msg\""
            ; "Relation.has_many: the model \"node\" has a belongs_to \
               \"parent\" to \"node\", not a has_many"
            ; "Relation.belongs_to: the model \"msg\" has a belongs_to \
               \"sender\" to \"person\", not to \"msg\"" ]
            [ ok (fun () ->
                  Relation.has_many ~name:"children"
                    (module Stubs.N)
                    (module Stubs.N)
                    ~key:nokey ~set )
            ; ok (fun () ->
                  Relation.belongs_to ~name:"parent"
                    (module Stubs.N)
                    (module Stubs.N)
                    ~key:nokey ~set )
            ; ok (fun () ->
                  Relation.belongs_to ~name:"sender"
                    (module Stubs.M)
                    (module Stubs.P)
                    ~key:nokey ~set )
            ; (* The second relation of msg, with a nullable key. *)
              ok (fun () ->
                  Relation.belongs_to ~name:"recipient"
                    (module Stubs.M)
                    (module Stubs.P)
                    ~key:nokey ~set )
            ; ok (fun () ->
                  Relation.has_many ~name:"children"
                    (module Stubs.N)
                    (module Stubs.M)
                    ~key:nokey ~set )
            ; ok (fun () ->
                  Relation.has_many ~name:"parent"
                    (module Stubs.N)
                    (module Stubs.N)
                    ~key:nokey ~set )
            ; ok (fun () ->
                  Relation.belongs_to ~name:"sender"
                    (module Stubs.M)
                    (module Stubs.M)
                    ~key:nokey ~set ) ] ;
          (* The schemas of the stubs pass Schema.check. *)
          equal int 0
            (List.length
               (Cistern.Private.Schema.faults
                  [Stubs.node; Stubs.person; Stubs.msg] ) ) ) ]
