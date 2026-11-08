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

(* The nested create, the reset of a Belongs_to key and the relation fields of
   the hand-written user and todo (phase 4c1, the plan of phase 4, E3, E9 and
   E10). *)
let nested =
  let module User = User_todo.User in
  let module Todo = User_todo.Todo in
  let module Q = User_todo.Todo.Q in
  let sql s = Cistern.Stmt.to_sql Cistern_core.Dialect.Mysql s in
  let redacted s =
    (Cistern.Stmt.Private.meta s).redacted Cistern_core.Dialect.Mysql
  in
  let atomic s = (Cistern.Stmt.Private.meta s).atomic in
  let ann ?todos () = User.create ~name:"Ann" ~email:"a@x" ?todos () in
  let milk = Todo.draft ~title:"Milk" ()
  and eggs = Todo.draft ~title:"Eggs" () in
  let todo user =
    {User_todo.id= 1L; user_id= 2L; user; title= "t"; done_= false; note= None}
  in
  let ann_row : User_todo.user =
    { id= 2L
    ; name= "Ann"
    ; email= "a@x"
    ; nick= None
    ; age= None
    ; active= true
    ; todos= Deferred.Unloaded }
  in
  let state (t : User_todo.todo) =
    match t.user with Deferred.Unloaded -> "Unloaded" | Loaded u -> u.name
  in
  group "The nested create"
    [ test "to_sql: the parent, then each child with <last_insert_id>" (fun () ->
          let s = ann ~todos:[milk; eggs] () in
          equal (list string)
            [ "INSERT INTO `user` (`name`, `email`, `nick`, `age`, `active`) \
               VALUES (_utf8mb4 X'416E6E', _utf8mb4 X'614078', NULL, NULL, 1)"
            ; "INSERT INTO `todo` (`user_id`, `title`, `done`, `note`) VALUES \
               (<last_insert_id>, _utf8mb4 X'4D696C6B', 0, NULL)"
            ; "INSERT INTO `todo` (`user_id`, `title`, `done`, `note`) VALUES \
               (<last_insert_id>, _utf8mb4 X'45676773', 0, NULL)" ]
            (sql s) ;
          (* The redacted SQL of the events: ? also for the key of the parent
             (E7). *)
          let child =
            "INSERT INTO `todo` (`user_id`, `title`, `done`, `note`) VALUES \
             (?, ?, ?, ?)"
          in
          equal (list string)
            [ "INSERT INTO `user` (`name`, `email`, `nick`, `age`, `active`) \
               VALUES (?, ?, ?, ?, ?)"
            ; child
            ; child ]
            (redacted s) ;
          is_true (atomic s) ;
          is_false (Cistern.Stmt.Private.meta s).for_update )
    ; test "a key that is not [@auto]: the key of the draft in each child"
        (fun () ->
          equal (list string)
            [ "INSERT INTO `p` (`code`, `x`) VALUES (_utf8mb4 X'41', 1e0)"
            ; "INSERT INTO `c` (`pcode`, `y`) VALUES (_utf8mb4 X'41', 2e0)" ]
            (sql (Coded.create [2.])) ;
          equal bool true
            ((Cistern.Stmt.Private.meta (Coded.create [2.])).text = None) )
    ; test "a value with no literal in the parent or in a child: Invalid_value"
        (fun () ->
          let text s = (Cistern.Stmt.Private.meta s).text in
          let nan = Float.nan in
          equal bool true
            ( text (Coded.create ~x:nan [2.])
              = Some (Error "a NaN is not a SQL value")
            && text (Coded.create [2.; nan])
               = Some (Error "a NaN is not a SQL value") ) ;
          equal (list string)
            [ "INSERT INTO `p` (`code`, `x`) VALUES (_utf8mb4 X'41', 1e0)"
            ; "INSERT INTO `c` (`pcode`, `y`) VALUES (_utf8mb4 X'41', 2e0)"
            ; "<Invalid_value: a NaN is not a SQL value>" ]
            (sql (Coded.create [2.; nan])) )
    ; test "each label of User.create" (fun () ->
          (* Each label gives its own value: N, E, K, 3, 0, and the todo T, 1,
             O. *)
          equal (list string)
            [ "INSERT INTO `user` (`name`, `email`, `nick`, `age`, `active`) \
               VALUES (_utf8mb4 X'4E', _utf8mb4 X'45', _utf8mb4 X'4B', 3, 0)"
            ; "INSERT INTO `todo` (`user_id`, `title`, `done`, `note`) VALUES \
               (<last_insert_id>, _utf8mb4 X'54', 1, _utf8mb4 X'4F')" ]
            (sql
               (User.create ~name:"N" ~email:"E" ~nick:"K" ~age:3 ~active:false
                  ~todos:[Todo.draft ~title:"T" ~done_:true ~note:"O" ()]
                  () ) ) )
    ; test "two has_many: the groups in the order of the list" (fun () ->
          let p =
            "INSERT INTO `p` (`code`, `x`) VALUES (_utf8mb4 X'41', 1e0)"
          in
          let c =
            "INSERT INTO `c` (`pcode`, `y`) VALUES (_utf8mb4 X'41', 2e0)"
          in
          let d =
            "INSERT INTO `d` (`owner`, `name`) VALUES (_utf8mb4 X'41', \
             _utf8mb4 X'726578')"
          in
          let s = Coded.create ~pets:["rex"] [2.; 2.] in
          equal (list string) [p; c; c; d] (sql s) ;
          equal int 4 (List.length (redacted s)) ;
          is_true (atomic s) ;
          (* One empty group and one with a child: atomic, with the child. *)
          equal (list string) [p; d] (sql (Coded.create ~pets:["rex"] [])) ;
          is_true (atomic (Coded.create ~pets:["rex"] [])) ;
          equal (list string) [p; c] (sql (Coded.create ~pets:[] [2.])) ;
          is_true (atomic (Coded.create ~pets:[] [2.])) ;
          (* Two empty groups: one INSERT, not atomic. *)
          equal (list string) [p] (sql (Coded.create ~pets:[] [])) ;
          is_false (atomic (Coded.create ~pets:[] [])) )
    ; test "no child, or an empty list: one INSERT, not atomic" (fun () ->
          equal (list string) (sql (ann ())) (sql (ann ~todos:[] ())) ;
          equal int 1 (List.length (sql (ann ~todos:[] ()))) ;
          is_false (atomic (ann ())) ;
          is_false (atomic (ann ~todos:[] ())) )
    ; test "decode and of_draft give Unloaded" (fun () ->
          let t =
            Cistern.Private.Decoder.run User_todo.Cistern_model__todo.decode
              [|Some "1"; Some "2"; Some "t"; Some "0"; None|]
          in
          equal (result string string) (Ok "Unloaded") (Result.map state t) ;
          let u =
            User_todo.Cistern_model__user.of_draft
              (User.draft ~name:"Ann" ~email:"a@x" ())
              (Some 2L)
          in
          is_true (u.todos = Deferred.Unloaded) ;
          let t = User_todo.Cistern_model__todo.of_draft milk (Some 1L) in
          equal string "Unloaded" (state t) )
    ; test "the setters of the relation fields set Loaded" (fun () ->
          let u =
            User_todo.Cistern_fields__user.Set_user.todos ann_row [todo Unloaded]
          in
          is_true (u.todos = Deferred.Loaded [todo Unloaded]) ;
          let t =
            User_todo.Cistern_fields__user.Set_todo.user (todo Unloaded) ann_row
          in
          equal string "Ann" (state t) )
    ; test "a new key resets the Belongs_to, the same key keeps it" (fun () ->
          let t = todo (Deferred.Loaded ann_row) in
          equal (list string)
            ["Unloaded"; "Ann"; "Unloaded"; "Ann"]
            [ state (Cistern.Private.Field.set Q.user_id t 3L)
            ; state (Cistern.Private.Field.set Q.user_id t 2L)
            ; state
                (Cistern.Private.Update.apply
                   (Cistern.Private.Update.set Q.user_id 3L)
                   t )
            ; state
                (Cistern.Private.Update.apply
                   (Cistern.Private.Update.set Q.user_id 2L)
                   t ) ] ;
          (* The key changes in each case. *)
          equal int64 3L (Cistern.Private.Field.set Q.user_id t 3L).user_id )
    ; test "Encode.reset compares the encoded keys" (fun () ->
          let e = Cistern.Private.Encode.int64 in
          let l = Deferred.Loaded "p" in
          equal bool true
            ( Cistern.Private.Encode.reset e 1L 1L l = l
            && Cistern.Private.Encode.reset e 1L 2L l = Deferred.Unloaded
            && Cistern.Private.Encode.reset Cistern.Private.Encode.float 0.0
                 (-0.0) l
               = Deferred.Unloaded ) ) ]
