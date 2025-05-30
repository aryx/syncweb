
let foo x =
  x + x

let bar x =
  x + x
[@@profiling]

let foo () =
  (
    some big call
  )

(* comment before *)
let foo () =
  ()

