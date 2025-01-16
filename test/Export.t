  $ cd forest

Export the forest defined by `export.toml`

  $ forester export export.toml
   ￮ Parse trees...
  
   ￮ Expand, evaluate and analyse forest...
  
   ￮ Exporting forest...
  
  $ ls export
  foreign.json

Verify that the foreign blob can be implanted.

  $ forester build forest.toml
   ￮ Implant foreign forest from `./export/foreign.json'...
  
   ￮ Parse trees...
  
   ￮ Expand, evaluate and analyse forest...
  
  Fatal error: exception Eio.Io Fs Permission_denied Unix_error (Invalid cross-device link, "openat2", ""),
    opening <cwd:output/forest.json>
  Raised at Stdlib__Effect.Deep.try_with.(fun) in file "effect.ml", line 101, characters 47-54
  Called from Eio_unix__Thread_pool.run in file "lib_eio/unix/thread_pool.ml", line 108, characters 8-13
  Re-raised at Eio_unix__Thread_pool.run in file "lib_eio/unix/thread_pool.ml", line 113, characters 4-39
  Called from Eio_linux__Sched.run.(fun) in file "lib_eio_linux/sched.ml", line 468, characters 22-90
  Re-raised at Eio_linux__Sched.run in file "lib_eio_linux/sched.ml", line 479, characters 22-57
  Called from Eio_linux__Sched.with_eventfd in file "lib_eio_linux/sched.ml", line 506, characters 8-18
  Re-raised at Eio_linux__Sched.with_eventfd in file "lib_eio_linux/sched.ml", line 511, characters 4-39
  Called from Eio_linux__Sched.with_sched in file "lib_eio_linux/sched.ml", lines 543-545, characters 8-109
  Re-raised at Eio_linux__Sched.with_sched in file "lib_eio_linux/sched.ml", line 556, characters 8-43
  Called from Dune__exe__Main in file "bin/forester/main.ml", lines 396-398, characters 2-44
  [2]
