open Common


let _biglock = Mutex.create () 
let atomic f = 
  Mutex.lock _biglock;
  finalize f (fun () -> Mutex.unlock _biglock)
  
