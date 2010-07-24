let version = "0.3"

let path = 
  try (Sys.getenv "SYNCWEB_HOME") 
  with Not_found->"/usr/local/share/syncweb"

let std_xxx = ref (Filename.concat path "xxx.yyy")
