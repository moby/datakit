module Sync: Git.Sync.IO
module Zlib: Git.Inflate.S
module Lock: Irmin_git.LOCK
module FS: Git.FS.IO
module Poll: sig
  val install_dir_polling_listener: float -> unit
end
