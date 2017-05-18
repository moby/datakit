(** DataKit client API *)

type stat = {
  kind: [`File | `Dir | `Link | `Exec];
  size: int64;
}

type status_state =
  [ `Pending
  | `Success
  | `Error
  | `Failure ]

type 'a diff = [ `Added of 'a | `Removed of 'a | `Updated of 'a ]
(** The type for diffs. *)

type value = [`File of Cstruct.t | `Dir of string list | `Link of string]
(** The type for values. *)

module Path: sig

  (** Locate files and directories within a DataKit tree. *)

  open Result

  type t
  (** A [path] identifies a file or directory (relative to some other directory).
      No component may be empty or contain a '/' character. "." and ".." steps
      are not permitted in a path. *)

  val empty : t
  (** The empty path. *)

  val of_steps : string list -> (t, string) result
  (** Converts a list of the form ["a"; "b"; "c"] to a path. *)

  val of_steps_exn : string list -> t
  (** Converts a list of the form ["a"; "b"; "c"] to a path. *)

  val of_string : string -> (t, string) result
  (** Converts a path of the form ["a/b/c"] to a path. *)

  val of_string_exn : string -> t

  val unwrap : t -> string list
  (** Cast to a list of strings *)

  val pop : t -> (t * string) option
  (** [pop (dir / leaf)] is [Some (dir, leaf)].
      [pop empty] is [None]. *)

  val pp : t Fmt.t
  (** [pp] is a formatter for human-readable paths. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for paths. *)

  val to_hum : t -> string
  (** Convert to a string, in the same format as [pp]. *)

  val basename: t -> string option
  (** [basename t] is [t]'s basename. *)

  val dirname: t -> t
  (** [dirname t] is [t]'s dirname. *)

  module Set: Set.S with type elt = t
  (** Sets of paths. *)

  module Map: Map.S with type key = t
  (** Maps of paths. *)

  module Infix: sig

    val ( / ) : t -> string -> t
    (** [a / b] is the path [a] with step [b] appended. Raises an
        exception if [b] is not a valid step, so this should only be
        used with string constants, not user input. *)

    val ( /@ ) : t -> t -> t
    (** [a /@ b] is the concatenation of paths [a] and [b]. *)

  end

end

module type READABLE_TREE = sig

  type t
  (** The type for trees. *)

  type +'a result
  (** The type for results. *)

  val read: t -> Path.t -> value result
  (** [read t path] is the contents of the object at the [path]. *)

  val stat: t -> Path.t -> stat option result
  (** [stat t path] is the metadata of the object at [path]. *)

  val exists: t -> Path.t -> bool result
  (** [exists t path] is [true] if [stat t path] isn't [None]. *)

  val exists_file: t -> Path.t -> bool result
  (** [exists_file t path] is similar to {!exists} but for files
      only. *)

  val exists_dir: t -> Path.t -> bool result
  (** [exists_dir t path] is similar to {!exists} but for directories
      only. *)

  val read_file: t -> Path.t -> Cstruct.t result
  (** [read_file t path] resolves [path] to a file, or returns an
      error if it isn't a file. *)

  val read_dir: t -> Path.t -> string list result
  (** [read_dir t path] resolves [path] to a directory, or returns an
      error if it isn't one. *)

  val read_link: t -> Path.t -> string result
  (** [read_link t path] resolves [path] to a symlink, or returns an
      error if it isn't one. *)
end

module type S = sig

  type t
  (** A [t] is a connection to a Datakit server. *)

  type error = private
    [>`Already_exists           (** Attempt to create something that already exists *)
    | `Does_not_exist           (** Attempt to access something that does not exist *)
    | `Is_dir                   (** Attempt to use a directory as a file *)
    | `Not_dir                  (** Attempt to use a non-directory as a directory *)
    | `Not_file                 (** Attempt to use a non-file as a file *)
    | `Not_symlink]             (** Attempt to use a non-symlink as a symlink *)

  val pp_error: error Fmt.t
  (** [pp_error] pretty-prints error values. *)

  type +'a result = ('a, error) Result.result Lwt.t

  (** Infix operators for client results. *)
  module Infix: sig
    val (>>=): 'a result -> ('a -> 'b result) -> 'b result
    val (>|=): 'a result -> ('a -> 'b) -> 'b result
  end

  module Tree: READABLE_TREE with type 'a result := 'a result
  (** A read-only tree of files, directories and symlinks. *)

  module Commit: sig
    type t
    (** A [t] is an immutable commit in the database. *)

    val pp: t Fmt.t
    (** [pp] is the pretty-printer for commits IDs. *)

    val compare: t -> t -> int
    (** [compare] compares commit IDs. *)

    val id: t -> string
    (** [id t] is the unique ID of this commit. *)

    val tree: t -> Tree.t result
    (** [tree t] is the content of the commit. *)

    val message: t -> string result
    (** [message t] is [t]'s log message. *)

    val parents: t -> t list result
    (** [parents t] is the list of [t]'s parent commits. *)

    val diff: t -> t -> Path.t diff list result
    (** [diff a b] returns the paths with differences between [a] and [b]. *)
  end

  module Transaction: sig
    (** All changes to a branch are made in transactions. When a
        transaction is committed, it is merged with the current
        contents of the branch. *)

    (** {2 Reading} *)

    include READABLE_TREE with type 'a result := 'a result

    (** {2 Writing} *)

    val create_dir: t -> Path.t -> unit result
    (** [create_dir t path] creates the directory [path]. *)

    val create_file: t -> Path.t -> ?executable:bool ->
      Cstruct.t -> unit result
    (** [create_file t path ?executable content] creates the file
        [path]. *)

    val create_symlink: t -> Path.t -> string -> unit result
    (** [create_symlink t path target] creates the symlink [path]. *)

    val replace_file: t -> Path.t -> Cstruct.t -> unit result
    (** [replace_file t path new_content] changes the content of
        the existing file [path]. *)

    val create_or_replace_file: t -> Path.t -> Cstruct.t -> unit result
    (** [create_or_replace_file t path content] uses either [create_file]
        or [replace_file] as appropriate to set the contents. *)

    val set_executable: t -> Path.t -> bool -> unit result
    (** [set_executable t path flag] marks the file at [path] as
        executable or not. *)

    val remove: t -> Path.t -> unit result
    (** [remove t path] removes [path]. If [path] is a directory then
        the entire subtree is removed. *)

    val truncate: t -> Path.t -> int64 -> unit result
    (** [truncate t path length] sets the length of the file at [path]
        to [length].  If [length] is longer than the current length,
        the file is padded with zero bytes. *)

    val make_dirs: t -> Path.t -> unit result
    (** [make_dirs t path] ensures that [path] exists and is a
        directory, creating it and any missing parents as
        necessary. *)

    (** {2 Finishing} *)

    val commit: t -> message:string -> unit result
    (** [commit t ~message] creates a new commit with the given log
        message and the current contents of the transaction and then
        merges it into the branch from which the transaction was
        created.  The transaction cannot be used after calling
        this. *)

    val abort: t -> unit result
    (** [abort t] aborts the transaction without committing it.  The
        transaction cannot be used after calling this. *)

    (** {2 Merging and history} *)

    type merge_inputs = {
      ours: Tree.t;
      theirs: Tree.t;
      base: Tree.t;
    }
    (** When performing a merge, these three directories can be used
        to calculate the final result.  [ours] is the previous
        contents of the transaction, [theirs] is the commit being
        merged and [base] is a least common ancestor. If there is no
        common ancestor then [base] is an empty tree. *)

    val merge: t -> Commit.t -> (merge_inputs * Path.t list) result
    (** [merge t commit] merges [commit] into the transaction. It
        performs any trivial merges it can and returns [(merge_inputs,
        conflicts)] to allow you to resolve the remaining ones. You
        must write to each path in [conflicts] at least once before
        you can commit the transaction.  You may perform multiple
        merges in one transaction, but the [merge_inputs] returned is
        only valid until the start of the next merge. *)

    val parents: t -> Commit.t list result
    (** [parents t] is the parents of the transaction (that is, the parents of
        the commit that would be generated if you committed now. *)

    val set_parents: t -> Commit.t list -> unit result
    (** [set_parents t new_parents] replaces the current list of
        parents.  Note that this does not perform a merge - it only
        affects the metadata.  Note also that [merge] automatically
        updates the parents, so it is not necessary to call it
        manually in that case. *)

    val conflicts: t -> Path.t list result
    (** [conflicts t] returns the current list of paths that had merge
        conflicts and have not been written to since.  It is not
        possible to commit while this is non-empty. *)

    val diff: t -> Commit.t -> Path.t diff list result
    (** [diff t c] returns the paths differences between [c] and [t]'s
        head. *)

    val closed: t -> bool
    (** [closed t] is true if [t] is closed and thus it is not valid
        to read/write on it anymore. *)

  end

  module Branch: sig

    type t
    (** A [t] is a named pointer to a commit. *)

    val name: t -> string
    (** [name t] is [t]'s name. *)

    val remove: t -> unit result
    (** [remove t] deletes branch [t]. If [t] does not exist, this
        does nothing. *)

    val head: t -> Commit.t option result
    (** [head t] is the current head of the branch, or [None] if it
        doesn't currently exist. *)

    val wait_for_head: t -> ?switch:Lwt_switch.t ->
      (Commit.t option -> [`Finish of 'a | `Again | `Abort] result) ->
      [`Abort | `Finish of 'a] result
    (** [wait_for_head t fn] calls [fn commit] on the current
        commit. If it returns [`Again] then it waits for the commit to
        change and tries again.  If [switch] is provided, then turning
        off the switch will make the wait return [`Abort] at the next
        opportunity. *)

    val wait_for_path: t -> ?switch:Lwt_switch.t -> Path.t ->
      ([`File of Cstruct.t | `Dir of Tree.t
       | `Link of string | `Exec of Cstruct.t] option ->
       [`Finish of 'a | `Again | `Abort] result) ->
      [`Abort | `Finish of 'a] result
    (** [wait_for_path] is similar to [wait_for_head], but waits for a
        particular sub-tree to change. *)

    val fast_forward: t -> Commit.t -> unit result
    (** [fast_forward t commit] changes [t] to point to [commit] if
        [t]'s head is an ancestor of [commit] (or returns an error if
        not). *)

    val with_transaction: t -> (Transaction.t -> 'a result) -> 'a result
    (** [with_transaction t fn] is the result of applying [fn] to a
        new transaction on branch [t].  If the transaction has not
        been committed when [fn trans] returns, the transaction is
        aborted (and a warning is logged). Use [Transaction.abort] to
        avoid the warning. *)

    val transaction: t -> Transaction.t result
    (** [transaction t] creates a new transaction on top of the branch
        [t]. Must be closed with {!Transaction.abort} or
        {!Transaction.commit}. Use {!with_transaction} to not have to
        worry about resource leaks.. *)

  end

  val branches: t -> string list result
  (** [branches t] is the current set of branches. *)

  val remove_branch: t -> string -> unit result
  (** [remove_branch t name] removes the branch named [name] (unlike
      [Branch.remove], this method doesn't require creating the branch
      directory first). *)

  val branch: t -> string -> Branch.t result
  (** [branch t name] is the branch named [name] (which might not exist yet). *)

  val commit: t -> string -> Commit.t result
  (** [commit t hash] is the commit with ID [hash]. *)

  val tree: t -> string -> Tree.t result
  (** [tree t id] is the tree with ID [id]. *)

  val fetch: t -> url:string -> branch:string -> Commit.t result
  (** [fetch t ~url ~branch] fetches the given remote branch and
      returns its head commit. *)

  val disconnect: t -> unit result
  (** [disconnect t] closes the connection. [t] cannot be used after
      this. *)
end
