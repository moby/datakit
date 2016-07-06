(** DataKit client API *)

open Result

type stat = {
  kind : [`File | `Dir | `Link | `Exec];
  size : int64;
}

type status_state =
  [ `Pending
  | `Success
  | `Error
  | `Failure ]

module type READABLE_TREE = sig
  type t
  type +'a or_error

  val read : t -> Datakit_path.t ->
    [`File of Cstruct.t | `Dir of string list | `Link of string] or_error Lwt.t
  (** [read t path] is the contents of the object at the [path]. *)

  val stat : t -> Datakit_path.t -> stat option or_error Lwt.t
  (** [stat t path] is the metadata of the object at [path]. *)

  val exists : t -> Datakit_path.t -> bool or_error Lwt.t
  (** [exists t path] is [true] if [stat t path] isn't [None]. *)

  val exists_file : t -> Datakit_path.t -> bool or_error Lwt.t
  (** [exists_file t path] is similar to {!exists} but for files
      only. *)

  val exists_dir : t -> Datakit_path.t -> bool or_error Lwt.t
  (** [exists_dir t path] is similar to {!exists} but for directories
      only. *)

  val read_file : t -> Datakit_path.t -> Cstruct.t or_error Lwt.t
  (** [read_file t path] resolves [path] to a file, or returns an
      error if it isn't a file. *)

  val read_dir : t -> Datakit_path.t -> string list or_error Lwt.t
  (** [read_dir t path] resolves [path] to a directory, or returns an
      error if it isn't one. *)

  val read_link : t -> Datakit_path.t -> string or_error Lwt.t
  (** [read_link t path] resolves [path] to a symlink, or returns an
      error if it isn't one. *)
end

module type CLIENT = sig
  type t
  (** A [t] is a connection to a Datakit server. *)

  type error

  val pp_error: error Fmt.t
  (** [pp_error] pretty-prints error values. *)

  type 'a or_error = ('a, error) result

  val error: ('a, unit, string, 'b or_error Lwt.t) format4 -> 'a
  (** [error] raises user-defined errors. *)

  module Tree : READABLE_TREE with type 'a or_error := 'a or_error
  (** A read-only tree of files, directories and symlinks. *)

  module Commit : sig
    type t
    (** A [t] is an immutable commit in the database. *)

    val tree : t -> Tree.t
    (** [tree t] is the content of the commit. *)

    val id : t -> string
    (** [id t] is the unique ID of this commit. *)

    val message : t -> string or_error Lwt.t
    (** [message t] is [t]'s log message. *)

    val parents : t -> t list or_error Lwt.t
    (** [parents t] is the list of [t]'s parent commits. *)
  end

  module Transaction : sig
    (** All changes to a branch are made in transactions. When a
        transaction is committed, it is merged with the current
        contents of the branch. *)

    (** {2 Reading} *)

    include READABLE_TREE with type 'a or_error := 'a or_error

    (** {2 Writing} *)

    val create_dir : t -> dir:Datakit_path.t -> string -> unit or_error Lwt.t
    (** [create_dir t ~dir name] creates the directory [dir/name]. *)

    val create_file : t -> dir:Datakit_path.t -> string -> ?executable:bool ->
      Cstruct.t -> unit or_error Lwt.t
    (** [create_file t ~dir name ?executable content] creates the file
        [dir/name]. *)

    val create_symlink : t -> dir:Datakit_path.t -> string -> string ->
      unit or_error Lwt.t
    (** [create_symlink t ~dir name target] creates the symlink
        [dir/name]. *)

    val replace_file : t -> dir:Datakit_path.t -> string -> Cstruct.t ->
      unit or_error Lwt.t
    (** [replace_file t ~dir name new_content] changes the content of
        the existing file [dir/name]. *)

    val create_or_replace_file : t -> dir:Datakit_path.t -> string -> Cstruct.t ->
      unit or_error Lwt.t
    (** [create_or_replace_file t ~dir leaf content] uses either [create_file]
        or [replace_file] as appropriate to set the contents. *)

    val set_executable : t -> Datakit_path.t -> bool -> unit or_error Lwt.t
    (** [set_executable t path flag] marks the file at [path] as
        executable or not. *)

    val remove : t -> Datakit_path.t -> unit or_error Lwt.t
    (** [remove t path] removes [path]. If [path] is a directory then
        the entire subtree is removed. *)

    val rename : t -> Datakit_path.t -> string -> unit or_error Lwt.t
    (** [rename t path new_name] changes the basename of [path] to
        [new_name].  Note: it is only possible to rename within a
        directory (this is a 9p limitation). *)

    val truncate : t -> Datakit_path.t -> int64 -> unit or_error Lwt.t
    (** [truncate t path length] sets the length of the file at [path]
        to [length].  If [length] is longer than the current length,
        the file is padded with zero bytes. *)

    val make_dirs : t -> Datakit_path.t -> unit or_error Lwt.t
    (** [make_dirs t path] ensures that [path] exists and is a
        directory, creating it and any missing parents as
        necessary. *)

    (** {2 Finishing} *)

    val commit : t -> message:string -> unit or_error Lwt.t
    (** [commit t ~message] creates a new commit with the given log
        message and the current contents of the transaction and then
        merges it into the branch from which the transaction was
        created.  The transaction cannot be used after calling
        this. *)

    val abort : t -> unit Lwt.t
    (** [abort t] aborts the transaction without committing it.  The
        transaction cannot be used after calling this. *)

    (** {2 Merging and history} *)

    type merge_inputs = {
      ours : Tree.t;
      theirs : Tree.t;
      base : Tree.t;
    }
    (** When performing a merge, these three directories can be used
        to calculate the final result.  [ours] is the previous
        contents of the transaction, [theirs] is the commit being
        merged and [base] is a least common ancestor. If there is no
        common ancestor then [base] is an empty tree. *)

    val merge : t -> Commit.t -> (merge_inputs * Datakit_path.t list) or_error Lwt.t
    (** [merge t commit] merges [commit] into the transaction. It
        performs any trivial merges it can and returns [(merge_inputs,
        conflicts)] to allow you to resolve the remaining ones. You
        must write to each path in [conflicts] at least once before
        you can commit the transaction.  You may perform multiple
        merges in one transaction, but the [merge_inputs] returned is
        only valid until the start of the next merge. *)

    val parents : t -> Commit.t list or_error Lwt.t
    (** [parents t] is the parents of the transaction (that is, the parents of
        the commit that would be generated if you committed now. *)

    val set_parents : t -> Commit.t list -> unit or_error Lwt.t
    (** [set_parents t new_parents] replaces the current list of
        parents.  Note that this does not perform a merge - it only
        affects the metadata.  Note also that [merge] automatically
        updates the parents, so it is not necessary to call it
        manually in that case. *)

    val conflicts : t -> Datakit_path.t list or_error Lwt.t
    (** [conflicts t] returns the current list of paths that had merge
        conflicts and have not been written to since.  It is not
        possible to commit while this is non-empty. *)
  end

  module Branch : sig
    type t
    (** A [t] is a named pointer to a commit. *)

    val name : t -> string
    (** [name t] is [t]'s name. *)

    val remove : t -> unit or_error Lwt.t
    (** [remove t] deletes branch [t]. If [t] does not exist, this
        does nothing. *)

    val rename : t -> string -> unit or_error Lwt.t
    (** [rename t new_name] changes the name of this branch. It is an
        error if [new_name] already exists. *)

    val head : t -> Commit.t option or_error Lwt.t
    (** [head t] is the current head of the branch, or [None] if it
        doesn't currently exist. *)

    val wait_for_head : t -> ?switch:Lwt_switch.t ->
      (Commit.t option -> [`Finish of 'a | `Again | `Abort] or_error Lwt.t) ->
      [`Abort | `Finish of 'a] or_error Lwt.t
    (** [wait_for_head t fn] calls [fn commit] on the current
        commit. If it returns [`Again] then it waits for the commit to
        change and tries again.  If [switch] is provided, then turning
        off the switch will make the wait return [`Abort] at the next
        opportunity. *)

    val wait_for_path : t -> ?switch:Lwt_switch.t -> Datakit_path.t ->
      ([`File of Cstruct.t | `Dir of Tree.t
       | `Link of string | `Exec of Cstruct.t] option ->
       [`Finish of 'a | `Again | `Abort] or_error Lwt.t) ->
      [`Abort | `Finish of 'a] or_error Lwt.t
    (** [wait_for_path] is similar to [wait_for_head], but waits for a
        particular sub-tree to change. *)

    val fast_forward : t -> Commit.t -> unit or_error Lwt.t
    (** [fast_forward t commit] changes [t] to point to [commit] if
        [t]'s head is an ancestor of [commit] (or returns an error if
        not). *)

    val with_transaction : t -> (Transaction.t -> 'a or_error Lwt.t) ->
      'a or_error Lwt.t
    (** [with_transaction t fn] is the result of applying [fn] to a
        new transaction on branch [t].  If the transaction has not
        been committed when [fn trans] returns, the transaction is
        aborted (and a warning is logged). Use [Transaction.abort] to
        avoid the warning. *)
  end

  module GitHub : sig
    type t

    module Status : sig
      type t
      (** A PR status (e.g. CI build status). *)

      val state : t -> status_state option or_error Lwt.t
      val set_state : t -> status_state option -> unit or_error Lwt.t

      val url : t -> Uri.t option or_error Lwt.t
      val set_url : t -> Uri.t option -> unit or_error Lwt.t

      val descr : t -> string option or_error Lwt.t
      val set_descr : t -> string option -> unit or_error Lwt.t
    end

    module PR : sig
      type t
      (** A GitHub Pull Request *)

      val id : t -> string
      (** [id t] is the GitHub identifier for the PR. *)

      val status : t -> Datakit_path.t -> Status.t or_error Lwt.t
    end

    val prs : t -> user:string -> project:string -> PR.t list or_error Lwt.t
    (** [prs t ~user ~project] is the list of PRs under "github.com/user/project" *)

    val pr : t -> user:string -> project:string -> string -> PR.t or_error Lwt.t
    (** [pr t ~user ~project id] is PR [id] under "github.com/user/project" *)

    (* TODO
       val watch_prs : t -> ?switch:Lwt_switch.t -> (PR.t -> unit Lwt.t) -> unit Lwt.t
       (** [watch_prs t ~switch fn] calls [fn pr] on each updated PR until the switch is
           turned off. *)
     *)
  end

  val branches : t -> string list or_error Lwt.t
  (** [branches t] is the current set of branches. *)

  val remove_branch : t -> string -> unit or_error Lwt.t
  (** [remove_branch t name] removes the branch named [name] (unlike
      [Branch.remove], this method doesn't require creating the branch
      directory first). *)

  val branch : t -> string -> Branch.t or_error Lwt.t
  (** [branch t name] is the branch named [name] (which might not exist yet). *)

  val commit : t -> string -> Commit.t
  (** [commit t hash] is the commit with ID [hash]. *)

  val tree : t -> string -> Tree.t
  (** [tree t id] is the tree with ID [id]. *)

  val fetch : t -> url:string -> branch:string -> Commit.t or_error Lwt.t
  (** [fetch t ~url ~branch] fetches the given remote branch and
      returns its head commit. *)

  val github : t -> GitHub.t option or_error Lwt.t
  (** [github t] is the GitHub API client for [t], if [t] supports
      [GitHub] integration. *)

  val disconnect : t -> unit Lwt.t
  (** [disconnect t] closes the connection. [t] cannot be used after
      this. *)
end
