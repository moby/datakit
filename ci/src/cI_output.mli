type saved = {
  title : string;
  commit : string;
  branch : string;
  failed : bool;
  rebuild : unit Lwt.t Lazy.t;
}

type logs =
  | Empty
  | Live of CI_live_log.t
  | Saved of saved
  | Pair of logs * logs

type 'a t = 'a CI_result.t * logs

val result : 'a t -> 'a CI_result.t
val logs : 'a t -> logs
val status : _ t -> [`Success | `Pending | `Failure]
val descr : string t -> string
