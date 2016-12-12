type saved = {
  title : string;
  commit : string;
  branch : string;
  failed : bool;
  rebuild : unit Lwt.t Lazy.t;
}

type t =
  | Empty
  | Live of CI_live_log.t
  | Saved of saved
  | Pair of t * t
