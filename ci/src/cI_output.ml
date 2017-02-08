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

let result = fst
let logs = snd

let status t = CI_result.status (result t)
let descr t = CI_result.descr (result t)
