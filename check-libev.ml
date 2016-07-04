(* Make sure we have libev, or we'll crash from time-to-time with EINVAL in select *)
#use "topfind";;
#require "lwt.unix";;
Lwt_engine.set (new Lwt_engine.libev :> Lwt_engine.t);;
