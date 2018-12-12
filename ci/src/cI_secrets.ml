open Sexplib.Std
open CI_utils
open Lwt.Infix

let ( / ) = Filename.concat

type t = {
  secrets_dir : string;
}

type github_auth = {
  client_id : string;
  client_secret : string;
  callback : Uri_sexp.t option;
} [@@deriving sexp]

class type ['a] disk_secret = object
  method read : 'a option Lwt.t
  method write : 'a option -> unit Lwt.t
end

type 'a secret = {
  mutable value : 'a option;
  conv : 'a disk_secret;
  lock : Lwt_mutex.t;
}

let private_key_path t = t.secrets_dir / "server.key"
let certificate_path t = t.secrets_dir / "server.crt"
let passwords_path t = t.secrets_dir / "passwords.sexp"
let github_auth_path t = t.secrets_dir / "github.sexp"

let get_private_key ~key_bits path =
  if Sys.file_exists path then (
    Lwt_io.with_file ~mode:Lwt_io.input path (fun ch -> Lwt_io.read ch) >|= fun data ->
    X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string data)
  ) else (
    Log.info (fun f -> f "Generating new private key...");
    let priv = `RSA (Nocrypto.Rsa.generate key_bits) in
    let data = X509.Encoding.Pem.Private_key.to_pem_cstruct1 priv |> Cstruct.to_string in
    Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch data) >|= fun () ->
    priv
  )

let ensure_crt ~private_key path =
  if Sys.file_exists path then Lwt.return ()
  else (
    let dn = [`CN "DataKitCI"] in
    let csr = X509.CA.request dn private_key in
    let valid_from = Ptime.of_date_time ((2016, 07, 25), ((12, 0, 0), 0)) in
    let valid_from = opt_get (fun () -> assert false) valid_from in
    let valid_until = Ptime.of_date_time ((3000, 01, 01), ((15, 0, 0), 0)) in
    let valid_until = opt_get (fun () -> assert false) valid_until in
    let crt = X509.CA.sign csr ~valid_from ~valid_until private_key dn in
    let data = X509.Encoding.Pem.Certificate.to_pem_cstruct1 crt |> Cstruct.to_string in
    Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch data)
  )

let secret conv =
  conv#read >|= fun value ->
  let lock = Lwt_mutex.create () in
  { value; conv; lock }

let const value =
  let conv = object method read = assert false method write = failwith "const" end in
  let lock = Lwt_mutex.create () in
  { value; conv; lock }

let github_auth t =
  let path = github_auth_path t in
  secret @@ object
    method read =
      match Sys.file_exists path with
      | false -> Lwt.return None
      | true ->
        Lwt_io.with_file ~mode:Lwt_io.input path (fun ch -> Lwt_io.read ch) >|= fun data ->
        Some (github_auth_of_sexp (Sexplib.Sexp.of_string data))

    method write = function
      | None ->
        if Sys.file_exists path then Unix.unlink path;
        Lwt.return_unit
      | Some settings ->
        let data = Sexplib.Sexp.to_string (sexp_of_github_auth settings) in
        Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch data)
  end

let get secret = secret.value

let set secret value =
  Lwt_mutex.with_lock secret.lock @@ fun () ->
  secret.conv#write value >|= fun () ->
  secret.value <- value

let create ~key_bits secrets_dir =
  let t = { secrets_dir } in
  get_private_key ~key_bits (private_key_path t) >>= fun private_key ->
  ensure_crt ~private_key (certificate_path t) >|= fun () ->
  t
