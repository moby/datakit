open CI_utils
open Lwt.Infix

let ( / ) = Filename.concat

type t = {
  secrets_dir : string;
}

let private_key_path t = t.secrets_dir / "server.key"
let certificate_path t = t.secrets_dir / "server.crt"
let passwords_path t = t.secrets_dir / "passwords.sexp"

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
    let valid_from = { Asn.Time.
                       date = (2016, 07, 25);
                       time = (12, 0, 0, 0.0);
                       tz = None;
                     } in
    let valid_until = { Asn.Time.
                        date = (3000, 01, 01);
                        time = (15, 0, 0, 0.0);
                        tz = None;
                      } in
    let crt = X509.CA.sign csr ~valid_from ~valid_until private_key dn in
    let data = X509.Encoding.Pem.Certificate.to_pem_cstruct1 crt |> Cstruct.to_string in
    Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch data)
  )

let create ~key_bits secrets_dir =
  let t = { secrets_dir } in
  get_private_key ~key_bits (private_key_path t) >>= fun private_key ->
  ensure_crt ~private_key (certificate_path t) >|= fun () ->
  t
