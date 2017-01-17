open! Astring
open Prometheus
open Prometheus_app

let test_metrics () =
  let registry = CollectorRegistry.create () in
  let requests =
    let label_names = [| "method"; "path" |] in
    Counter.v_labels ~label_names ~registry ~help:"Requests" ~namespace:"dkci" ~subsystem:"tests" "requests" in
  let m = Counter.v ~registry ~help:"Test \\counter:\n1" "tests" in
  Counter.inc_one m;
  let get_index = Counter.labels requests [| "GET"; "\"\\-\n" |] in
  let post_login = Counter.labels requests [| "POST"; "/login" |] in
  Counter.inc get_index 5.;
  Counter.inc post_login 2.;
  let post_login2 = Counter.labels requests [| "POST"; "/login" |] in
  Counter.inc_one post_login2;
  let output = Fmt.to_to_string TextFormat_0_0_4.output (CollectorRegistry.collect registry) in
  Alcotest.(check string) "Text output"
    "#HELP dkci_tests_requests Requests\n\
     #TYPE dkci_tests_requests counter\n\
     dkci_tests_requests{method=\"GET\", path=\"\\\"\\\\-\\n\"} 5\n\
     dkci_tests_requests{method=\"POST\", path=\"/login\"} 3\n\
     #HELP tests Test \\\\counter:\\n1\n\
     #TYPE tests counter\n\
     tests 1\n\
    "
    output

let test_set = [
  "Metrics", `Quick, test_metrics;
]

let () =
  Alcotest.run "prometheus" [
    "main", test_set;
  ]
