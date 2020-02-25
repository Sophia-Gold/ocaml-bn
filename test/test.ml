let bytes = Alcotest.testable Fmt.(on_bytes (octets ())) Bytes.equal

let reserialize_fr () =
  let one = (Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001") in
  Alcotest.(check bytes)
    "same bytes"
    one
    (Bn.fr_to_int (Result.get_ok (Bn.fr_of_int one)))

let reserialize_g1 () =
  let one = ((Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001"),
             (Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000002")) in
  Alcotest.(check (pair bytes bytes))
    "same bytes"
    one
    (Bn.g1_to_pair (Result.get_ok (Bn.g1_of_pair one)))

let reserialize_g2 () =
  let one = (((Bytes.of_string "1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed"),
              (Bytes.of_string "198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2")),
             ((Bytes.of_string "12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"),
              (Bytes.of_string "090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b"))) in
  Alcotest.(check (pair (pair bytes bytes) (pair bytes bytes)))
    "same bytes"
    one
    (Bn.g2_to_pair (Result.get_ok (Bn.g2_of_pair one)))

let fr_malformed () =
  Alcotest.(check string)
    "error"
    "Malformed input"
    (Result.get_error (Bn.fr_of_int (Bytes.of_string "0")))

let fr_error () =
  Alcotest.(check string)
    "error"
    "Not in field"
    (Result.get_error (Bn.fr_of_int (Bytes.of_string "f000000000000000000000000000000000000000000000000000000000000000")))

let g1_error () =
  Alcotest.(check string)
    "error"
    "Point not on curve"
    (Result.get_error (Bn.g1_of_pair ((Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001"),
                                      (Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001"))))

let g2_error () =
  Alcotest.(check string)
    "error"
    "Point not on curve"
    (Result.get_error (Bn.g2_of_pair (((Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001"),
                                       (Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001")),
                                      ((Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001"),
                                       (Bytes.of_string "0000000000000000000000000000000000000000000000000000000000000001")))))

let add_identity () =
  let p = Bn.g1_rand () in
  Alcotest.(check (pair bytes bytes))
    "same bytes"
    (Bn.g1_to_pair p)
    (Bn.g1_to_pair (Bn.add (Bn.g1_zero ()) p))

let mul_identity () =
  Alcotest.(check (pair bytes bytes))
    "same bytes"
    (Bn.g1_to_pair (Bn.g1_zero ()))
    (Bn.g1_to_pair (Bn.mul (Bn.g1_zero ()) (Bn.fr_rand ())))

let g2_add_identity () =
  let p = Bn.g2_rand () in
  Alcotest.(check (pair (pair bytes bytes) (pair bytes bytes)))
    "same bytes"
    (Bn.g2_to_pair p)
    (Bn.g2_to_pair (Bn.g2_add (Bn.g2_zero ()) p))

let g2_mul_identity () =
  Alcotest.(check (pair (pair bytes bytes) (pair bytes bytes)))
    "same bytes"
    (Bn.g2_to_pair (Bn.g2_zero ()))
    (Bn.g2_to_pair (Bn.g2_mul (Bn.g2_zero ()) (Bn.fr_rand ())))

let pairing () =
  Alcotest.(check bool)
    "true"
    true
    (Bn.pairing [((Bn.g1_zero ()), (Bn.g2_rand ()))])

let add_stresstest () =
  let p = ref (Bn.g1_rand ()) in
  Alcotest.(check bool)
    "doesn't blow up"
    true
    (for _i = 0 to 1000 do
       p := Bn.add !p (Bn.g1_rand ())
     done;
     true)

let mul_stresstest () =
  let s = Bn.fr_rand () in
  let p = ref (Bn.g1_rand ()) in
  Alcotest.(check bool)
    "doesn't blow up"
    true
    (for _i = 0 to 1000 do
       p := Bn.mul !p s
     done;
     true)

let g2_add_stresstest () =
  let p = ref (Bn.g2_rand ()) in
  Alcotest.(check bool)
    "doesn't blow up"
    true
    (for _i = 0 to 100 do
       p := Bn.g2_add !p (Bn.g2_rand ())
     done;
     true)

let g2_mul_stresstest () =
  let s = Bn.fr_rand () in
  let p = ref (Bn.g2_rand ()) in
  Alcotest.(check bool)
    "doesn't blow up"
    true
    (for _i = 0 to 100 do
       p := Bn.g2_mul !p s
     done;
     true)

let tests = [
    "reserialize fr",                        `Quick, reserialize_fr ;
    "reserialize g1",                        `Quick, reserialize_g1 ;
    "reserialize g2",                        `Quick, reserialize_g2 ;
    "fr malformed input",                    `Quick, fr_malformed ;
    "fr too large",                          `Quick, fr_error ;
    "g1 not on curve",                       `Quick, g1_error ;
    "g2 not on curve",                       `Quick, g2_error ;
    "g1 additive identity",                  `Quick, add_identity ;
    "g1 multiplicative identity",            `Quick, mul_identity ;
    "g2 additive identity",                  `Quick, g2_add_identity ;
    "g2 multiplicative identity",            `Quick, g2_mul_identity ;
    "pairing check",                         `Quick, pairing ;
    "g1 addition memory stress test",        `Slow, add_stresstest ;
    "g1 multiplication memory stress test",  `Slow, mul_stresstest ;
    "g2 addition memory stress test",        `Slow, g2_add_stresstest ;
    "g2 multiplication memory stress test",  `Slow, g2_mul_stresstest ;
  ]

let () =
  Alcotest.run "dmz" [
      "tests", tests;
    ]
