extern crate dmz;
extern crate bn;
extern crate rand;
extern crate bincode;
extern crate rustc_serialize;

use dmz::*;
use std::io::{self, Write};
use bn::*;

use bincode::SizeLimit::Infinite;
use bincode::rustc_serialize::{encode, decode};
use rustc_serialize::{Encodable, Decodable};
use rustc_serialize::hex::{FromHex, ToHex};

fn into_hex<S: Encodable>(obj: S) -> core::option::Option<String> {
    encode(&obj, Infinite).ok().map(|e| e.to_hex())
}

fn from_hex<S: Decodable>(s: &str) -> std::result::Result<S, std::string::String> {
    match s.from_hex() {
        Ok(s) => match decode(&s) {
            Ok(s) => Ok(s),
            Err(_) => Err("Not in field".to_string()),
        },
        Err(_) => Err("Malformed input".to_string()),
    }
}

pub struct Fr(String);
impl StringNewtype for Fr {
    fn as_string(a: Fr) -> String {
        a.0
    }
    fn to_string(a: String) -> Fr {
        Fr(a)
    }
}
impl<'a> MLType for Fr {
    fn name() -> String {
        "fr".to_owned()
    }

    fn type_def() -> String {
        "type fr".to_owned()
    }
}

pub struct G1(String);
impl StringNewtype for G1 {
    fn as_string(a: G1) -> String {
        a.0
    }
    fn to_string(a: String) -> G1 {
        G1(a)
    }
}
impl MLType for G1 {
    fn name() -> String {
        "g1".to_owned()
    }

    fn type_def() -> String {
        "type g1".to_owned()
    }
}

pub struct G2(String);
impl StringNewtype for G2 {
    fn as_string(a: G2) -> String {
        a.0
    }
    fn to_string(a: String) -> G2 {
        G2(a)
    }
}
impl MLType for G2 {
    fn name() -> String {
        "g2".to_owned()
    }

    fn type_def() -> String {
        "type g2".to_owned()
    }
}

pub struct Gt(String);
impl StringNewtype for Gt {
    fn as_string(a: Gt) -> String {
        a.0
    }
    fn to_string(a: String) -> Gt {
        Gt(a)
    }
}
impl MLType for Gt {
    fn name() -> String {
        "gt".to_owned()
    }

    fn type_def() -> String {
        "type gt".to_owned()
    }
}

camlmod!{
    fn fr_of_int(gc, a: String) -> Result<Fr> {
        match from_hex::<bn::Fr>(a.as_str()) {
            Ok(a) => call!{ alloc_ok(gc, call!{ alloc_string_newtype(gc, into_hex(a).unwrap()) }) },
            Err(a) => call!{ alloc_error(gc, call!{ alloc_string(gc, &a) }) },
        }
    }

    fn fr_to_int(gc, a: Fr) -> String {
        let fr = from_hex::<bn::Fr>(&as_string_newtype(a)[..]).unwrap();
        call!{ alloc_bytes(gc, into_hex(fr).unwrap()) }
    }

    fn fr_zero(gc, _a: ()) -> Fr {
        call!{ alloc_string_newtype(gc, into_hex(bn::Fr::zero()).unwrap()) }
    }
    fn fr_one(gc, _a: ()) -> Fr {
        call!{ alloc_string_newtype(gc, into_hex(bn::Fr::one()).unwrap()) }
    }
    fn fr_rand(gc, _a: ()) -> Fr {
        call!{ alloc_string_newtype(gc, into_hex(bn::Fr::random(&mut rand::thread_rng())).unwrap()) }
    }


    fn g1_of_pair(gc, a: Pair<String, String>) -> Result<G1> {
        match (from_hex::<Fq>(a.fst().as_str()), from_hex::<Fq>(a.snd().as_str())) {
            (Ok(x), Ok(y)) =>
                if x.is_zero() && y.is_zero() {
                    call!{ alloc_ok(gc, call!{ alloc_string_newtype(gc, into_hex(bn::G1::zero()).unwrap()) }) }
                } else {
                    match AffineG1::new(x, y) {
                        Ok(point) => call!{ alloc_ok(gc, call!{ alloc_string_newtype(gc, into_hex(bn::G1::from(point)).unwrap()) }) },
                        _ => call!{ alloc_error(gc, call!{ alloc_string(gc, "Point not on curve") }) }
                    }
                },
            (Ok(_), Err(e)) => call!{ alloc_error(gc, call!{ alloc_string(gc, &format!("Y coordinate: {}", e)) }) },
            (Err(e), Ok(_)) => call!{ alloc_error(gc, call!{ alloc_string(gc, &format!("X coordinate: {}", e)) }) },
            (Err(e_x), Err(e_y)) => call!{ alloc_error(gc, call!{ alloc_string(gc, &format!("X coordinate: {}. Y coordinate: {}.", e_x, e_y)) }) },
        }
    }
    
    fn g1_to_pair(gc, a: G1) -> Pair<String, String> {
        let point = from_hex::<bn::G1>(&as_string_newtype(a)[..]).unwrap();
        
        // is this sound?
        let gc2 = &mut gc.clone();

        let x = call!{ alloc_bytes(gc, into_hex(point.x()).unwrap()) };
        let y = call!{ alloc_bytes(gc2, into_hex(point.y()).unwrap()) };
        
        call!{ alloc_pair(gc, 0, x, y) }
    }

    
    fn g1_zero(gc, _a: ()) -> G1 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G1::zero()).unwrap()) }
    }
    fn g1_one(gc, _a: ()) -> G1 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G1::one()).unwrap()) }
    }
    fn g1_rand(gc, _a: ()) -> G1 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G1::random(&mut rand::thread_rng())).unwrap()) }
    }

    // // a: ((real x, imaginary x), (real y, imaginary y))
    fn g2_of_pair(gc, a: Pair<Pair<String, String>, Pair<String, String>>) -> Result<G2> {
        let mut error = "".to_string();
        let mut real_x = Fq::zero();
        let mut imag_x = Fq::zero();
        let mut real_y = Fq::zero();
        let mut imag_y = Fq::zero();
        
        match from_hex::<Fq>(a.fst().fst().as_str()) {
            Ok(a) => real_x = a,
            Err(e) => error.push_str(&format!("Real part of X: {}. ", e)),
        }
        match from_hex::<Fq>(a.fst().snd().as_str()) {
            Ok(a) => imag_x = a,
            Err(e) => error.push_str(&format!("Imaginary part of X: {}. ", e)),
        }
        match from_hex::<Fq>(a.snd().fst().as_str()) {
            Ok(a) => real_y = a,
            Err(e) => error.push_str(&format!("Real part of Y: {}. ", e)),
        }
        match from_hex::<Fq>(a.snd().snd().as_str()) {
            Ok(a) => imag_y = a,
            Err(e) => error.push_str(&format!("Imaginary part of Y: {}. ", e)),
        }

        if error.is_empty() {
            if real_x.is_zero() && imag_x.is_zero() && real_y.is_zero() && imag_y.is_zero() {
                call!{ alloc_ok(gc, call!{ alloc_string_newtype(gc, into_hex(bn::G2::zero()).unwrap()) }) }
            } else {
                match AffineG2::new(Fq2::new(real_x, imag_x), Fq2::new(real_y, imag_y)) {
                    Ok(point) => call!{ alloc_ok(gc, call!{ alloc_string_newtype(gc, into_hex(bn::G2::from(point)).unwrap()) }) },
                    _ => call!{ alloc_error(gc, call!{ alloc_string(gc, "Point not on curve") }) },
                }
            }
        } else {
            call!{ alloc_error(gc, call!{ alloc_string(gc, &error) }) }
        }
    }
    
    fn g2_to_pair(gc, a: G2) -> Pair<Pair<String, String>, Pair<String, String>> {
        let point = from_hex::<bn::G2>(&as_string_newtype(a)[..]).unwrap();

        // is this sound?
        let gc2 = &mut gc.clone();
        let gc3 = &mut gc.clone();
        let gc4 = &mut gc.clone();

        let real_x = call!{ alloc_bytes(gc, into_hex(point.x().real()).unwrap()) };
        let imag_x = call!{ alloc_bytes(gc2, into_hex(point.x().imaginary()).unwrap()) };

        let real_y = call!{ alloc_bytes(gc3, into_hex(point.y().real()).unwrap()) };
        let imag_y = call!{ alloc_bytes(gc4, into_hex(point.y().imaginary()).unwrap()) };

        call!{ alloc_pair(gc, 0,
                          call!{ alloc_pair(gc, 0, real_x, imag_x) },
                          call!{ alloc_pair(gc2, 0, real_y, imag_y) })
        }
    }

    fn g2_zero(gc, _a: ()) -> G2 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G2::zero()).unwrap()) }
    }
    fn g2_one(gc, _a: ()) -> G2 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G2::one()).unwrap()) }
    }
    fn g2_rand(gc, _a: ()) -> G2 {
        call!{ alloc_string_newtype(gc, into_hex(bn::G2::random(&mut rand::thread_rng())).unwrap()) }
    }


    fn add(gc, a: G1, b: G1) -> G1 {
        let point_a = from_hex::<bn::G1>(&as_string_newtype(a)[..]).unwrap();
        let point_b = from_hex::<bn::G1>(&as_string_newtype(b)[..]).unwrap();
        call!{ alloc_string_newtype(gc, into_hex(point_a + point_b).unwrap()) }
    }
    
    fn mul(gc, a: G1, b: Fr) -> G1 {
        let point = from_hex::<bn::G1>(&as_string_newtype(a)[..]).unwrap();
        let fr = from_hex::<bn::Fr>(&as_string_newtype(b)[..]).unwrap();
        call!{ alloc_string_newtype(gc, into_hex(point * fr).unwrap()) }
    }
    
    fn g2_add(gc, a: G2, b: G2) -> G2 {
        let point_a = from_hex::<bn::G2>(&as_string_newtype(a)[..]).unwrap();
        let point_b = from_hex::<bn::G2>(&as_string_newtype(b)[..]).unwrap();
        call!{ alloc_string_newtype(gc, into_hex(point_a + point_b).unwrap()) }
    }
    
    fn g2_mul(gc, a: G2, b: Fr) -> G2 {
        let point = from_hex::<bn::G2>(&as_string_newtype(a)[..]).unwrap();
        let fr = from_hex::<bn::Fr>(&as_string_newtype(b)[..]).unwrap();
        call!{ alloc_string_newtype(gc, into_hex(point * fr).unwrap()) }
    }
    
    fn pairing(gc, points: List<Pair<G1, G2>>) -> bool {
        let v = points.as_vec();
        let mut jacobian_points = vec![];
        for i in 0..v.len() {
            let point = from_hex::<bn::G1>(&as_string_newtype(v[i].fst())[..]).unwrap();
            let twistpoint = from_hex::<bn::G2>(&as_string_newtype(v[i].snd())[..]).unwrap();
            jacobian_points.push((point, twistpoint))
        }

        let test = pairing_batch(&jacobian_points[..]);
        of_bool(test == bn::Gt::one())
    }

    // Encodable not implemented for Gt :(
    // fn target_one(gc, _a: ()) -> Gt {
    //     call!{ alloc_string_newtype(gc, into_hex(bn::Gt::one().0).unwrap()) }
    // }
    // fn target_pow(gc, a: Gt, b: Fr) -> Gt {
    //     let t = bn::Gt::from(from_hex::<bn::Gt>(&as_string_newtype(a)[..]).unwrap());
    //     let s = from_hex::<bn::Fr>(&as_string_newtype(b)[..]).unwrap();
    //     call!{ alloc_string_newtype(gc, into_hex(t.pow(s)).unwrap()) }
    // }
}
