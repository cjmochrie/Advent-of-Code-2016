use crypto::digest::Digest;
use crypto::md5::Md5;

pub fn main() {
    let input = "";
}

fn findChar(input: &str, length: u8) -> String {
    let mut password = String::new();
    let mut count = 0;
    loop {
        let mut sh = Md5::new();
        let test = input.to_string() + &number.to_string();
        sh.input_str(&test);
        let result: &str = &sh.result_str();
        if &result[0..5] == "00000" {
            password.push_str(&result[6])
        }
        if password.len() == 8 { 
            break
        }
    }
    password
}