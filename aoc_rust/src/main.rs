extern crate crypto;

use std::io;
use crypto::digest::Digest;
use crypto::md5::Md5;

pub fn main() {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(n) => {
            let result = solve_2(&input.trim());
            println!("Answer {}", result);
        }
        Err(error) => println!("error: {}", error),
    }

}

fn solve(input: &str) -> String {
    let mut password = String::new();
    let mut count = 0;
    loop {
        let str_to_test = String::from(input) + &count.to_string();
        let result = test_string(&str_to_test);
        if result.starts_with("00000") {
            password.push(result.chars().nth(5).unwrap());
        }
        if password.len() == 8 {
            break
        }
        count += 1;
    }
    password
}

fn solve_2(input: &str) -> String {
    let mut password: [char; 8] = ['?'; 8];
    let mut count = 0;
    let mut solved = 0;
    loop {
        let str_to_test = String::from(input) + &count.to_string();
        let result = test_string(&str_to_test);
        if result.starts_with("00000") {
            println!("{}", result);
            match result.chars().nth(5).unwrap().to_digit(10) {
                Some(n) => {
                    if n < 8 && password[n as usize] == '?' {
                        password[n as usize] = result.chars().nth(6).unwrap();
                        solved += 1;
                        if password.iter().all(|&c| c != '?') {
                            break
                        }
                    }
                }
                None => {}
            }
        }
        count += 1;
    }
    password.iter().cloned().collect::<String>()
}

fn test_string(input: &str) -> String {
    let mut sh = Md5::new();
    sh.input_str(input);
    sh.result_str().to_string()
}