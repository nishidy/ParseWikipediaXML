extern crate regex;

use std::env;
use std::collections::HashMap;
use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;
use regex::Regex;

fn output_result(dicts: &HashMap<String, isize>) {
	for (k,v) in dicts.iter() {
		print!("{} {} ",k,v)
	}
	println!("")
}

fn update_dict(dicts: &mut HashMap<String, isize>, lines_str: &String) -> bool {
	let re_tag = Regex::new(r"<text[^<>]*>([^<>]*)</text>").unwrap();
	let re_word= Regex::new(r"^[a-z][0-9a-z'-]*[0-9a-z]$").unwrap();
	if let Some(text_cap) = re_tag.captures(&lines_str) {
		for word in text_cap.at(1).unwrap().split(" ") {
			if re_word.is_match(word) {
				let lc_word = word.to_string().to_lowercase();
				let counter = dicts.entry(lc_word).or_insert(0);
				*counter += 1;
			}
		}
		true
	}else{
		false
	}
}

fn main() {

	let args: Vec<String> = env::args().collect();
	if args.len() != 2 {
		panic!("Not enough argument.");
	}

	let db_file = &args[1];

	let fd = match File::open(db_file) {
		Ok(v) => v,
		Err(_) => return ()
	};

	let mut dicts = HashMap::new();
	let mut lines_str: String = "".to_string();

	let buf = BufReader::new(&fd);
	for line in buf.lines() {
		let line_str = line.unwrap();
		lines_str.push_str(&line_str);
		if update_dict(&mut dicts,&lines_str) {
			if dicts.len() > 0 {
				output_result(&dicts)
			}
			dicts.clear();
			lines_str.clear();
			()
		}
	}

}

