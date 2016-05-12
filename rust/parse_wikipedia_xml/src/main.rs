extern crate regex;

use std::env;
use std::collections::HashMap;
use std::io::BufReader;
use std::io::BufRead;
use std::io::Write;
use std::fs::File;
use regex::Regex;

fn output_result(dicts: &HashMap<String, isize>, fd: &mut File) {
	let mut output = Vec::new();
	for (k,v) in dicts.iter() {
		output.push(k.clone());
		output.push(v.to_string());
	}
	match fd.write_fmt(format_args!("{}\n",output.join(" "))) {
		Ok(_) => (),
		Err(v) => panic!("{}",v)
	}
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
	if args.len() != 3 {
		panic!("Not enough argument.");
	}

	let db_file = &args[1];
	let save_file = &args[2];

	let fd_in = match File::open(db_file) {
		Ok(v) => v,
		Err(_) => return ()
	};

	let mut fd_out = match File::create(save_file) {
		Ok(v) => v,
		Err(_) => return ()
	};

	let mut dicts = HashMap::new();
	let mut lines_str: String = "".to_string();

	let buf = BufReader::new(&fd_in);
	for line in buf.lines() {
		let line_str = line.unwrap();
		lines_str.push_str(&line_str);
		if update_dict(&mut dicts,&lines_str) {
			if dicts.len() > 0 {
				output_result(&dicts,&mut fd_out)
			}
			dicts.clear();
			lines_str.clear();
			()
		}
	}

}

