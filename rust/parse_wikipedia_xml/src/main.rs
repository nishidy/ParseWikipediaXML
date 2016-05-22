extern crate regex;
extern crate time;

use std::env;
use std::collections::HashMap;
use std::io::BufReader;
use std::io::BufRead;
use std::io::Write;
use std::io::stdout;
use std::fs::File;
use regex::Regex;

use std::thread;
use std::sync::mpsc;

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

fn update_dict<'a>(tx: &mpsc::SyncSender<String>, lines_str: &'a String) -> bool {
	let re_tag = Regex::new(r"<text[^<>]*>([^<>]*)</text>").unwrap();
	if let Some(text_cap) = re_tag.captures(&lines_str) {
		let text_clone = text_cap.at(1).unwrap().clone().to_string();
		tx.send(text_clone).unwrap();
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

	let (txd,rxd) = mpsc::sync_channel(10);
	let (txc,rxc) = mpsc::sync_channel(1);

	thread::spawn( move || {
		let mut docs = 0;
		let mut alldocs = 0;
		loop { 
			let text_str:String = rxd.recv().unwrap();
			if text_str == "::FINISHED::" {
				println!("\nGJ.");
				txc.send(()).unwrap();
				break;
			}
			let mut dicts = HashMap::new();
			let re_word= Regex::new(r"^[a-z][0-9a-z'-]*[0-9a-z]$").unwrap();
			for word in text_str.split(" ") {
				if re_word.is_match(word) {
					let lc_word = word.to_string().to_lowercase();
					let counter = dicts.entry(lc_word).or_insert(0);
					*counter += 1;
				}
			}

			if dicts.len() > 0 {
				output_result(&dicts,&mut fd_out);
				docs += 1;
			}
			alldocs += 1;
			print!("\r# of docs {}/{}",docs,alldocs);
			let _ = stdout().flush();
		}
	});

	let mut lines_str: String = "".to_string();
	let buf = BufReader::new(&fd_in);

	let start_time = time::precise_time_ns();

	for line in buf.lines() {
		let line_str = line.unwrap();
		lines_str.push_str(&line_str);
		if update_dict(&txd,&lines_str) {
			lines_str.clear();
		}
	}

	txd.send("::FINISHED::".to_string()).unwrap();
	rxc.recv().unwrap();

	let end_time = time::precise_time_ns();
	let dur = (end_time-start_time) as f64;
	println!("Elapsed {:.3} sec.",dur/(10u64.pow(9) as f64));
}

