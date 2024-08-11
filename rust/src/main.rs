use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

const BUFFER_SIZE: usize = 16; //TODO: make dynamic w/ -c/--col CLI arg

// ANSI color codes
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const BLUE: &str = "\x1b[34m";
const _MAGENTA: &str = "\x1b[35m";
const _CYAN: &str = "\x1b[36m";
const RESET: &str = "\x1b[0m";
const BOLDON: &str = "\x1b[1m";
const _BOLDOFF: &str = "\x1b[21m";


fn hex_width(n: u8) -> usize {
    (2 * n + (((n as f32 / 2.0).ceil() as u8) - 1)) as usize
}


fn byte_to_printable(byte: u8) -> String {
    match byte {
        0x20..0x7f => { format!("{}{}", GREEN, byte as char) }, // Printable
        0 | 9 | 10 | 13 => { format!("{}.", YELLOW) }, // Whitespace
        0xff => { format!("{}.", BLUE) }, // Delete
        _ => { format!("{}.", RED) }, // Non-printable
    }
}


fn byte_to_hexadecimal(byte: u8, add_padding: bool) -> String {
    let padding = if add_padding { " " } else { "" };

    match byte {
        0x20..0x7f => { format!("{}{:02x}{}", GREEN, byte, padding) }, // Printable
        0 | 9 | 10 | 13 => { format!("{}{:02x}{}", YELLOW, byte, padding) }, // Whitespace
        0xff => { format!("{}{:02x}{}", BLUE, byte, padding) }, // Delete
        _ => { format!("{}{:02x}{}", RED, byte, padding) }, // Non-printable
    }
}


fn process(address: u64, buffer: [u8; BUFFER_SIZE], bytes_read: usize) {
    let hex: String = buffer[..bytes_read].iter().enumerate().map(
        |(i, byte)| if i % 2 == 0 { 
            byte_to_hexadecimal(*byte, false)
        } else {
            byte_to_hexadecimal(*byte, true)
        }
    ).collect::<Vec<String>>().join("");

    let readable: String = buffer[..bytes_read].iter().map(
        |byte| byte_to_printable(*byte)
    ).collect();

    if bytes_read == BUFFER_SIZE {
        println!("{:08x}: {}{:120} {}{}", address, BOLDON, hex, readable, RESET);

    } else {
        // Properly pad last line (hidden ANSI color codes distort string lengths)
        let void: usize = hex_width(BUFFER_SIZE as u8) - hex_width(bytes_read as u8);
        println!("{:08x}: {}{}{}  {}{}", address, BOLDON, hex, " ".repeat(void), readable, RESET);
    }
}


fn xxd<R: BufRead> (mut reader: R ) -> io::Result<()> {
    let mut buffer = [0; BUFFER_SIZE];

    let mut address: u64 = 0;
    loop {
        let bytes_read = reader.read(&mut buffer)?;
        match bytes_read {
            BUFFER_SIZE => { process(address, buffer, BUFFER_SIZE); },
            0 => { break; },
            _ => { process(address, buffer, bytes_read); },  //buffer[bytes_read..].fill(0); },
        }
        address += BUFFER_SIZE as u64;
    }

    Ok(())
}


fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    //dbg!(args);

    if args.len() == 1 {
        // Read from stdin
        let stdin = io::stdin();
        let reader = stdin.lock();
        xxd(reader)?;

    } else {
        // Read from file
        let filename: &String = &args[1];
        let file = File::open(filename)?;
        let reader = BufReader::new(file);
        xxd(reader)?;
    }
    Ok(())
}
