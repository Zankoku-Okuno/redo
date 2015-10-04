#[macro_use] extern crate log;
extern crate clap;

mod cli {
    use clap::*;
    use std::env;
    use std::path::{Path, PathBuf};
    use std::fs;

    pub struct Vars {
        pub verbosity: u8,
        pub files: Vec<String>,
        pub dir: PathBuf,
    }

    use std::cmp;
    fn clamp<T>(val: T, low: T, high: T) -> T where T: cmp::Ord {
        if val > high { high }
        else if val < low { low }
        else { val }
    }

    pub fn get_args() -> Vars {
        let matches = App::new("redo")
            .version("0.1.0")
            .author("Zankoku Okuno")
            .about("The simpler, better incremental build system")
            .arg(Arg::with_name("verbose")
                .help("increase verbosity")
                .short("v")
                .multiple(true))
            .arg(Arg::with_name("quiet")
                .help("decrease verbosity")
                .short("q")
                .multiple(true))
            .arg(Arg::with_name("files")
                .help("files to build")
                .multiple(true))
            .get_matches();

        let vs = matches.occurrences_of("verbose") as i8;
        let qs = matches.occurrences_of("quiet") as i8;
        let files = matches.values_of("files").unwrap_or(vec![]);
        let dir;

        // TODO get environment vars:
        match env::var("REDO_DIR") {
            Ok(val) => dir = Path::new(&val).to_path_buf(),
            Err(env::VarError::NotPresent) => {
                let mut d = env::current_dir().unwrap();
                loop {
                    d.push(".redo/");
                    match fs::metadata(&d) {
                        Ok(md) => {
                            if md.is_dir() {
                                dir = d;
                                break;
                            }
                            else {
                                panic!("{} is not a directory.", d.to_str().unwrap());
                            }
                        },
                        Err(e) => { d.pop(); },
                    }
                    if !d.pop() {
                        panic!("No redo repository found.")
                    }
                }
            },
            Err(e) => panic!(e),
        }
        // redo config dir
        // redo configs

        for file in &files {
            println!("{}", file);
        }

        Vars {
            verbosity: clamp(1 + vs - qs, 0, 5) as u8,
            files: files.iter().map(|x| {x.to_string()}).collect(), //FIXME I'm sure there's a way to not copy the text
            dir: dir,
        }
    }
}

mod logger {
    use log::*;
    use std::io::{stderr, Write};

    struct Logger;

    impl Log for Logger {
        fn enabled(&self, _: &LogMetadata) -> bool {
            true // TODO
        }

        fn log(&self, record: &LogRecord) {
            // TODO don't log if not enabled
            // TODO figure out how to format indentation and identify the do script beign run
            writeln!(&mut stderr(),
                     "[{}] {}", record.level(), record.args())
                .ok()
                .expect("[FATAL] Could not write to log");
        }
    }

    pub fn init(verbosity: u8) -> Result<(), SetLoggerError> {
        set_logger(|max_level| {
            max_level.set(LogLevelFilter::Trace);
            Box::new(Logger)
        })
    }
}

fn main() {
    let args = cli::get_args();

    logger::init(args.verbosity)
        .ok()
        .expect("[FATAL] could not initialize logging");
    warn!("TODO: have verbosity affect log level");
    info!("Verbosity set to: {}", args.verbosity);
    info!("Project dir: {}", args.dir.to_str().unwrap());

    if args.files.len() == 0 {
        warn!("no files given");
    }
    else {
        for name in &args.files {
            warn!("TODO running {}", name);
        }
    }
    trace!("done!");
}
