#[macro_use] extern crate log;
extern crate clap;

mod cli {
    use clap::*;

    pub struct Vars {
        pub verbosity: u8,
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
            .get_matches();

        let vs = matches.occurrences_of("verbose") as i8;
        let qs = matches.occurrences_of("quiet") as i8;
        
        Vars {
            verbosity: clamp(1 + vs - qs, 0, 5) as u8,
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

    pub fn init() -> Result<(), SetLoggerError> {
        set_logger(|max_level| {
            max_level.set(LogLevelFilter::Trace);
            Box::new(Logger)
        })
    }
}

fn main() {
    let args = cli::get_args();

    logger::init(args.verbosity).unwrap();
    info!("Verbosity set to: {}", args.verbosity);
    warn!("TODO: have verbosity affect log level");

    warn!("TODO: get names from arguments");
    let names = vec!["foo", "bar"];
    for name in &names {
        warn!("TODO running {}", name);
    }
    trace!("done!");
}
