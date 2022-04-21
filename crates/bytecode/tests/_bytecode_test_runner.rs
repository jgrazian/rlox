use std::io::{self, prelude::*, BufReader};

use bytecode::run_file;

fn get_expected_output(path: &str) -> io::Result<Vec<u8>> {
    let mut expected_output = String::new();
    let file = std::fs::File::open(path).unwrap();
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.unwrap();
        let line = line.trim_start_matches(' ');
        if !line.starts_with("///") {
            continue;
        }
        let matches: &[_] = &['/', ' '];
        expected_output.push_str(&line.trim_start_matches(matches));
        expected_output.push_str(" ");
    }
    Ok(expected_output.as_bytes().to_owned())
}

macro_rules! test_file {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() -> Result<(), Box<dyn std::error::Error>> {
            let expected_output = get_expected_output($path)?;
            let mut output = Vec::new();
            run_file($path, &mut output)?;
            assert_eq!(
                String::from_utf8(expected_output)?.trim(),
                String::from_utf8(output)?.replace("\n", " ").trim(),
                "\n{}: Expected output did not match",
                $path
            );
            Ok(())
        }
    };
}

test_file!(test_hello_world, "tests/hello_world.lox");
test_file!(test_equality, "tests/equality.lox");
test_file!(test_factorial, "tests/factorial.lox");
test_file!(test_fibonacci, "tests/fib.lox");
test_file!(test_if, "tests/if.lox");
test_file!(test_if_else, "tests/if_else.lox");
test_file!(test_while, "tests/while.lox");
test_file!(test_for, "tests/for.lox");
test_file!(test_closure_01, "tests/closure_01.lox");
test_file!(test_closure_02, "tests/closure_02.lox");
test_file!(test_closure_03, "tests/closure_03.lox");
test_file!(test_closure_04, "tests/closure_04.lox");
test_file!(test_closure_05, "tests/closure_05.lox");
test_file!(test_gc, "tests/test_gc.lox");
test_file!(test_class_01, "tests/class_01.lox");
test_file!(test_class_02, "tests/class_02.lox");
test_file!(test_class_03, "tests/class_03.lox");
test_file!(test_class_04, "tests/class_04.lox");
test_file!(test_class_05, "tests/class_05.lox");
test_file!(test_superclass_01, "tests/superclass_01.lox");
// test_file!(test_superclass_02, "tests/superclass_02.lox");
// test_file!(test_superclass_03, "tests/superclass_03.lox");
// test_file!(test_superclass_04, "tests/superclass_04.lox");
// test_file!(test_superclass_05, "tests/superclass_05.lox");
