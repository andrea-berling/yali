use std::{fs, path::Path, process::Command};

fn test_command(test_path: &str, cmd: &str) -> Command {
    let mut command = Command::new("/usr/bin/zsh");
    command
        .env("RUSTFLAGS", "-Awarnings")
        .env("CARGO_TERM_QUIET", "true")
        .args([
            "-c",
            &format!(
                "/tmp/codecrafters-build-interpreter-rust/release/codecrafters-interpreter {cmd} {test_path}",
            ),
        ]);
    command
}

fn compile() -> Result<(), Box<dyn std::error::Error>> {
    let project_path = Path::new(file!())
        .parent()
        .unwrap()
        .join("..")
        .canonicalize()
        .unwrap();
    let mut command = Command::new("/usr/bin/zsh");
    command
        .args([
            "-c",
            &format!(
                "cd {} && cargo build --release --target-dir=/tmp/codecrafters-build-interpreter-rust --manifest-path Cargo.toml",project_path.to_str().unwrap()
            ),
        ]).output().expect("Something's up");
    Ok(())
}

fn run_tests(
    unit: &str,
    command: &str,
    error_exit_code: i32,
) -> Result<(), Box<dyn std::error::Error>> {
    compile()?;
    let cases_path = Path::new(file!())
        .parent()
        .unwrap()
        .join(format!("cases/{unit}"));
    let tests_input_files: Vec<_> = fs::read_dir(&cases_path)?
        .filter_map(|dirent| {
            if let Ok(dirent) = dirent {
                if dirent
                    .path()
                    .extension()
                    .is_some_and(|extension| extension == "in")
                {
                    return Some(dirent.path());
                }
            }
            return None;
        })
        .collect();

    for (input_file, output_file, error_file, exit_code_file) in
        tests_input_files.iter().cloned().map(|mut path| {
            let path1 = path.clone();
            path.set_extension("out");
            let path2 = path.clone();
            path.set_extension("err");
            let path3 = path.clone();
            path.set_extension("exit");
            let path4 = path.clone();
            (path1, path2, path3, path4)
        })
    {
        let mut command = test_command(input_file.to_str().unwrap(), command);
        let expected_stdout = std::fs::read_to_string(&output_file).unwrap();
        let expected_stderr = std::fs::read_to_string(&error_file);
        let mut expected_exit_code = 0;
        let output = command.output().unwrap();
        let filename = input_file.file_name().unwrap().to_str().unwrap();
        assert_eq!(
            expected_stdout,
            String::from_utf8(output.stdout).unwrap(),
            "Standard output mismatch for {}",
            &filename
        );

        if expected_stderr.is_ok() {
            expected_exit_code = error_exit_code;
        }
        let _ = expected_stderr.inspect(|expected_stderr| {
            assert_eq!(
                *expected_stderr,
                String::from_utf8(output.stderr).unwrap(),
                "Standard error mismatch for {}",
                &filename
            );
        });

        assert_eq!(
            match std::fs::read_to_string(&exit_code_file) {
                Ok(exit_code) => exit_code.trim().parse().unwrap(),
                _ => expected_exit_code,
            },
            output.status.code().unwrap(),
            "Exit code mishmatch for {}",
            &filename
        );
    }

    Ok(())
}

#[test]
fn lexer() -> Result<(), Box<dyn std::error::Error>> {
    run_tests("tokenizer", "tokenize", 65)
}

#[test]
fn parser() -> Result<(), Box<dyn std::error::Error>> {
    run_tests("parser", "parse", 65)
}

#[test]
fn eval() -> Result<(), Box<dyn std::error::Error>> {
    run_tests("eval", "evaluate", 70)
}

#[test]
fn interpreter() -> Result<(), Box<dyn std::error::Error>> {
    run_tests("interpreter", "run", 70)
}
