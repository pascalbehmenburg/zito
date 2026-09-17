use std::{
    fs,
    process::{Command, Output},
};
use tempfile::TempDir;
fn cli(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_zito"))
        .args(args)
        .output()
        .unwrap()
}
#[test]
fn find_refreshes_by_default_and_snapshot_mode_is_explicit() {
    let temp = TempDir::new().unwrap();
    let corpus = temp.path().join("corpus");
    let index = temp.path().join("index");
    fs::create_dir(&corpus).unwrap();
    fs::write(corpus.join("a.rs"), "old café 🦀").unwrap();
    let root = corpus.to_str().unwrap();
    let dest = index.to_str().unwrap();
    assert!(
        !cli(&["find", "old", root, "-i", dest, "--no-update"])
            .status
            .success()
    );
    let first = cli(&["find", "old", root, "-i", dest]);
    assert!(first.status.success(), "{:?}", first);
    assert!(String::from_utf8_lossy(&first.stdout).contains("old"));
    fs::write(corpus.join("a.rs"), "new café 🦀").unwrap();
    let snapshot = cli(&["find", "old", root, "-i", dest, "--no-update"]);
    assert!(snapshot.status.success());
    assert!(String::from_utf8_lossy(&snapshot.stdout).contains("old"));
    let refreshed = cli(&["find", "🦀", root, "-i", dest]);
    assert!(refreshed.status.success());
    assert!(String::from_utf8_lossy(&refreshed.stdout).contains("new café 🦀"));
    fs::remove_file(corpus.join("a.rs")).unwrap();
    assert!(cli(&["index", "extend", root, dest]).status.success());
    assert!(cli(&["find", "café", root, "-i", dest]).stdout.is_empty());
    assert!(cli(&["index", "compact", dest]).status.success());
}
#[test]
fn corrupt_index_is_reported_and_only_explicit_create_replaces_it() {
    let temp = TempDir::new().unwrap();
    let corpus = temp.path().join("corpus");
    fs::create_dir(&corpus).unwrap();
    fs::write(corpus.join("a"), "needle").unwrap();
    let path = temp.path().join("main.zito");
    fs::write(&path, "corrupt").unwrap();
    let root = corpus.to_str().unwrap();
    let dest = temp.path().to_str().unwrap();
    assert!(!cli(&["find", "needle", root, "-i", dest]).status.success());
    assert_eq!(fs::read(&path).unwrap(), b"corrupt");
    assert!(cli(&["index", "create", root, dest]).status.success());
    assert!(
        cli(&["find", "needle", root, "-i", dest, "--no-update"])
            .status
            .success()
    );
}
