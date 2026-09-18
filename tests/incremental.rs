use std::{
    fs,
    path::{Path, PathBuf},
};
use tempfile::TempDir;
use zito::{Index, IndexView, SearchOptions, UpdateOptions};

fn setup() -> (TempDir, PathBuf, PathBuf) {
    let temp = TempDir::new().unwrap();
    let corpus = temp.path().join("corpus");
    fs::create_dir(&corpus).unwrap();
    let path = temp.path().join("main.zito");
    (temp, corpus, path)
}
fn matches(view: &IndexView, needle: &str) -> usize {
    view.search(needle, SearchOptions::default()).unwrap().len()
}
fn segment_files(path: &Path) -> Vec<PathBuf> {
    let manifest: serde_json::Value =
        serde_json::from_slice(&fs::read(path).unwrap()).unwrap();
    manifest["segments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|m| path.parent().unwrap().join(m["name"].as_str().unwrap()))
        .collect()
}

#[test]
fn updates_replace_delete_rename_and_preserve_old_snapshots() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "old marker café").unwrap();
    fs::write(corpus.join("b"), "kept marker").unwrap();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    let old = IndexView::try_from(&path).unwrap();
    let first_segment = segment_files(&path).remove(0);
    let old_bytes = fs::read(&first_segment).unwrap();
    fs::write(corpus.join("a"), "new marker 🦀").unwrap();
    fs::rename(corpus.join("b"), corpus.join("renamed")).unwrap();
    fs::write(corpus.join("c"), "added marker").unwrap();
    let stats = index
        .update_by_path(&corpus, UpdateOptions::default())
        .unwrap();
    assert_eq!((stats.added, stats.modified, stats.removed), (2, 1, 1));
    index.store(&path).unwrap();
    assert_eq!(fs::read(&first_segment).unwrap(), old_bytes);
    let new = IndexView::try_from(&path).unwrap();
    assert_eq!(new.segment_count(), 2);
    assert_eq!(matches(&new, "old"), 0);
    assert_eq!(matches(&new, "new"), 1);
    assert_eq!(matches(&new, "marker"), 3);
    assert_eq!(matches(&old, "old"), 1);
    assert!(
        new.search("kept", SearchOptions::default()).unwrap()[0]
            .file_path
            .ends_with("renamed")
    );
    index.compact(&path).unwrap();
    assert!(!first_segment.exists());
    assert_eq!(IndexView::try_from(&path).unwrap().segment_count(), 1);
    assert_eq!(matches(&old, "old"), 1);
    assert_eq!(matches(&new, "new"), 1);
    fs::remove_file(corpus.join("a")).unwrap();
    fs::remove_file(corpus.join("renamed")).unwrap();
    fs::remove_file(corpus.join("c")).unwrap();
    index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
    assert_eq!(IndexView::try_from(&path).unwrap().document_count(), 0);
}

#[test]
fn noop_reads_no_content_and_writes_no_segment_or_manifest() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "unchanged").unwrap();
    Index::new_from_path(&corpus).unwrap().store(&path).unwrap();
    let before = fs::read(&path).unwrap();
    let modified = fs::metadata(&path).unwrap().modified().unwrap();
    let mut index = Index::from(IndexView::try_from(&path).unwrap());
    let stats = index
        .update_by_path(&corpus, UpdateOptions::default())
        .unwrap();
    assert_eq!((stats.unchanged, stats.bytes_read), (1, 0));
    index.store(&path).unwrap();
    assert_eq!(fs::read(&path).unwrap(), before);
    assert_eq!(fs::metadata(&path).unwrap().modified().unwrap(), modified);
    let stats = index
        .update_by_path(
            &corpus,
            UpdateOptions {
                verify_contents: true,
            },
        )
        .unwrap();
    assert_eq!((stats.unchanged, stats.bytes_read), (1, 9));
    index.store(&path).unwrap();
    assert_eq!(fs::read(&path).unwrap(), before);
}

#[test]
fn stale_writers_cannot_discard_a_committed_update() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "initial").unwrap();
    Index::new_from_path(&corpus).unwrap().store(&path).unwrap();
    let mut writer1 = Index::from(IndexView::try_from(&path).unwrap());
    let mut writer2 = Index::from(IndexView::try_from(&path).unwrap());
    fs::write(corpus.join("a"), "committed").unwrap();
    writer1
        .extend_by_path(&corpus)
        .unwrap()
        .store(&path)
        .unwrap();
    fs::write(corpus.join("a"), "stale").unwrap();
    writer2.extend_by_path(&corpus).unwrap();
    assert!(
        writer2
            .store(&path)
            .unwrap_err()
            .to_string()
            .contains("another writer")
    );
    assert_eq!(
        matches(&IndexView::try_from(&path).unwrap(), "committed"),
        1
    );
}

#[test]
fn failed_scan_does_not_partially_modify_pending_state() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "initial").unwrap();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    let before = fs::read(&path).unwrap();
    assert!(index.extend_by_path(corpus.join("missing")).is_err());
    index.store(&path).unwrap();
    assert_eq!(fs::read(&path).unwrap(), before);
}

#[test]
fn merge_and_subdirectory_update_do_not_remove_other_roots() {
    let (_temp, corpus, path) = setup();
    let one = corpus.join("one");
    let two = corpus.join("two");
    fs::create_dir(&one).unwrap();
    fs::create_dir(&two).unwrap();
    fs::write(one.join("a"), "first").unwrap();
    fs::write(two.join("b"), "second").unwrap();
    let mut index = Index::new_from_path(&one).unwrap();
    index
        .merge(Index::new_from_path(&two).unwrap())
        .unwrap()
        .store(&path)
        .unwrap();
    fs::write(one.join("a"), "replacement").unwrap();
    index
        .merge(Index::new_from_path(&one).unwrap())
        .unwrap()
        .store(&path)
        .unwrap();
    fs::remove_file(one.join("a")).unwrap();
    index.extend_by_path(&one).unwrap().store(&path).unwrap();
    let view = IndexView::try_from(&path).unwrap();
    assert_eq!(view.document_count(), 1);
    assert_eq!(matches(&view, "second"), 1);
    assert_eq!(matches(&view, "first"), 0);
    let copy = path.with_file_name("copy.zito");
    index.store(&copy).unwrap();
    index.compact(&copy).unwrap();
    assert_eq!(matches(&IndexView::try_from(&path).unwrap(), "second"), 1);
}

#[test]
fn binary_transitions_and_index_artifacts_are_handled() {
    let (_temp, corpus, _path) = setup();
    let path = corpus.join("main.zito");
    fs::create_dir(corpus.join(".git")).unwrap();
    fs::write(corpus.join(".git/config"), "hidden").unwrap();
    fs::write(corpus.join("a"), "text").unwrap();
    fs::write(corpus.join("b"), [0xff, 0xfe]).unwrap();
    fs::write(corpus.join("nul"), b"binary\0content").unwrap();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
    assert_eq!(IndexView::try_from(&path).unwrap().document_count(), 1);
    fs::write(corpus.join("a"), [0xff, 0xfe]).unwrap();
    fs::write(corpus.join("b"), "newtext").unwrap();
    index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
    let view = IndexView::try_from(&path).unwrap();
    assert_eq!(view.document_count(), 1);
    assert_eq!(matches(&view, "newtext"), 1);
}

#[cfg(unix)]
#[test]
fn restored_mtime_and_symlink_replacement_do_not_leave_stale_content() {
    use std::os::unix::fs::symlink;
    let (_temp, corpus, path) = setup();
    let file = corpus.join("a");
    fs::write(&file, "before").unwrap();
    let mtime = fs::metadata(&file).unwrap().modified().unwrap();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    fs::write(&file, "after!").unwrap();
    fs::File::options()
        .write(true)
        .open(&file)
        .unwrap()
        .set_modified(mtime)
        .unwrap();
    index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
    assert_eq!(matches(&IndexView::try_from(&path).unwrap(), "after!"), 1);
    fs::remove_file(&file).unwrap();
    symlink(&path, &file).unwrap();
    index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
    assert_eq!(IndexView::try_from(&path).unwrap().document_count(), 0);
}

#[test]
fn corrupt_truncated_and_legacy_indices_return_errors() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "contents").unwrap();
    Index::new_from_path(&corpus).unwrap().store(&path).unwrap();
    let segment = segment_files(&path).remove(0);
    let original = fs::read(&segment).unwrap();
    fs::write(&segment, &original[..original.len() / 2]).unwrap();
    assert!(IndexView::try_from(&path).is_err());
    // A valid checksum must not bypass structural archive validation.
    let mut manifest: serde_json::Value =
        serde_json::from_slice(&fs::read(&path).unwrap()).unwrap();
    let garbage = [0xff; 128];
    fs::write(&segment, garbage).unwrap();
    manifest["segments"][0]["checksum"] = crc32fast::hash(&garbage).into();
    fs::write(&path, serde_json::to_vec(&manifest).unwrap()).unwrap();
    assert!(IndexView::try_from(&path).is_err());
    manifest["segments"][0]["name"] = "../escape.zseg".into();
    fs::write(&path, serde_json::to_vec(&manifest).unwrap()).unwrap();
    assert!(IndexView::try_from(&path).is_err());
    fs::write(&path, b"legacy zlib or garbage").unwrap();
    assert!(
        IndexView::try_from(&path)
            .err()
            .unwrap()
            .to_string()
            .contains("rebuild")
    );
}

#[test]
fn edit_sequences_are_equivalent_to_fresh_rebuilds() {
    let (_temp, corpus, path) = setup();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    let mut state = 17u64;
    for step in 0..24 {
        state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
        let file = corpus.join(format!("{}.rs", (state >> 32) % 8));
        match step % 5 {
            0 => {
                let _ = fs::remove_file(&file);
            }
            1 => fs::write(&file, "needle café 🦀\naaaaa\n").unwrap(),
            2 => fs::write(&file, "é").unwrap(),
            3 => fs::write(&file, [0xff, 0]).unwrap(),
            _ => fs::write(&file, "").unwrap(),
        }
        index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
        if step % 8 == 7 {
            index.compact(&path).unwrap();
        }
        let incremental = IndexView::try_from(&path).unwrap();
        let mut rebuilt = Index::new_from_path(&corpus).unwrap();
        rebuilt
            .replace(path.with_file_name("rebuild.zito"))
            .unwrap();
        let rebuilt = rebuilt.into_view().unwrap();
        for (query, regex) in [
            ("needle", false),
            ("a", false),
            ("é", false),
            ("🦀", false),
            ("[a-z]+|é", true),
            (".*", true),
        ] {
            assert_eq!(
                incremental
                    .search(query, SearchOptions::new(regex))
                    .unwrap(),
                rebuilt.search(query, SearchOptions::new(regex)).unwrap(),
                "step {step}, query {query}"
            );
        }
    }
}

#[test]
fn new_store_refuses_overwrite_and_pending_views_are_rejected() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "committed").unwrap();
    Index::new_from_path(&corpus).unwrap().store(&path).unwrap();
    assert!(Index::new().store(&path).is_err());
    assert!(Index::new_from_path(&corpus).unwrap().into_view().is_err());
    assert_eq!(
        matches(&IndexView::try_from(&path).unwrap(), "committed"),
        1
    );
}

#[test]
fn concurrent_readers_open_complete_snapshots_during_compaction() {
    let (_temp, corpus, path) = setup();
    fs::write(corpus.join("a"), "version initial").unwrap();
    let mut index = Index::new_from_path(&corpus).unwrap();
    index.store(&path).unwrap();
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
    let reader_barrier = barrier.clone();
    let reader_path = path.clone();
    let reader = std::thread::spawn(move || {
        reader_barrier.wait();
        for _ in 0..100 {
            let view = IndexView::try_from(&reader_path).unwrap();
            assert_eq!(matches(&view, "version"), 1);
        }
    });
    barrier.wait();
    for n in 0..8 {
        fs::write(corpus.join("a"), format!("version {n}")).unwrap();
        index.extend_by_path(&corpus).unwrap().store(&path).unwrap();
        index.compact(&path).unwrap();
    }
    reader.join().unwrap();
}
