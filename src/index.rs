use crate::storage::{
    self, ArchivedDocument, Document, Entry, Manifest, Segment, SegmentView,
    Stamp,
};
use eyre::{Result, WrapErr, ensure};
use fxhash::{FxHashMap, FxHashSet};
use rkyv::rancor::Error;
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    time::UNIX_EPOCH,
};
use walkdir::WalkDir;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Location {
    pub segment: usize,
    pub entry: usize,
}

/// Immutable snapshot. Old views remain usable after updates or compaction.
#[derive(Default)]
pub struct IndexView {
    pub(crate) segments: Vec<SegmentView>,
    pub(crate) live: FxHashMap<String, Location>,
    pub(crate) manifest: Manifest,
    pub(crate) source: Option<PathBuf>,
}

impl IndexView {
    pub(crate) fn append(&mut self, segment: SegmentView) {
        let number = self.segments.len();
        for (entry, value) in segment.archive().entries.iter().enumerate() {
            if value.document.is_some() {
                self.live.insert(
                    value.path.to_string(),
                    Location {
                        segment: number,
                        entry,
                    },
                );
            } else {
                self.live.remove(value.path.as_str());
            }
        }
        self.segments.push(segment);
    }

    pub(crate) fn document(&self, location: Location) -> &ArchivedDocument {
        self.segments[location.segment].archive().entries[location.entry]
            .document
            .as_ref()
            .unwrap()
    }

    pub fn document_count(&self) -> usize {
        self.live.len()
    }
    pub fn segment_count(&self) -> usize {
        self.segments.len()
    }
}

impl TryFrom<&Path> for IndexView {
    type Error = eyre::Report;
    fn try_from(path: &Path) -> Result<Self> {
        let path = storage::canonical_destination(path)?;
        let _lock = storage::lock(&path, false)?;
        let manifest = storage::read_manifest(&path)?;
        let mut view = Self::default();
        for meta in &manifest.segments {
            view.append(storage::load_segment(path.parent().unwrap(), meta)?);
        }
        view.manifest = manifest;
        view.source = Some(path);
        Ok(view)
    }
}

impl TryFrom<&PathBuf> for IndexView {
    type Error = eyre::Report;
    fn try_from(path: &PathBuf) -> Result<Self> {
        Self::try_from(path.as_path())
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct UpdateOptions {
    /// Read and compare every file, even if metadata is unchanged. Useful on
    /// filesystems with coarse timestamps or when timestamps are manipulated.
    pub verify_contents: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct UpdateStats {
    pub added: usize,
    pub modified: usize,
    pub removed: usize,
    pub unchanged: usize,
    pub skipped: usize,
    pub bytes_read: u64,
}

/// A snapshot plus pending document replacements/tombstones. Updates touch no
/// old posting lists. `store` appends only the delta; `compact` is explicit.
#[derive(Default)]
pub struct Index {
    base: IndexView,
    pending: BTreeMap<String, Option<Document>>,
}

impl From<IndexView> for Index {
    fn from(base: IndexView) -> Self {
        Self {
            base,
            pending: BTreeMap::new(),
        }
    }
}

impl Index {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_from_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut index = Self::new();
        index.update_by_path(path, UpdateOptions::default())?;
        Ok(index)
    }

    pub fn extend_by_path<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<&mut Self> {
        self.update_by_path(path, UpdateOptions::default())?;
        Ok(self)
    }

    /// Synchronize one directory, including deletions. Other indexed roots are
    /// untouched. Stage the complete scan before mutating pending state, so a
    /// read/traversal error cannot partially delete or replace the index.
    pub fn update_by_path<P: AsRef<Path>>(
        &mut self,
        path: P,
        options: UpdateOptions,
    ) -> Result<UpdateStats> {
        let root = path.as_ref().canonicalize()?;
        ensure!(root.is_dir(), "search root must be a directory");
        let mut stats = UpdateStats::default();
        let mut seen = FxHashSet::default();
        let mut changes = BTreeMap::new();
        let walker = WalkDir::new(&root)
            .follow_links(false)
            .into_iter()
            .filter_entry(|entry| {
                entry.file_name() != ".git"
                    && !(entry.file_type().is_file()
                        && is_index_artifact(entry.path()))
            });
        for entry in walker {
            let entry = entry?;
            if !entry.file_type().is_file() {
                continue;
            }
            let path = entry.path();
            let name = path.to_str().ok_or_else(|| {
                eyre::eyre!("non-UTF-8 file path: {}", path.display())
            })?;
            seen.insert(name.to_owned());
            let before = stamp(&fs::metadata(path)?)?;
            let previous = self.previous(name)?;
            if !options.verify_contents
                && previous.as_ref().is_some_and(|(old, _)| *old == before)
            {
                stats.unchanged += 1;
                continue;
            }
            ensure!(
                before.len <= u32::MAX as u64,
                "file exceeds 4 GiB: {}",
                path.display()
            );
            let bytes = fs::read(path)
                .wrap_err_with(|| format!("reading {}", path.display()))?;
            stats.bytes_read += bytes.len() as u64;
            let after = stamp(&fs::metadata(path)?)?;
            ensure!(
                before == after && after.len == bytes.len() as u64,
                "file changed during indexing; retry: {}",
                path.display()
            );
            let content = match String::from_utf8(bytes) {
                Ok(text) if !text.contains('\0') => text,
                _ => {
                    stats.skipped += 1;
                    if previous.is_some() {
                        changes.insert(name.to_owned(), None);
                        stats.removed += 1;
                    }
                    continue;
                }
            };
            ensure!(
                content.len() <= u32::MAX as usize,
                "file exceeds 4 GiB: {}",
                path.display()
            );
            if let Some((old_stamp, old_content)) = previous {
                if old_stamp == after && old_content == content {
                    stats.unchanged += 1;
                    continue;
                }
                stats.modified += 1;
            } else {
                stats.added += 1;
            }
            let line_starts = std::iter::once(0)
                .chain(
                    memchr::memchr_iter(b'\n', content.as_bytes())
                        .map(|i| i as u32 + 1),
                )
                .collect();
            changes.insert(
                name.to_owned(),
                Some(Document {
                    stamp: after,
                    content,
                    line_starts,
                }),
            );
        }
        for name in self.paths() {
            if Path::new(&name).starts_with(&root) && !seen.contains(&name) {
                changes.insert(name, None);
                stats.removed += 1;
            }
        }
        self.pending.extend(changes);
        Ok(stats)
    }

    fn previous(&self, name: &str) -> Result<Option<(Stamp, &str)>> {
        if let Some(value) = self.pending.get(name) {
            return Ok(value
                .as_ref()
                .map(|doc| (doc.stamp.clone(), doc.content.as_str())));
        }
        self.base
            .live
            .get(name)
            .map(|&location| {
                let doc = self.base.document(location);
                Ok((
                    rkyv::deserialize::<Stamp, Error>(&doc.stamp)?,
                    doc.content.as_str(),
                ))
            })
            .transpose()
    }

    fn paths(&self) -> FxHashSet<String> {
        let mut paths: FxHashSet<_> = self.base.live.keys().cloned().collect();
        for (name, doc) in &self.pending {
            if doc.is_some() {
                paths.insert(name.clone());
            } else {
                paths.remove(name);
            }
        }
        paths
    }

    fn snapshot_entries(&self) -> Result<Vec<Entry>> {
        let mut docs = BTreeMap::new();
        for (name, &location) in &self.base.live {
            if !self.pending.contains_key(name) {
                docs.insert(
                    name.clone(),
                    Some(rkyv::deserialize::<Document, Error>(
                        self.base.document(location),
                    )?),
                );
            }
        }
        docs.extend(
            self.pending
                .iter()
                .filter(|(_, doc)| doc.is_some())
                .map(|(path, doc)| (path.clone(), doc.clone())),
        );
        Ok(docs
            .into_iter()
            .map(|(path, document)| Entry { path, document })
            .collect())
    }

    /// Merge by canonical path; incoming live documents replace existing ones.
    pub fn merge(&mut self, other: Index) -> Result<&mut Self> {
        for entry in other.snapshot_entries()? {
            self.pending.insert(entry.path, entry.document);
        }
        Ok(self)
    }

    pub fn store<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.commit(path.as_ref(), false, false)
    }

    /// Rewrite live documents into a single segment and reclaim obsolete
    /// segments. Already-open readers own their buffers and remain valid.
    pub fn compact<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.commit(path.as_ref(), true, false)
    }

    /// Explicitly replace an existing index, including a legacy format.
    pub fn replace<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.commit(path.as_ref(), true, true)
    }

    /// Consume a committed index without reopening and revalidating segments.
    pub fn into_view(self) -> Result<IndexView> {
        ensure!(
            self.pending.is_empty(),
            "store pending changes before taking a view"
        );
        Ok(self.base)
    }

    fn commit(
        &mut self,
        path: &Path,
        compact: bool,
        replace: bool,
    ) -> Result<()> {
        let path = storage::canonical_destination(path)?;
        let _lock = storage::lock(&path, true)?;
        let same = self.base.source.as_ref() == Some(&path);
        if !same && !replace {
            ensure!(
                !path.try_exists()?,
                "destination exists; use explicit index replacement"
            );
        }
        if same && !replace {
            ensure!(
                storage::read_manifest(&path)? == self.base.manifest,
                "index changed by another writer; reopen and retry"
            );
            if self.pending.is_empty() && !compact {
                return Ok(());
            }
        }
        let full = compact || !same;
        let entries = if full {
            self.snapshot_entries()?
        } else {
            self.pending
                .iter()
                .map(|(path, doc)| Entry {
                    path: path.clone(),
                    document: doc.clone(),
                })
                .collect()
        };
        let old = if path.exists() {
            storage::read_manifest(&path).ok()
        } else {
            None
        };
        let manifest = if full {
            Manifest::default()
        } else {
            self.base.manifest.clone()
        };
        let (manifest, segment) =
            storage::publish(&path, manifest, &Segment::new(entries)?)?;
        if full {
            self.base = IndexView::default();
        }
        self.base.append(segment);
        self.base.manifest = manifest;
        self.base.source = Some(path.clone());
        self.pending.clear();
        if full && let Some(old) = old {
            for segment in old.segments {
                // Cleanup failure doesn't invalidate the published index.
                let _ =
                    fs::remove_file(path.parent().unwrap().join(segment.name));
            }
        }
        Ok(())
    }
}

fn is_index_artifact(path: &Path) -> bool {
    let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
    name.ends_with(".zito")
        || name.ends_with(".zito.lock")
        || name.starts_with(".zito-segment-")
        || name.starts_with(".zito-manifest-")
}

fn stamp(metadata: &fs::Metadata) -> Result<Stamp> {
    let modified = metadata.modified()?.duration_since(UNIX_EPOCH)?;
    let mut stamp = Stamp {
        len: metadata.len(),
        modified_secs: modified.as_secs(),
        modified_nanos: modified.subsec_nanos(),
        changed_secs: 0,
        changed_nanos: 0,
        device: 0,
        inode: 0,
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        stamp.changed_secs = metadata.ctime();
        stamp.changed_nanos = metadata.ctime_nsec();
        stamp.device = metadata.dev();
        stamp.inode = metadata.ino();
    }
    Ok(stamp)
}
