use crate::ngram;
use eyre::{Result, bail, ensure};
use fs2::FileExt;
use fxhash::FxHashMap;
use rkyv::{Archive, Deserialize, Serialize, rancor::Error, util::AlignedVec};
use serde::{Deserialize as SerdeDeserialize, Serialize as SerdeSerialize};
use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Write},
    path::{Path, PathBuf},
};
use tempfile::Builder;

const FORMAT: &str = "zito-sparse-v1";

#[derive(Archive, Deserialize, Serialize, Clone, Debug, PartialEq, Eq)]
pub(crate) struct Stamp {
    pub len: u64,
    pub modified_secs: u64,
    pub modified_nanos: u32,
    // Unix ctime detects in-place edits even when mtime is restored.
    pub changed_secs: i64,
    pub changed_nanos: i64,
    pub device: u64,
    pub inode: u64,
}

#[derive(Archive, Deserialize, Serialize, Clone)]
pub(crate) struct Document {
    pub stamp: Stamp,
    pub content: String,
    pub line_starts: Vec<u32>,
}

#[derive(Archive, Deserialize, Serialize)]
pub(crate) struct Entry {
    pub path: String,
    pub document: Option<Document>,
}

#[derive(Archive, Deserialize, Serialize)]
pub(crate) struct Segment {
    pub entries: Vec<Entry>,
    pub postings: FxHashMap<u64, Vec<u32>>,
}

impl Segment {
    pub fn new(entries: Vec<Entry>) -> Result<Self> {
        ensure!(
            entries.len() <= u32::MAX as usize,
            "too many documents in segment"
        );
        let mut postings: FxHashMap<u64, Vec<u32>> = FxHashMap::default();
        for (id, entry) in entries.iter().enumerate() {
            if let Some(doc) = &entry.document {
                for key in ngram::index_keys(doc.content.as_bytes()) {
                    // Entries are visited in ascending ID order; each key is
                    // emitted only once per document. No posting sort needed.
                    postings.entry(key).or_default().push(id as u32);
                }
            }
        }
        Ok(Self { entries, postings })
    }
}

pub(crate) struct SegmentView {
    bytes: AlignedVec,
}

impl SegmentView {
    pub fn new(bytes: AlignedVec) -> Result<Self> {
        let archive = rkyv::access::<ArchivedSegment, Error>(&bytes)?;
        for list in archive.postings.values() {
            let mut previous = None;
            for id in list.iter() {
                let id = id.to_native() as usize;
                ensure!(id < archive.entries.len(), "invalid posting ID");
                ensure!(previous.is_none_or(|p| p < id), "unsorted postings");
                ensure!(
                    archive.entries[id].document.is_some(),
                    "posting points to deletion"
                );
                previous = Some(id);
            }
        }
        for entry in archive.entries.iter() {
            if let Some(doc) = entry.document.as_ref() {
                ensure!(
                    doc.content.len() <= u32::MAX as usize,
                    "document too large"
                );
                let expected = std::iter::once(0).chain(
                    memchr::memchr_iter(b'\n', doc.content.as_bytes())
                        .map(|i| i as u32 + 1),
                );
                ensure!(
                    doc.line_starts.iter().map(|n| n.to_native()).eq(expected),
                    "invalid line table"
                );
            }
        }
        Ok(Self { bytes })
    }

    pub fn archive(&self) -> &ArchivedSegment {
        // SAFETY: new() checks structure, alignment and bounds. This private,
        // owned buffer is immutable for the entire lifetime of the view.
        unsafe { rkyv::access_unchecked::<ArchivedSegment>(&self.bytes) }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, SerdeSerialize, SerdeDeserialize)]
pub(crate) struct SegmentMeta {
    pub name: String,
    pub checksum: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, SerdeSerialize, SerdeDeserialize)]
pub(crate) struct Manifest {
    pub format: String,
    pub segments: Vec<SegmentMeta>,
}

impl Default for Manifest {
    fn default() -> Self {
        Self {
            format: FORMAT.into(),
            segments: Vec::new(),
        }
    }
}

pub(crate) fn read_manifest(path: &Path) -> Result<Manifest> {
    let bytes = fs::read(path)?;
    let manifest: Manifest = serde_json::from_slice(&bytes).map_err(|e| {
        eyre::eyre!(
            "invalid or legacy index; rebuild with `zito index create`: {e}"
        )
    })?;
    ensure!(
        manifest.format == FORMAT,
        "unsupported index version; rebuild with `zito index create`"
    );
    for segment in &manifest.segments {
        if !segment.name.starts_with(".zito-segment-")
            || !segment.name.ends_with(".zseg")
            || Path::new(&segment.name).components().count() != 1
            || segment.name.contains(['/', '\\'])
        {
            bail!("invalid segment filename");
        }
    }
    Ok(manifest)
}

pub(crate) fn load_segment(
    parent: &Path,
    meta: &SegmentMeta,
) -> Result<SegmentView> {
    let mut file = File::open(parent.join(&meta.name))?;
    let mut bytes = AlignedVec::new();
    bytes.resize(usize::try_from(file.metadata()?.len())?, 0);
    file.read_exact(&mut bytes)?;
    ensure!(
        crc32fast::hash(&bytes) == meta.checksum,
        "index segment checksum mismatch"
    );
    SegmentView::new(bytes)
}

pub(crate) fn canonical_destination(path: &Path) -> Result<PathBuf> {
    let parent = path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    fs::create_dir_all(parent)?;
    let name = path
        .file_name()
        .ok_or_else(|| eyre::eyre!("index path must name a file"))?;
    Ok(parent.canonicalize()?.join(name))
}

pub(crate) fn lock(path: &Path, exclusive: bool) -> Result<File> {
    let mut name = path.as_os_str().to_owned();
    name.push(".lock");
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(Path::new(&name))?;
    if exclusive {
        FileExt::lock_exclusive(&file)?;
    } else {
        FileExt::lock_shared(&file)?;
    }
    Ok(file)
}

/// Publish a durable segment before atomically replacing the small manifest.
/// Readers and writers hold the same lock during open/publication/cleanup.
pub(crate) fn publish(
    path: &Path,
    mut manifest: Manifest,
    segment: &Segment,
) -> Result<(Manifest, SegmentView)> {
    let parent = path.parent().unwrap();
    let bytes = rkyv::to_bytes::<Error>(segment)?;
    let view = SegmentView::new(bytes)?;
    let mut file = Builder::new()
        .prefix(".zito-segment-")
        .suffix(".zseg")
        .tempfile_in(parent)?;
    file.write_all(&view.bytes)?;
    file.as_file().sync_all()?;
    let (_, segment_path) = file.keep()?;
    manifest.segments.push(SegmentMeta {
        name: segment_path.file_name().unwrap().to_str().unwrap().into(),
        checksum: crc32fast::hash(&view.bytes),
    });
    // Sync the segment's directory entry before making it reachable.
    sync_directory(parent)?;
    let mut manifest_file = Builder::new()
        .prefix(".zito-manifest-")
        .tempfile_in(parent)?;
    serde_json::to_writer(manifest_file.as_file_mut(), &manifest)?;
    manifest_file.as_file().sync_all()?;
    manifest_file.persist(path)?;
    sync_directory(parent)?;
    Ok((manifest, view))
}

fn sync_directory(path: &Path) -> Result<()> {
    #[cfg(unix)]
    File::open(path)?.sync_all()?;
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}
