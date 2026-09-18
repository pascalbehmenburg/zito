//! Byte-oriented adaptation of pascalbehmenburg/sparse_ngram (MIT),
//! revision 980469f26c1e18478da7a015686c43ac6f115c6e, derived from
//! Danila Kutenin's sparse_ngrams. See THIRD_PARTY_NOTICES.md.
//!
//! The monotonic stack defines context-independent sparse grams. Hashes are
//! only a candidate filter: collisions must never substitute for verification.
use fxhash::FxHashSet;
use std::collections::VecDeque;

const MAX_GRAM: usize = 16;

#[derive(Clone, Copy)]
struct Anchor {
    hash: u32,
    pos: usize,
}

fn bigram_hash(pair: &[u8]) -> u32 {
    let a = (pair[0] as u64)
        .wrapping_mul(0xc6a4a7935bd1e995)
        .wrapping_add((pair[1] as u64).wrapping_mul(0x0228876a7198b743));
    a.wrapping_add(!a >> 47) as u32
}

// Defined on bytes, independent of platform endianness and Rust's Hash ABI.
fn fingerprint(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |hash, &byte| {
        (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3)
    })
}

/// At most 3n grams, with bounded fingerprint work per gram. Bigrams provide
/// the fallback used by the bounded query cover and by two-byte literals.
pub(crate) fn index_keys(bytes: &[u8]) -> Vec<u64> {
    let mut keys = FxHashSet::default();
    let mut stack: VecDeque<Anchor> = VecDeque::with_capacity(MAX_GRAM);
    for (pos, pair) in bytes.windows(2).enumerate() {
        // Older anchors cannot emit a gram within MAX_GRAM now or later.
        // Expiring them bounds workspace even for gigabytes of equal bytes.
        while stack
            .front()
            .is_some_and(|first| pos + 2 - first.pos > MAX_GRAM)
        {
            stack.pop_front();
        }
        keys.insert(fingerprint(pair));
        let anchor = Anchor {
            hash: bigram_hash(pair),
            pos,
        };
        while stack.back().is_some_and(|last| anchor.hash > last.hash) {
            let last = stack.back().unwrap();
            if pos + 2 - last.pos <= MAX_GRAM {
                keys.insert(fingerprint(&bytes[last.pos..pos + 2]));
            }
            while stack.len() > 1
                && stack[stack.len() - 1].hash == stack[stack.len() - 2].hash
            {
                stack.pop_back();
            }
            stack.pop_back();
        }
        if let Some(last) = stack.back()
            && pos + 2 - last.pos <= MAX_GRAM
        {
            keys.insert(fingerprint(&bytes[last.pos..pos + 2]));
        }
        stack.push_back(anchor);
    }
    let mut keys: Vec<_> = keys.into_iter().collect();
    keys.sort_unstable();
    keys
}

/// A bounded covering set from the upstream algorithm. Index and query use
/// identical byte hashes; UTF-8 boundaries are irrelevant to the filter.
pub(crate) fn query_keys(bytes: &[u8]) -> Vec<u64> {
    if bytes.len() < 2 {
        return Vec::new();
    }
    if bytes.len() == 2 {
        return vec![fingerprint(bytes)];
    }
    let mut keys = Vec::new();
    let mut stack: VecDeque<Anchor> = VecDeque::new();
    let mut emit = |start: usize, end: usize| {
        if end - start <= MAX_GRAM {
            keys.push(fingerprint(&bytes[start..end]));
        } else {
            keys.extend(bytes[start..end].windows(2).map(fingerprint));
        }
    };
    for (pos, pair) in bytes.windows(2).enumerate() {
        let anchor = Anchor {
            hash: bigram_hash(pair),
            pos,
        };
        if stack.len() > 1 && pos - stack.front().unwrap().pos + 3 >= MAX_GRAM {
            let front = stack.pop_front().unwrap();
            emit(front.pos, front.pos + 2);
        }
        while stack.back().is_some_and(|last| anchor.hash > last.hash) {
            if stack.front().unwrap().hash == stack.back().unwrap().hash {
                emit(stack.back().unwrap().pos, pos + 2);
                while stack.len() > 1 {
                    let end = stack.pop_back().unwrap().pos + 2;
                    emit(stack.back().unwrap().pos, end);
                }
            }
            stack.pop_back();
        }
        stack.push_back(anchor);
    }
    while stack.len() > 1 {
        let end = stack.pop_back().unwrap().pos + 2;
        emit(stack.back().unwrap().pos, end);
    }
    keys.sort_unstable();
    keys.dedup();
    keys
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::quickcheck;

    quickcheck! {
        fn substring_cover_never_excludes_a_match(bytes: Vec<u8>, start: usize, len: usize) -> bool {
            if bytes.is_empty() { return true; }
            let start = start % bytes.len();
            let end = start + len % (bytes.len() - start + 1);
            let indexed = index_keys(&bytes);
            query_keys(&bytes[start..end]).iter().all(|key| indexed.binary_search(key).is_ok())
        }
    }

    #[test]
    fn every_substring_of_repetitive_and_unicode_inputs_is_covered() {
        for bytes in [
            "aaaaaaaaabaaaaaaaaa",
            "abcabcabcabcabcabcabc",
            "café 🦀 日本語 café",
            "0123456789_abcdefghijklmnopqrstuvwxyz",
        ]
        .map(str::as_bytes)
        {
            let indexed = index_keys(bytes);
            for start in 0..bytes.len() {
                for end in start + 2..=bytes.len() {
                    let cover = query_keys(&bytes[start..end]);
                    assert!(!cover.is_empty());
                    assert!(
                        cover.iter().all(|k| indexed.binary_search(k).is_ok()),
                        "{bytes:?} {start}..{end}"
                    );
                }
            }
        }
    }
}
