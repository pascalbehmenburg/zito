use std::collections::HashSet;
use std::fs;

use tree_sitter::{
    Language, Node, Parser, Query, QueryCursor, StreamingIterator,
};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: String,
    pub byte_range: (usize, usize),
}

// todo use recursive AST walking to keep track of scope-paths
// for blocks use smth. like block@node_id
// in the rust case functions and modules need
// to be unique anyway or else the code is syntactically incorrect hence we wont store id's for them (yet)
// create symbol postings as such:
// struct SymbolOrRef {
//     name: String,
//     kind: SymbolKind,
//     location: FilePos,
//     scope_path: Vec<String>, <-- this might just be a string which we split not sure yet
//     is_reference: bool,
// }
// todo implement goto definition
// todo implement find references (see how this usually behaves in IDEs)
// todo make logic as modular and generic as possible to support other languages
// todo store the symbol index in a seperate folder but use rkyv + zlib compression
pub fn extract_symbols(
    source_code: &str,
    lang: Language,
    query_scm: &str,
) -> Vec<Symbol> {
    let mut parser = Parser::new();
    parser.set_language(&lang).unwrap();
    let tree = parser.parse(source_code, None).unwrap();

    let query_source = fs::read_to_string(query_scm).expect("query file");
    let query = Query::new(&lang, &query_source).expect("valid TSQuery");

    let mut cursor = QueryCursor::new();
    let matches =
        cursor.matches(&query, tree.root_node(), source_code.as_bytes());

    let mut unique_symbols = HashSet::new();
    let mut symbols = Vec::new();
    matches.for_each(|m| {
        for capture in m.captures.iter() {
            let name = query.capture_names()[capture.index as usize];
            let node: Node = capture.node;
            if name == "name" {
                let text =
                    node.utf8_text(source_code.as_bytes()).unwrap().to_string();
                let byte_range = (node.start_byte(), node.end_byte());
                let parent = node.parent().unwrap();
                let kind = match parent.kind() {
                    "function_item" => "@definition.function",
                    "trait_item" => "@definition.interface",
                    "type_item" => "@definition.class",
                    "struct_item" => "@definition.class",
                    "enum_item" => "@definition.enum",
                    "union_item" => "@definition.union",
                    "declaration_list" => "@definition.method",
                    "macro_definition" => "@definition.macro",
                    "mod_item" => "@definition.module",
                    _ => continue,
                };
                let symbol = Symbol {
                    name: text,
                    kind: kind.to_string(),
                    byte_range,
                };
                if unique_symbols.insert(symbol.clone()) {
                    symbols.push(symbol);
                }
            }
        }
    });

    symbols
}
