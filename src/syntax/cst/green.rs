use std::{collections::HashMap, fmt, rc::Rc};

use crate::syntax::SyntaxKind;

#[derive(Debug, Default)]
pub struct GreenNodeBuilder {
    // TODO: Calculate the hash of the green tree and use that as the key
    // instead of having to store the GreenTree as well
    cache: HashMap<GreenTree, Rc<GreenTree>>,
    stack: Vec<(SyntaxKind, Vec<Rc<GreenTree>>)>,
    sk: SyntaxKind,
    children: Vec<Rc<GreenTree>>,
}

impl GreenNodeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start_node(&mut self, sk: SyntaxKind) {
        let current_state = (
            std::mem::replace(&mut self.sk, sk),
            std::mem::take(&mut self.children),
        );
        self.stack.push(current_state);
    }

    pub fn finish_node(&mut self) {
        let tree = self.finish();
        self.children.push(tree);
    }

    pub fn push_token(&mut self, kind: SyntaxKind, text: String) {
        let tree = self.get_or_intern_tree(GreenTree::token(kind, text));
        self.children.push(tree);
    }

    fn get_or_intern_tree(&mut self, green_tree: GreenTree) -> Rc<GreenTree> {
        if let Some(v) = self.cache.get(&green_tree) {
            Rc::clone(v)
        } else {
            let value = Rc::new(green_tree.clone());
            self.cache.insert(green_tree, Rc::clone(&value));
            value
        }
    }

    pub fn finish(&mut self) -> Rc<GreenTree> {
        let (mut kind, mut children) = self
            .stack
            .pop()
            .expect("start_node and finish_node don't match");
        std::mem::swap(&mut self.sk, &mut kind);
        std::mem::swap(&mut self.children, &mut children);
        self.get_or_intern_tree(GreenTree::node(kind, children))
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum GreenTree {
    Node(GreenNode),
    Token(GreenToken),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode {
    kind: SyntaxKind,
    text_length: usize,
    children: Vec<Rc<GreenTree>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    kind: SyntaxKind,
    text: String,
}

impl GreenTree {
    pub fn node(kind: SyntaxKind, children: Vec<Rc<GreenTree>>) -> Self {
        let text_length = children.iter().map(|c| c.text_length()).sum();
        Self::Node(GreenNode {
            kind,
            text_length,
            children,
        })
    }

    pub fn token(kind: SyntaxKind, text: String) -> Self {
        Self::Token(GreenToken { kind, text })
    }

    pub const fn kind(&self) -> SyntaxKind {
        match &self {
            GreenTree::Node(GreenNode { kind, .. }) | GreenTree::Token(GreenToken { kind, .. }) => {
                *kind
            }
        }
    }

    pub fn text_length(&self) -> usize {
        match &self {
            GreenTree::Node(GreenNode { text_length, .. }) => *text_length,
            GreenTree::Token(GreenToken { text, .. }) => text.len(),
        }
    }
}

impl GreenNode {
    pub const fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn children(&self) -> &[Rc<GreenTree>] {
        &self.children
    }
}

impl GreenToken {
    pub fn text(&self) -> &str {
        &self.text
    }

    pub const fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

impl fmt::Display for GreenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            GreenTree::Node(GreenNode { children, .. }) => {
                for c in children {
                    c.fmt(f)?;
                }
                Ok(())
            }
            GreenTree::Token(GreenToken { text, .. }) => text.fmt(f),
        }
    }
}

impl fmt::Debug for GreenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let mut precision = f.precision().unwrap_or_default();
            let padding = " ".repeat(width);
            match &self {
                GreenTree::Node(GreenNode { kind, children, .. }) => {
                    write!(f, "{padding}{kind:?}@{precision}..{}", self.text_length())?;
                    if !children.is_empty() {
                        writeln!(f)?;
                    }
                    for (i, c) in children.iter().enumerate() {
                        write!(
                            f,
                            "{:#width$.precision$?}",
                            c,
                            width = width + 2,
                            precision = precision
                        )?;
                        precision += c.text_length();
                        if i < children.len() - 1 {
                            writeln!(f)?;
                        }
                    }
                    Ok(())
                }
                GreenTree::Token(GreenToken { kind, text }) => write!(
                    f,
                    "{padding}{kind:?}@{precision}..{} \"{text}\"",
                    precision + self.text_length()
                ),
            }
        } else {
            match &self {
                GreenTree::Node(node) => node.fmt(f),
                GreenTree::Token(token) => token.fmt(f),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::SyntaxKind as SK;
    use super::*;

    #[test]
    fn tree_to_string() {
        let five = Rc::new(GreenTree::token(SK::Number, String::from("5")));
        let ten = Rc::new(GreenTree::token(SK::Number, String::from("10")));
        let ws = Rc::new(GreenTree::token(SK::Whitespace, String::from(" ")));
        let plus = Rc::new(GreenTree::token(SK::Identifier, String::from("+")));
        let tree = GreenTree::node(SK::Root, vec![five, ws.clone(), plus, ws, ten]);
        assert_eq!("5 + 10", tree.to_string());
    }

    #[test]
    fn build_simple_tree() {
        let mut b = GreenNodeBuilder::default();
        b.start_node(SK::Root);
        b.start_node(SK::List);
        b.push_token(SK::OpenDelim, "(".into());
        b.push_token(SK::Identifier, "+".into());
        b.push_token(SK::Whitespace, " ".into());
        b.push_token(SK::Identifier, "5".into());
        b.push_token(SK::Whitespace, " ".into());
        b.push_token(SK::Identifier, "10".into());
        b.push_token(SK::OpenDelim, ")".into());
        b.finish_node();
        let tree = b.finish();
        assert_eq!("(+ 5 10)", tree.to_string());

        expect![[r#"
            Root@0..8
              List@0..8
                OpenDelim@0..1 "("
                Identifier@1..2 "+"
                Whitespace@2..3 " "
                Identifier@3..4 "5"
                Whitespace@4..5 " "
                Identifier@5..7 "10"
                OpenDelim@7..8 ")"
        "#]]
        .assert_debug_eq(&tree);
    }

    #[test]
    fn interning() {
        fn plus_1_1(b: &mut GreenNodeBuilder) {
            b.start_node(SK::List);
            b.push_token(SK::OpenDelim, "(".into());
            b.push_token(SK::Identifier, "+".into());
            b.push_token(SK::Whitespace, " ".into());
            b.push_token(SK::Identifier, "1".into());
            b.push_token(SK::Whitespace, " ".into());
            b.push_token(SK::Identifier, "1".into());
            b.push_token(SK::OpenDelim, ")".into());
            b.finish_node();
        }

        let mut b = GreenNodeBuilder::default();
        b.start_node(SK::Root);
        b.start_node(SK::List);
        b.push_token(SK::OpenDelim, "(".into());
        b.push_token(SK::Identifier, "*".into());
        b.push_token(SK::Whitespace, " ".into());
        plus_1_1(&mut b);
        b.push_token(SK::Whitespace, " ".into());
        plus_1_1(&mut b);
        b.push_token(SK::OpenDelim, ")".into());
        b.finish_node();
        let tree = b.finish();
        assert_eq!("(* (+ 1 1) (+ 1 1))", tree.to_string());

        match tree.as_ref() {
            GreenTree::Node(GreenNode {
                kind: SK::Root,
                children,
                ..
            }) => {
                assert_eq!(1, children.len());
                match children[0].as_ref() {
                    GreenTree::Node(GreenNode {
                        kind: SK::List,
                        children,
                        ..
                    }) => {
                        assert_eq!(7, children.len());
                        // (+ 1 1)
                        assert!(Rc::ptr_eq(&children[3], &children[5]));
                        // whitespace
                        assert!(Rc::ptr_eq(&children[2], &children[4]));

                        match children[3].as_ref() {
                            GreenTree::Node(GreenNode {
                                kind: SK::List,
                                children: inner_children,
                                ..
                            }) => {
                                assert_eq!(7, inner_children.len());
                                // 1
                                assert!(Rc::ptr_eq(&inner_children[3], &inner_children[5]));
                                // whitespace
                                assert!(Rc::ptr_eq(&inner_children[2], &inner_children[4]));

                                // (
                                assert!(Rc::ptr_eq(&children[0], &inner_children[0]));
                                // )
                                assert!(Rc::ptr_eq(&children[6], &inner_children[6]));
                                // whitespace
                                assert!(Rc::ptr_eq(&children[2], &inner_children[2]));
                            }
                            _ => panic!(),
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}
