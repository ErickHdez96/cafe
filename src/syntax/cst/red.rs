use std::{fmt, rc::Rc};

use crate::syntax::SyntaxKind;

use super::GreenTree;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RedTree {
    green: Rc<GreenTree>,
    offset: usize,
    parent: Option<Rc<RedTree>>,
}

impl RedTree {
    pub fn new(green: &Rc<GreenTree>) -> Rc<Self> {
        Rc::new(Self {
            offset: 0,
            parent: None,
            green: Rc::clone(green),
        })
    }

    pub const fn green(&self) -> &Rc<GreenTree> {
        &self.green
    }

    pub const fn offset(&self) -> usize {
        self.offset
    }

    pub fn with_parent(parent: &Rc<RedTree>, offset: usize, green: &Rc<GreenTree>) -> Rc<Self> {
        Rc::new(Self {
            offset,
            parent: Some(Rc::clone(parent)),
            green: Rc::clone(green),
        })
    }

    pub fn children(red: &Rc<Self>) -> Vec<Rc<Self>> {
        match red.green.as_ref() {
            GreenTree::Node(node) => {
                let children = node.children();
                let mut offset = red.offset;
                let mut red_children = Vec::with_capacity(children.len());

                for c in children {
                    red_children.push(RedTree::with_parent(red, offset, c));
                    offset += c.text_length();
                }

                red_children
            }
            GreenTree::Token { .. } => vec![],
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.green.kind()
    }
}

impl fmt::Display for RedTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.green.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::{cst::GreenNodeBuilder, SyntaxKind as SK};

    use super::*;

    #[test]
    fn simple_tree() {
        let mut b = GreenNodeBuilder::default();
        b.start_node(SK::Root);
        b.push_token(SK::OpenDelim, "(".into());
        b.push_token(SK::Identifier, "+".into());
        b.push_token(SK::Whitespace, " ".into());
        b.push_token(SK::Identifier, "5".into());
        b.push_token(SK::Whitespace, " ".into());
        b.push_token(SK::Identifier, "10".into());
        b.push_token(SK::OpenDelim, ")".into());
        let tree = RedTree::new(&b.finish());

        let children = RedTree::children(&tree);
        assert_eq!(7, children.len());
        assert!(Rc::ptr_eq(children[0].parent.as_ref().unwrap(), &tree));
        assert_eq!(0, children[0].offset);
        assert_eq!(1, children[1].offset);
        assert_eq!(2, children[2].offset);
        assert_eq!(3, children[3].offset);
        assert_eq!(4, children[4].offset);
        assert_eq!(5, children[5].offset);
        assert_eq!(7, children[6].offset);
    }
}
