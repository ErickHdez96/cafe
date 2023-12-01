# Half-baked ideas

Merge the parser and scanner into one step.

* Support for shebangs (e.g. `#!r6rs`).
* Make it easier to parse datum comments `#;`

Flatten the green-red parse tree into an array

```
struct Token {
    kind: SyntaxKind,
    // padding to keep token and node to the same size and alignment
    _: usize,
    __: usize,
}

type Atom = Id<[str]>;
struct ListData {
    // How many children the list has (may not be equal to the number
    // of tokens in the list.
    length: usize,
    // Index of the closing delimiter/last element in the list (if unclosed).
    end: usize,
}

union NodeData {
    atom: Atom,
    list: ListData,
}

struct Node {
    kind: SyntaxKind,
    data: NodeData,
}
```

```text
(lambda (x) x)

     -----------------------------------------------
     |                                             v
     0    1      2     3      4    5   6   7   8   9
------------------------------------------------------
| (-6-9 |   | lambda |   | (-1-6 | x | ) |   | x | ) |
------------------------------------------------------
                              |        ^
                              ----------
```
