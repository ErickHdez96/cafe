# Parser

## Shebangs

Add support for shebangs (e.g. `#!r6rs`, `#!cafe`) to alter the lexical and datum syntax of the rest of the program.

[ ] Stream scanner

The scanner must be able to change rules on the fly.

## Validation

[ ] Strings
    [ ] Unterminated
    [ ] Invalid escape characters
    [ ] Invalid hex escape sequences
[ ] Numbers

# Build System

[ ] Mark parents clean if the newly calculated value is the same as the last one.
[ ] (maybe) If the task needs to be recalculated, delete all dependencies and trace again.
[ ] (maybe - depends on above one) Run all dependencies, if they're unchanged, return the currently cached value
