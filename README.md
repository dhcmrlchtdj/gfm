http://www.vfmd.org/

---

> Parsing has two phases:
> In the first phase, construct the block structure of the document
> In the second phase, parse the contents of paragraphs into markdown elements

---

- One or more open blocks may be closed.
- One or more new blocks may be created as children of the last open block.
- Text may be added to the last (deepest) open block remaining on the tree.

