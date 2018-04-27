# Armored Bits Server

## Coding Guidelines

### Style

Please use the existing code base as a guideline.

- Spaces. Never tabs.
- Two space indents.
- 80 character column width.
- Never use partial functions. `head`, `tail`, `fromJust`, etc.

### Pull Requests

Please open an issue about what you intend to implement or solve and wait for a
core member to approve it.

If the issue was approved fork the repository, make your changes, and then open
a PR with a reference to the initial issue which was opened.

We want to avoid adding features which aren't on the roadmap, or that may
conflict with existing work.

### Lenses

We avoid Template Haskell because the code in this repository should be able to
be read by someone with no Haskell experience. Generated code obsfucates this.

Due to this restriction all lense definitions are to be written out manually.

```haskell
data Point = { _x :: Float, _y :: Float }

x :: Lens' Point Float
x = lens _x (\point value -> point { _x = value })

y :: Lens' Point Float
y = lens _y (\point value -> point { _y = value })
```
