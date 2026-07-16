# CRAN comments for qgraph 2.0

This is a version update of the existing CRAN package qgraph. The current
CRAN release is 1.9.8.

## Summary of the release

See NEWS for the full list of changes. Highlights:

* New interactive HTML output: `filetype = "html"` (or the new exported
  function `qgraphHTML()`) writes a self-contained HTML file rendering the
  graph as SVG in the same layout and style as the R plot.
* New bridge centrality measures (Jones, Ma, & McNally, 2021).
* The most commonly used arguments of `qgraph()` are now named explicitly in
  the function definition so that they are visible to auto-completion. They
  are placed after `...` and are therefore matched by exact name only,
  exactly as when they were part of `...`; existing code is unaffected.
* Consistently styled alternative names are now accepted for arguments with
  historically mixed naming styles (e.g. `node.size` for `vsize`). The
  original names continue to work.
* Bug fixes found in a code audit, and a documentation overhaul.
* The Description field no longer begins with "Fork of qgraph -". That
  wording was contributed in 2021 by an author maintaining a fork and does
  not describe this package, which is the canonical qgraph.

## Test environments

* Local: macOS Sonoma 14.2.1 (aarch64-apple-darwin20), R 4.5.3 (2026-03-11)

## R CMD check --as-cran results

0 ERRORs, 0 WARNINGs, 1 NOTE.

The NOTE is a property of the local test machine rather than of the package:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: 'tidy' doesn't look like recent enough
HTML Tidy.
```

macOS ships an outdated HTML Tidy, so this check is skipped locally; it is
expected to pass on the CRAN build machines.

Examples (including `--run-donttest`) and compiled code check cleanly.
