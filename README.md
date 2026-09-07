# Permission, prevalence, and the labeller: three separable reasons a chest-radiograph reporting pilot cannot answer its own question

Derived label tables, the recorded access-obstacle enumeration, the split and selection protocols, figure code and manuscript source for a study that decomposes one blocked chest-radiograph reporting project into an administrative obstacle, a corpus-level power ceiling and a labeller artifact.

This repository has not been deposited in a public archive, so it has no persistent identifier yet. One will be recorded here when an archive exists.

Repository: https://github.com/PeterPonyu/blocked-pilot-decomposition

## What is here

- `paper/tex/` — manuscript source. The abstract, the methods and the figure
  captions are separate files and each is self-contained.
- `paper/figs/` — the R code that draws every figure and emits every number the
  manuscript prints.
- `paper/evidence/` — the manifest binding each artifact to its SHA-256 digest.
- `data/` — the 12 artifacts the manifest names, at the bytes that
  were hashed.

## Not redistributed

The manuscript's evidence manifest binds one further artifact that this archive does not carry. No number in the manuscript is derived from that material; it is bound because the manuscript refers to the content, and held back for the reason below.

- The project's own working record of this direction. It is an internal narrative that names other directions, planning decisions and process labels, and no number in the manuscript comes from it. Everything it contributes to the manuscript is stated in the methods section and is separately bound in the recorded-state artifacts that are redistributed.

## Rebuild

```bash
bash build.sh
```

The build re-hashes every artifact before reading it and stops if any byte has
moved. Figures and printed numbers are regenerated from those bytes rather than
transcribed, so the manuscript cannot quietly disagree with its own data.

Requires `python3`, `Rscript` with `digest`, `ggplot2`, `jsonlite` and
`systemfonts`, and a TeX distribution with `latexmk`.

## Status

Working draft. Not submitted to any venue.

## Licence

Code: MIT (`LICENSE`). Manuscript text, figures and recorded result data:
CC BY 4.0 (`LICENSE-CONTENT`).
