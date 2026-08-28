# Redaction of recorded paths

The result files in this archive were written by the runs that produced them,
and they recorded where they were running. Those paths describe a private
machine and are not published, so the archived copies were rewritten before
deposit. Describing the rules below without reproducing the paths they remove is
the point of this file, so the left column names each class of string rather
than quoting it.

The rewrite is textual and total: it substitutes path strings and nothing else.
Rules are applied longest match first.

| replaced | with |
|---|---|
| the absolute filesystem prefix of the machine the runs executed on | removed |
| the recorded path of an artifact that is archived here | the path it now has in this archive |
| the home directory of the account the runs executed under | `~/` |
| any remaining directory prefix belonging to the private source tree | `source-tree/` |

## What was checked

Every rewritten file was reparsed after substitution and compared against the
original with all string leaves erased. A changed number, a dropped key, a
reordered list or a lost record fails the export rather than being deposited.
For line-oriented records the record count is compared as well.

## Files rewritten

The digest on the left is the file as the run wrote it; the digest on the right
is the file in this archive, and it is the one the manifest binds and the build
verifies.

| path | substitutions | original sha256 | archived sha256 |
|---|---|---|---|
| `data/e-locks/p003c_nogo_lock.json` | 17 | `4b1a1e3e4f777383…` | `0853c9988fac11ce…` |
| `data/e-tier-blocked/sota_copy.json` | 1 | `1dd3365c638955fc…` | `5e9df8764ec61aea…` |
| `data/e-next/next_design.json` | 4 | `4a8dd1e543aab01d…` | `c1abcdd8c6a95003…` |
| `data/e-split/003-iu-split-manifest.v1.json` | 2 | `8550a81f9285eb3b…` | `587752f8dd4ae90a…` |
| `data/e-heldout/003-iu-heldout-32-uids.v1.json` | 2 | `4b67f58384bc18ed…` | `225222ccf7bc2e6d…` |
| `data/e-prevalence/iu_chexbert_gold_prevalence.json` | 3 | `b3128f550d0be2e4…` | `8c82c23ed0895a6b…` |
| `data/e-controls/iu_chexbert_control_baselines.json` | 1 | `1d64a6eeb89722d0…` | `b0e10241561f5363…` |
| `data/e-retrieval/static.json` | 1 | `e7426042fcb4d85d…` | `67934c64ca690ca7…` |
| `data/e-majority/naive.json` | 1 | `21c4b64cda7cfb90…` | `bbd757064bb6366c…` |
| `data/e-harness/paper_measurement_iu_harness.json` | 1 | `b8dd79a6dc313cc2…` | `7fde6811101d92b8…` |
| `data/e-primary/paper_primary.json` | 1 | `85c7bbd6cb5bd9a6…` | `d8d228267e044fc3…` |
