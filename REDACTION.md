# Local paths rewritten before deposit

Some result files recorded the machine they ran on, including local folder
names. Those strings are not published. The copies in this archive were
rewritten before deposit. The left column names each class of string rather
than quoting it.

The rewrite changes path strings only. Numbers and table structure stay the
same. Longer matches are applied first.

| replaced | with |
|---|---|
| the absolute filesystem prefix of the machine the archive was assembled on | removed |
| the checkout prefix of a rented machine a run executed on | removed |
| the remaining scratch-mount prefix of that rented machine | `<remote>/` |
| the recorded path of an artifact that is archived here | the path it now has in this archive |
| the home directory of the account the runs executed under | `~/` |
| any remaining directory prefix belonging to the private source tree | `source-tree/` |
| a branch of the private repository named in a recorded instruction | `<private-branch>` |
| a private project status word | the ordinary word it stands for |

Both machine prefixes are removed before a file is mapped to its place in this
archive, so the same path recorded on two machines becomes the same archived
string. A study that never left one machine will only show some of these
substitutions.

The last two rows rewrite recorded values, never keys. A reader comparing an
archived file with the original should see the same fields and the same
numbers; only a local name is changed.

## What was checked

Every rewritten file was read again after substitution and compared with the
original after all string values were blanked. A changed number, a dropped
field, a reordered list or a lost record stops the export. For line-oriented
files the line count is compared as well.

## Files rewritten

The hash on the left is the file as the run wrote it. The hash on the right is
the file in this archive, and it is the one the file list names and the build
checks.

| path | path substitutions | receipt-link refreshes | original sha256 | archived sha256 |
|---|---:|---:|---|---|
| `data/e-locks/p003c_nogo_lock.json` | 20 | 0 | `4b1a1e3e4f777383…` | `8e2a238a70bf41e2…` |
| `data/e-tier-blocked/sota_copy.json` | 1 | 0 | `1dd3365c638955fc…` | `5e9df8764ec61aea…` |
| `data/e-next/next_design.json` | 7 | 0 | `4a8dd1e543aab01d…` | `a9da86729b88bfa1…` |
| `data/e-split/003-iu-split-manifest.v1.json` | 2 | 0 | `8550a81f9285eb3b…` | `587752f8dd4ae90a…` |
| `data/e-heldout/003-iu-heldout-32-uids.v1.json` | 2 | 0 | `4b67f58384bc18ed…` | `225222ccf7bc2e6d…` |
| `data/e-prevalence/iu_chexbert_gold_prevalence.json` | 3 | 0 | `b3128f550d0be2e4…` | `8c82c23ed0895a6b…` |
| `data/e-controls/iu_chexbert_control_baselines.json` | 1 | 0 | `1d64a6eeb89722d0…` | `b0e10241561f5363…` |
| `data/e-retrieval/static.json` | 1 | 0 | `e7426042fcb4d85d…` | `67934c64ca690ca7…` |
| `data/e-majority/naive.json` | 1 | 0 | `21c4b64cda7cfb90…` | `bbd757064bb6366c…` |
| `data/e-harness/paper_measurement_iu_harness.json` | 1 | 0 | `b8dd79a6dc313cc2…` | `7fde6811101d92b8…` |
| `data/e-primary/paper_primary.json` | 1 | 0 | `85c7bbd6cb5bd9a6…` | `d8d228267e044fc3…` |
