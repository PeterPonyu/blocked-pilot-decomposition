# Reproducing the numbers

Every quantity printed in the manuscript is emitted by `paper/figs/make_figs.R`
from the artifacts listed below. None is typed into the prose. The figure code
re-hashes each artifact before reading it, so a modified or missing file stops
the build instead of producing a stale number.

## Bound artifacts

| path | role | bytes | sha256 |
|---|---|---|---|
| `data/e-locks/p003c_nogo_lock.json` | recorded_state | 3475 | `0853c9988fac11ce…` |
| `data/e-blocker/sota_chexbench_blocker.json` | recorded_state | 392 | `9e7ff622a2f6ba52…` |
| `data/e-tier-blocked/sota_copy.json` | recorded_state | 508 | `5e9df8764ec61aea…` |
| `data/e-next/next_design.json` | recorded_state | 11015 | `c1abcdd8c6a95003…` |
| `data/e-split/003-iu-split-manifest.v1.json` | protocol | 105795 | `587752f8dd4ae90a…` |
| `data/e-heldout/003-iu-heldout-32-uids.v1.json` | protocol | 1330 | `225222ccf7bc2e6d…` |
| `data/e-prevalence/iu_chexbert_gold_prevalence.json` | derived_table | 3248 | `8c82c23ed0895a6b…` |
| `data/e-controls/iu_chexbert_control_baselines.json` | derived_table | 6225 | `b0e10241561f5363…` |
| `data/e-retrieval/static.json` | derived_table | 816 | `67934c64ca690ca7…` |
| `data/e-majority/naive.json` | derived_table | 896 | `bbd757064bb6366c…` |
| `data/e-harness/paper_measurement_iu_harness.json` | recorded_state | 468 | `7fde6811101d92b8…` |
| `data/e-primary/paper_primary.json` | recorded_state | 548 | `d8d228267e044fc3…` |

Some of these files recorded the paths of the machine that produced them. Those path strings were rewritten before deposit; `REDACTION.md` states the rules, lists every file touched with both digests, and describes the check that proves no number changed.

## Not redistributed

The manuscript's evidence manifest binds one further artifact that this archive does not carry. No number in the manuscript is derived from that material; it is bound because the manuscript refers to the content, and held back for the reason below.

- The project's own working record of this direction. It is an internal narrative that names other directions, planning decisions and process labels, and no number in the manuscript comes from it. Everything it contributes to the manuscript is stated in the methods section and is separately bound in the recorded-state artifacts that are redistributed.

## Checking the archive without building it

```bash
python3 tools/bind_evidence.py paper --check
```

This re-hashes every path above against `paper/evidence/evidence_manifest.json`
and reports the first artifact that has drifted.

## Rebuilding

```bash
bash build.sh
```

Stage order is verify, regenerate, typeset. Each stage is a hard gate on the
next.
