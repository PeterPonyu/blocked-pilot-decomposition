# Reproducing the numbers

Every quantity printed in the manuscript is emitted by `paper/figs/make_figs.R`
from the artifacts listed below. None is typed into the prose. The figure code
re-hashes each artifact before reading it, so a modified or missing file stops
the build instead of producing a stale number.

## Bound artifacts

| path | role | bytes | sha256 |
|---|---|---|---|
| `data/e-locks/p003c_nogo_lock.json` | recorded_state | 3496 | `8e2a238a70bf41e2…` |
| `data/e-blocker/sota_chexbench_blocker.json` | recorded_state | 392 | `9e7ff622a2f6ba52…` |
| `data/e-tier-blocked/sota_copy.json` | recorded_state | 508 | `5e9df8764ec61aea…` |
| `data/e-next/next_design.json` | recorded_state | 10990 | `a9da86729b88bfa1…` |
| `data/e-split/003-iu-split-manifest.v1.json` | protocol | 105795 | `587752f8dd4ae90a…` |
| `data/e-heldout/003-iu-heldout-32-uids.v1.json` | protocol | 1330 | `225222ccf7bc2e6d…` |
| `data/e-prevalence/iu_chexbert_gold_prevalence.json` | derived_table | 3248 | `8c82c23ed0895a6b…` |
| `data/e-controls/iu_chexbert_control_baselines.json` | derived_table | 6225 | `b0e10241561f5363…` |
| `data/e-retrieval/static.json` | derived_table | 816 | `67934c64ca690ca7…` |
| `data/e-majority/naive.json` | derived_table | 896 | `bbd757064bb6366c…` |
| `data/e-harness/paper_measurement_iu_harness.json` | recorded_state | 468 | `7fde6811101d92b8…` |
| `data/e-primary/paper_primary.json` | recorded_state | 548 | `d8d228267e044fc3…` |
| `data/e-battery/battery.json` | derived_table | 96222 | `4fbae4864f7be134…` |
| `data/e-controls-333/controls_333.json` | derived_table | 232193 | `ed1bdbdc3d50bd6c…` |
| `data/e-gold-ablation/gold_placeholder_ablation.json` | derived_table | 167117 | `eca24590238e2d14…` |
| `data/e-audit-receipt/RECEIPT.json` | receipt | 5099 | `da16f78bdeffa705…` |

Some of these files recorded the paths of the machine that produced them. Those path strings and source links were refreshed before deposit; `REDACTION.md` states the rules, lists every file touched with both digests, and describes the check that proves no number changed.

## Not included

This archive leaves out one extra file named in the paper's evidence list. The paper does not take any number from it.

- A private working note. The paper does not use any number from it. Those facts are already in the methods and in the result files included here.

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

The steps are check the files, redraw the figures, then typeset. Each step
must finish before the next one starts.
