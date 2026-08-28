#!/usr/bin/env python3
"""Bind on-disk bytes into a paper's evidence manifest, and re-verify them later.

A manuscript in this workspace may not print a number that is not bound here.
Binding is deliberately dumb: it records a repo-relative path, its size, and its
sha256. Nothing is copied, nothing is regenerated. If the bytes move or change,
``--check`` fails and the manuscript is stale until a human looks at it.

Self-contained on purpose: the ``scripts`` name collides with a third-party
package in at least one conda env on this machine, so this file imports nothing
from its own directory.

Usage:
    python3 scripts/bind_evidence.py papers/<...>/<paper> --write
    python3 scripts/bind_evidence.py papers/<...>/<paper> --check
"""

from __future__ import annotations

import argparse
import datetime as _dt
import hashlib
import json
import sys
from pathlib import Path

BINDINGS_NAME = "BINDINGS.tsv"
MANIFEST_NAME = "evidence_manifest.json"
CHUNK = 1 << 20


def repo_root(start: Path) -> Path:
    for candidate in [start, *start.parents]:
        if (candidate / ".git").exists():
            return candidate
    raise SystemExit(f"no git root above {start}")


def sha256_of(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(CHUNK), b""):
            digest.update(block)
    return digest.hexdigest()


def read_bindings(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        raise SystemExit(f"missing {path}; nothing to bind")
    rows: list[dict[str, str]] = []
    for lineno, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        parts = [field.strip() for field in line.split("\t") if field.strip() != ""]
        if len(parts) < 3:
            raise SystemExit(f"{path}:{lineno}: expected id<TAB>path<TAB>role[<TAB>note]")
        rows.append(
            {
                "id": parts[0],
                "path": parts[1],
                "role": parts[2],
                "note": parts[3] if len(parts) > 3 else "",
            }
        )
    if not rows:
        raise SystemExit(f"{path} has no binding rows")
    ids = [row["id"] for row in rows]
    if len(set(ids)) != len(ids):
        raise SystemExit(f"{path} has duplicate entry ids")
    return rows


def under_allowed_root(rel: str, allowed: list[str]) -> bool:
    return any(rel == root or rel.startswith(root.rstrip("/") + "/") for root in allowed)


def load_manifest(paper_dir: Path) -> tuple[Path, dict]:
    manifest_path = paper_dir / "evidence" / MANIFEST_NAME
    if not manifest_path.exists():
        raise SystemExit(f"missing {manifest_path}")
    return manifest_path, json.loads(manifest_path.read_text(encoding="utf-8"))


def do_write(paper_dir: Path, root: Path) -> int:
    manifest_path, manifest = load_manifest(paper_dir)
    allowed = list(manifest.get("allowed_roots", []))
    rows = read_bindings(paper_dir / "evidence" / BINDINGS_NAME)

    entries = []
    for row in rows:
        rel = row["path"]
        if allowed and not under_allowed_root(rel, allowed):
            print(f"FAIL {row['id']}: {rel} is outside the declared evidence roots", file=sys.stderr)
            return 1
        target = root / rel
        if not target.is_file():
            print(f"FAIL {row['id']}: {rel} does not exist on disk", file=sys.stderr)
            return 1
        entries.append(
            {
                "id": row["id"],
                "path": rel,
                "role": row["role"],
                "note": row["note"],
                "bytes": target.stat().st_size,
                "sha256": sha256_of(target),
            }
        )

    manifest["entries"] = entries
    manifest["state"] = "BOUND"
    manifest["bound_utc"] = _dt.datetime.now(_dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    manifest_path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(f"bound {len(entries)} entries into {manifest_path.relative_to(root)}")
    return 0


def do_check(paper_dir: Path, root: Path) -> int:
    manifest_path, manifest = load_manifest(paper_dir)
    entries = manifest.get("entries", [])
    if manifest.get("state") != "BOUND" or not entries:
        print(f"FAIL {manifest_path.relative_to(root)}: state is not BOUND", file=sys.stderr)
        return 1

    failures = 0
    for entry in entries:
        target = root / entry["path"]
        if not target.is_file():
            print(f"FAIL {entry['id']}: bound path has disappeared", file=sys.stderr)
            failures += 1
            continue
        actual = sha256_of(target)
        if actual != entry["sha256"]:
            print(f"FAIL {entry['id']}: sha256 drifted from the bound value", file=sys.stderr)
            failures += 1
            continue
        if target.stat().st_size != entry["bytes"]:
            print(f"FAIL {entry['id']}: byte count drifted from the bound value", file=sys.stderr)
            failures += 1

    if failures:
        print(f"{failures} evidence entr{'y' if failures == 1 else 'ies'} failed re-verification", file=sys.stderr)
        return 1
    print(f"OK {len(entries)} evidence entries re-verified against disk")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("paper_dir", type=Path)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true", help="hash the bindings and freeze them into the manifest")
    mode.add_argument("--check", action="store_true", help="re-hash the bound paths and fail on any drift")
    args = parser.parse_args(argv)

    paper_dir = args.paper_dir.resolve()
    if not paper_dir.is_dir():
        raise SystemExit(f"not a directory: {paper_dir}")
    root = repo_root(paper_dir)

    return do_write(paper_dir, root) if args.write else do_check(paper_dir, root)


if __name__ == "__main__":
    raise SystemExit(main())
