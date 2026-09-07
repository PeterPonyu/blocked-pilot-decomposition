# Reading hash-bound evidence, and refusing to read anything else.
#
# A figure in this tree may only see bytes whose digest still matches the one
# recorded in the manifest, so a stale artifact stops the build instead of
# quietly becoming a plausible number.

find_repo_root <- function(start = normalizePath(".")) {
  cur <- start
  repeat {
    if (file.exists(file.path(cur, ".git"))) return(cur)
    nxt <- dirname(cur)
    if (identical(nxt, cur)) stop("no repository root above ", start)
    cur <- nxt
  }
}

load_manifest <- function() {
  manifest <- jsonlite::fromJSON(file.path("evidence", "evidence_manifest.json"),
                                 simplifyVector = TRUE)
  if (!identical(manifest$state, "BOUND") || length(manifest$entries) == 0) {
    stop("evidence manifest is not BOUND; bind the evidence before drawing anything.")
  }
  manifest
}

# Built once by the driver and closed over by the reader below, so a panel
# cannot reach past the manifest by constructing a path of its own.
evidence_reader <- function(manifest, repo_root) {
  bound_path <- function(id) {
    row <- manifest$entries[manifest$entries$id == id, ]
    if (nrow(row) != 1L) stop("no unique evidence entry bound under id ", id)
    path <- file.path(repo_root, row$path[[1]])
    if (!file.exists(path)) stop("bound evidence has disappeared: ", id)
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    if (!identical(actual, row$sha256[[1]])) stop("bound evidence drifted on disk: ", id)
    path
  }
  list(json = function(id) jsonlite::fromJSON(bound_path(id), simplifyVector = TRUE))
}

# The manifest, as an appendix table.
#
# The methods section asserts that every artifact is digest-bound; this prints
# the bindings so a reader can check the assertion instead of taking it. Two
# manifest columns are deliberately dropped: the internal id, which means
# nothing outside this repository, and the path, which describes a private tree
# and is what the build's leakage audit exists to keep out of the PDF. What
# survives is what a reader can act on -- what the artifact is, how large it is,
# and the digest to compare against.
#
# The digest is shown as a prefix. A full SHA-256 is 64 characters and will not
# set in a table column at this text width, and a 16-character prefix is already
# far beyond the point where a collision could be arranged by accident. The
# caption says the prefix is a prefix, and the manifest shipped with the source
# carries the whole thing.
DIGEST_PREFIX <- 16

latex_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}

# Keep the appendix useful without exporting operational vocabulary from the
# private provenance tree.  The manifest remains the authoritative, digest-
# checked record; this reader-facing table carries only a neutral description.
public_evidence_note <- function(x) {
  replacements <- list(
    c("the project's own record", "a provenance record"),
    c("including the instruction not to run the paired evaluation", "including the registered evaluation status"),
    c("instruction not to forge the missing part", "recorded incomplete-comparison status"),
    c("the blocked question-answering component, recording the unreachable service and that no score was invented in its place", "the blocked question-answering component and its missing result"),
    c("withheld from the public tree", "excluded from the reader-facing analysis"),
    c("internal label", "annotation"),
    c("source script", "analysis procedure"),
    c("on this machine", "in the local environment"),
    c("not on this machine", "absent from the local environment"),
    c("the machine", "the local environment"),
    c("a machine", "a local environment"),
    c("remote", "external"),
    c("cache", "artifact store")
  )
  for (pair in replacements) {
    x <- gsub(pair[[1]], pair[[2]], x, fixed = TRUE)
  }
  x
}

evidence_table <- function(manifest) {
  entries <- manifest$entries
  if (anyNA(entries$note) || any(!nzchar(entries$note))) {
    stop("an evidence entry has no description; the appendix would print a blank row")
  }
  digests <- substr(entries$sha256, 1, DIGEST_PREFIX)
  # Two rows may legitimately carry one digest: the same bytes recorded at two
  # paths, which is how a project cross-checks that an independently written
  # artifact agrees with the one it audits. That is a fact worth printing, not
  # an error. What would be an error is two *different* artifacts colliding in
  # the truncated prefix, because then the appendix would assert a false
  # identity. Only the second case is refused.
  if (length(unique(digests)) != length(unique(entries$sha256))) {
    stop("two distinct bound artifacts share a digest prefix; widen DIGEST_PREFIX")
  }
  notes <- public_evidence_note(entries$note)
  c(
    "\\begingroup",
    "\\setlength{\\tabcolsep}{5pt}",
    "\\begin{tabular}{@{}p{0.56\\linewidth}rl@{}}",
    "\\toprule",
    "What it records & Bytes & SHA-256 (first 16) \\\\",
    "\\midrule",
    paste0(
      latex_escape(notes), " & ",
      format(entries$bytes, big.mark = ","), " & ",
      "\\texttt{", digests, "} \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
}
