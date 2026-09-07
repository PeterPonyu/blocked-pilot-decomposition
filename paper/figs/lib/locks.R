# The access obstacles, and the wording the manuscript is allowed to use for
# them.
#
# The recorded facts name internal scripts and flags, which do not belong in a
# manuscript. The set of obstacles is therefore taken from the evidence and the
# wording from this table; if the two ever disagree the build stops, so the
# figure cannot describe a set of obstacles that is no longer the recorded one.

LOCK_TEXT <- list(
  "HF contact" = list(
    kind = "Permission: a person or an institution must act",
    title = "Access request to the benchmark",
    detail = "The benchmark repository is contact-gated. Access is granted by its maintainers on request; no amount of local computation substitutes for the grant."),
  "CITI" = list(
    kind = "Permission: a person or an institution must act",
    title = "Human-subjects research training",
    detail = "A recognised research-ethics training certificate, issued to a named person. It is a separate requirement from the access request and does not imply it."),
  "PhysioNet MIMIC-CXR DUA" = list(
    kind = "Permission: a person or an institution must act",
    title = "Data use agreement for the underlying images",
    detail = "The images the benchmark scores are governed by their own credentialed data use agreement, which is a further step after access to the benchmark itself."),
  "data.json absent" = list(
    kind = "Artifact: a file absent from the retained workspace",
    title = "The evaluation split file",
    detail = "The file that defines which cases the benchmark scores was not present at any checked location. It comes with the access grant above rather than being a further permission, so it is a second thing to hold, not a second door."),
  "IU is not CheXbench" = list(
    kind = "Substitution refused: the local corpus is a different thing",
    title = "The locally held corpus is not the benchmark",
    detail = "A different public chest-radiograph corpus is available here. It is not the benchmark's data, and reporting a score on it under the benchmark's name would be a category error."),
  "wrap refuses n<=8" = list(
    kind = "Substitution refused: the local corpus is a different thing",
    title = "The scoring wrapper rejects the local sample size",
    detail = "The scoring code refuses any sample that is not the benchmark's own split size, so the eight-image local run cannot be dressed as a benchmark result even by accident.")
)

# The manuscript's central claim about the obstacles is that they are of three
# separable kinds and that no two of them are the same kind of thing. The order
# is the order the figure stacks them in and the order the counts are reported.
KIND_ORDER <- c("Permission: a person or an institution must act",
                "Artifact: a file absent from the retained workspace",
                "Substitution refused: the local corpus is a different thing")

# Tile geometry, in units of one line of detail text.  The obstacles are worded
# to different lengths, so no single wrap width gives them all the same number
# of lines; a fixed tile height therefore either clips the longest entry or
# leaves the others half empty.  Each tile is instead sized to the entry it
# holds, which is what keeps the figure as short as its content allows.
LOCK_WRAP_CHARS <- 95
LOCK_TITLE_UNITS <- 1.55   # the title line plus the space under it
LOCK_PAD_UNITS <- 0.42     # inset above the title and below the last detail line
LOCK_GAP_UNITS <- 0.34     # between one tile and the next

build_locks <- function(lock_rows) {
  if (!setequal(names(LOCK_TEXT), lock_rows$lock)) {
    stop("the recorded set of obstacles differs from the set this figure describes")
  }
  locks <- data.frame(
    lock = lock_rows$lock,
    stringsAsFactors = FALSE
  )
  field <- function(name) {
    vapply(locks$lock, function(k) LOCK_TEXT[[k]][[name]], character(1), USE.NAMES = FALSE)
  }
  locks$kind <- factor(field("kind"), levels = KIND_ORDER)
  if (anyNA(locks$kind)) stop("an obstacle carries a category this figure does not order")
  locks$title <- field("title")
  locks$detail <- wrap_to(field("detail"), LOCK_WRAP_CHARS)

  locks$lines <- lengths(strsplit(locks$detail, "\n", fixed = TRUE))
  locks$height <- 2 * LOCK_PAD_UNITS + LOCK_TITLE_UNITS + locks$lines
  # Stack downward from zero so the first obstacle reads at the top and the
  # panel's lower limit is simply the total the tiles came to.
  locks$top <- -cumsum(c(0, head(locks$height + LOCK_GAP_UNITS, -1)))
  locks$y <- locks$top - locks$height / 2
  locks
}

# The figure's y limits: the stack's own extent with the inter-tile gap repeated
# above and below, so the panel is exactly as tall as the tiles need and the
# outer spacing matches the spacing between them.
locks_limits <- function(locks) {
  c(min(locks$top - locks$height) - LOCK_GAP_UNITS, LOCK_GAP_UNITS)
}

# One line of detail text, in inches.  The y scale is in line units, so this is
# what converts the stack's height into a canvas height and the only place the
# figure's physical size is decided.
LOCK_LINE_IN <- 0.118

# The leading word of each category, which is what the counts are reported by.
lock_kind_of <- function(locks) sub(":.*$", "", as.character(locks$kind))
