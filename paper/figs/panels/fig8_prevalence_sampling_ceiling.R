# Figure 8. Full-split prevalence against the observed 32-report draw.
# The red cells are underpowered under the preregistered n_present >= 5 gate.

gold <- controls_333$gold$present_counts
obs <- controls$majority$per_finding
findings8 <- names(gold)
observed <- vapply(findings8, function(f) as.numeric(obs[[f]]$n_present), numeric(1))
if (!identical(as.integer(N_TEST), as.integer(controls_333$n)) ||
    !identical(as.integer(N_COHORT), as.integer(controls$n))) {
  stop("sampling-ceiling panel denominators disagree with bound 333/32 artifacts")
}
full <- as.numeric(gold)
expected <- N_COHORT * full / N_TEST
d <- data.frame(finding = findings8, code = sprintf("F%02d", seq_along(findings8)),
                full = full, observed = observed, expected = expected,
                underpowered = observed < MIN_PRESENT,
                stringsAsFactors = FALSE)
d$code <- factor(d$code, levels = d$code)

p <- ggplot(d, aes(x = code)) +
  geom_col(aes(y = full, fill = underpowered), width = 0.68) +
  geom_point(aes(y = observed), size = 1.8, colour = "#2166AC") +
  geom_point(aes(y = expected), shape = 1, size = 1.8, colour = "grey20") +
  geom_hline(yintercept = MIN_PRESENT, linetype = "22", linewidth = 0.4, colour = "#B2182B") +
  scale_fill_manual(values = c(`FALSE` = "#B7D4EA", `TRUE` = "#E6A6A6"), guide = "none") +
  scale_y_continuous(name = sprintf("Positive labels (full split; n = %d)", N_TEST),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Finding code (see caption key)",
       subtitle = sprintf("Bars: %d-row prevalence; filled/open points: observed/expected in %d rows; red = n_present < 5", N_TEST, N_COHORT)) +
  rtx_theme() +
  theme(axis.text.x = element_text(size = 7.4),
        plot.subtitle = element_text(size = 7.0, colour = "grey25"))

save_fig(p, "fig8_prevalence_sampling_ceiling", FIGURE_TEXT_WIDTH_IN, 3.35)
