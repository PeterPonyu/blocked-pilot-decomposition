# Figure 4. What the two controls actually assert across every observation, next
# to what the draw actually contains. The majority arm emits one constant report,
# so its bars are all-or-nothing by construction; drawing them beside the gold
# counts shows how little of the schema either control ever claims.

f4 <- rbind(
  data.frame(finding = per_finding$finding, series = "Gold positives in the draw",
             count = per_finding$n_present, stringsAsFactors = FALSE),
  data.frame(finding = per_finding$finding, series = "Asserted by the majority control",
             count = per_finding$maj_pred, stringsAsFactors = FALSE),
  data.frame(finding = per_finding$finding, series = "Asserted by the retrieval control",
             count = per_finding$ret_pred, stringsAsFactors = FALSE)
)
pred_order <- per_finding$finding[order(-per_finding$n_present, -per_finding$ret_pred)]
# Assign compact labels from the frozen schema order, then apply the panel's
# data-driven sort only to their factor levels.  The underlying finding column
# remains untouched for tables and evidence tracing.
finding_code <- setNames(sprintf("F%02d", seq_along(findings)), findings)
f4$label <- unname(finding_code[f4$finding])
if (anyNA(f4$label) || anyDuplicated(f4$label[seq_len(nrow(per_finding))])) {
  stop("fig5 finding display key is not a one-to-one mapping")
}
f4$label <- factor(f4$label, levels = unname(finding_code[pred_order]))
f4$series <- factor(f4$series, levels = c("Gold positives in the draw",
                                          "Asserted by the majority control",
                                          "Asserted by the retrieval control"))

p <- ggplot(f4, aes(x = label, y = count, fill = series)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_hline(yintercept = N_COHORT, linetype = "22", linewidth = 0.4, colour = "grey30") +
  annotate("text", x = nrow(per_finding) - 0.1, y = N_COHORT - 1.6, hjust = 1, size = 2.5,
           colour = "grey30",
           label = sprintf("all %d labelled reports", N_COHORT)) +
  scale_fill_manual(values = c("Gold positives in the draw" = "grey55",
                               "Asserted by the majority control" = "#B2182B",
                               "Asserted by the retrieval control" = "#4D7EA8"),
                    name = NULL) +
  scale_y_continuous(name = "Reports labelled positive", limits = c(0, N_COHORT + 1.2),
                     breaks = seq(0, 32, 8), expand = c(0, 0)) +
  scale_x_discrete(name = "Finding code (see caption key)") +
  labs(subtitle = sprintf("Counts are out of %d labelled reports; grey = gold-positive denominator",
                          N_COHORT)) +
  guides(fill = guide_legend(nrow = 1)) +
  rtx_theme() +
  theme(axis.text.x = element_text(hjust = 0.5, size = 7.6),
        legend.position = "bottom", legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"), legend.margin = margin(t = -2),
        plot.subtitle = element_text(size = 7.1, colour = "grey25"))

save_fig(p, "fig5_control_assertions", FIGURE_TEXT_WIDTH_IN, 3.4)
