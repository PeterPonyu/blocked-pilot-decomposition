# Figure 5. What the two controls actually assert across every observation, next
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
# The axis prints the schema's finding names in this panel's data-driven order;
# the underlying finding column stays untouched for tables and evidence tracing.
f4$label <- factor(short_name(f4$finding), levels = short_name(pred_order))
if (anyNA(f4$label) || anyDuplicated(f4$label[seq_len(nrow(per_finding))])) {
  stop("fig5 finding axis labels are not a one-to-one mapping")
}
f4$series <- factor(f4$series, levels = c("Gold positives in the draw",
                                          "Asserted by the majority control",
                                          "Asserted by the retrieval control"))

p <- ggplot(f4, aes(x = label, y = count, fill = series)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_hline(yintercept = N_COHORT, linetype = "22", linewidth = FIGURE_RULE_WIDTH,
             colour = "grey20") +
  annotate("text", x = nrow(per_finding) + 0.4, y = N_COHORT, hjust = 1, vjust = -0.5,
           size = FIGURE_ANNOTATION_SIZE, colour = "grey20",
           label = sprintf("all %d labelled reports", N_COHORT)) +
  scale_fill_manual(values = c("Gold positives in the draw" = PAL$gold,
                               "Asserted by the majority control" = PAL$red,
                               "Asserted by the retrieval control" = PAL$blue),
                    name = NULL) +
  scale_y_continuous(name = "Reports labelled positive", limits = c(0, N_COHORT * 1.12),
                     breaks = seq(0, N_COHORT, 8), expand = c(0, 0)) +
  scale_x_discrete(name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  rtx_theme() +
  theme(axis.text.x = rotated_axis_text(),
        legend.position = "bottom")

save_fig(p, "fig5_control_assertions", FIGURE_TEXT_WIDTH_IN, 3.4)
