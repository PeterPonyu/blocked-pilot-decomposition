# Figure 5. The only cells that cleared the presence gate, and what the controls
# did with them. The interval runs from the observed miss rate up to its 90 per
# cent upper bound, which is the honest width at these counts: both arms miss
# nearly everything, and the bound is wide enough that the two cannot be ranked.

f5 <- rbind(
  data.frame(finding = scored$finding, arm = "Majority template",
             n_present = scored$n_present, miss = scored$maj_miss,
             rate = scored$maj_rate, upper = scored$maj_upper, stringsAsFactors = FALSE),
  data.frame(finding = scored$finding, arm = "Indication retrieval",
             n_present = scored$n_present, miss = scored$ret_miss,
             rate = scored$ret_rate, upper = scored$ret_upper, stringsAsFactors = FALSE)
)
f5$panel <- sprintf("%s (%d gold positives)", short_name(f5$finding), f5$n_present)

p <- ggplot(f5, aes(x = arm, y = rate, colour = arm)) +
  geom_hline(yintercept = 1, linetype = "22", linewidth = 0.35, colour = "grey35") +
  geom_linerange(aes(ymin = rate, ymax = upper), linewidth = 0.5) +
  geom_point(size = 2.4) +
  geom_text(aes(y = upper, label = sprintf("90%% upper %.2f", upper)),
            vjust = -0.9, size = 2.4, colour = "grey25") +
  geom_text(aes(label = sprintf("%d of %d missed", miss, n_present)),
            vjust = 2.1, size = 2.4, colour = "grey25") +
  facet_wrap(~panel) +
  scale_colour_manual(values = c("Majority template" = "#B2182B",
                                 "Indication retrieval" = "#4D7EA8"), guide = "none") +
  scale_y_continuous(name = "Share of gold positives the control missed",
                     limits = c(0.74, 1.10), breaks = seq(0.75, 1.0, 0.05)) +
  scale_x_discrete(name = NULL) +
  labs(subtitle = sprintf("Each panel is conditional on its gold-positive count; n ranges from %d to %d",
                          min(scored$n_present), max(scored$n_present))) +
  rtx_theme() +
  theme(axis.text.x = element_text(size = 7.5),
        plot.subtitle = element_text(size = 7.1, colour = "grey25"))

save_fig(p, "fig6_scored_cells", 0.86 * FIGURE_TEXT_WIDTH_IN, 2.9)
