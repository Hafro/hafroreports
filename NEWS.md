# hafroreports (development version)

## Category 3 figures and advice helpers (October 2026)

The figure and data helpers that the category 3 (rfb rule) stock
repositories each kept a copy of:

* **Tech report figures of the rule:** `hr_techreport_plot_rfb_index()`,
  `hr_techreport_plot_rfb_lc()`, `hr_techreport_plot_rfb_ml()`,
  `hr_techreport_plot_rfb_f()` and `hr_techreport_rfb_theme()` (13-cas,
  14-mon, 24-lem, 25-wit, 26-meg, 27-dab, 60-norway-redfish). I_loss as a
  point (default, at `ref_points$I_lim_year` or the lowest index) or a line,
  label positions, axis ranges, unit and interval are arguments.
* **DSL figures of Norway redfish:** `hr_techreport_dsl_basis()`,
  `hr_techreport_plot_dsl_lc()`, `hr_techreport_plot_dsl_ml()`.
* **Advice sheets:** `hr_advice_plot_index()` gains `index_ab_span`
  (`"periods"`: the index A and B lines meet at the half years) and
  `index_ab_colour`; new `hr_advice_plot_fproxy()` for the length-based
  fishing pressure proxy (years, points, a y axis around 1);
  `hr_advice_data_tac()` gains `landings_year_end` (only the fishing years
  that had ended when the advice was given) and accepts a lazy table.
* New `hr_locale` keys for these figures (`rfb_biomass_index`, `tonnes`,
  `length_cm`, `frequency`, `number`, `modal_abundance_50`,
  `modal_abundance_50_of`, `length_based_ref_points`,
  `length_distribution_year`, `max_length`, `quantile_99`,
  `mean_length_catch_cm`), so the figures follow `hr.lang`.

The figures keep each stock's axis ranges, labels, units and reference
lines. Where the copies differed only in style, one style was kept: the
L_inf label is plotmath `L[infinity]`, the 50% modal abundance label is bold
at 0.6 x the base size, the L_c / L_F=M bars are 0.5 wide and their labels
2% of the mode below the axis.

AI use: Claude (Anthropic) wrote these functions, their tests and this entry
from the stock repositories' helpers (9 October 2026). The stock pipelines
were rerun with them on fixed database snapshots: every target and text is
unchanged, and each changed figure was compared with the old one (see the
stock READMEs).
