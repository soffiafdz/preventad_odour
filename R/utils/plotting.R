# =============================================================================
# Plotting Utilities
# =============================================================================
# Generic theme, colours and saving with ggplot2. Project-specific
# plot_<thing>() functions go below the generic section.
# =============================================================================

source(here::here("R/utils/data_io.R"))

# ----- Color Palettes -----

#' Color palettes for manuscript figures
#'
#' The single source of colours for every figure, so a variable has the same
#' colour everywhere. Do not redefine colours in individual plotting
#' functions; add a named entry here instead.
#'
#' @return Named list of color values
manuscript_colors <- function() {
  list(
    female = "#8B0000",        # Dark red
    male = "#191970",          # Midnight blue
    combined = "gray40",
    reference_line = "gray50",
    # Project entries go here: one per group, predictor or measure, taken
    # from the tol_bright or tol_muted palettes in get_palette
    # Group colours are at least 15 (OKLab x 100) from the slide brand
    # colours (docs/STYLE.md): tol_bright blue was 8.9 from StoP-AD teal and
    # tol_bright red 10.9 from McGill red, so these come from tol_muted
    # Trajectory classes, in the order of trajectories.class_labels
    trajectory_class_1 = "#332288",   # tol_muted indigo
    trajectory_class_2 = "#882255",   # tol_muted wine
    trajectory_class_3 = "#999933",   # tol_muted olive
    # Random-effect structures compared in 02_fit_trajectories.R
    random_none = "#DDCC77",          # tol_muted sand
    random_intercept = "#44AA99",     # tol_muted teal
    random_slope = "#332288",         # tol_muted indigo
    # PET groups: negative, positive, and A+T- (amyloid without tau).
    # Negative is the reference, in grey, so it does not share the main
    # trajectory class's colour; 4.5:1 on white, as lines and label fills
    pathology_negative = "#777777",
    pathology_positive = "#882255",   # tol_muted wine
    # Tol medium-contrast dark yellow: tol_bright yellow (#CCBB44) has 1.9:1
    # contrast on white, too little for its direct label
    pathology_amyloid_only = "#997700",
    # Baseline UPSIT levels shown in the RBANS and MCI figures
    upsit_high = "#228833",           # tol_bright green
    upsit_low = "#AA3377",            # tol_bright purple
    # A-T+ (tau without amyloid), the fourth A/T group
    pathology_tau_only = "#88CCEE",   # tol_muted cyan
    # Olfactory diagnoses from the UPSIT norms, light to dark with severity
    # (Tol's YlOrBr; his #CC4C02 is 8.6 from McGill red, so #993404)
    diagnosis_normosmia = "#FEE391",
    diagnosis_mild = "#FEC44F",
    diagnosis_moderate = "#FB9A29",
    diagnosis_severe = "#993404",
    diagnosis_anosmia = "#662506"
  )
}

#' Get standard color palette
#'
#' Colourblind-safe palettes. Use manuscript_colors() for named, per-variable
#' colours.
#'
#' @param type "default" (Okabe-Ito), "tol_bright", "tol_muted", "sex",
#'   "sex_md" (ggtext labels)
#' @return Color vector
get_palette <- function(type = "default") {
  colors.lst <- manuscript_colors()
  palettes <- list(
    default = c(
      "#999999", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
    ),
    # Paul Tol, https://personal.sron.nl/~pault/
    tol_bright = c(
      blue = "#4477AA", cyan = "#66CCEE", green = "#228833",
      yellow = "#CCBB44", red = "#EE6677", purple = "#AA3377",
      grey = "#BBBBBB"
    ),
    tol_muted = c(
      indigo = "#332288", cyan = "#88CCEE", teal = "#44AA99",
      green = "#117733", olive = "#999933", sand = "#DDCC77",
      rose = "#CC6677", wine = "#882255", purple = "#AA4499"
    ),
    sex = c(
      Female = colors.lst$female,
      Male = colors.lst$male,
      Combined = colors.lst$combined
    ),
    sex_md = c(
      Female = sprintf(
        "<span style='color: %s;'>Female</span>", colors.lst$female
      ),
      Male = sprintf(
        "<span style='color: %s;'>Male</span>", colors.lst$male
      )
    )
  )

  if (!type %in% names(palettes)) {
    stop("Unknown palette: ", type, call. = FALSE)
  }
  palettes[[type]]
}

# ----- Plot Themes -----

#' Standard ggplot theme for publications
#' @param base_size Base font size
#' @param use_markdown Whether to use ggtext markdown in axis titles and text,
#'   strips and the plot title (requires the ggtext package)
#' @return ggplot2 theme object
theme_publication <- function(base_size = 10, use_markdown = FALSE) {
  base_theme <- ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1),
      plot.caption = ggplot2::element_text(size = base_size - 3),
      legend.position = "bottom"
    )

  if (use_markdown) {
    if (!requireNamespace("ggtext", quietly = TRUE)) {
      stop("use_markdown = TRUE requires the ggtext package", call. = FALSE)
    }
    base_theme <- base_theme +
      ggplot2::theme(
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        axis.text.x = ggtext::element_markdown(),
        axis.text.y = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown()
      )
  }

  base_theme
}

# ----- Plot Helpers -----

#' Save plot with standard settings
#' @param plot ggplot object
#' @param filename Output filename; the extension sets the format
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution
save_plot <- function(plot, filename, width = 7, height = 7, dpi = 600) {
  ensure_directory(dirname(filename))

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )

  invisible(filename)
}

#' Wrap long text (titles, captions)
#' @param text Text to wrap, or NULL
#' @param width Maximum line width
#' @param indent Indentation for wrapped lines
#' @return Wrapped text, or NULL for NULL
wrap_text <- function(text, width = 100, indent = 0) {
  # str_wrap(NULL) gives character(0), for which ggplot2 still lays out a
  # label; NULL leaves it out
  if (is.null(text)) return(NULL)
  stringr::str_wrap(text, width = width, exdent = indent)
}

#' Coefficient plot: estimates with CIs, by group
#' @param data Data frame
#' @param x Term variable (axis after coord_flip)
#' @param y Estimate variable
#' @param group Grouping variable (colour and shape)
#' @param ci_lower Lower CI bound variable
#' @param ci_upper Upper CI bound variable
#' @param sig Optional variable with values "sig" / "non-sig"
#' @param colors Color palette
#' @return ggplot object
plot_comparison <- function(data, x, y, group, ci_lower, ci_upper,
                            sig = NULL, colors = NULL) {
  if (is.null(colors)) {
    colors <- get_palette("default")
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[x]], y = .data[[y]],
      colour = .data[[group]], group = .data[[group]]
    )
  )

  if (!is.null(sig)) {
    p <- p + ggplot2::aes(alpha = .data[[sig]])
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[[ci_lower]], ymax = .data[[ci_upper]]),
      position = ggplot2::position_dodge(width = 1),
      width = 0
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[[group]]),
      position = ggplot2::position_dodge(width = 1),
      size = 0.75
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      alpha = 0.5,
      colour = manuscript_colors()$reference_line
    ) +
    ggplot2::scale_colour_manual(values = colors) +
    theme_publication()

  if (!is.null(sig)) {
    p <- p +
      ggplot2::scale_alpha_manual(
        values = c("non-sig" = 0.5, "sig" = 1),
        guide = "none"
      )
  }

  p + ggplot2::coord_flip()
}

# =============================================================================
# Project-specific figures: one plot_<thing>() per figure
# =============================================================================

#' Class mean trajectories with 95% CIs over individual data
#'
#' @param obs.dt Assessments with columns time, score, class and the ID
#' @param curves.dt Predicted class means: time, class, fit, lower, upper
#' @param colors.v Colours named by class
#' @param id_col Participant ID column in obs.dt
#' @param y_range.v Limits of the score axis
#' @param x_lab Label of the time axis
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @return ggplot object
plot_class_trajectories.fn <- function(obs.dt, curves.dt, colors.v, id_col,
                                       y_range.v, x_lab, title, subtitle,
                                       caption, base_size) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = obs.dt,
      ggplot2::aes(time, score, group = .data[[id_col]], colour = class),
      alpha = 0.2, linewidth = 0.3
    ) +
    ggplot2::geom_ribbon(
      data = curves.dt,
      ggplot2::aes(time, ymin = lower, ymax = upper, fill = class),
      alpha = 0.3
    ) +
    ggplot2::geom_line(
      data = curves.dt, ggplot2::aes(time, fit, colour = class),
      linewidth = 1.2
    ) +
    ggplot2::scale_colour_manual(values = colors.v, name = NULL) +
    ggplot2::scale_fill_manual(values = colors.v, name = NULL) +
    ggplot2::scale_y_continuous(limits = y_range.v) +
    ggplot2::labs(
      x = x_lab, y = "UPSIT total score",
      title = title, subtitle = subtitle, caption = wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
}

#' BIC by number of classes for each model specification
#'
#' @param comparison.dt Columns n_classes, bic, random, degree, selected
#' @param colors.v Colours named by random-effect structure
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @return ggplot object
plot_bic_comparison.fn <- function(comparison.dt, colors.v, title, subtitle,
                                   caption, base_size) {
  ggplot2::ggplot(
    comparison.dt,
    ggplot2::aes(
      n_classes, bic,
      colour = random, linetype = degree, group = interaction(random, degree)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_point(
      data = comparison.dt[selected == TRUE],
      shape = 21, size = 5, stroke = 1.2, colour = "black"
    ) +
    ggplot2::scale_colour_manual(values = colors.v, name = "Random effects") +
    ggplot2::labs(
      x = "Number of classes", y = "BIC (lower is better)",
      linetype = "Trajectory",
      title = title, subtitle = subtitle, caption = wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
}

#' Predicted curves with 95% bands by group, one panel per marker
#'
#' Lines are labelled by a key of coloured text in an empty corner of each
#' panel, in the order of the lines at their last point. The time axis
#' covers the data only.
#'
#' @param curves.dt Columns panel, group, time, fit, lower, upper
#' @param ends.dt Last point of each curve, with a label column
#' @param colors.v Colours named by group
#' @param x_lab Label of the time axis
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @param y_lab Label of the value axis; predicted UPSIT when NULL
#' @param key_corner "bottom" or "top": the left corner that holds the key
#' @return ggplot object
plot_pathology_trajectories.fn <- function(curves.dt, ends.dt, colors.v,
                                           x_lab, title, subtitle, caption,
                                           base_size, y_lab = NULL,
                                           key_corner = "bottom") {
  if (is.null(y_lab)) y_lab <- "Predicted UPSIT total score"
  stopifnot(key_corner %in% c("bottom", "top"))
  y_range.v <- range(curves.dt[, c(lower, upper)])
  line_step <- 0.075 * diff(y_range.v)
  labels.dt <- copy(ends.dt)
  setorder(labels.dt, panel, -fit)
  labels.dt[, `:=`(
    key_x = min(curves.dt$time),
    key_y = if (key_corner == "bottom") {
      y_range.v[1] + line_step * (.N - seq_len(.N))
    } else {
      y_range.v[2] - line_step * (seq_len(.N) - 1)
    }
  ), by = panel]
  # A single panel needs no strip: the axis title names it
  facet <- if (length(unique(curves.dt$panel)) > 1) {
    ggplot2::facet_wrap(~panel, nrow = 1)
  }
  ggplot2::ggplot(
    curves.dt, ggplot2::aes(time, fit, colour = group, fill = group)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_text(
      data = labels.dt, ggplot2::aes(key_x, key_y, label = label),
      hjust = 0, size = base_size / 3, fontface = "bold"
    ) +
    facet +
    ggplot2::scale_colour_manual(values = colors.v, guide = "none") +
    ggplot2::scale_fill_manual(values = colors.v, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = pretty(range(curves.dt$time), n = 4),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::labs(
      x = x_lab, y = y_lab,
      title = title, subtitle = subtitle, caption = wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
}

#' Each person's estimated yearly change in two measures
#'
#' Change keeps its sign, so decline is down and left, as in the trajectory
#' figures. Dashed lines mark no change; the quadrant where both measures
#' decline is shaded and named, and a note (such as the correlation) sits in
#' its lower right corner, which a positive association leaves empty.
#'
#' @param slopes.dt Columns x and y, one row per person
#' @param x_lab,y_lab Axis labels; markdown
#' @param note Text for the lower right corner of the shaded quadrant;
#'   markdown
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @return ggplot object
plot_slope_coupling.fn <- function(slopes.dt, x_lab, y_lab, note, title,
                                   subtitle, caption, base_size) {
  muted <- manuscript_colors()$reference_line
  ggplot2::ggplot(slopes.dt, ggplot2::aes(x, y)) +
    ggplot2::annotate(
      "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
      fill = muted, alpha = 0.12
    ) +
    ggplot2::annotate(
      "text", x = -Inf, y = 0, label = "Both declining", hjust = -0.08,
      vjust = 1.6, size = base_size / 3, fontface = "bold", colour = muted
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = muted) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = muted) +
    ggplot2::geom_point(
      colour = manuscript_colors()$combined, alpha = 0.6, size = base_size / 8
    ) +
    ggplot2::geom_smooth(
      method = "lm", formula = y ~ x, se = FALSE,
      colour = manuscript_colors()$combined, linewidth = 1.1
    ) +
    ggtext::geom_richtext(
      data = data.frame(x = 0, y = -Inf, label = note),
      ggplot2::aes(label = label), hjust = 1.03, vjust = -0.6,
      size = base_size / 3, fill = NA, label.colour = NA
    ) +
    # True minus signs on the axes (docs/STYLE.md)
    ggplot2::scale_x_continuous(
      labels = scales::label_number(style_negative = "minus")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(style_negative = "minus")
    ) +
    ggplot2::labs(
      x = x_lab, y = y_lab, title = title, subtitle = subtitle,
      caption = if (!is.null(caption)) wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
}

#' Share of each class with a characteristic, with 95% CIs
#'
#' One panel per characteristic; a point per class with its Wilson interval
#' and its count ("7/18") to the right.
#'
#' @param props.dt Columns panel, group, x (count), n, pct, lower, upper,
#'   with group and panel as factors in display order
#' @param colors.v Colours named by group
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @return ggplot object
plot_class_proportions.fn <- function(props.dt, colors.v, title, subtitle,
                                      caption, base_size) {
  # Horizontal, so the class names sit once on the left and read flat
  ggplot2::ggplot(props.dt, ggplot2::aes(pct, group, colour = group)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lower, xmax = upper), linewidth = 1.2
    ) +
    ggplot2::geom_point(size = base_size / 5) +
    ggplot2::geom_text(
      ggplot2::aes(x = upper, label = sprintf("%d/%d", x, n)),
      hjust = -0.15, size = base_size / 3, colour = "#1A1A1A"
    ) +
    ggplot2::facet_wrap(~panel, nrow = 2) +
    ggplot2::scale_colour_manual(values = colors.v, guide = "none") +
    ggplot2::scale_y_discrete(limits = rev(levels(props.dt$group))) +
    ggplot2::scale_x_continuous(
      limits = c(0, 115), breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      x = NULL, y = NULL, title = title, subtitle = subtitle,
      caption = if (!is.null(caption)) wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
}

#' Composition of each group as 100% stacked bars, with counts inside
#'
#' @param shares.dt Columns group, level (fill), N, and panel when there
#'   is more than one; group and level as factors in display order
#' @param fill.v Fill colours named by level
#' @param dark.v Levels whose fill is dark enough for white labels
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @param horizontal One bar per row, the first group on top and the key
#'   above in the order of the segments; otherwise columns with the key at
#'   the right
#' @return ggplot object
plot_stacked_shares.fn <- function(shares.dt, fill.v, dark.v, title,
                                   subtitle, caption, base_size,
                                   horizontal = FALSE) {
  plot.dt <- copy(shares.dt)
  plot.dt[, label_colour := fifelse(level %in% dark.v, "white", "#1A1A1A")]
  # Counts only where the segment is wide enough to hold them
  by.v <- intersect(c("panel", "group"), names(plot.dt))
  plot.dt[, share := N / sum(N), by = by.v]
  plot.dt[, label := fifelse(share >= 0.07, as.character(N), "")]
  facet <- if ("panel" %in% names(plot.dt)) {
    ggplot2::facet_wrap(~panel, nrow = 1)
  }
  shares_aes <- if (horizontal) {
    ggplot2::aes(N, group, fill = level, group = level)
  } else {
    ggplot2::aes(group, N, fill = level, group = level)
  }
  share_scale <- if (horizontal) {
    ggplot2::scale_x_continuous
  } else {
    ggplot2::scale_y_continuous
  }
  ggplot2::ggplot(plot.dt, shares_aes) +
    ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE),
                      width = 0.7, colour = "white",
                      orientation = if (horizontal) "y" else "x") +
    ggplot2::geom_text(
      ggplot2::aes(label = label, colour = label_colour),
      position = ggplot2::position_fill(vjust = 0.5, reverse = TRUE),
      size = base_size / 3
    ) +
    facet +
    ggplot2::scale_fill_manual(values = fill.v, name = NULL) +
    ggplot2::scale_colour_identity() +
    share_scale(labels = function(x) paste0(100 * x, "%"),
                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    (if (horizontal) {
      ggplot2::scale_y_discrete(limits = rev(levels(plot.dt$group)))
    }) +
    ggplot2::labs(
      x = NULL, y = NULL, title = title, subtitle = subtitle,
      caption = if (!is.null(caption)) wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE) +
    ggplot2::theme(legend.position = if (horizontal) "top" else "right") +
    (if (horizontal) {
      # Rows are named by their labels: no axis line or ticks; the key
      # starts flush with the labels
      ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     legend.justification = "left",
                     legend.location = "plot")
    })
}

#' Counts as bars with their values on top (none on empty bars), in one or
#' more panels
#'
#' @param counts.dt Columns panel, x (factor), N
#' @param title,subtitle,caption Figure text; the title may use markdown
#' @param base_size Base font size for theme_publication()
#' @param x_labels Labels for the x axis: a function of the levels, as in
#'   scale_x_discrete()
#' @return gtable, for save_plot()
plot_counts.fn <- function(counts.dt, title, subtitle, caption, base_size,
                           x_labels = ggplot2::waiver()) {
  counts.plt <- ggplot2::ggplot(counts.dt, ggplot2::aes(x, N)) +
    ggplot2::geom_col(fill = manuscript_colors()$combined, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(N > 0, N, "")),
                       vjust = -0.4, size = base_size / 3) +
    ggplot2::facet_wrap(~panel, scales = "free", nrow = 1) +
    ggplot2::scale_x_discrete(labels = x_labels) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::labs(
      x = NULL, y = NULL, title = title, subtitle = subtitle,
      caption = if (!is.null(caption)) wrap_text(caption, 120)
    ) +
    theme_publication(base_size = base_size, use_markdown = TRUE)
  # Each panel as wide as its number of bars, so bars match across panels;
  # facet_grid() could do this only with one y scale for all panels
  counts_grob <- ggplot2::ggplotGrob(counts.plt)
  layout.dt <- as.data.table(counts_grob$layout)
  panel_cols.v <- sort(unique(layout.dt[grepl("^panel", name), l]))
  bars.v <- counts.dt[, .N, keyby = panel]$N
  stopifnot(length(panel_cols.v) == length(bars.v))
  counts_grob$widths[panel_cols.v] <- grid::unit(bars.v, "null")
  counts_grob
}

#' Save a figure for the manuscript and its slide version
#'
#' @param build.fn Function of (title, subtitle, caption, base_size)
#'   returning a ggplot
#' @param title,subtitle,caption Manuscript figure text
#' @param output_key,slide_key Keys of data.outputs in the config
#' @return Paths of the saved files
save_figure_pair.fn <- function(build.fn, title, subtitle, caption,
                                output_key, slide_key) {
  out.lst <- get_config("output")
  figure.plt <- build.fn(title, subtitle, caption, out.lst$base_size)
  slide.plt <- build.fn(NULL, NULL, NULL, out.lst$slide_base_size)
  paths.v <- vapply(unlist(out.lst$figure_format), \(fmt) {
    save_plot(
      figure.plt, paste0(get_data_path("outputs", output_key), ".", fmt),
      width = out.lst$figure_size[[1]], height = out.lst$figure_size[[2]],
      dpi = out.lst$dpi
    )
  }, character(1))
  c(paths.v, slide = save_plot(
    slide.plt, paste0(get_data_path("outputs", slide_key), ".png"),
    width = out.lst$slide_figure_size[[1]],
    height = out.lst$slide_figure_size[[2]], dpi = out.lst$dpi
  ))
}
