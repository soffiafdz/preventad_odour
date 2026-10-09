# =============================================================================
# Table Utilities
# =============================================================================
# Generic table styling, formatting and saving with gt. Project-specific
# create_<thing>_table() functions go below the generic section.
# =============================================================================

source(here::here("R/utils/data_io.R"))

# ----- Table Styling -----

#' Apply manuscript-style formatting to gt tables
#'
#' Consistent styling for publication-ready tables.
#'
#' @param gt_tbl A gt table object
#' @param table_type Type: "main" (larger), "supplementary" (smaller), "compact"
#' @return Formatted gt table
style_manuscript_table <- function(gt_tbl, table_type = "main") {
  # Font sizes based on table type (in pt for PDF compatibility)
  if (table_type == "main") {
    title_size <- 12
    subtitle_size <- 10
    body_size <- 9
    footnote_size <- 8
    source_size <- 8
  } else if (table_type == "supplementary") {
    title_size <- 11
    subtitle_size <- 9
    body_size <- 8
    footnote_size <- 7
    source_size <- 7
  } else if (table_type == "compact") {
    title_size <- 10
    subtitle_size <- 8
    body_size <- 7
    footnote_size <- 6
    source_size <- 6
  } else {
    stop("table_type must be 'main', 'supplementary', or 'compact'")
  }

  # In LaTeX, cols_align() sets the tabular column spec, which affects both
  # headers AND data. To get centered headers with right-aligned data, use
  # centered columns and then style data cells specifically.
  gt_tbl |>
    gt::tab_options(
      table.font.size = gt::px(body_size),
      quarto.disable_processing = FALSE,
      heading.title.font.size = gt::px(title_size),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(subtitle_size),
      column_labels.font.size = gt::px(body_size),
      column_labels.font.weight = "bold",
      footnotes.font.size = gt::px(footnote_size),
      footnotes.multiline = FALSE,
      source_notes.font.size = gt::px(source_size),
      row_group.font.size = gt::px(body_size),
      row_group.font.weight = "bold",
      table.border.top.style = "solid",
      table.border.bottom.style = "solid",
      heading.border.bottom.style = "solid"
    ) |>
    # Center all columns (affects both headers and data in LaTeX)
    gt::cols_align(align = "center", columns = gt::everything()) |>
    # Left-align first column (row labels)
    gt::cols_align(align = "left", columns = 1) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_spanners()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    )
}

#' Apply significance styling to table
#'
#' Non-significant rows are set in italics; the indicator column is hidden.
#'
#' @param gt_table gt table object
#' @param sig_col Logical column indicating significance
#' @param style_cols Character vector of columns to style (default: all)
#' @return Styled gt table
apply_significance_style <- function(gt_table,
                                     sig_col = "SIGN",
                                     style_cols = NULL) {
  gt_table |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_body(
        columns = if (is.null(style_cols)) {
          gt::everything()
        } else {
          gt::all_of(style_cols)
        },
        rows = .data[[sig_col]] == FALSE
      )
    ) |>
    gt::cols_hide(columns = gt::contains(sig_col))
}

# ----- Number Formatting -----

#' Format p-value for tables (vectorized, NA-safe)
#' @param p P-value (scalar or vector)
#' @return Formatted p-value string(s)
format_p <- function(p) {
  sapply(p, function(x) {
    if (is.na(x)) return("NA")
    if (x < 0.0001) return("<0.0001")
    if (x < 0.001) return("<0.001")
    if (x < 0.01) return(sprintf("%.3f", x))
    sprintf("%.2f", x)
  })
}

#' Format p-value for inline reporting in prose
#'
#' @param p  Numeric p-value
#' @return Character string, e.g. "p < 0.001"
fmt_p_inline <- function(p) {
  stopifnot(length(p) == 1, !is.na(p))
  if (p < 0.001) "p < 0.001"
  else sprintf("p = %.3f", p)
}

#' Get significance stars (vectorized, NA-safe)
#' @param p P-value (scalar or vector)
#' @return Significance stars string(s)
sig_stars <- function(p) {
  sapply(p, function(x) {
    if (is.na(x)) return("")
    if (x < 0.001) return("***")
    if (x < 0.01) return("**")
    if (x < 0.05) return("*")
    ""
  })
}

#' Format effect size with CI
#' @param estimate Point estimate
#' @param ci_lower Lower CI bound
#' @param ci_upper Upper CI bound
#' @param digits Number of decimal places
#' @return Formatted string, e.g. "0.12 [0.03, 0.21]"
format_effect_ci <- function(estimate, ci_lower, ci_upper, digits = 2) {
  fmt <- sprintf("%%.%df [%%.%df, %%.%df]", digits, digits, digits)
  sprintf(fmt, estimate, ci_lower, ci_upper)
}

#' Format standardized coefficient
#' @param value Coefficient value
#' @param threshold Absolute values below this use scientific notation
#' @return Formatted string
format_beta_std <- function(value, threshold = 0.01) {
  ifelse(abs(value) < threshold,
    sprintf("%.0e", value),
    sprintf("%.2f", value)
  )
}

#' Format confidence interval
#' @param lower Lower bound
#' @param upper Upper bound
#' @param threshold Absolute values below this use scientific notation
#' @return Formatted string, e.g. "0.03, 0.21"
format_ci <- function(lower, upper, threshold = 0.01) {
  lower_fmt <- ifelse(abs(lower) < threshold, "%.0e", "%.2f")
  upper_fmt <- ifelse(abs(upper) < threshold, "%.0e", "%.2f")
  sprintf(paste(lower_fmt, upper_fmt, sep = ", "), lower, upper)
}

#' Format mean and SD
#' @param values Numeric vector
#' @param digits Decimal places for the mean and SD
#' @param big_mark Thousands separator (e.g. "," for volumes)
#' @return Formatted string, e.g. "71.2 (5.4)"
format_mean_sd <- function(values, digits = 1, big_mark = "") {
  fmt.fn <- function(x) {
    formatC(x, format = "f", digits = digits, big.mark = big_mark)
  }
  sprintf(
    "%s (%s)",
    fmt.fn(mean(values, na.rm = TRUE)),
    fmt.fn(sd(values, na.rm = TRUE))
  )
}

#' Bin ages into intervals
#' @param ages Vector of ages
#' @param bin_width Width of age bins in years
#' @return Factor with labels such as "45-50", "50-55"
bin_ages <- function(ages, bin_width = 5) {
  breaks <- seq(
    floor(min(ages) / bin_width) * bin_width,
    ceiling(max(ages) / bin_width) * bin_width,
    by = bin_width
  )
  bins <- cut(ages, breaks = breaks, include.lowest = TRUE, right = FALSE)
  levels(bins) <- sapply(seq_along(breaks[-length(breaks)]), \(i) {
    sprintf("%d-%d", breaks[i], breaks[i + 1])
  })
  bins
}

#' Two-group t-test, formatted
#' @param data data.table
#' @param variable Variable name
#' @param group_var Grouping variable with two levels
#' @return List with test statistics
ttest_summary <- function(data, variable, group_var = "SEX") {
  tt <- data[, t.test(get(variable) ~ get(group_var), na.rm = TRUE)]
  list(
    X = variable,
    Tstat = tt$statistic,
    DF = tt$parameter,
    Pval = tt$p.value,
    Pval_fmt = format_p(tt$p.value)
  )
}

# ----- Manuscript Inline Helpers -----

#' Check whether a bootstrap CI excludes zero
#'
#' @param r  List with boot_ci_lower, boot_ci_upper
#' @return TRUE if CI excludes zero
ci_sig_check.fn <- function(r) {
  !is.null(r$boot_ci_lower) &&
    !is.null(r$boot_ci_upper) &&
    !is.na(r$boot_ci_lower) &&
    !is.na(r$boot_ci_upper) &&
    (r$boot_ci_lower > 0 ||
       r$boot_ci_upper < 0)
}

#' Convert count/total to English prose
#'
#' @param n  Integer count
#' @param total  Integer total
#' @return Character string, e.g. "four of six", "all six", "none of the six"
n_of_total.fn <- function(n, total) {
  WORDS <- c(
    "zero", "one", "two", "three",
    "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven",
    "twelve"
  )
  word.fn <- function(x) {
    if (x >= 0 && x <= 12) WORDS[x + 1L]
    else as.character(x)
  }
  if (n == 0L) {
    paste("none of the", word.fn(total))
  } else if (n == total) {
    paste("all", word.fn(total))
  } else {
    paste(word.fn(n), "of", word.fn(total))
  }
}

# ----- LaTeX Post-processing -----

#' Wrap LaTeX table fragment in standalone document
#' @param tex_fragment_path Path to .tex file with table code
#' @param output_path Path for standalone .tex document (optional)
#' @param title Document title (optional, if NULL table title is used)
#' @return Path to standalone document
wrap_latex_table <- function(tex_fragment_path,
                             output_path = NULL,
                             title = NULL) {
  if (is.null(output_path)) {
    output_path <- sub("\\.tex$", "_standalone.tex", tex_fragment_path)
  }

  table_code <- readLines(tex_fragment_path)

  # Minimal header; the table carries its own title
  doc_header <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{geometry}",
    "\\geometry{a4paper, margin=0.75in}",
    "\\usepackage{caption}",
    "\\begin{document}",
    "\\pagestyle{empty}" # No page numbers
  )

  if (!is.null(title)) {
    doc_header <- c(doc_header, "", paste0("\\section*{", title, "}"), "")
  }

  doc_footer <- c("", "\\end{document}")
  standalone_doc <- c(doc_header, "", table_code, doc_footer)
  writeLines(standalone_doc, output_path)

  output_path
}

#' Fix LaTeX produced by gt
#'
#' Sets font sizes, centres longtables, moves source notes out of gt's
#' minipage, and repairs symbols gt escapes ($, \times, ^, %, dagger).
#'
#' @param tex_path Path to .tex file (modified in place)
#' @param table_type "main", "supplementary" or "compact", matching
#'   style_manuscript_table()
#' @param table_size,table_leading,source_notes_size Optional overrides
#'   (pt) for the preset chosen by table_type
#' @return Invisibly returns the path
fix_latex_source_notes <- function(tex_path,
                                   table_type = "main",
                                   table_size = NULL,
                                   table_leading = NULL,
                                   source_notes_size = NULL) {
  if (!file.exists(tex_path)) {
    return(invisible(tex_path))
  }

  lines <- readLines(tex_path, warn = FALSE)

  # Title/subtitle sizes: consistent across all tables
  title_size <- "16"
  title_leading <- "20"
  subtitle_size <- "14"
  subtitle_leading <- "18"

  presets.lst <- list(
    main = c(table = "11", leading = "14", notes = "9"),
    supplementary = c(table = "9", leading = "11.5", notes = "8"),
    compact = c(table = "7.5", leading = "9.5", notes = "7")
  )
  if (!table_type %in% names(presets.lst)) {
    stop(
      "table_type must be 'main', 'supplementary' or 'compact'",
      call. = FALSE
    )
  }
  preset.v <- presets.lst[[table_type]]
  table_size <- as.character(table_size %||% preset.v[["table"]])
  table_leading <- as.character(table_leading %||% preset.v[["leading"]])
  source_notes_size <- as.character(
    source_notes_size %||% preset.v[["notes"]]
  )

  # Fix table font size
  fontsize_line <- grep("^\\\\fontsize\\{", lines)[1]
  if (!is.na(fontsize_line)) {
    lines[fontsize_line] <- sprintf(
      "\\fontsize{%spt}{%spt}\\selectfont",
      table_size, table_leading
    )
  }

  # Fix title/subtitle font sizes in caption
  for (i in seq_along(lines)) {
    if (grepl("\\\\caption\\*\\{", lines[i])) {
      # Title line
      if (
        i + 1 <= length(lines) &&
          grepl("\\{\\\\fontsize\\{", lines[i + 1])
      ) {
        lines[i + 1] <- gsub(
          "\\{\\\\fontsize\\{[0-9.]+\\}\\{[0-9.]+\\}",
          sprintf("{\\\\fontsize{%s}{%s}", title_size, title_leading),
          lines[i + 1]
        )
      }
      # Subtitle line
      if (
        i + 2 <= length(lines) &&
          grepl("\\{\\\\fontsize\\{", lines[i + 2])
      ) {
        lines[i + 2] <- gsub(
          "\\{\\\\fontsize\\{[0-9.]+\\}\\{[0-9.]+\\}",
          sprintf(
            "{\\\\fontsize{%s}{%s}",
            subtitle_size, subtitle_leading
          ),
          lines[i + 2]
        )
      }
      # Vertical space between title and subtitle:
      # replace \\ at end of title line with \\[6pt]
      if (i + 1 <= length(lines)) {
        lines[i + 1] <- gsub(
          "\\\\\\\\\\s*$",
          "\\\\\\\\[6pt]",
          lines[i + 1]
        )
      }
      break
    }
  }

  # Add longtable centering if not present
  ltpost_line <- grep("\\\\setlength\\{\\\\LTpost\\}", lines)
  if (length(ltpost_line) > 0) {
    has_ltleft <- any(grepl("\\\\setlength\\{\\\\LTleft\\}", lines))
    if (!has_ltleft) {
      lines <- c(
        lines[1:ltpost_line[1]],
        "\\setlength{\\LTleft}{0pt plus 1fill}",
        "\\setlength{\\LTright}{0pt plus 1fill}",
        lines[(ltpost_line[1] + 1):length(lines)]
      )
    }
  }

  # Find minipage or center with source notes
  minipage_start <- grep("\\\\begin\\{minipage\\}", lines)
  center_start <- grep("\\\\begin\\{center\\}", lines)

  if (length(minipage_start) > 0) {
    for (start_idx in minipage_start) {
      end_idx <- start_idx
      while (
        end_idx <= length(lines) &&
          !grepl("\\\\end\\{minipage\\}", lines[end_idx])
      ) {
        end_idx <- end_idx + 1
      }

      if (end_idx <= length(lines)) {
        note_lines <- lines[(start_idx + 1):(end_idx - 1)]

        # Remove gt's formatting and spacing commands
        note_lines <- note_lines[
          !grepl("^\\\\centering", note_lines) &
            !grepl("^\\\\fontsize", note_lines) &
            !grepl("^\\\\vspace\\{[^}]*\\}\\s*(\\\\\\\\)?\\s*$", note_lines)
        ]

        # Clean up trailing \\ but keep separate lines if multiple
        note_lines <- gsub("\\\\\\\\$", "", note_lines)
        note_lines <- note_lines[trimws(note_lines) != ""]

        if (length(note_lines) > 3) {
          # Many short fragments: consolidate
          note_lines <- paste(note_lines, collapse = " ")
        } else if (length(note_lines) > 1) {
          # 2-3 lines: LaTeX line breaks between them
          note_lines <- paste(note_lines, collapse = " \\\\\n")
        }

        # Replace minipage with centred notes, closer to the table
        lines <- c(
          lines[1:(start_idx - 1)],
          "\\vspace{-12pt}",
          "\\begin{center}",
          sprintf(
            "\\fontsize{%s}{%s}\\selectfont", source_notes_size,
            as.character(as.numeric(source_notes_size) * 1.2)
          ),
          note_lines,
          "\\end{center}",
          lines[(end_idx + 1):length(lines)]
        )
      }
    }
  } else if (length(center_start) > 0) {
    for (start_idx in center_start) {
      has_vspace <- start_idx > 1 &&
        grepl("\\\\vspace", lines[start_idx - 1])

      if (!has_vspace) {
        lines <- c(
          lines[1:(start_idx - 1)],
          "\\vspace{-12pt}",
          lines[start_idx:length(lines)]
        )
      }
    }
  }

  # Fix escaped dollar signs (gt converts $ to \$)
  lines <- gsub("\\\\\\$", "$", lines)

  # Fix \times that gt escaped as \textbackslash{}times
  lines <- gsub(
    "\\\\textbackslash\\{\\}times", "\\\\times", lines
  )

  # Fix superscripts: \textasciicircum{}\{-3\} -> ^{-3}
  lines <- gsub(
    "\\\\textasciicircum\\{\\}\\\\\\{(-?[0-9]+)\\\\\\}",
    "^{\\1}",
    lines
  )

  # Fix percent signs
  lines <- gsub("\\\\textbackslash\\{\\}\\\\%", "\\\\%", lines)
  lines <- gsub("\\\\textbackslash\\{\\}%", "\\\\%", lines)
  lines <- gsub("textbackslash\\{\\}%", "\\\\%", lines)

  # Fix Unicode symbols
  lines <- gsub("†", "\\\\dag", lines)
  lines <- gsub("×", "$\\\\times$", lines)
  lines <- gsub("⁻³", "$^{-3}$", lines)

  writeLines(lines, tex_path)

  invisible(tex_path)
}

# ----- Table Saving -----

#' Save table in multiple formats
#' @param gt_table gt table object
#' @param filename Base filename (without extension)
#' @param output_dir Output directory
#' @param formats Vector of formats ("html", "tex", "rtf")
#' @param table_type Size preset passed to fix_latex_source_notes()
#' @param fix_latex Whether to apply LaTeX post-processing fixes
#' @param create_standalone Whether to create standalone tex documents
save_table <- function(gt_table,
                       filename,
                       output_dir,
                       formats = c("html", "tex"),
                       table_type = "main",
                       fix_latex = TRUE,
                       create_standalone = TRUE) {
  ensure_directory(output_dir)

  for (fmt in formats) {
    fpath <- file.path(output_dir, paste0(filename, ".", fmt))
    gt::gtsave(gt_table, fpath)

    if (fmt == "tex") {
      if (fix_latex) {
        fix_latex_source_notes(fpath, table_type = table_type)
      }
      if (create_standalone) {
        wrap_latex_table(fpath)
      }
    }
  }

  invisible(filename)
}

#' Save table to both HTML and LaTeX, in separate directories
#' @param gt_table gt table object
#' @param filename Base filename
#' @param html_dir HTML output directory
#' @param tex_dir LaTeX output directory
save_table_dual <- function(gt_table, filename, html_dir, tex_dir) {
  ensure_directory(html_dir)
  ensure_directory(tex_dir)

  gt::gtsave(gt_table, file.path(html_dir, paste0(filename, ".html")))
  gt::gtsave(gt_table, file.path(tex_dir, paste0(filename, ".tex")))

  invisible(filename)
}

#' Add a markdown title unless it is NULL
#'
#' In Quarto documents the caption carries the title, so tables are built
#' with title = NULL there; standalone HTML tables keep their own title.
#'
#' @param gt_tbl gt table
#' @param title Title (may use markdown) or NULL
#' @return gt table
add_title.fn <- function(gt_tbl, title) {
  if (is.null(title)) return(gt_tbl)
  gt::tab_header(gt_tbl, title = gt::md(title))
}

#' Manuscript styling with text sized for a browser or slides
#'
#' @param gt_tbl gt table
#' @return gt table
style_screen_table.fn <- function(gt_tbl) {
  gt_tbl |>
    style_manuscript_table(table_type = "main") |>
    gt::tab_options(
      table.font.size = gt::px(16),
      heading.title.font.size = gt::px(20),
      column_labels.font.size = gt::px(16),
      row_group.font.size = gt::px(16),
      source_notes.font.size = gt::px(14)
    )
}

#' Text size for a gt table in a Quarto PDF
#'
#' gt passes its px sizes to LaTeX, so screen-sized tables overflow the page.
#' 14 px fits most tables; shorten long row labels before going smaller.
#'
#' @param gt_tbl gt table
#' @param size Body text size in px
#' @return gt table
style_pdf_table.fn <- function(gt_tbl, size = 14) {
  gt::tab_options(
    gt_tbl,
    table.font.size = gt::px(size),
    column_labels.font.size = gt::px(size),
    row_group.font.size = gt::px(size),
    source_notes.font.size = gt::px(size - 1)
  )
}

# =============================================================================
# Project-specific tables: one create_<thing>_table() per manuscript table
# =============================================================================

#' Characteristics of the trajectory classes
#'
#' Text is enlarged after the print styling because the table is read in a
#' browser or pasted into slides.
#'
#' @param summary.dt One row per characteristic: a characteristic column
#'   and one formatted column per class and for the total
#' @param title Bold short title, may use markdown; NULL in documents, where
#'   the caption carries it
#' @param source_note Text under the table
#' @return gt table
create_class_table.fn <- function(summary.dt, title, source_note) {
  gt::gt(summary.dt, rowname_col = "characteristic") |>
    add_title.fn(title) |>
    gt::tab_source_note(source_note) |>
    style_screen_table.fn()
}

#' UPSIT change per year by PET marker
#'
#' @param slopes.dt Columns section, row, slope, difference, p and
#'   difference_sensitivity, already formatted
#' @param column_labels.lst Display labels for the value columns
#' @param title Bold short title, may use markdown; NULL in documents
#' @param source_note Text under the table
#' @return gt table
create_pathology_table.fn <- function(slopes.dt, column_labels.lst, title,
                                      source_note) {
  gt::gt(slopes.dt, groupname_col = "section", rowname_col = "row") |>
    gt::cols_label(.list = column_labels.lst) |>
    add_title.fn(title) |>
    gt::tab_source_note(source_note) |>
    style_screen_table.fn()
}
