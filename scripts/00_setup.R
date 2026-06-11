# Bq table versions
vi <- "vessel_info_v_20250815"
vms <- "mex_vms_processed_v_20260409"

# Reset theme
ggplot2::theme_set(ggplot2::theme_bw())

ggplot2::theme_update(
  text = ggplot2::element_text(color = "black"),
  axis.text = ggplot2::element_text(color = "black"),
  axis.title.y = ggplot2::element_text(size = 10),
  axis.title.x = ggplot2::element_text(size = 10),
  axis.text.y = ggplot2::element_text(size = 8),
  axis.text.x = ggplot2::element_text(size = 8),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  panel.grid.major.x = ggplot2::element_blank(),#element_line(colour = "gray", linewidth = 0.1),
  panel.grid.major.y = ggplot2::element_line(colour = "gray",
                                    linewidth = 0.1,
                                    linetype = "dashed"),
  panel.grid.minor = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank()
)

ggplot2::update_geom_defaults(geom = "point",
                              new = list(color = "black",
                                         fill = "steelblue",
                                         shape = 21,
                                         size = 2))

ggplot2::update_geom_defaults(geom = "col",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "bar",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "area",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "segment",
                              new = list(color = "black",
                                         linetype = "dashed"))

ggplot2::update_geom_defaults(geom = "hline",
                              new = list(color = "black",
                                         linetype = "dashed"))

# Post-process tabularray tables to add vertical spacing between panels
add_panel_spacing <- function(file_path) {
  lines <- readLines(file_path)

  # Find the cell spec line with column span (identifies panel header rows)
  # Pattern: cell{2,7,13}{1}={c=4,}{halign=l,}
  panel_cell_idx <- grep("cell\\{[^}]+\\}\\{[^}]*\\}=\\{c=", lines)
  if (length(panel_cell_idx) == 0) return(invisible(NULL))

  # Extract panel header row numbers
  panel_row_str <- sub(".*cell\\{([^}]+)\\}.*", "\\1", lines[panel_cell_idx])
  panel_rows <- as.integer(strsplit(panel_row_str, ",")[[1]])

  # Need at least 2 panels to add spacing
  if (length(panel_rows) < 2) return(invisible(NULL))

  # Rows before which we insert a blank row (all panels except the first)
  insert_before <- panel_rows[-1]

  # Determine number of columns from colspec
  colspec_line <- grep("colspec=", lines, value = TRUE)
  n_cols <- length(gregexpr("Q\\[", colspec_line)[[1]])
  blank_row <- paste(c(rep(" &", n_cols - 1), " \\\\"), collapse = "")

  # Helper: shift a row number by counting how many insertions occur at or before it
  shift_row <- function(r) {
    r + sum(insert_before <= r)
  }

  # Helper: parse and shift row references in a cell{...} spec line
  shift_cell_spec <- function(cell_line) {
    row_str <- sub("^(cell\\{)([^}]+)(\\}.*)$", "\\2", cell_line)
    prefix <- sub("^(cell\\{)([^}]+)(\\}.*)$", "\\1", cell_line)
    suffix <- sub("^(cell\\{)([^}]+)(\\}.*)$", "\\3", cell_line)

    parts <- strsplit(row_str, ",")[[1]]
    new_parts <- vapply(parts, function(p) {
      if (grepl("-", p)) {
        bounds <- as.integer(strsplit(p, "-")[[1]])
        paste(vapply(bounds, shift_row, integer(1)), collapse = "-")
      } else {
        as.character(shift_row(as.integer(p)))
      }
    }, character(1), USE.NAMES = FALSE)

    paste0(prefix, paste(new_parts, collapse = ","), suffix)
  }

  # Update all cell spec lines (these are before \toprule, so indices stay valid)
  cell_idxs <- grep("^cell\\{", lines)
  for (idx in cell_idxs) {
    lines[idx] <- shift_cell_spec(lines[idx])
  }

  # Insert blank rows in the body (work backwards to preserve line indices)
  toprule_idx <- grep("\\\\toprule", lines)
  for (row in rev(insert_before)) {
    line_idx <- toprule_idx + row  # the line corresponding to this row
    lines <- append(lines, blank_row, after = line_idx - 1)
  }

  writeLines(lines, file_path)
  invisible(NULL)
}
