#!/usr/bin/env Rscript

# Game Theory Table Generator using R flextable + webshot2
# Usage: Rscript generate_game_tables.R
# To customize the game and output, edit game_settings.json.

# Default JSON settings path. Override with --settings-json=/path/to/file.json.
DEFAULT_SETTINGS_JSON <- "game_settings.json"

# Check and install required packages
#' Check if a package is installed and install if necessary
#' @param package_name Name of the package
#' @param auto_install Whether to install automatically without prompting
check_and_install_package <- function(package_name, auto_install = FALSE) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    if (auto_install) {
      cat("Installing package:", package_name, "\n")
      install.packages(package_name)
    } else {
      cat("Package", package_name, "is not installed.\n")
      response <- readline(prompt = "Install now? (y/n): ")

      if (tolower(response) %in% c("y", "yes")) {
        cat("Installing package:", package_name, "\n")
        install.packages(package_name)
      } else {
        stop(paste("Package", package_name, "is required. Aborting."))
      }
    }
  }
}

# Required packages
required_packages <- c("flextable", "webshot2", "magrittr",
                       "officer", "grid", "gridExtra", "png",
                       "jsonlite")

cat("Checking package dependencies...\n")
for (pkg in required_packages) {
  check_and_install_package(pkg)
}

# Load packages
library(flextable)
library(webshot2)
library(magrittr)
library(officer)
library(grid)
library(gridExtra)
library(png)

cat("All packages loaded successfully\n")

#' Return font families for Latin and Japanese text
get_game_table_fonts <- function() {
  japanese_font <- if (Sys.info()["sysname"] == "Darwin") {
    "YuMincho"
  } else if (Sys.info()["sysname"] == "Windows") {
    "Yu Mincho"
  } else {
    "Noto Serif CJK JP"
  }

  list(
    latin = "Times New Roman",
    japanese = japanese_font
  )
}

#' Normalize font family settings
normalize_font_families <- function(font_families = NULL) {
  default_fonts <- get_game_table_fonts()

  if (is.null(font_families)) {
    return(default_fonts)
  }

  modifyList(default_fonts, font_families)
}

#' Split a label into leading Latin text and remaining Japanese text
split_label_text <- function(label) {
  latin_match <- regexpr("^[ -~]+", label, perl = TRUE)

  if (latin_match[1] == -1) {
    return(list(latin = "", japanese = label))
  }

  latin_text <- regmatches(label, latin_match)
  japanese_text <- substring(label, nchar(latin_text) + 1)

  list(latin = latin_text, japanese = japanese_text)
}

#' Measure rendered width of a mixed label in npc units (current grid viewport)
measure_mixed_label_width_npc <- function(label, fontsize,
                                        font_families = get_game_table_fonts()) {
  font_families <- normalize_font_families(font_families)
  label_parts <- split_label_text(label)

  if (label_parts$japanese == "") {
    grob <- textGrob(
      label,
      gp = gpar(fontsize = fontsize, fontfamily = font_families$latin)
    )
    return(convertWidth(grobWidth(grob), "npc", valueOnly = TRUE))
  }

  if (label_parts$latin == "") {
    grob <- textGrob(
      label,
      gp = gpar(fontsize = fontsize, fontfamily = font_families$japanese)
    )
    return(convertWidth(grobWidth(grob), "npc", valueOnly = TRUE))
  }

  latin_grob <- textGrob(
    label_parts$latin,
    gp = gpar(fontsize = fontsize, fontfamily = font_families$latin)
  )
  japanese_grob <- textGrob(
    label_parts$japanese,
    gp = gpar(fontsize = fontsize, fontfamily = font_families$japanese)
  )
  convertWidth(grobWidth(latin_grob), "npc", valueOnly = TRUE) +
    convertWidth(grobWidth(japanese_grob), "npc", valueOnly = TRUE)
}

#' Draw mixed Latin/Japanese labels with separate fonts
draw_mixed_label <- function(label, x_npc, y_npc, rot = 0,
                             fontsize = 80,
                             font_families = get_game_table_fonts(),
                             text_color = "black",
                             hjust = 0.5,
                             vjust = 0.5) {
  font_families <- normalize_font_families(font_families)

  # Rotated labels: draw as one string (split Latin/Japanese breaks order)
  if (rot != 0) {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              hjust = 0.5,
              vjust = vjust,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$japanese,
                        col = text_color))
    return(invisible(NULL))
  }

  label_parts <- split_label_text(label)

  if (label_parts$japanese == "") {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              hjust = hjust,
              vjust = vjust,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$latin,
                        col = text_color))
    return(invisible(NULL))
  }

  if (label_parts$latin == "") {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              hjust = hjust,
              vjust = vjust,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$japanese,
                        col = text_color))
    return(invisible(NULL))
  }

  latin_grob <- textGrob(
    label_parts$latin,
    gp = gpar(fontsize = fontsize,
              fontfamily = font_families$latin,
              col = text_color)
  )
  japanese_grob <- textGrob(
    label_parts$japanese,
    gp = gpar(fontsize = fontsize,
              fontfamily = font_families$japanese,
              col = text_color)
  )
  latin_width <- convertWidth(grobWidth(latin_grob), "npc",
                              valueOnly = TRUE)
  japanese_width <- convertWidth(grobWidth(japanese_grob), "npc",
                                 valueOnly = TRUE)
  total_width <- latin_width + japanese_width

  if (rot == 90) {
    latin_x <- x_npc
    japanese_x <- x_npc
    latin_y <- y_npc - total_width / 2 + latin_width / 2
    japanese_y <- y_npc - total_width / 2 + latin_width +
      japanese_width / 2
  } else if (hjust >= 0.99) {
    latin_x <- x_npc - total_width + latin_width / 2
    japanese_x <- x_npc - japanese_width / 2
    latin_y <- y_npc
    japanese_y <- y_npc
  } else if (hjust <= 0.01) {
    latin_x <- x_npc + latin_width / 2
    japanese_x <- x_npc + latin_width + japanese_width / 2
    latin_y <- y_npc
    japanese_y <- y_npc
  } else {
    latin_x <- x_npc - total_width / 2 + latin_width / 2
    japanese_x <- x_npc - total_width / 2 + latin_width +
      japanese_width / 2
    latin_y <- y_npc
    japanese_y <- y_npc
  }

  grid.text(label_parts$latin,
            x = unit(latin_x, "npc"),
            y = unit(latin_y, "npc"),
            rot = rot,
            hjust = 0.5,
            vjust = vjust,
            gp = gpar(fontsize = fontsize,
                      fontface = "plain",
                      fontfamily = font_families$latin,
                      col = text_color))
  grid.text(label_parts$japanese,
            x = unit(japanese_x, "npc"),
            y = unit(japanese_y, "npc"),
            rot = rot,
            hjust = 0.5,
            vjust = vjust,
            gp = gpar(fontsize = fontsize,
                      fontface = "plain",
                      fontfamily = font_families$japanese,
                      col = text_color))

  invisible(NULL)
}

#' Return a Japanese-capable font family (legacy helper)
get_game_table_font <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    return("YuMincho")
  }
  if (Sys.info()["sysname"] == "Windows") {
    return("Yu Mincho")
  }
  "Noto Serif CJK JP"
}

#' Resolve repository-root output directory from this script's location
#'
#' Repo root is the directory containing this script (works when run with
#' Rscript generate_game_tables.R from any working directory).
get_repo_output_dir <- function(...) {
  script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  repo_root <- normalizePath(dirname(script_path[1]), winslash = "/", mustWork = FALSE)
  file.path(repo_root, "output", ...)
}

#' Parse --settings-json=/path/to/file.json from CLI args
parse_settings_json_path <- function(default_path = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  key <- "--settings-json="
  matches <- startsWith(args, key)
  if (any(matches)) {
    return(sub(key, "", args[which(matches)[1]]))
  }
  default_path
}

#' Convert JSON payoff matrix into list(c(a, b), ...)
normalize_payoff_matrix <- function(value) {
  if (!is.list(value)) {
    stop("settings JSON: payoff_matrix must be an array of [a, b] pairs")
  }
  lapply(value, function(cell) {
    nums <- as.numeric(unlist(cell))
    if (length(nums) != 2 || any(is.na(nums))) {
      stop("settings JSON: each payoff_matrix cell must contain two numbers")
    }
    nums
  })
}

#' Load and normalize game settings overrides from JSON
load_settings_from_json <- function(path) {
  if (is.null(path) || identical(path, "")) {
    stop("settings JSON is required. Use --settings-json=...")
  }
  if (!file.exists(path)) {
    stop("settings JSON not found: ", path)
  }

  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(raw)) {
    stop("settings JSON must be an object")
  }

  normalized <- raw
  if (!is.null(normalized$payoff_matrix)) {
    normalized$payoff_matrix <- normalize_payoff_matrix(normalized$payoff_matrix)
  }
  if (!is.null(normalized$strategies)) {
    normalized$strategies <- as.character(unlist(normalized$strategies))
  }

  allowed_keys <- c(
    "enabled", "file_prefix", "calculate_nash", "save_images",
    "player_labels", "strategies", "payoff_matrix",
    "cell_annotations_enabled", "cell_annotations",
    "cell_colors_enabled", "cell_colors",
    "output_dir", "font_families"
  )
  unknown_keys <- setdiff(names(normalized), allowed_keys)
  if (length(unknown_keys) > 0) {
    warning("settings JSON: ignored unknown keys: ",
            paste(unknown_keys, collapse = ", "))
    normalized[unknown_keys] <- NULL
  }

  normalized
}

#' Load required game settings from JSON
resolve_game_settings <- function() {
  settings_path <- parse_settings_json_path(default_path = DEFAULT_SETTINGS_JSON)
  settings <- load_settings_from_json(settings_path)
  if (is.null(settings$cell_colors_enabled)) {
    settings$cell_colors_enabled <- FALSE
  }
  if (is.null(settings$cell_colors)) {
    settings$cell_colors <- list()
  }

  required_keys <- c(
    "enabled", "file_prefix", "calculate_nash", "save_images",
    "player_labels", "strategies", "payoff_matrix",
    "cell_annotations_enabled", "cell_annotations",
    "output_dir", "font_families"
  )
  missing_keys <- setdiff(required_keys, names(settings))
  if (length(missing_keys) > 0) {
    stop("settings JSON is missing required keys: ",
         paste(missing_keys, collapse = ", "))
  }

  settings$settings_json <- settings_path
  settings
}

# Game definition
#' Define game parameters
#'
#' @param output_dir Output directory (NULL uses repo output/)
#' @param file_prefix File name prefix
#' @param calculate_nash Whether to calculate Nash equilibrium
#' @param player_labels Player labels for top and left positions
#' @param font_families Latin and Japanese font families
#' @param strategies Strategy names for both players
#' @param payoff_matrix Payoff matrix in list format
#' @param cell_annotations_enabled Whether to draw cell marks and footnotes
#' @param cell_annotations Per-cell annotation specs
#' @param enabled Whether this game is active
#' @param cell_colors_enabled Whether to draw per-cell background colors
#' @param cell_colors Per-cell color specs
#' @return Game definition list
define_game <- function(file_prefix,
                        calculate_nash,
                        player_labels,
                        strategies,
                        payoff_matrix,
                        cell_annotations_enabled,
                        cell_annotations,
                        enabled,
                        output_dir = NULL,
                        font_families = NULL,
                        cell_colors_enabled = FALSE,
                        cell_colors = list()) {
  if (is.null(output_dir)) {
    output_dir <- get_repo_output_dir()
  }
  font_families <- normalize_font_families(font_families)

  game_definition <- list(
    enabled = enabled,
    strategies = strategies,
    output_dir = output_dir,
    file_prefix = file_prefix,
    calculate_nash = calculate_nash,
    player_labels = player_labels,
    font_families = font_families,
    payoff_matrix = if (enabled) payoff_matrix else list(),
    cell_annotations_enabled = enabled && isTRUE(cell_annotations_enabled),
    cell_annotations = if (enabled && isTRUE(cell_annotations_enabled)) {
      cell_annotations
    } else {
      NULL
    },
    cell_colors_enabled = enabled && isTRUE(cell_colors_enabled),
    cell_colors = if (enabled && isTRUE(cell_colors_enabled)) {
      cell_colors
    } else {
      NULL
    }
  )
  return(game_definition)
}

# Game analysis functions
#' Create game data structure from payoff matrix
#' @param payoffs Payoff matrix in list format (e.g., list(c(4,4)...))
#' @param strategy_names_a Strategy names for row / left player (1)
#' @param strategy_names_b Strategy names for column / top player (2)
#' @param epsilon Tolerance for floating-point comparisons
create_game_data <- function(payoffs, strategy_names_a,
                             strategy_names_b = NULL,
                             epsilon = 1e-10) {
  if (is.null(strategy_names_b)) {
    strategy_names_b <- strategy_names_a
  }
  num_strategies_a <- length(strategy_names_a)
  num_strategies_b <- length(strategy_names_b)
  if (length(payoffs) != num_strategies_a * num_strategies_b) {
    stop("Payoff matrix size does not match number of strategies")
  }
  return(list(
    payoffs = payoffs,
    num_strategies_a = num_strategies_a,
    num_strategies_b = num_strategies_b,
    strategy_names_a = strategy_names_a,
    strategy_names_b = strategy_names_b,
    epsilon = epsilon
  ))
}

#' Calculate Nash equilibrium
calculate_nash_equilibrium <- function(game_data) {
  if (!is.list(game_data) || is.null(game_data$payoffs)) {
    stop("Invalid game data format")
  }
  nash_equilibria <- list()
  is_nash_equilibrium <- function(strategy_a, strategy_b) {
    current_idx <- (strategy_a - 1) * game_data$num_strategies_b + strategy_b
    current_payoff <- game_data$payoffs[[current_idx]]

    # Check best response for row / left player (1)
    for (i in 1:game_data$num_strategies_a) {
      if (i != strategy_a) {
        other_idx <- (i - 1) * game_data$num_strategies_b + strategy_b
        other_payoff <- game_data$payoffs[[other_idx]]
        if (other_payoff[1] > current_payoff[1] + game_data$epsilon) {
          return(FALSE)
        }
      }
    }

    # Check best response for column / top player (2)
    for (j in 1:game_data$num_strategies_b) {
      if (j != strategy_b) {
        other_idx <- (strategy_a - 1) * game_data$num_strategies_b + j
        other_payoff <- game_data$payoffs[[other_idx]]
        if (other_payoff[2] > current_payoff[2] + game_data$epsilon) {
          return(FALSE)
        }
      }
    }

    TRUE
  }

  for (i in 1:game_data$num_strategies_a) {
    for (j in 1:game_data$num_strategies_b) {
      if (is_nash_equilibrium(i, j)) {
        idx <- (i - 1) * game_data$num_strategies_b + j
        nash_equilibria[[length(nash_equilibria) + 1]] <- list(
          strategies = c(game_data$strategy_names_a[i],
                         game_data$strategy_names_b[j]),
          payoffs = game_data$payoffs[[idx]]
        )
      }
    }
  }

  nash_equilibria
}

#' Create display dataframe for game table
create_display_dataframe <- function(game_data) {
  display_data <- data.frame(
    Strategy = game_data$strategy_names_a,
    matrix(NA,
           nrow = game_data$num_strategies_a,
           ncol = game_data$num_strategies_b)
  )
  names(display_data)[2:(game_data$num_strategies_b + 1)] <-
    game_data$strategy_names_b

  for (i in 1:game_data$num_strategies_a) {
    for (j in 1:game_data$num_strategies_b) {
      idx <- (i - 1) * game_data$num_strategies_b + j
      payoff <- game_data$payoffs[[idx]]
      display_data[i, j + 1] <- paste(payoff[1], payoff[2], sep = ", ")  # (row, col)
    }
  }

  display_data
}

MAX_CELL_ANNOTATIONS <- 99L
ANNOTATION_MARK_SYMBOL <- "\u203b"  # ※
ANNOTATION_TEXT_COLOR <- "black"

#' In-cell / footnote mark: ※ when alone, ※1 ※2 … when multiple
annotation_mark_label <- function(number, show_number = TRUE) {
  if (!show_number) {
    return(ANNOTATION_MARK_SYMBOL)
  }
  paste0(ANNOTATION_MARK_SYMBOL, as.integer(number))
}

#' Parse row/col indices from one per-cell setting spec
parse_cell_positions <- function(spec, context = "cell setting") {
  if (!is.null(spec$cells)) {
    rows <- cols <- integer()
    for (cell in spec$cells) {
      cell <- as.integer(unlist(cell))
      if (length(cell) != 2 || any(is.na(cell))) {
        stop("Each ", context, " cells entry must be c(row, col)")
      }
      rows <- c(rows, cell[1])
      cols <- c(cols, cell[2])
    }
    return(list(rows = rows, cols = cols))
  }

  if (!is.null(spec$row) && !is.null(spec$col)) {
    rows <- as.integer(unlist(spec$row))
    cols <- as.integer(unlist(spec$col))
    if (any(is.na(rows)) || any(is.na(cols))) {
      stop("Each ", context, " row/col must be integers")
    }
    return(list(rows = rows, cols = cols))
  }

  stop("Each ", context, " needs row and col, or cells")
}

#' Parse row/col indices from one annotation spec
parse_annotation_cells <- function(spec) {
  parse_cell_positions(spec, context = "annotation")
}

#' Resolve cell annotation specs into marks and footnotes
#'
#' @param annotations List of annotation specs from settings JSON
#' @param game_data Game data from create_game_data()
#' @return list(footnotes, cell_marks) for drawing marks and legend below table
resolve_cell_annotations <- function(annotations, game_data) {
  empty <- list(footnotes = list(), cell_marks = list())
  if (is.null(annotations) || length(annotations) == 0) {
    return(empty)
  }

  footnotes <- list()
  text_to_number <- list()
  cell_marks <- list()
  cell_mark_by_key <- list()
  next_number <- 1L

  assign_number_for_text <- function(text, explicit_mark = NULL) {
    if (!is.null(text_to_number[[text]])) {
      return(text_to_number[[text]])
    }
    number <- explicit_mark
    if (is.null(number)) {
      if (next_number > MAX_CELL_ANNOTATIONS) {
        stop("Too many distinct annotations (max ", MAX_CELL_ANNOTATIONS, ")")
      }
      number <- next_number
      next_number <<- next_number + 1L
    }
    text_to_number[[text]] <<- number
    footnotes[[length(footnotes) + 1]] <<- list(
      number = number,
      text = text
    )
    number
  }

  for (spec in annotations) {
    if (is.null(spec$text) || !nzchar(spec$text)) {
      next
    }

    parsed <- parse_annotation_cells(spec)
    rows <- parsed$rows
    cols <- parsed$cols
    if (length(rows) != length(cols)) {
      stop("row and col must have the same length in annotation: ", spec$text)
    }
    if (length(rows) == 0) {
      next
    }

    number <- assign_number_for_text(spec$text, spec$mark)

    for (k in seq_along(rows)) {
      i <- rows[k]
      j <- cols[k]
      if (i < 1 || i > game_data$num_strategies_a ||
          j < 1 || j > game_data$num_strategies_b) {
        stop("Annotation row/col out of range: (", i, ", ", j, ")")
      }

      cell_key <- paste(i, j, sep = ",")
      if (!is.null(cell_mark_by_key[[cell_key]])) {
        if (cell_mark_by_key[[cell_key]] != number) {
          stop(
            "Cell (", i, ", ", j, ") already has comment ",
            cell_mark_by_key[[cell_key]], "; cannot also assign ", number
          )
        }
        next
      }
      cell_mark_by_key[[cell_key]] <- number

      cell_marks[[length(cell_marks) + 1]] <- list(
        row = i,
        col = j,
        number = number
      )
    }
  }

  show_numbers <- length(footnotes) > 1L
  for (idx in seq_along(footnotes)) {
    n <- footnotes[[idx]]$number
    label <- annotation_mark_label(n, show_numbers)
    footnotes[[idx]]$footnote_mark <- label
    footnotes[[idx]]$cell_mark <- label
  }
  for (idx in seq_along(cell_marks)) {
    n <- cell_marks[[idx]]$number
    cell_marks[[idx]]$mark <- annotation_mark_label(n, show_numbers)
    cell_marks[[idx]]$number <- NULL
  }

  list(footnotes = footnotes, cell_marks = cell_marks)
}

#' Validate an R/flextable-compatible color value
validate_color_value <- function(color, context = "cell color") {
  color <- as.character(unlist(color))
  if (length(color) != 1 || is.na(color) || !nzchar(color)) {
    stop(context, " must have a non-empty color")
  }

  tryCatch(
    {
      grDevices::col2rgb(color)
      color
    },
    error = function(e) {
      stop(context, " has invalid color: ", color, call. = FALSE)
    }
  )
}

#' Resolve cell background color specs
#'
#' @param cell_colors List of cell color specs from settings JSON
#' @param game_data Game data from create_game_data()
#' @return list(row, col, color) entries for flextable background styling
resolve_cell_colors <- function(cell_colors, game_data) {
  if (is.null(cell_colors) || length(cell_colors) == 0) {
    return(list())
  }

  resolved <- list()
  color_by_key <- list()

  for (spec_idx in seq_along(cell_colors)) {
    spec <- cell_colors[[spec_idx]]
    color_value <- spec$color
    if (is.null(color_value)) {
      color_value <- spec$bg_color
    }
    color <- validate_color_value(
      color_value,
      context = paste0("cell_colors[[", spec_idx, "]]")
    )

    parsed <- parse_cell_positions(
      spec,
      context = paste0("cell_colors[[", spec_idx, "]]")
    )
    rows <- parsed$rows
    cols <- parsed$cols
    if (length(rows) != length(cols)) {
      stop("row and col must have the same length in cell_colors[[",
           spec_idx, "]]")
    }
    if (length(rows) == 0) {
      next
    }

    for (k in seq_along(rows)) {
      i <- rows[k]
      j <- cols[k]
      if (i < 1 || i > game_data$num_strategies_a ||
          j < 1 || j > game_data$num_strategies_b) {
        stop("Cell color row/col out of range: (", i, ", ", j, ")")
      }

      cell_key <- paste(i, j, sep = ",")
      if (!is.null(color_by_key[[cell_key]])) {
        if (color_by_key[[cell_key]] != color) {
          stop(
            "Cell (", i, ", ", j, ") already has color ",
            color_by_key[[cell_key]], "; cannot also assign ", color
          )
        }
        next
      }
      color_by_key[[cell_key]] <- color

      resolved[[length(resolved) + 1]] <- list(
        row = i,
        col = j,
        color = color
      )
    }
  }

  resolved
}

#' Payoff-area geometry in image pixels (strategy column / header row ratios)
payoff_cell_geometry <- function(game_data, img_width, img_height,
                                 strategy_col_ratio = 0.25,
                                 header_row_ratio = 0.25) {
  payoff_width <- (1 - strategy_col_ratio) * img_width
  payoff_height <- (1 - header_row_ratio) * img_height
  list(
    cell_width = payoff_width / game_data$num_strategies_b,
    cell_height = payoff_height / game_data$num_strategies_a,
    payoff_left = strategy_col_ratio * img_width,
    payoff_top = header_row_ratio * img_height
  )
}

#' Convert image y (from top) to grid npc (origin at bottom)
image_y_top_to_npc <- function(py, img_height, margin_bottom, new_height) {
  (margin_bottom + img_height - py) / new_height
}

#' Footnote font size (points) from table image width
footnote_fontsize_for_image <- function(img_width) {
  max(42, min(56, round(img_width * 300 / 1200)))
}

#' Line height in image pixels for footnote text at res dpi
footnote_line_height_px <- function(fontsize_pt, res = 300, leading = 1.3) {
  fontsize_pt * res / 72 * leading
}

#' Gap between table bottom and first footnote line (pixels)
footnote_gap_after_table_px <- function(fontsize_pt, res = 300) {
  round(fontsize_pt * res / 72 * 0.12)
}

#' Total height of footnote block below the table (pixels)
footnote_block_height_px <- function(footnote_count, fontsize_pt, res = 300) {
  line_px <- footnote_line_height_px(fontsize_pt, res = res)
  gap_px <- footnote_gap_after_table_px(fontsize_pt, res = res)
  gap_px + footnote_count * line_px + round(line_px * 0.12)
}

#' Bottom margin pixels for footnote block below the table image
footnote_margin_bottom_px <- function(footnote_count, img_width, res = 300) {
  if (footnote_count < 1) {
    return(0)
  }
  fs <- footnote_fontsize_for_image(img_width)
  footnote_block_height_px(footnote_count, fs, res = res)
}

#' Draw mark symbols on annotated payoff cells
draw_cell_marks <- function(cell_marks, game_data, img_width, img_height,
                            margin_left, margin_bottom, new_width, new_height,
                            strategy_col_ratio = 0.25,
                            header_row_ratio = 0.25,
                            font_families = get_game_table_fonts()) {
  if (length(cell_marks) == 0) {
    return(invisible(NULL))
  }

  font_families <- normalize_font_families(font_families)
  geom <- payoff_cell_geometry(
    game_data, img_width, img_height,
    strategy_col_ratio, header_row_ratio
  )
  mark_fontsize <- max(36, min(54, round(geom$cell_height * 300 / 20)))

  for (entry in cell_marks) {
    i <- entry$row
    j <- entry$col
    cell_left <- geom$payoff_left + (j - 1) * geom$cell_width
    cell_right <- cell_left + geom$cell_width
    cell_top <- geom$payoff_top + (i - 1) * geom$cell_height
    # Slightly less inset for inner columns (PNG grid vs. flextable is a bit off)
    inset_x_frac <- if (j == game_data$num_strategies_b) 0.11 else 0.07
    px <- cell_right - geom$cell_width * inset_x_frac
    py <- cell_top + geom$cell_height * 0.14
    x_npc <- (margin_left + px) / new_width
    y_npc <- image_y_top_to_npc(py, img_height, margin_bottom, new_height)

    draw_mixed_label(
      entry$mark,
      x_npc = x_npc,
      y_npc = y_npc,
      fontsize = mark_fontsize,
      font_families = font_families,
      text_color = ANNOTATION_TEXT_COLOR,
      hjust = 1,
      vjust = 1
    )
  }

  invisible(NULL)
}

#' Draw footnote lines directly below the payoff table
draw_annotation_footnotes <- function(footnotes, img_width, img_height,
                                     margin_left, margin_bottom, new_width,
                                     new_height,
                                     font_families = get_game_table_fonts(),
                                     footnote_fontsize = NULL) {
  if (length(footnotes) == 0) {
    return(invisible(NULL))
  }

  font_families <- normalize_font_families(font_families)
  if (is.null(footnote_fontsize)) {
    footnote_fontsize <- footnote_fontsize_for_image(img_width)
  }
  line_spacing_px <- footnote_line_height_px(footnote_fontsize)
  gap_after_table_px <- footnote_gap_after_table_px(footnote_fontsize)
  table_bottom_py <- img_height
  x_right <- (margin_left + img_width * 0.98) / new_width

  footnote_lines <- vapply(
    footnotes,
    function(note) paste0(note$footnote_mark, " ", note$text),
    character(1)
  )
  line_widths <- vapply(
    footnote_lines,
    measure_mixed_label_width_npc,
    numeric(1),
    fontsize = footnote_fontsize,
    font_families = font_families
  )
  x_start <- x_right - max(line_widths)

  for (idx in seq_along(footnotes)) {
    py <- table_bottom_py + gap_after_table_px + (idx - 0.5) * line_spacing_px
    y_npc <- image_y_top_to_npc(py, img_height, margin_bottom, new_height)
    draw_mixed_label(
      footnote_lines[idx],
      x_npc = x_start,
      y_npc = y_npc,
      fontsize = footnote_fontsize,
      font_families = font_families,
      text_color = ANNOTATION_TEXT_COLOR,
      hjust = 0
    )
  }

  invisible(NULL)
}

#' Create and customize flextable for game matrix
create_game_table <- function(display_data, nash_equilibria,
                              style_options = list(),
                              highlight_nash = TRUE,
                              cell_colors = list()) {
  # Default style settings
  default_fonts <- get_game_table_fonts()
  default_style <- list(
    font_size = 10.5,
    latin_font_family = default_fonts$latin,
    japanese_font_family = default_fonts$japanese,
    padding = 8,
    width_scale = 1.25,
    bg_color = "white",
    highlight_color = "lightgray",
    border_color = "black",
    header_border_width = 2,
    first_col_border_width = 2
  )

  # Override defaults with user-specified styles
  style <- modifyList(default_style, style_options)

  # Create main table
  ft <- flextable(display_data) %>%
    align(align = "center", part = "all") %>%
    set_header_labels(Strategy = "") %>%
    fontsize(size = style$font_size, part = "all") %>%
    padding(padding = style$padding, part = "all") %>%
    bg(bg = style$bg_color, part = "all") %>%
    font(fontname = style$latin_font_family,
         part = "all",
         hansi.family = style$latin_font_family,
         cs.family = style$latin_font_family,
         eastasia.family = style$japanese_font_family) %>%
    bold(j = 1, part = "body") %>%
    bold(part = "header", bold = TRUE) %>%
    border_outer(border = fp_border(color = style$border_color)) %>%
    border_inner(border = fp_border(color = style$border_color)) %>%
    hline(part = "header",
          border = fp_border(width = style$header_border_width,
                             color = style$border_color)) %>%
    vline(j = 1,
          border = fp_border(width = style$first_col_border_width,
                             color = style$border_color))

  # Calculate column widths based on content
  strategy_names <- c(display_data$Strategy, names(display_data)[-1])
  payoff_cells <- unlist(display_data[, -1, drop = FALSE])
  all_content <- c(strategy_names, as.character(payoff_cells))
  max_chars <- max(nchar(all_content))

  # Width calculation in inches, accounting for font size and padding
  char_width_inch <- 0.08
  padding_inch <- style$padding / 72
  cell_width <- (max_chars * char_width_inch * style$font_size / 10.5 +
    2 * padding_inch) * style$width_scale

  # Apply uniform width to all columns
  num_cols <- ncol(display_data)
  ft <- ft %>% width(j = seq_len(num_cols), width = cell_width)

  if (highlight_nash) {
    for (eq in nash_equilibria) {
      row_idx <- which(display_data$Strategy == eq$strategies[1])
      col_idx <- which(names(display_data) == eq$strategies[2])

      ft <- ft %>%
        bg(i = row_idx, j = col_idx, bg = style$highlight_color) %>%
        bold(i = row_idx, j = col_idx)
    }
  }

  if (length(cell_colors) > 0) {
    for (entry in cell_colors) {
      ft <- ft %>%
        bg(i = entry$row, j = entry$col + 1, bg = entry$color)
    }
  }

  return(ft)
}

#' Save game table as image
#' @param ft flextable object
#' @param output_path Output file path
#' @param zoom Zoom factor for webshot2
#' @param resolution Image resolution
#' @param add_player_labels Whether to add player labels
#' @param player_labels Player label text for top and left positions (from define_game() or analyze_game())
#' @param label_font_families Latin and Japanese font families for labels
#' @param annotation_data Resolved annotations from resolve_cell_annotations()
#' @param game_data Game data for annotation layout (required if annotations present)
save_game_table <- function(ft, output_path, zoom = 3,
                            resolution = 2000,
                            add_player_labels = TRUE,
                            player_labels,
                            label_font_families = get_game_table_fonts(),
                            annotation_data = NULL,
                            game_data = NULL) {
  # Layout defaults (computed from rendered image size later when labels are used)
  label_font_size <- 64
  margin_top <- 0
  margin_left <- 0
  label_font_families <- normalize_font_families(label_font_families)
  png_type <- if (isTRUE(capabilities("cairo"))) "cairo" else "default"

  if (is.null(annotation_data)) {
    annotation_data <- list(footnotes = list(), cell_marks = list())
  }
  has_annotations <- length(annotation_data$cell_marks) > 0 ||
    length(annotation_data$footnotes) > 0
  footnote_count <- length(annotation_data$footnotes)

  # Table structure ratios
  strategy_col_ratio <- 0.25
  header_row_ratio <- 0.25

  # Create output directory
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Save to temporary file
  temp_path <- tempfile(fileext = ".png")

  tryCatch({
    # Generate table image
    save_as_image(ft, path = temp_path, webshot = "webshot2",
                  zoom = zoom, res = resolution)

    if (add_player_labels || has_annotations) {
      # Read image
      img <- readPNG(temp_path)
      img_height <- nrow(img)
      img_width <- ncol(img)
      if (add_player_labels) {
        base_dim <- min(img_width, img_height)
        label_font_size <- max(52, min(72, round(base_dim * 0.055)))
        label_gap_px <- round(label_font_size * 0.9)
        margin_top <- max(round(img_height * 0.08),
                          label_gap_px + round(label_font_size * 0.95))
        margin_left <- max(round(img_width * 0.08),
                           label_gap_px + round(label_font_size * 1.05))
      }

      margin_bottom <- footnote_margin_bottom_px(footnote_count, img_width)
      footnote_fontsize <- if (footnote_count > 0) {
        footnote_fontsize_for_image(img_width)
      } else {
        NULL
      }

      # New image size with margins
      new_width <- img_width + margin_left
      new_height <- img_height + margin_top + margin_bottom

      # Open PNG device and prepare canvas
      png(output_path, width = new_width,
          height = new_height, res = 300, type = png_type)
      grid.newpage()

      if (add_player_labels) {
        # Column / top player (2)
        payoff_area_center <- strategy_col_ratio +
          (1 - strategy_col_ratio) / 2
        col_player_x_npc <- (margin_left +
                               img_width * payoff_area_center) / new_width
        top_label_gap_px <- round(label_font_size * 0.9)
        col_player_y_npc <- (margin_bottom + img_height +
                               top_label_gap_px) / new_height
        draw_mixed_label(player_labels$top,
                         x_npc = col_player_x_npc,
                         y_npc = col_player_y_npc,
                         fontsize = label_font_size,
                         font_families = label_font_families)

        # Row / left player (1)
        left_label_gap_px <- round(label_font_size * 0.9)
        payoff_row_center_from_top <- header_row_ratio +
          (1 - header_row_ratio) / 2
        # Slightly below table center, but not as low as full payoff-area center
        left_label_vertical_blend <- 0.70
        left_label_center_from_top <- 0.5 * (1 - left_label_vertical_blend) +
          payoff_row_center_from_top * left_label_vertical_blend
        row_player_x_npc <- (margin_left - left_label_gap_px) / new_width
        row_player_y_npc <- (margin_bottom +
                               img_height * (1 - left_label_center_from_top)) /
          new_height
        draw_mixed_label(player_labels$left,
                         x_npc = row_player_x_npc,
                         y_npc = row_player_y_npc,
                         rot = 90,
                         fontsize = label_font_size,
                         font_families = label_font_families)
      }

      # Place table image
      img_center_x_npc <- (margin_left + img_width / 2) / new_width
      img_center_y_npc <- (margin_bottom + img_height / 2) / new_height
      grid.raster(img,
                  x = unit(img_center_x_npc, "npc"),
                  y = unit(img_center_y_npc, "npc"),
                  width = unit(img_width / new_width, "npc"),
                  height = unit(img_height / new_height, "npc"),
                  interpolate = TRUE)

      if (has_annotations) {
        if (is.null(game_data)) {
          stop("game_data is required when annotations are present")
        }
        draw_cell_marks(
          annotation_data$cell_marks,
          game_data = game_data,
          img_width = img_width,
          img_height = img_height,
          margin_left = margin_left,
          margin_bottom = margin_bottom,
          new_width = new_width,
          new_height = new_height,
          strategy_col_ratio = strategy_col_ratio,
          header_row_ratio = header_row_ratio,
          font_families = label_font_families
        )
        draw_annotation_footnotes(
          annotation_data$footnotes,
          img_width = img_width,
          img_height = img_height,
          margin_left = margin_left,
          margin_bottom = margin_bottom,
          new_width = new_width,
          new_height = new_height,
          font_families = label_font_families,
          footnote_fontsize = footnote_fontsize
        )
      }

      dev.off()
      unlink(temp_path)
    } else {
      # Without labels, simply copy
      file.copy(temp_path, output_path, overwrite = TRUE)
      unlink(temp_path)
    }
  }, error = function(e) {
    message("Error saving image: ", e$message)
    if (file.exists(temp_path)) unlink(temp_path)
  })
}

#' Analyze game and generate visualizations
#' @param game_def Game definition list (from define_game)
#' @param output_dir Output directory path
#' @param file_prefix File name prefix
#' @param save_images Whether to save images
#' @param calculate_nash Whether to calculate Nash equilibrium
#' @param zoom Zoom factor for webshot2
#' @param resolution Resolution for webshot2
#' @param player_labels Player labels for top and left positions
#' @param font_families Latin and Japanese font families
analyze_game <- function(game_def,
                         output_dir = get_repo_output_dir(),
                         file_prefix = "game_matrix",
                         save_images = TRUE,
                         calculate_nash = TRUE,
                         zoom = 3, resolution = 2000,
                         player_labels = NULL,
                         font_families = NULL) {

  if (is.null(player_labels)) {
    player_labels <- game_def$player_labels
  }
  if (is.null(font_families)) {
    font_families <- game_def$font_families
  }
  font_families <- normalize_font_families(font_families)

  # High-quality style settings
  high_quality_style <- list(
    font_size = 11,
    latin_font_family = font_families$latin,
    japanese_font_family = font_families$japanese,
    padding = 10,
    bg_color = "white",
    highlight_color = "#E8E8E8",
    border_color = "#2c3e50"
  )

  # Create game data
  game_data <- create_game_data(
    payoffs = game_def$payoff_matrix,
    strategy_names_a = game_def$strategies,
    strategy_names_b = game_def$strategies
  )

  # Calculate Nash equilibrium (optional)
  nash_equilibria <- list()
  if (calculate_nash) {
    cat("Calculating Nash equilibrium...\n")
    nash_equilibria <- calculate_nash_equilibrium(game_data)
  } else {
    cat("Skipping Nash equilibrium calculation\n")
  }

  # Create display dataframe
  display_data <- create_display_dataframe(game_data)

  annotation_data <- resolve_cell_annotations(
    game_def$cell_annotations,
    game_data
  )
  cell_colors <- resolve_cell_colors(
    game_def$cell_colors,
    game_data
  )

  # Create flextables
  ft_without_nash <- create_game_table(display_data, nash_equilibria,
                                       high_quality_style,
                                       highlight_nash = FALSE,
                                       cell_colors = cell_colors)
  ft_with_nash <- create_game_table(display_data, nash_equilibria,
                                    high_quality_style,
                                    highlight_nash = TRUE,
                                    cell_colors = cell_colors)

  # Save images
  if (save_images) {
    # Create output directory
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Generate filenames
    filename_without_nash <- paste0(file_prefix, ".png")
    filename_with_nash <- paste0(file_prefix, "_with_nash.png")

    # Save images
    save_game_table(ft_without_nash,
                    file.path(output_dir, filename_without_nash),
                    zoom = zoom, resolution = resolution,
                    player_labels = player_labels,
                    label_font_families = font_families,
                    annotation_data = annotation_data,
                    game_data = game_data)

    # Save Nash equilibrium version only if calculated
    if (calculate_nash) {
      save_game_table(ft_with_nash,
                      file.path(output_dir, filename_with_nash),
                      zoom = zoom, resolution = resolution,
                      player_labels = player_labels,
                      label_font_families = font_families,
                      annotation_data = annotation_data,
                      game_data = game_data)
    }

    cat("Images saved to:", output_dir, "\n")
    cat("  -", filename_without_nash, "\n")
    if (calculate_nash) {
      cat("  -", filename_with_nash, "\n")
    }
  }

  # Return results
  return(list(
    game_definition = game_def,
    game_data = game_data,
    nash_equilibria = nash_equilibria,
    display_data = display_data,
    cell_colors = cell_colors,
    without_nash = ft_without_nash,
    with_nash = if (calculate_nash) ft_with_nash else NULL,
    nash_calculated = calculate_nash
  ))
}

# Main execution
cat("Game Theory Table Generator\n")

resolved_settings <- resolve_game_settings()
if (!is.null(resolved_settings$settings_json) &&
    !identical(resolved_settings$settings_json, "")) {
  cat("Loaded settings JSON:", resolved_settings$settings_json, "\n")
}
save_images <- resolved_settings$save_images

# Load game definition
mini_nash_game <- define_game(
  output_dir = resolved_settings$output_dir,
  file_prefix = resolved_settings$file_prefix,
  calculate_nash = resolved_settings$calculate_nash,
  player_labels = resolved_settings$player_labels,
  font_families = resolved_settings$font_families,
  strategies = resolved_settings$strategies,
  payoff_matrix = resolved_settings$payoff_matrix,
  cell_annotations_enabled = resolved_settings$cell_annotations_enabled,
  cell_annotations = resolved_settings$cell_annotations,
  cell_colors_enabled = resolved_settings$cell_colors_enabled,
  cell_colors = resolved_settings$cell_colors,
  enabled = resolved_settings$enabled
)

# Execute game processing
if (mini_nash_game$enabled) {
  # Display game information
  cat("\nGame definition:\n")
  cat("Strategies:", paste(mini_nash_game$strategies, collapse = ", "), "\n")
  cat("Output directory:", mini_nash_game$output_dir, "\n")
  cat("File prefix:", mini_nash_game$file_prefix, "\n")
  cat("Latin font:", mini_nash_game$font_families$latin, "\n")
  cat("Japanese font:", mini_nash_game$font_families$japanese, "\n")
  cat("Player labels:",
      mini_nash_game$player_labels$top, "/",
      mini_nash_game$player_labels$left, "\n")
  nash_status <- if (mini_nash_game$calculate_nash) "enabled" else "disabled"
  cat("Nash equilibrium:", nash_status, "\n")
  annotation_status <- if (isTRUE(mini_nash_game$cell_annotations_enabled)) {
    "enabled"
  } else {
    "disabled"
  }
  cat("Cell annotations:", annotation_status, "\n")
  color_status <- if (isTRUE(mini_nash_game$cell_colors_enabled)) {
    "enabled"
  } else {
    "disabled"
  }
  cat("Cell colors:", color_status, "\n")

  # Run game analysis
  cat("\nRunning analysis...\n")
  if (mini_nash_game$calculate_nash) {
    cat("Nash equilibrium: enabled\n")
  } else {
    cat("Nash equilibrium: disabled (fast mode)\n")
  }

  results <- analyze_game(
    game_def = mini_nash_game,
    output_dir = mini_nash_game$output_dir,
    file_prefix = mini_nash_game$file_prefix,
    save_images = save_images,
    calculate_nash = mini_nash_game$calculate_nash
  )

  # Display Nash equilibria
  cat("\nNash equilibria:\n")
  if (results$nash_calculated) {
    if (length(results$nash_equilibria) > 0) {
      for (i in seq_along(results$nash_equilibria)) {
        eq <- results$nash_equilibria[[i]]
        cat(sprintf("  Strategies: (%s, %s), Payoffs: (%s, %s)\n",
                    eq$strategies[1], eq$strategies[2],
                    eq$payoffs[1], eq$payoffs[2]))
      }
    } else {
      cat("  No Nash equilibrium exists\n")
    }
  } else {
    cat("  Nash equilibrium calculation was skipped\n")
  }

  cat("\nGeneration complete\n")
  if (save_images) {
    cat("Generated images:\n")
    cat("  -", file.path(mini_nash_game$output_dir,
                         paste0(mini_nash_game$file_prefix, ".png")), "\n")
    if (mini_nash_game$calculate_nash) {
      cat("  -", file.path(mini_nash_game$output_dir,
                           paste0(mini_nash_game$file_prefix,
                                  "_with_nash.png")), "\n")
    }
  } else {
    cat("Image saving was skipped\n")
  }
} else {
  cat("\nGame processing skipped\n")
  cat("This game is disabled\n")
  cat("To enable, set enabled to true in the settings JSON\n")
}