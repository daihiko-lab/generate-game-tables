#!/usr/bin/env Rscript

# Game Theory Table Generator using R flextable + webshot2
# Usage: Rscript generate_game_tables.R
# To customize output files, edit default values in define_game() function

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
                       "officer", "grid", "gridExtra", "png")

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

#' Draw mixed Latin/Japanese labels with separate fonts
draw_mixed_label <- function(label, x_npc, y_npc, rot = 0,
                             fontsize = 80,
                             font_families = get_game_table_fonts()) {
  font_families <- normalize_font_families(font_families)

  # Rotated labels: draw as one string (split Latin/Japanese breaks order)
  if (rot != 0) {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              hjust = 0.5,
              vjust = 0.5,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$japanese))
    return(invisible(NULL))
  }

  label_parts <- split_label_text(label)

  if (label_parts$japanese == "") {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$latin))
    return(invisible(NULL))
  }

  if (label_parts$latin == "") {
    grid.text(label,
              x = unit(x_npc, "npc"),
              y = unit(y_npc, "npc"),
              rot = rot,
              gp = gpar(fontsize = fontsize,
                        fontface = "plain",
                        fontfamily = font_families$japanese))
    return(invisible(NULL))
  }

  latin_grob <- textGrob(
    label_parts$latin,
    gp = gpar(fontsize = fontsize,
              fontfamily = font_families$latin)
  )
  japanese_grob <- textGrob(
    label_parts$japanese,
    gp = gpar(fontsize = fontsize,
              fontfamily = font_families$japanese)
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
            gp = gpar(fontsize = fontsize,
                      fontface = "plain",
                      fontfamily = font_families$latin))
  grid.text(label_parts$japanese,
            x = unit(japanese_x, "npc"),
            y = unit(japanese_y, "npc"),
            rot = rot,
            gp = gpar(fontsize = fontsize,
                      fontface = "plain",
                      fontfamily = font_families$japanese))

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

#' Resolve repository-root images directory from this script's location
#'
#' Repo root is the directory containing this script (works when run with
#' Rscript --vanilla generate_game_tables.R from the checkout root).
get_repo_images_dir <- function(...) {
  script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  repo_root <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
  file.path(repo_root, "images", ...)
}

# Game definition
#' Define game parameters
#'
#' @param output_dir Output directory
#' @param file_prefix File name prefix
#' @param calculate_nash Whether to calculate Nash equilibrium
#' @param player_labels Player labels for top and left positions
#' @param font_families Latin and Japanese font families
#' @return Game definition list
define_game <- function(output_dir = get_repo_images_dir("matrix", "demo"),
                        file_prefix = "3strategies",
                        calculate_nash = TRUE,
                        player_labels = list(
                          top = "Bタイプの成員",
                          left = "Aタイプの成員"
                        ),
                        font_families = get_game_table_fonts()) {
  enabled <- TRUE
  font_families <- normalize_font_families(font_families)

  game_definition <- list(
    enabled = enabled,
    strategies = c("40％を要求", "50％を要求", "60％を要求"),
    output_dir = output_dir,
    file_prefix = file_prefix,
    calculate_nash = calculate_nash,
    player_labels = player_labels,
    font_families = font_families
  )

  if (enabled) {
    # Payoff matrix (row-major): rows = left player (A), cols = top player (B);
    # each cell is c(row payoff, col payoff) = c(A, B)
    game_definition$payoff_matrix <- list(
      c(40, 40), c(40, 50), c(40, 60),
      c(50, 40), c(50, 50), c(0, 0),
      c(60, 40), c(0, 0), c(0, 0)
    )
  } else {
    game_definition$payoff_matrix <- list()
  }
  return(game_definition)
}

# Game analysis functions
#' Create game data structure from payoff matrix
#' @param payoffs Payoff matrix in list format (e.g., list(c(4,4)...))
#' @param strategy_names_a Strategy names for Player A
#' @param strategy_names_b Strategy names for Player B (defaults to Player A)
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

    # Check best response for Player A
    for (i in 1:game_data$num_strategies_a) {
      if (i != strategy_a) {
        other_idx <- (i - 1) * game_data$num_strategies_b + strategy_b
        other_payoff <- game_data$payoffs[[other_idx]]
        if (other_payoff[1] > current_payoff[1] + game_data$epsilon) {
          return(FALSE)
        }
      }
    }

    # Check best response for Player B
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

#' Create and customize flextable for game matrix
create_game_table <- function(display_data, nash_equilibria,
                              style_options = list(),
                              highlight_nash = TRUE) {
  # Default style settings
  default_fonts <- get_game_table_fonts()
  default_style <- list(
    font_size = 10.5,
    latin_font_family = default_fonts$latin,
    japanese_font_family = default_fonts$japanese,
    padding = 8,
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
  cell_width <- max_chars * char_width_inch * style$font_size / 10.5 +
    2 * padding_inch

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

  return(ft)
}

#' Save game table as image
#' @param ft flextable object
#' @param output_path Output file path
#' @param zoom Zoom factor for webshot2
#' @param resolution Image resolution
#' @param add_player_labels Whether to add player labels
#' @param player_labels Player label text for top and left positions
#' @param label_font_families Latin and Japanese font families for labels
save_game_table <- function(ft, output_path, zoom = 3,
                            resolution = 2000,
                            add_player_labels = TRUE,
                            player_labels = list(
                              top = "Aタイプエージェント",
                              left = "Bタイプエージェント"
                            ),
                            label_font_families = get_game_table_fonts()) {
  # Layout constants (scale margins with label size to avoid clipping)
  label_font_size <- 80
  margin_top <- max(360, round(label_font_size * 6.5))
  margin_left <- max(440, round(label_font_size * 5.5))
  player_labels <- modifyList(list(
    top = "Aタイプエージェント",
    left = "Bタイプエージェント"
  ), player_labels)
  label_font_families <- normalize_font_families(label_font_families)
  png_type <- if (isTRUE(capabilities("cairo"))) "cairo" else "default"

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

    if (add_player_labels) {
      # Read image
      img <- readPNG(temp_path)
      img_height <- nrow(img)
      img_width <- ncol(img)

      # New image size with margins
      new_width <- img_width + margin_left
      new_height <- img_height + margin_top

      # Open PNG device and prepare canvas
      png(output_path, width = new_width,
          height = new_height, res = 300, type = png_type)
      grid.newpage()

      # Place column-player label (top center)
      payoff_area_center <- strategy_col_ratio +
        (1 - strategy_col_ratio) / 2
      player2_x_npc <- (margin_left +
                          img_width * payoff_area_center) / new_width
      player2_y_npc <- (img_height + margin_top * 0.32) / new_height
      draw_mixed_label(player_labels$top,
                       x_npc = player2_x_npc,
                       y_npc = player2_y_npc,
                       fontsize = label_font_size,
                       font_families = label_font_families)

      # Place row-player label: center in left margin, align with full table
      player1_x_npc <- (margin_left * 0.5) / new_width
      player1_y_npc <- (img_height / 2) / new_height
      draw_mixed_label(player_labels$left,
                       x_npc = player1_x_npc,
                       y_npc = player1_y_npc,
                       rot = 90,
                       fontsize = label_font_size,
                       font_families = label_font_families)

      # Place table image
      img_center_x_npc <- (margin_left + img_width / 2) / new_width
      img_center_y_npc <- (img_height / 2) / new_height
      grid.raster(img,
                  x = unit(img_center_x_npc, "npc"),
                  y = unit(img_center_y_npc, "npc"),
                  width = unit(img_width / new_width, "npc"),
                  height = unit(img_height / new_height, "npc"),
                  interpolate = TRUE)

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
                         output_dir = "_webshot/matrix/default",
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

  # Create flextables
  ft_without_nash <- create_game_table(display_data, nash_equilibria,
                                       high_quality_style,
                                       highlight_nash = FALSE)
  ft_with_nash <- create_game_table(display_data, nash_equilibria,
                                    high_quality_style,
                                    highlight_nash = TRUE)

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
                    label_font_families = font_families)

    # Save Nash equilibrium version only if calculated
    if (calculate_nash) {
      save_game_table(ft_with_nash,
                      file.path(output_dir, filename_with_nash),
                      zoom = zoom, resolution = resolution,
                      player_labels = player_labels,
                      label_font_families = font_families)
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
    without_nash = ft_without_nash,
    with_nash = if (calculate_nash) ft_with_nash else NULL,
    nash_calculated = calculate_nash
  ))
}

# Main execution
cat("Game Theory Table Generator\n")

# Execution options
save_images <- TRUE

# Load game definition
mini_nash_game <- define_game()

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
  cat("To enable, set enabled = TRUE in define_game()\n")
}