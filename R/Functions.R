#' BPC Corporate Color Palette Utilities
#'
#' Provides corporate color palettes and ggplot2 themes for BPC research organization.
#' Includes qualitative, sequential, and divergent color scales with consistent branding.
#'
#' @name bpcplot-package
#' @aliases bpcplot
NULL

#' BPC Corporate Colors
#'
#' The official qualitative color palette for BPC research organization.
#'
#' @format A character vector with 6 HEX color codes:
#' \describe{
#'   \item{1}{#6CC9D0 - Light blue}
#'   \item{2}{#ACAD71 - Olive green}
#'   \item{3}{#7F7B64 - Dark taupe}
#'   \item{4}{#CF9B62 - Peach}
#'   \item{5}{#9f2020 - Red}
#'   \item{6}{#585458 - Dark gray}
#' }
#'
#' @examples
#' bpc_colors
#' scales::show_col(bpc_colors)
#'
#' @export
bpc_colors <- c("#6CC9D0", "#ACAD71", "#7F7B64", "#CF9B62", "#9f2020", "#585458")

#' BPC Corporate Palettes
#'
#' BPC color palettes
#'
#' @examples
#' bpc_palettes
#'
#' @export
bpc_palettes <- function() {
  pals <- c("qualitative", "sequential_1", "sequential_2", "sequential_3",
  "sequential_4", "sequential_5", "sequential_6", "divergent_1", "divergent_2",
  "divergent_3", "divergent_4", "divergent_5", "divergent_6", "divergent_7")
  return(pals)
}

#' BPC Custom ggplot2 Theme
#'
#' A minimal ggplot2 theme with BPC corporate styling preferences.
#'
#' @return A ggplot2 theme object
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_bpc()
#'
#' @export
theme_bpc <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.key.width  = ggplot2::unit(0.5, "cm"),
      legend.key.height = ggplot2::unit(0.3, "cm"),
      strip.background  = ggplot2::element_rect(color = "white", fill = "gray95"),
      legend.text       = ggplot2::element_text(size = 6),
      plot.title        = ggplot2::element_text(face = 2)
    )
}

#' Get BPC Color Palette
#'
#' Retrieve BPC corporate color palettes for use in plots.
#'
#' @param palette Name of the palette. One of: "qualitative", "sequential_1" to
#'   "sequential_6", or "divergent_1" to "divergent_7".
#' @param n Number of colors desired. If NULL, uses default length for the palette.
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#'
#' @return A character vector of color hex codes.
#'
#' @examples
#' # Qualitative palette
#' bpc_cols("qualitative")
#'
#' # Sequential palette
#' bpc_cols("sequential_1", n = 5)
#'
#' # Divergent palette reversed
#' bpc_cols("divergent_1", direction = -1)
#'
#' @export
bpc_cols <- function(palette = "qualitative", n = NULL, direction = 1) {

  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 (normal) or -1 (reversed)")
  }

  # Define all palettes as functions that take 'n'
  palettes_list <- list(

    # Qualitative: Just return the bcp colors
    qualitative = function(n) {
      if (is.null(n)) n <- length(bpc_colors)
      if (n > length(bpc_colors)) {
        warning("Requested more qualitative colors than available.")
      }
      unname(bpc_colors[1:min(n, length(bpc_colors))])
    },

    # Sequential: Create one palette for each seed color
      sequential_1 = grDevices::colorRampPalette(c("white", bpc_colors[1], bpc_colors[6]))
    , sequential_2 = grDevices::colorRampPalette(c("white", bpc_colors[2], bpc_colors[6]))
    , sequential_3 = grDevices::colorRampPalette(c("white", bpc_colors[3], bpc_colors[6]))
    , sequential_4 = grDevices::colorRampPalette(c("white", bpc_colors[4], bpc_colors[6]))
    , sequential_5 = grDevices::colorRampPalette(c("white", bpc_colors[5], bpc_colors[6]))
    , sequential_6 = grDevices::colorRampPalette(c("white", bpc_colors[4], bpc_colors[5], bpc_colors[6]))

    # Divergent: Create palettes between two contrasting corporate colors
    , divergent_1 = grDevices::colorRampPalette(c(bpc_colors[1], "white", bpc_colors[2]))
    , divergent_2 = grDevices::colorRampPalette(c(bpc_colors[1], "white", bpc_colors[4]))
    , divergent_3 = grDevices::colorRampPalette(c(bpc_colors[1], "white", bpc_colors[5]))
    , divergent_4 = grDevices::colorRampPalette(c(bpc_colors[2], "white", bpc_colors[4]))
    , divergent_5 = grDevices::colorRampPalette(c(bpc_colors[2], "white", bpc_colors[5]))
    , divergent_6 = grDevices::colorRampPalette(c(bpc_colors[3], "white", bpc_colors[4]))
    , divergent_7 = grDevices::colorRampPalette(c(bpc_colors[3], "white", bpc_colors[5]))
  )

  # Check if the requested palette exists
  if (!palette %in% names(palettes_list)) {
    stop("Palette not found. Choose from: ", paste(names(palettes_list), collapse = ", "))
  }

  # Get the selected palette function
  pal_func <- palettes_list[[palette]]

  # If n is NULL, use the default for qualitative, else 11 is a good default for continuous
  if (is.null(n)) {
    if (palette == "qualitative") {
      n <- length(bpc_colors)
    } else {
      n <- 11
    }
  }

  # Call the palette function with n and return the colors
  colors <- pal_func(n)
  if (direction == -1) {
    colors <- rev(colors)
  }
  return(colors)
}

#' BPC Qualitative Color Scale for ggplot2
#'
#' Qualitative color scale using BPC corporate colors for ggplot2.
#'
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to discrete_scale.
#'
#' @return A ggplot2 discrete scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_bpc_qualitative()
#'
#' @export
scale_color_bpc_qualitative <- function(..., direction = 1) {
  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 (normal) or -1 (reversed)")
  }

  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "bpc_qualitative",
    palette = function(n) {
      if (n > length(bpc_colors)) {
        warning("Only ", length(bpc_colors), " qualitative colors available. Reusing colors.")
        colors <- rep(bpc_colors, length.out = n)
      } else {
        colors <- bpc_colors[1:n]
      }
      if (direction == -1) {
        colors <- rev(colors)
      }
      return(colors)
    },
    ...
  )
}

#' BPC Qualitative Fill Scale for ggplot2
#'
#' Qualitative fill scale using BPC corporate colors for ggplot2.
#'
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to discrete_scale.
#'
#' @return A ggplot2 discrete scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_bpc_qualitative()
#'
#' @export
scale_fill_bpc_qualitative <- function(..., direction = 1) {
  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 (normal) or -1 (reversed)")
  }

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "bpc_qualitative",
    palette = function(n) {
      if (n > length(bpc_colors)) {
        warning("Only ", length(bpc_colors), " qualitative colors available. Reusing colors.")
        colors <- rep(bpc_colors, length.out = n)
      } else {
        colors <- bpc_colors[1:n]
      }
      if (direction == -1) {
        colors <- rev(colors)
      }
      return(colors)
    },
    ...
  )
}

#' BPC Sequential Color Scale for ggplot2
#'
#' Sequential color scale using BPC corporate colors for ggplot2.
#'
#' @param palette Name of the sequential palette ("sequential_1" to "sequential_6").
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to scale_color_gradientn.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, color = density)) +
#'   geom_point() +
#'   scale_color_bpc_sequential("sequential_1")
#'
#' @export
scale_color_bpc_sequential <- function(palette = "sequential_1", direction = 1, ...) {
  ggplot2::scale_color_gradientn(
    colours = bpc_cols(palette, n = 101, direction = direction),
    ...
  )
}

#' BPC Sequential Fill Scale for ggplot2
#'
#' Sequential fill scale using BPC corporate colors for ggplot2.
#'
#' @param palette Name of the sequential palette ("sequential_1" to "sequential_6").
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to scale_fill_gradientn.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_bpc_sequential("sequential_1")
#'
#' @export
scale_fill_bpc_sequential <- function(palette = "sequential_1", direction = 1, ...) {
  ggplot2::scale_fill_gradientn(
    colours = bpc_cols(palette, n = 101, direction = direction),
    ...
  )
}

#' BPC Divergent Color Scale for ggplot2
#'
#' Divergent color scale using BPC corporate colors for ggplot2.
#'
#' @param palette Name of the divergent palette ("divergent_1" to "divergent_7").
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to scale_color_gradient2.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(msleep, aes(bodywt, brainwt, color = sleep_total - mean(sleep_total))) +
#'   geom_point() +
#'   scale_color_bpc_divergent("divergent_1")
#'
#' @export
scale_color_bpc_divergent <- function(palette = "divergent_1", direction = 1, ...) {
  cols <- bpc_cols(palette, n = 3, direction = direction)
  ggplot2::scale_color_gradient2(
    low  = cols[1],
    mid  = cols[2],
    high = cols[3],
    ...
  )
}

#' BPC Divergent Fill Scale for ggplot2
#'
#' Divergent fill scale using BPC corporate colors for ggplot2.
#'
#' @param palette Name of the divergent palette ("divergent_1" to "divergent_7").
#' @param direction Direction of the palette (1 = normal, -1 = reversed).
#' @param ... Additional arguments passed to scale_fill_gradient2.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(msleep, aes(bodywt, brainwt, fill = sleep_total - mean(sleep_total))) +
#'   geom_tile() +
#'   scale_fill_bpc_divergent("divergent_1")
#'
#' @export
scale_fill_bpc_divergent <- function(palette = "divergent_1", direction = 1, ...) {
  cols <- bpc_cols(palette, n = 3, direction = direction)
  ggplot2::scale_fill_gradient2(
      low  = cols[1]
    , mid  = cols[2]
    , high = cols[3]
    , ...
  )
}

#' Display BPC Palette Colors
#'
#' Create a visualization of BPC palette colors.
#'
#' @param palette_name Name of the palette to display
#' @param n Number of colors to show (for non-qualitative palettes)
#' @param direction Direction of the palette (1 = normal, -1 = reversed)
#' @return A ggplot object showing the palette colors
#'
#' @examples
#' show_bpc_palette("qualitative")
#' show_bpc_palette("sequential_1", n = 9)
#'
#' @export
show_bpc_palette <- function(palette_name = "qualitative", n = NULL, direction = 1) {

  # Pull colors
  colors <- bpc_cols(palette_name, n = n, direction = direction)

  # Prepare data frame for plotting
  df <- data.frame(
      x     = seq_along(colors)
    , y     = 1
    , color = colors
  )

  # Prepare plot of colors
  p <- ggplot(df, aes(x = x, y = y, fill = color)) +
    geom_tile(color = "black", linewidth = 0.2) +
    scale_fill_identity() +
    theme_minimal() +
    theme(
        axis.text.x  = element_blank()
      , axis.text.y  = element_blank()
      , axis.ticks.x = element_blank()
      , axis.ticks.y = element_blank()
      , axis.title.y = element_text(angle = 0, vjust = 0.5)
      , panel.grid   = element_blank()
      , plot.margin = margin(0, 0, 0, 0)
    ) +
    xlab("") +
    ylab(palette_name) +
    coord_fixed()

  # Return the plot
  return(p)
}

#' Display All BPC Color Palettes
#'
#' Create an overview of all available BPC color palettes, similar to
#' RColorBrewer's display.brewer.all() function.
#'
#' @param n Number of colors to display for each palette (if NULL, uses default)
#' @param type Type of palettes to display: "qual", "seq", "div", or "all"
#' @param select Specific palettes to display (by name)
#' @param exact.n If TRUE, only show palettes that can generate exactly n colors
#' @param direction Direction of the palette (1 = normal, -1 = reversed)
#' @return A base R plot showing all available palettes
#'
#' @examples
#' display_bpc_all()
#' display_bpc_all(type = "seq")
#' display_bpc_all(select = c("qualitative", "sequential_1", "divergent_1"))
#'
#' @export
display_bpc_all <- function(n = NULL, type = "all", select = NULL,
                           exact.n = TRUE, direction = 1) {

  # Define all palette lists
  qual_list <- list(qualitative = "qualitative")
  seq_list <- list(
    sequential_1 = "sequential_1",
    sequential_2 = "sequential_2",
    sequential_3 = "sequential_3",
    sequential_4 = "sequential_4",
    sequential_5 = "sequential_5",
    sequential_6 = "sequential_6"
  )
  div_list <- list(
    divergent_1 = "divergent_1",
    divergent_2 = "divergent_2",
    divergent_3 = "divergent_3",
    divergent_4 = "divergent_4",
    divergent_5 = "divergent_5",
    divergent_6 = "divergent_6",
    divergent_7 = "divergent_7"
  )

  # Combine all lists in logical order
  total_list <- c(qual_list, seq_list, div_list)

  # Maximum number of colors for each palette type
  qual_max <- length(bpc_colors)
  seq_max <- 11  # Default for sequential
  div_max <- 11  # Default for divergent

  max_num <- c(
    rep(qual_max, length(qual_list)),
    rep(seq_max, length(seq_list)),
    rep(div_max, length(div_list))
  )
  names(max_num) <- names(total_list)

  # Filter by type if specified
  if (!(type %in% c("div", "qual", "seq", "all"))) {
    stop("type must be one of: 'div', 'qual', 'seq', or 'all'")
  }

  color_list <- switch(type,
    div = div_list,
    qual = qual_list,
    seq = seq_list,
    all = total_list
  )

  max_num <- switch(type,
    div = rep(div_max, length(div_list)),
    qual = rep(qual_max, length(qual_list)),
    seq = rep(seq_max, length(seq_list)),
    all = max_num
  )

  # Filter by select if specified
  if (!is.null(select)) {
    color_list <- color_list[select]
    max_num <- max_num[select]
    if (any(is.na(color_list))) {
      stop("Illegal value(s) of select: ",
           paste(select[is.na(color_list)], collapse = " "))
    }
  }

  # Handle n parameter
  if (is.null(n)) n <- max_num
  if (length(n) == 1) n <- rep(n, length(color_list))

  # Filter by exact.n if requested
  if (exact.n) {
    keep <- n <= max_num
    color_list <- color_list[keep]
    n <- n[keep]
    max_num <- max_num[keep]
  }

  # Validate n values
  if (any(n < 3) | exact.n & any(n > max_num) | length(n) != length(color_list)) {
    warning("Some palettes may not be displayed correctly due to color number issues")
  }

  n[n < 3] <- 3
  n[n > max_num] <- max_num[n > max_num]

  # Set up plotting parameters
  nr <- length(color_list)
  nc <- max(n)

  # Set up plot with better margins
  old_par <- par(mar = c(2, 8, 3, 1), mgp = c(2, 0.25, 0))
  on.exit(par(old_par))

  plot(1, 1,
       xlim = c(0, nc),
       ylim = c(0, nr + 0.5),
       type = "n",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       main = "BPC Color Palettes Overview")

  # Draw the color palettes
  for (i in 1:nr) {
    palette_name <- unlist(color_list[i])
    nj <- n[i]

    # Get the colors
    shades <- bpc_cols(palette_name, n = nj, direction = direction)

    # Draw the color rectangles
    rect(xleft = 0:(nj-1),
         ybottom = nr - i + 0.3,
         xright = 1:nj,
         ytop = nr - i + 0.8,
         col = shades,
         border = "light grey")

    # Add palette name with better positioning
    text(x = -0.5,
         y = nr - i + 0.55,
         labels = palette_name,
         xpd = TRUE,
         adj = 1,
         cex = 0.85)
  }

  # Add direction indicator
  dir_text <- ifelse(direction == 1, "Normal", "Reversed")
  mtext(paste("Direction:", dir_text), side = 1, line = 0.5, cex = 0.8)

  # Add color count indicator
  mtext(paste("Number of colors:", ifelse(length(unique(n)) == 1, unique(n), "variable")),
        side = 1, line = 1.5, cex = 0.7)
}
