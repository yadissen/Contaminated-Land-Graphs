# ═══════════════════════════════════════════════════════════════════════════════
#  Groundwater Contour Map Studio — Professional R Shiny Application
#  Academic-quality spatial visualisation for contaminated land assessment
# ═══════════════════════════════════════════════════════════════════════════════
#
#  Required packages:
#    install.packages(c("shiny","bslib","ggplot2","scales","svglite","ggrepel",
#                       "png","jpeg"))
# ═══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(bslib)
library(ggplot2)
library(scales)
library(svglite)
library(ggrepel)
library(png)
library(jpeg)

# ═══════════════════════════════════════════════════════════════════════════════
#  DATA
# ═══════════════════════════════════════════════════════════════════════════════

gw_data    <- read.csv("groundwater_elevations.csv", stringsAsFactors = FALSE)
site_bnds  <- read.csv("site_boundaries.csv",        stringsAsFactors = FALSE)

# Rename columns to safe names for use in code
names(gw_data) <- c("Borehole", "Easting", "Northing",
                    "BH_Elev_mamsl", "WL_mbgl", "GW_Elev_mamsl")

# ═══════════════════════════════════════════════════════════════════════════════
#  CONSTANTS
# ═══════════════════════════════════════════════════════════════════════════════

# Plot extent (site boundary + 5 m padding)
XMIN <- min(site_bnds$Easting)  - 5
XMAX <- max(site_bnds$Easting)  + 5
YMIN <- min(site_bnds$Northing) - 5
YMAX <- max(site_bnds$Northing) + 5

# Colour palettes (low → high elevation)
PALETTES <- list(
  "Hydroblue"        = c("#eaf4fb", "#9ecae1", "#3182bd", "#08306b"),
  "Hydroblue (Rev)"  = c("#08306b", "#3182bd", "#9ecae1", "#eaf4fb"),
  "Viridis"          = c("#440154", "#31688e", "#35b779", "#fde725"),
  "Plasma"           = c("#0d0887", "#7e03a8", "#cc4778", "#f89441", "#f0f921"),
  "Terrain"          = c("#336600", "#99cc33", "#ffff99", "#cc9933", "#996633", "#ffffff"),
  "RdYlBu"           = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#74add1", "#4575b4"),
  "RdYlBu (Rev)"     = c("#4575b4", "#74add1", "#e0f3f8", "#fee090", "#fc8d59", "#d73027"),
  "Greens"           = c("#f7fcf5", "#74c476", "#006d2c"),
  "Grayscale"        = c("#ffffff", "#969696", "#252525"),
  "Classic Academic" = c("#2a4f6e", "#2e6b45", "#8b8b00", "#8b3a1e")
)

EXPORT_PRESETS <- list(
  "Custom"                = list(w = 10,   h = 8,    dpi = 300),
  "A4 Landscape (300dpi)" = list(w = 11.7, h = 8.3,  dpi = 300),
  "A4 Portrait  (300dpi)" = list(w = 8.3,  h = 11.7, dpi = 300),
  "A3 Landscape (300dpi)" = list(w = 16.5, h = 11.7, dpi = 300),
  "PowerPoint 16:9"       = list(w = 13.3, h = 7.5,  dpi = 150),
  "Poster A0"             = list(w = 16,   h = 12,   dpi = 300)
)

# ═══════════════════════════════════════════════════════════════════════════════
#  SPATIAL HELPERS
# ═══════════════════════════════════════════════════════════════════════════════

# Vectorised IDW interpolation onto a regular grid
idw_grid <- function(pts_x, pts_y, pts_z,
                     nx = 80, ny = 80, power = 2,
                     xmin = XMIN, xmax = XMAX,
                     ymin = YMIN, ymax = YMAX) {
  xo <- seq(xmin, xmax, length.out = nx)
  yo <- seq(ymin, ymax, length.out = ny)
  gdf <- expand.grid(x = xo, y = yo)          # x cycles fastest → nx*ny rows

  n   <- nrow(gdf)
  m   <- length(pts_x)

  # Distance-squared matrix  [n_grid × n_pts]
  dx2 <- outer(gdf$x, pts_x, `-`)^2
  dy2 <- outer(gdf$y, pts_y, `-`)^2
  d2  <- dx2 + dy2

  w   <- 1 / d2^(power / 2)                   # IDW weights
  z_rep <- matrix(pts_z, nrow = n, ncol = m, byrow = TRUE)
  z_vals <- rowSums(w * z_rep) / rowSums(w)

  # Exact-match override (borehole sits exactly on a grid node)
  exact <- which(d2 == 0, arr.ind = TRUE)
  if (nrow(exact) > 0) {
    z_vals[exact[, 1]] <- pts_z[exact[, 2]]
  }

  gdf$z <- z_vals
  gdf
}

# Extract contour line label positions from base-R contourLines()
get_label_positions <- function(grid_df, breaks) {
  xo <- sort(unique(grid_df$x))
  yo <- sort(unique(grid_df$y))
  nx <- length(xo)
  ny <- length(yo)
  # expand.grid fills x fastest → matrix has nx rows, ny cols
  z_mat <- matrix(grid_df$z, nrow = nx, ncol = ny, byrow = FALSE)
  cl    <- contourLines(x = xo, y = yo, z = z_mat, levels = breaks)
  if (!length(cl)) {
    return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
  }
  do.call(rbind, lapply(cl, function(seg) {
    mid <- ceiling(length(seg$x) / 2)
    data.frame(x = seg$x[mid], y = seg$y[mid],
               label = formatC(seg$level, digits = 2, format = "f"))
  }))
}

# Compute flow-direction arrows (negative GW gradient = flow direction)
flow_arrows <- function(grid_df, nx, ny, arrow_density = 8) {
  xo <- sort(unique(grid_df$x))
  yo <- sort(unique(grid_df$y))
  z_mat <- matrix(grid_df$z, nrow = nx, ncol = ny, byrow = FALSE)

  xi_idx <- round(seq(3, nx - 2, length.out = arrow_density))
  yi_idx <- round(seq(3, ny - 2, length.out = arrow_density))
  grid_idx <- expand.grid(i = xi_idx, j = yi_idx)

  dx_field <- diff(range(xo)) * 0.045
  dy_field <- diff(range(yo)) * 0.045

  res <- mapply(function(i, j) {
    gx <- -(z_mat[min(i+1,nx), j] - z_mat[max(i-1,1), j]) /
           (xo[min(i+1,nx)]      - xo[max(i-1,1)])
    gy <- -(z_mat[i, min(j+1,ny)] - z_mat[i, max(j-1,1)]) /
           (yo[min(j+1,ny)]       - yo[max(j-1,1)])
    mag <- sqrt(gx^2 + gy^2)
    if (!is.finite(mag) || mag == 0) return(c(NA, NA, NA, NA))
    c(xo[i], yo[j],
      xo[i] + (gx / mag) * dx_field,
      yo[j] + (gy / mag) * dy_field)
  }, grid_idx$i, grid_idx$j)

  df <- as.data.frame(t(res))
  names(df) <- c("x", "y", "xend", "yend")
  df[complete.cases(df), ]
}

# ═══════════════════════════════════════════════════════════════════════════════
#  ACADEMIC MAP THEME
# ═══════════════════════════════════════════════════════════════════════════════

theme_map <- function(base_size = 11) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(size = rel(1.25), face = "bold",
                                      hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle    = element_text(size = rel(0.85), face = "italic",
                                      hjust = 0.5, color = "#555555",
                                      margin = margin(b = 4)),
      axis.title       = element_text(size = rel(0.9), face = "plain"),
      axis.title.x     = element_text(margin = margin(t = 8)),
      axis.title.y     = element_text(margin = margin(r = 8), angle = 90),
      axis.text        = element_text(size = rel(0.8), color = "#333333"),
      panel.border     = element_rect(fill = NA, color = "#1a1a1a", linewidth = 0.7),
      axis.line        = element_blank(),
      axis.ticks       = element_line(color = "#1a1a1a", linewidth = 0.35),
      panel.background = element_rect(fill = "#f8f8f8", color = NA),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = alpha("white", 0.92),
                                        color = "#cccccc", linewidth = 0.3),
      legend.title     = element_text(size = rel(0.78), face = "bold"),
      legend.text      = element_text(size = rel(0.72)),
      legend.key.width = unit(0.9, "lines"),
      plot.margin      = margin(12, 14, 10, 12),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# ═══════════════════════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════════════════════

ui <- page_navbar(
  title = tags$span(
    tags$span("Groundwater Contour Map Studio",
              style = "font-weight:700; letter-spacing:-0.02em;"),
    tags$span(" \u2014 Contaminated Land Assessment",
              style = "font-weight:300; font-size:0.75em; opacity:0.7;")
  ),
  id    = "main_nav",
  theme = bs_theme(
    version      = 5,
    bootswatch   = "flatly",
    bg           = "#fdfcfa", fg = "#1a1714",
    primary      = "#2a4f6e", secondary = "#8b3a1e",
    success      = "#2e6b45", info = "#1a6b6b",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font    = font_google("JetBrains Mono"),
    "navbar-bg"  = "#1a1714",
    "border-radius" = "4px"
  ),
  header = tags$head(tags$style(HTML("
    .card { border-color: #d4cec4 !important; }
    .card-header {
      background: #f7f4ef !important;
      border-bottom: 1px solid #d4cec4 !important;
      font-size: 0.72rem; text-transform: uppercase;
      letter-spacing: 0.1em; font-weight: 700;
      color: #7a7060; padding: 10px 16px;
    }
    .form-label {
      font-size: 0.68rem !important; text-transform: uppercase;
      letter-spacing: 0.08em; color: #7a7060 !important;
      font-weight: 600 !important;
    }
    .form-control, .form-select {
      font-size: 0.82rem; border-color: #c8bfb0;
    }
    .form-control:focus, .form-select:focus {
      border-color: #2a4f6e;
      box-shadow: 0 0 0 2px rgba(42,79,110,0.15);
    }
    .btn-academic {
      background: #1a1714; color: #f7f4ef;
      font-size: 0.72rem; letter-spacing: 0.07em;
      text-transform: uppercase; border: none;
      padding: 8px 16px; border-radius: 3px;
    }
    .btn-academic:hover { background: #8b3a1e; color: #f7f4ef; }
    .btn-export {
      background: #2a4f6e; color: #fff;
      font-size: 0.72rem; letter-spacing: 0.07em;
      text-transform: uppercase; border: none; padding: 8px 20px;
    }
    .btn-export:hover { background: #1a3a5e; color: #fff; }
    .accordion-button {
      font-size: 0.72rem; text-transform: uppercase;
      letter-spacing: 0.08em; font-weight: 600;
      color: #7a7060; padding: 10px 16px;
    }
    .accordion-button:not(.collapsed) {
      background: #f7f4ef; color: #1a1714;
    }
    .stat-card {
      background: #f7f4ef; border: 1px solid #d4cec4;
      border-radius: 4px; padding: 10px 14px; text-align: center;
    }
    .stat-value { font-size: 1.3rem; font-weight: 700; color: #1a1714; }
    .stat-label {
      font-size: 0.62rem; text-transform: uppercase;
      letter-spacing: 0.1em; color: #7a7060; margin-top: 2px;
    }
    .plot-wrap {
      background: white; border: 1px solid #d4cec4;
      border-radius: 4px; padding: 8px;
    }
  "))),

  # ── TAB 1: Contour Map ────────────────────────────────────────────────────
  nav_panel("Contour Map", icon = icon("map"),
    layout_sidebar(
      fillable = TRUE,
      sidebar  = sidebar(
        width = 320,
        id    = "ctrl_sidebar",

        accordion(
          id   = "acc_ctrl",
          open = c("Contours", "Display"),

          # ── Background Image ───────────────────────────────────────────
          accordion_panel("Background Image", icon = icon("image"),
            helpText("Upload a PNG/JPG of the borehole location plan to overlay.",
                     style = "font-size:0.72rem; color:#7a7060;"),
            fileInput("bg_image", NULL,
                      accept      = c("image/png", "image/jpeg", "image/jpg"),
                      placeholder = "Upload site plan (.png / .jpg)"),
            sliderInput("bg_alpha", "Image Opacity",
                        min = 0.05, max = 1, value = 0.55, step = 0.05),
            checkboxInput("bg_grayscale", "Convert to greyscale", value = TRUE)
          ),

          # ── Contour Settings ───────────────────────────────────────────
          accordion_panel("Contours", icon = icon("layer-group"),
            sliderInput("n_levels", "Number of Contour Levels",
                        min = 5, max = 40, value = 15, step = 1),
            sliderInput("idw_power", "IDW Power (smoothness)",
                        min = 1, max = 5, value = 2, step = 0.5),
            sliderInput("grid_res", "Grid Resolution",
                        min = 40, max = 200, value = 100, step = 10),
            hr(style = "margin:8px 0;"),
            checkboxInput("show_fill",   "Filled Contour Bands",  value = TRUE),
            checkboxInput("show_lines",  "Contour Lines",         value = TRUE),
            conditionalPanel("input.show_lines",
              sliderInput("line_width", "Line Weight",
                          min = 0.15, max = 1.5, value = 0.45, step = 0.05),
              selectInput("line_col", "Line Colour",
                          choices = c("Dark grey" = "grey25",
                                      "Black"     = "#1a1a1a",
                                      "White"     = "white",
                                      "Dark blue" = "#1a3a5e"),
                          selected = "grey25")
            ),
            checkboxInput("show_labels", "Contour Elevation Labels", value = TRUE),
            conditionalPanel("input.show_labels && input.show_lines",
              numericInput("label_size", "Label Size (pt)",
                           value = 2.6, min = 1.5, max = 5, step = 0.1),
              selectInput("label_col", "Label Colour",
                          choices = c("Dark grey" = "grey20",
                                      "Black"     = "#1a1a1a",
                                      "White"     = "white"),
                          selected = "grey20")
            )
          ),

          # ── Display ────────────────────────────────────────────────────
          accordion_panel("Display", icon = icon("palette"),
            selectInput("palette", "Colour Palette",
                        choices  = names(PALETTES),
                        selected = "Hydroblue"),
            checkboxInput("show_boreholes", "Borehole Locations",   value = TRUE),
            checkboxInput("show_bh_labels", "Borehole Labels",      value = TRUE),
            conditionalPanel("input.show_boreholes",
              sliderInput("bh_size", "Marker Size",
                          min = 1, max = 6, value = 2.5, step = 0.25),
              numericInput("bh_lbl_size", "Label Size (pt)",
                           value = 2.5, min = 1.5, max = 5, step = 0.1)
            ),
            checkboxInput("show_boundary", "Site Boundary",         value = TRUE),
            checkboxInput("show_arrows",   "Groundwater Flow Arrows", value = FALSE),
            conditionalPanel("input.show_arrows",
              sliderInput("arrow_density", "Arrow Density",
                          min = 4, max = 16, value = 8, step = 1)
            )
          ),

          # ── Labels & Title ────────────────────────────────────────────
          accordion_panel("Labels & Title", icon = icon("font"),
            textInput("map_title",    "Map Title",
                      value = "Groundwater Elevation Contour Map"),
            textInput("map_subtitle", "Subtitle",
                      value = "Groundwater Elevation (mamsl)"),
            textInput("legend_title", "Legend Title",
                      value = "GW Elevation\n(mamsl)"),
            numericInput("base_size", "Base Font Size",
                         value = 11, min = 8, max = 18, step = 1),
            checkboxInput("coord_fixed", "Equal Axes (1:1 aspect)", value = TRUE)
          ),

          # ── Export ────────────────────────────────────────────────────
          accordion_panel("Export", icon = icon("download"),
            selectInput("export_preset", "Size Preset",
                        choices  = names(EXPORT_PRESETS),
                        selected = "A4 Landscape (300dpi)"),
            conditionalPanel("input.export_preset == 'Custom'",
              numericInput("exp_w",   "Width (inches)",  value = 10,  min = 3, max = 24),
              numericInput("exp_h",   "Height (inches)", value = 8,   min = 3, max = 20),
              numericInput("exp_dpi", "DPI",             value = 300, min = 72, max = 600)
            ),
            selectInput("exp_fmt", "File Format",
                        choices  = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                        selected = "png"),
            downloadButton("dl_map", "Export Map", class = "btn-export w-100 mt-2")
          )
        ) # end accordion
      ),  # end sidebar

      # ── Main panel ──────────────────────────────────────────────────────
      div(
        # KPI stats row
        div(class = "d-flex gap-3 mb-3",
          div(class = "stat-card flex-fill",
            div(class = "stat-value", textOutput("stat_max", inline = TRUE)),
            div(class = "stat-label", "Max GW Elevation (mamsl)")
          ),
          div(class = "stat-card flex-fill",
            div(class = "stat-value", textOutput("stat_min", inline = TRUE)),
            div(class = "stat-label", "Min GW Elevation (mamsl)")
          ),
          div(class = "stat-card flex-fill",
            div(class = "stat-value", textOutput("stat_range", inline = TRUE)),
            div(class = "stat-label", "Range (m)")
          ),
          div(class = "stat-card flex-fill",
            div(class = "stat-value", textOutput("stat_n", inline = TRUE)),
            div(class = "stat-label", "Boreholes")
          )
        ),

        # Map
        div(class = "plot-wrap",
          plotOutput("contour_map", height = "620px")
        ),

        # Data table
        hr(style = "margin:16px 0 10px;"),
        div(style = "font-size:0.68rem; text-transform:uppercase; letter-spacing:0.1em;
                     color:#7a7060; font-weight:700; margin-bottom:8px;",
            "Borehole Data"),
        div(style = "font-size:0.82rem;",
          tableOutput("data_table")
        )
      )
    )
  ),

  # ── TAB 2: Data View ─────────────────────────────────────────────────────
  nav_panel("Data", icon = icon("table"),
    div(style = "padding:20px;",
      h5("Groundwater Elevation Data",
         style = "font-weight:700; margin-bottom:12px;"),
      div(style = "font-size:0.85rem;",
        tableOutput("full_table")
      )
    )
  )
)

# ═══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive: interpolated grid ─────────────────────────────────────────
  r_grid <- reactive({
    idw_grid(
      pts_x = gw_data$Easting,
      pts_y = gw_data$Northing,
      pts_z = gw_data$GW_Elev_mamsl,
      nx    = input$grid_res,
      ny    = input$grid_res,
      power = input$idw_power,
      xmin  = XMIN, xmax = XMAX,
      ymin  = YMIN, ymax = YMAX
    )
  })

  # ── Reactive: background image (greyscale-optionally converted) ─────────
  r_bg <- reactive({
    req(input$bg_image)
    ext <- tolower(tools::file_ext(input$bg_image$name))
    img <- tryCatch({
      if (ext == "png") {
        m <- png::readPNG(input$bg_image$datapath)
      } else {
        m <- jpeg::readJPEG(input$bg_image$datapath)
      }
      # Ensure RGB (drop alpha if present, convert grey → RGB)
      if (length(dim(m)) == 2) {
        m <- array(rep(m, 3), dim = c(dim(m), 3))
      } else if (dim(m)[3] == 4) {
        m <- m[, , 1:3]
      }
      if (isTRUE(input$bg_grayscale)) {
        lum <- 0.2126 * m[,,1] + 0.7152 * m[,,2] + 0.0722 * m[,,3]
        m   <- array(rep(lum, 3), dim = dim(m))
      }
      m
    }, error = function(e) NULL)
    img
  })

  # ── Reactive: build ggplot ───────────────────────────────────────────────
  r_plot <- reactive({
    gd      <- r_grid()
    pal     <- PALETTES[[input$palette]]
    n_lev   <- input$n_levels
    zbreaks <- pretty(range(gd$z, na.rm = TRUE), n = n_lev)

    # Site boundary (closed ring) — correct corner order: SW→SE→NE→NW→SW
    sb <- site_bnds[c(1, 2, 3, 4, 1), ]

    p <- ggplot()

    # 1. Background image ──────────────────────────────────────────────────
    img <- r_bg()
    if (!is.null(img)) {
      p <- p +
        annotation_raster(img,
                          xmin = XMIN, xmax = XMAX,
                          ymin = YMIN, ymax = YMAX,
                          interpolate = TRUE) +
        # White veil to control opacity (opacity = 1 - bg_alpha)
        annotate("rect",
                 xmin = XMIN, xmax = XMAX,
                 ymin = YMIN, ymax = YMAX,
                 fill = "white", alpha = 1 - input$bg_alpha)
    }

    # 2. Filled contour bands ─────────────────────────────────────────────
    if (input$show_fill) {
      p <- p +
        geom_raster(data = gd, aes(x = x, y = y, fill = z),
                    interpolate = TRUE) +
        scale_fill_gradientn(
          colours = colorRampPalette(pal)(256),
          name    = gsub("\\\\n", "\n", input$legend_title),
          breaks  = zbreaks,
          labels  = sprintf("%.2f", zbreaks),
          guide   = guide_colorbar(
            barwidth  = unit(0.5, "cm"),
            barheight = unit(5,   "cm"),
            ticks     = TRUE,
            frame.colour = "grey40"
          )
        )
    }

    # 3. Contour lines ────────────────────────────────────────────────────
    if (input$show_lines) {
      p <- p +
        geom_contour(data = gd, aes(x = x, y = y, z = z),
                     breaks    = zbreaks,
                     colour    = input$line_col,
                     linewidth = input$line_width)
    }

    # 4. Contour elevation labels ─────────────────────────────────────────
    if (input$show_labels && input$show_lines) {
      lbl_df <- get_label_positions(gd, zbreaks)
      if (nrow(lbl_df) > 0) {
        p <- p +
          geom_label(data = lbl_df, aes(x = x, y = y, label = label),
                     size        = input$label_size,
                     colour      = input$label_col,
                     fill        = alpha("white", 0.75),
                     label.size  = 0.15,
                     label.r     = unit(2, "pt"),
                     fontface    = "bold",
                     check_overlap = TRUE)
      }
    }

    # 5. Groundwater flow arrows ──────────────────────────────────────────
    if (input$show_arrows) {
      nx_r <- input$grid_res; ny_r <- input$grid_res
      arr_df <- flow_arrows(gd, nx_r, ny_r, input$arrow_density)
      if (nrow(arr_df) > 0) {
        p <- p +
          geom_segment(data = arr_df,
                       aes(x = x, y = y, xend = xend, yend = yend),
                       arrow     = arrow(length = unit(4, "pt"),
                                         type = "closed"),
                       colour    = "#1a3a5e",
                       linewidth = 0.4,
                       alpha     = 0.7)
      }
    }

    # 6. Site boundary ────────────────────────────────────────────────────
    if (input$show_boundary) {
      p <- p +
        geom_path(data = sb, aes(x = Easting, y = Northing),
                  colour    = "#8b3a1e",
                  linewidth = 0.9,
                  linetype  = "dashed")
    }

    # 7. Borehole markers ─────────────────────────────────────────────────
    if (input$show_boreholes) {
      p <- p +
        geom_point(data = gw_data,
                   aes(x = Easting, y = Northing),
                   shape  = 21,
                   fill   = "white",
                   colour = "#1a1714",
                   size   = input$bh_size,
                   stroke = 0.7)

      if (input$show_bh_labels) {
        p <- p +
          geom_text_repel(
            data          = gw_data,
            aes(x = Easting, y = Northing, label = Borehole),
            size          = input$bh_lbl_size,
            colour        = "#1a1714",
            fontface      = "bold",
            segment.size  = 0.3,
            segment.colour = "#555555",
            box.padding   = 0.35,
            point.padding = 0.2,
            min.segment.length = 0.15,
            max.overlaps  = 30,
            bg.color      = alpha("white", 0.6),
            bg.r          = 0.1
          )
      }
    }

    # 8. Labels, coord, theme ─────────────────────────────────────────────
    p <- p +
      labs(
        title    = input$map_title,
        subtitle = input$map_subtitle,
        x        = "Easting (m)",
        y        = "Northing (m)"
      ) +
      theme_map(base_size = input$base_size)

    if (input$coord_fixed) p <- p + coord_fixed()

    p
  })

  # ── Outputs ──────────────────────────────────────────────────────────────

  output$contour_map <- renderPlot({
    r_plot()
  }, res = 110)

  output$stat_max   <- renderText(sprintf("%.3f", max(gw_data$GW_Elev_mamsl)))
  output$stat_min   <- renderText(sprintf("%.3f", min(gw_data$GW_Elev_mamsl)))
  output$stat_range <- renderText(sprintf("%.3f",
                                  max(gw_data$GW_Elev_mamsl) -
                                  min(gw_data$GW_Elev_mamsl)))
  output$stat_n     <- renderText(as.character(nrow(gw_data)))

  output$data_table <- renderTable({
    gw_data[order(gw_data$GW_Elev_mamsl, decreasing = TRUE),
            c("Borehole", "Easting", "Northing",
              "BH_Elev_mamsl", "WL_mbgl", "GW_Elev_mamsl")]
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     digits = 3, align = "c",
     colnames = TRUE)

  output$full_table <- renderTable({
    gw_data[, c("Borehole", "Easting", "Northing",
                "BH_Elev_mamsl", "WL_mbgl", "GW_Elev_mamsl")]
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     digits = 3, align = "c")

  # ── Export ───────────────────────────────────────────────────────────────

  output$dl_map <- downloadHandler(
    filename = function() {
      paste0("groundwater_contour_map_", format(Sys.Date(), "%Y%m%d"),
             ".", input$exp_fmt)
    },
    content = function(file) {
      p <- r_plot()
      preset <- EXPORT_PRESETS[[input$export_preset]]
      w   <- if (input$export_preset == "Custom") input$exp_w   else preset$w
      h   <- if (input$export_preset == "Custom") input$exp_h   else preset$h
      dpi <- if (input$export_preset == "Custom") input$exp_dpi else preset$dpi
      switch(input$exp_fmt,
        "png" = ggsave(file, p, width = w, height = h, dpi = dpi, device = "png"),
        "pdf" = ggsave(file, p, width = w, height = h, dpi = dpi, device = "pdf"),
        "svg" = ggsave(file, p, width = w, height = h,
                       device = svglite::svglite, fix_text_size = FALSE)
      )
    }
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
