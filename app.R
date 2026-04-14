# ═══════════════════════════════════════════════════════════════════════════════
#  Academic Graph Studio — Premium R Shiny Application
#  Publication-quality scientific visualisation (Nature / Science / IEEE ready)
# ═══════════════════════════════════════════════════════════════════════════════

# ── Dependencies ──────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(svglite)
library(plotly)
library(colourpicker)
library(scales)
library(splines)
library(ggrepel)
library(gstat)
library(sp)
library(png)

# ═══════════════════════════════════════════════════════════════════════════════
#  CONSTANTS & PALETTES
# ═══════════════════════════════════════════════════════════════════════════════

# ── Journal colour palettes ───────────────────────────────────────────────────
PALETTES <- list(
  "Nature" = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488",
               "#F39B7F", "#8491B4", "#91D1C2", "#DC0000",
               "#7E6148", "#B09C85"),
  "Science (AAAS)" = c("#3B4992", "#EE0000", "#008B45", "#631879",
                        "#008280", "#BB0021", "#5F559B", "#A20056",
                        "#808180", "#1B1919"),
  "Lancet" = c("#00468B", "#ED0000", "#42B540", "#0099B4",
               "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6",
               "#1B1919", "#E6A800"),
  "NEJM" = c("#BC3C29", "#0072B5", "#E18727", "#20854E",
             "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97",
             "#8C564B", "#BCBD22"),
  "IEEE" = c("#0072BD", "#D95319", "#EDB120", "#7E2F8E",
             "#77AC30", "#4DBEEE", "#A2142F", "#5E5E5E",
             "#2E8B57", "#FF6347"),
  "Classic Academic" = c("#8b3a1e", "#2a4f6e", "#2e6b45", "#7a5318",
                         "#5b2d8e", "#1a6b6b", "#8b1a4a", "#3d5a1e",
                         "#1e3a6e", "#8b6b00"),
  "Grayscale" = c("#000000", "#404040", "#808080", "#B0B0B0",
                  "#2A2A2A", "#5A5A5A", "#9A9A9A", "#C8C8C8",
                  "#1A1A1A", "#707070"),
  "Colorblind Safe" = c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                        "#F0E442", "#56B4E9", "#E69F00", "#000000",
                        "#999999", "#882255")
)

# ── Marker shapes (academic gold standard) ────────────────────────────────────
# Shapes 21-25 are fill+border shapes: THE standard for publication-quality
# plots. They allow color fill with a distinct black border for maximum clarity.
# Shapes 0-14 are outline-only; 15-20 are solid without border control.
MARKER_SHAPES <- c(
  "Circle"       = 21L,  # ● filled circle with border
  "Square"       = 22L,  # ■ filled square with border
  "Triangle"     = 24L,  # ▲ filled triangle with border
  "Diamond"      = 23L,  # ◆ filled diamond with border
  "Inv Triangle" = 25L,  # ▼ filled inverted triangle with border
  "Cross"        = 4L,   # ✕ open cross
  "Plus"         = 3L,   # + plus
  "Asterisk"     = 8L,   # * asterisk
  "Circle Open"  = 1L,   # ○ open circle
  "Square Open"  = 0L,   # □ open square
  "Triangle Open" = 2L,  # △ open triangle
  "Diamond Open" = 5L    # ◇ open diamond
)

# Shapes 21-25 use fill aesthetic (color = border, fill = interior)
FILLED_SHAPES <- c(21L, 22L, 23L, 24L, 25L)

# ── Line type options ─────────────────────────────────────────────────────────
LINE_TYPES <- c(
  "Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted",
  "Dot-Dash" = "dotdash", "Long Dash" = "longdash", "Two Dash" = "twodash"
)

# ── Journal export presets ────────────────────────────────────────────────────
JOURNAL_PRESETS <- list(
  "Custom"  = list(w = 10,    h = 6,    dpi = 300,  fmt = "svg"),
  "Nature (single col)"   = list(w = 3.5,  h = 2.625, dpi = 300, fmt = "pdf"),
  "Nature (double col)"   = list(w = 7.08, h = 4.5,   dpi = 300, fmt = "pdf"),
  "Science (single col)"  = list(w = 3.5,  h = 2.5,   dpi = 300, fmt = "pdf"),
  "Science (double col)"  = list(w = 7.25, h = 5,     dpi = 300, fmt = "pdf"),
  "IEEE (single col)"     = list(w = 3.5,  h = 2.625, dpi = 600, fmt = "tiff"),
  "IEEE (double col)"     = list(w = 7.16, h = 4.5,   dpi = 600, fmt = "tiff"),
  "Elsevier (single col)" = list(w = 3.54, h = 2.65,  dpi = 300, fmt = "pdf"),
  "Elsevier (full page)"  = list(w = 7.48, h = 5.5,   dpi = 300, fmt = "pdf"),
  "SPE"                   = list(w = 6.75, h = 4.5,   dpi = 300, fmt = "pdf"),
  "PowerPoint (16:9)"     = list(w = 13.3, h = 7.5,   dpi = 150, fmt = "png"),
  "Poster (A0)"           = list(w = 16,   h = 10,    dpi = 300, fmt = "png")
)

# ═══════════════════════════════════════════════════════════════════════════════
#  CONTOUR MAP — GLOBAL DATA & CONFIG
# ═══════════════════════════════════════════════════════════════════════════════

# ── Resolve app directory robustly (works both interactively and via Rscript) ─
.app_dir <- tryCatch(
  normalizePath(getSrcDirectory(function() {})),
  error = function(e) getwd()
)

# ── Groundwater and NAPL data ─────────────────────────────────────────────────
GW_DATA <- tryCatch(
  read.csv(file.path(.app_dir, "groundwater_elevations.csv"),
           stringsAsFactors = FALSE, check.names = FALSE),
  error = function(e) NULL
)

NAPL_DATA <- tryCatch(
  read.csv(file.path(.app_dir, "napl_locations_summary.csv"),
           stringsAsFactors = FALSE, check.names = FALSE),
  error = function(e) NULL
)

# ── Background image extent in real-world coords (Easting / Northing, metres) ─
# Derived from least-squares affine georeferencing of the borehole location plan
# PDF using 18 ground control points (borehole circles).  RMSE < 1 mm.
# Transform:  E =  0.2642162 * px_pdf − 18.31373
#             N = −0.2642147 * py_pdf + 185.18993
# Full PDF page (612 × 792 pts at 72 dpi) maps to the extents below.
IMG_EXTENT <- list(
  xmin = -18.314,   # E at px=0
  xmax =  143.389,  # E at px=612
  ymin =  -24.066,  # N at py=792 (bottom of page)
  ymax =  185.190   # N at py=0   (top of page)
)

# ── Site boundary polygon (Easting / Northing) ───────────────────────────────
# Extracted from DWG drawing via georeferenced PDF path tracing (17-vertex polygon)
SITE_BOUNDARY <- data.frame(
  E = c(71.298, 65.304, 91.356, 86.745, 63.229, 64.612, 47.551, 46.860,
        39.252, 39.713, 36.254, 37.177, 40.865, 42.249, 35.563, 35.563,
        43.271, 71.298),
  N = c(145.114,  76.180,  74.105,  23.614,  25.690,  46.208,  47.822,
         43.673,  44.595,  48.975,  49.206,  56.583,  56.122,  73.183,
         73.644,  75.488, 146.959, 145.114)
)

# ── Background PNG (288 DPI raster of borehole location plan PDF) ─────────────
BOREHOLE_PNG <- tryCatch(
  png::readPNG(file.path(.app_dir, "borehole_plan.png")),
  error = function(e) NULL
)

# ── Contour colour palettes ───────────────────────────────────────────────────
CONTOUR_PALETTES <- list(
  "Groundwater (Blues)"  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
  "Blue-Red (Diverging)"  = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8",
                                   "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  "Blues (Sequential)"   = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
                               "#4292c6", "#2171b5", "#08519c", "#08306b"),
  "Viridis"              = c("#440154", "#482878", "#3e4989", "#31688e", "#26828e",
                               "#1f9e89", "#35b779", "#6ece58", "#b5de2b", "#fde725"),
  "Terrain"              = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
  "Classic Academic"     = c("#2a4f6e", "#1a6b6b", "#2e6b45", "#7a5318", "#8b3a1e")
)

# ── Flow-arrow helper: estimate downhill direction at each borehole ───────────
compute_flow_arrows <- function(gw, arrow_len = 4, search_r = 40) {
  n   <- nrow(gw)
  res <- data.frame(E = gw$Easting, N = gw$Northing, dE = 0, dN = 0)
  gw_elev <- gw[["GW_Elevation_mamsl"]]
  for (i in seq_len(n)) {
    dx <- gw$Easting  - gw$Easting[i]
    dy <- gw$Northing - gw$Northing[i]
    d2 <- dx^2 + dy^2
    ok <- d2 > 0.01 & d2 <= search_r^2
    if (sum(ok) < 2L) next
    dz <- gw_elev - gw_elev[i]
    w  <- 1 / pmax(d2[ok], 0.1)
    X  <- cbind(dx[ok], dy[ok])
    XtW <- t(X * w)
    g   <- tryCatch(
      solve(XtW %*% X, XtW %*% dz[ok]),
      error = function(e) c(0, 0)
    )
    len <- sqrt(g[1]^2 + g[2]^2)
    if (len > 1e-8) {
      res$dE[i] <- -g[1] / len * arrow_len
      res$dN[i] <- -g[2] / len * arrow_len
    }
  }
  res
}

# ── Water cut mapping for production years (C01–C10) ─────────────────────────
WATER_CUTS <- c(0, 5, 25, 50, 65, 75, 85, 92, 95, 96.2)
CASE_LABELS <- paste0("C", sprintf("%02d", 1:10))
YEAR_LABELS <- paste("Year", 1:10)

# ── Flow regime definitions ───────────────────────────────────────────────────
FLOW_REGIMES <- list(
  `1` = list(label = "Stratified", fill = "#2a4f6e"),
  `2` = list(label = "Wavy",       fill = "#2e6b45"),
  `3` = list(label = "Slug",       fill = "#8b3a1e"),
  `4` = list(label = "Annular",    fill = "#7a5318"),
  `5` = list(label = "Bubble",     fill = "#5b2d8e"),
  `6` = list(label = "Dispersed",  fill = "#1a6b6b")
)


# ═══════════════════════════════════════════════════════════════════════════════
#  THEME ENGINE
# ═══════════════════════════════════════════════════════════════════════════════

theme_academic <- function(base_size = 14, base_family = "sans",
                           grid = "none", border = TRUE,
                           ticks_inward = TRUE) {
  # Inward ticks: negative length draws them inside the plot area
  tick_len <- if (ticks_inward) unit(-4, "pt") else unit(4, "pt")

  t <- theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text             = element_text(color = "#1a1714"),
      plot.title       = element_text(size = rel(1.25), face = "bold",
                                      hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle    = element_text(size = rel(0.85), face = "italic",
                                      hjust = 0.5, color = "#666666",
                                      margin = margin(b = 8)),
      axis.title       = element_text(size = rel(1.0), face = "plain"),
      axis.title.x     = element_text(margin = margin(t = 10)),
      axis.title.y     = element_text(margin = margin(r = 10), angle = 90),
      axis.text        = element_text(size = rel(0.85), color = "#333333"),
      axis.text.x      = element_text(margin = if (ticks_inward) margin(t = 8) else margin(t = 4)),
      axis.text.y      = element_text(margin = if (ticks_inward) margin(r = 8) else margin(r = 4)),
      axis.line        = element_line(color = "#1a1a1a", linewidth = 0.5),
      axis.ticks       = element_line(color = "#1a1a1a", linewidth = 0.35),
      axis.ticks.length = tick_len,
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = alpha("white", 0.95),
                                       color = "#cccccc", linewidth = 0.3),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.key.size  = unit(1.1, "lines"),
      legend.text      = element_text(size = rel(0.78)),
      legend.title     = element_blank(),
      legend.margin    = margin(4, 6, 4, 6),
      plot.margin      = margin(12, 12, 12, 12),
      strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"),
      strip.text       = element_text(size = rel(0.9), face = "bold")
    )

  if (border) {
    t <- t + theme(
      panel.border = element_rect(fill = NA, color = "#1a1a1a", linewidth = 0.7),
      axis.line    = element_blank()
    )
  }

  if (grid == "major") {
    t <- t + theme(
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  } else if (grid == "both") {
    t <- t + theme(
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#f2f2f2", linewidth = 0.15)
    )
  } else if (grid == "x") {
    t <- t + theme(
      panel.grid.major.x = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  } else if (grid == "y") {
    t <- t + theme(
      panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  }

  t
}

# ── Parse _{sub} and ^{sup} to plotmath expressions ──────────────────────────
parse_label <- function(s) {
  if (is.null(s) || nchar(trimws(s)) == 0) return("")
  # Replace _{...} with [...]  and ^{...} with [...] for plotmath
  has_markup <- grepl("(_\\{|\\^\\{)", s)
  if (!has_markup) return(s)
  expr_str <- s
  expr_str <- gsub("_\\{([^}]*)\\}", "[\\1]", expr_str, perl = TRUE)
  expr_str <- gsub("\\^\\{([^}]*)\\}", "^{\\1}", expr_str, perl = TRUE)
  # Wrap plain text segments in quotes for plotmath
  expr_str <- gsub("([A-Za-z][A-Za-z0-9 .,]*)", "\"\\1\"", expr_str, perl = TRUE)
  # Clean double-quoted numbers
  expr_str <- gsub("\"([0-9.]+)\"", "\\1", expr_str, perl = TRUE)
  tryCatch(parse(text = expr_str)[[1]], error = function(e) s)
}

# ── Legend position helpers ───────────────────────────────────────────────────
leg_pos <- function(pos) {
  switch(pos,
    "Top Left" = c(0.02, 0.98), "Top Center" = c(0.50, 0.98),
    "Top Right" = c(0.98, 0.98), "Mid Left" = c(0.02, 0.50),
    "Mid Right" = c(0.98, 0.50), "Bottom Left" = c(0.02, 0.02),
    "Bottom Center" = c(0.50, 0.02), "Bottom Right" = c(0.98, 0.02),
    "Hidden" = "none", c(0.98, 0.02))
}
leg_just <- function(pos) {
  switch(pos,
    "Top Left" = c(0,1), "Top Center" = c(0.5,1), "Top Right" = c(1,1),
    "Mid Left" = c(0,0.5), "Mid Right" = c(1,0.5),
    "Bottom Left" = c(0,0), "Bottom Center" = c(0.5,0), "Bottom Right" = c(1,0),
    c(1,0))
}


# ═══════════════════════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════════════════════

ui <- page_navbar(
  title = tags$span(
    tags$span("Academic Graph Studio",
              style = "font-weight:700;letter-spacing:-0.02em;"),
    tags$span(" \u2014 Publication-Quality Visualisation",
              style = "font-weight:300;font-size:0.75em;opacity:0.7;")
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#fdfcfa", fg = "#1a1714",
    primary = "#2a4f6e", secondary = "#8b3a1e",
    success = "#2e6b45", info = "#1a6b6b",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    "navbar-bg" = "#1a1714",
    "border-radius" = "4px"
  ),
  header = tags$head(tags$style(HTML("
    .card { border-color: #d4cec4 !important; }
    .card-header { background: #f7f4ef !important; border-bottom: 1px solid #d4cec4 !important;
                   font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.1em;
                   font-weight: 700; color: #7a7060; padding: 10px 16px; }
    .form-label { font-size: 0.68rem !important; text-transform: uppercase;
                  letter-spacing: 0.08em; color: #7a7060 !important; font-weight: 600 !important; }
    .form-control, .form-select { font-size: 0.82rem; border-color: #c8bfb0; }
    .form-control:focus, .form-select:focus { border-color: #2a4f6e; box-shadow: 0 0 0 2px rgba(42,79,110,0.15); }
    .btn-academic { background: #1a1714; color: #f7f4ef; font-size: 0.72rem; letter-spacing: 0.07em;
                    text-transform: uppercase; border: none; padding: 8px 16px; border-radius: 3px; }
    .btn-academic:hover { background: #8b3a1e; color: #f7f4ef; }
    .btn-export { background: #2a4f6e; color: #fff; font-size: 0.72rem; letter-spacing: 0.07em;
                  text-transform: uppercase; border: none; padding: 8px 20px; }
    .btn-export:hover { background: #1a3a5e; color: #fff; }
    .btn-ghost { background: transparent; border: 1px solid #c8bfb0; color: #7a7060;
                 font-size: 0.68rem; letter-spacing: 0.07em; text-transform: uppercase; }
    .btn-ghost:hover { border-color: #8b3a1e; color: #8b3a1e; }
    .series-chip { display: inline-flex; align-items: center; gap: 6px; padding: 5px 10px;
                   margin: 2px; border-radius: 3px; font-size: 0.72rem; border: 1px solid #d4cec4;
                   background: #f7f4ef; cursor: pointer; transition: all 0.15s; }
    .series-chip:hover { border-color: #2a4f6e; }
    .series-chip.hidden { opacity: 0.35; }
    .series-swatch { width: 20px; height: 3px; border-radius: 2px; }
    .plot-container { background: white; border: 1px solid #d4cec4; border-radius: 4px;
                      padding: 8px; min-height: 500px; }
    .stat-card { background: #f7f4ef; border: 1px solid #d4cec4; border-radius: 4px;
                 padding: 12px 16px; text-align: center; }
    .stat-value { font-size: 1.5rem; font-weight: 700; color: #1a1714; }
    .stat-label { font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.1em;
                  color: #7a7060; margin-top: 2px; }
    .accordion-button { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.08em;
                        font-weight: 600; color: #7a7060; padding: 10px 16px; }
    .accordion-button:not(.collapsed) { background: #f7f4ef; color: #1a1714; }
    .ref-item { background: #f7f4ef; border: 1px solid #d4cec4; border-radius: 3px;
                padding: 6px 10px; margin-top: 4px; font-size: 0.75rem;
                display: flex; align-items: center; gap: 8px; }
    .color-swatch { width: 14px; height: 14px; border-radius: 2px; border: 1px solid #d4cec4; display: inline-block; }
    .tab-content { padding-top: 0 !important; }
    .plotly .modebar { opacity: 0.4 !important; }
    .plotly .modebar:hover { opacity: 1 !important; }
  "))),

  # ═══════════════════════════════════════════════════════
  #  TAB 1: MAIN PLOTTER
  # ═══════════════════════════════════════════════════════
  nav_panel("Graph Studio", icon = icon("chart-line"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 340,
        id = "sidebar",

        # ── DATA IMPORT ────────────────────────────────────
        accordion(
          id = "acc_sidebar",
          open = c("Import Data", "Series"),

          accordion_panel("Import Data", icon = icon("file-import"),
            fileInput("file_input", NULL,
                      accept = c(".xlsx", ".xls", ".csv"),
                      placeholder = "Drop .xlsx or .csv here"),
            uiOutput("sheet_selector"),
            uiOutput("column_selectors"),
            actionButton("load_btn", "Load into chart",
                         class = "btn-academic w-100", icon = icon("chart-line"))
          ),

          # ── SERIES MANAGEMENT ──────────────────────────────
          accordion_panel("Series", icon = icon("layer-group"),
            uiOutput("series_panel"),
            div(class = "d-flex gap-2 mt-2",
              actionButton("show_all", "Show All", class = "btn-ghost flex-fill"),
              actionButton("hide_all", "Hide All", class = "btn-ghost flex-fill")
            )
          ),

          # ── LABELS & TITLE ─────────────────────────────────
          accordion_panel("Labels & Title", icon = icon("font"),
            textInput("chart_title", "Chart title", value = "Pipeline Profile"),
            textInput("chart_subtitle", "Subtitle (optional)", value = ""),
            textInput("xlabel", "X axis label", value = "Pipeline Length [m]"),
            textInput("ylabel", "Y axis label", value = "Pressure (bara)"),
            helpText("Tip: _{...} = subscript, ^{...} = superscript",
                     style = "font-size:0.62rem;color:#999;font-style:italic;"),
            numericInput("title_size", "Title font size", value = 16, min = 8, max = 32, step = 1),
            numericInput("axis_label_size", "Axis label size", value = 12, min = 8, max = 28, step = 1),
            numericInput("axis_text_size", "Tick label size", value = 10, min = 6, max = 24, step = 1)
          ),

          # ── APPEARANCE ─────────────────────────────────────
          accordion_panel("Appearance", icon = icon("palette"),
            selectInput("palette", "Colour palette",
                        choices = names(PALETTES), selected = "Classic Academic"),
            radioButtons("curve_type", "Curve fitting",
                         choices = c("Smooth (loess)" = "loess",
                                     "Natural spline" = "spline",
                                     "Straight lines" = "linear",
                                     "Step" = "step",
                                     "None (markers only)" = "none"),
                         selected = "loess"),
            conditionalPanel("input.curve_type == 'loess'",
              sliderInput("loess_span", "Smoothness", min = 0.1, max = 1.5,
                          value = 0.35, step = 0.05)
            ),
            selectInput("line_type", "Line style", choices = LINE_TYPES, selected = "solid"),
            sliderInput("line_weight", "Line weight", min = 0.3, max = 3, value = 0.9, step = 0.1),
            hr(),
            radioButtons("marker_mode", "Markers",
                         choices = c("All" = "all", "Every N" = "nth",
                                     "First & Last" = "ends", "None" = "none"),
                         selected = "all", inline = TRUE),
            conditionalPanel("input.marker_mode == 'nth'",
              sliderInput("marker_nth", "Show every Nth point",
                          min = 2, max = 100, value = 10, step = 1)
            ),
            sliderInput("marker_size", "Marker size", min = 1, max = 8, value = 2.8, step = 0.2),
            hr(),
            selectInput("grid_lines", "Grid lines",
                        choices = c("None" = "none", "Major" = "major",
                                    "Major + Minor" = "both",
                                    "X only" = "x", "Y only" = "y"),
                        selected = "none"),
            checkboxInput("show_border", "Plot border", value = TRUE),
            radioButtons("tick_dir", "Tick marks",
                         choices = c("Inward" = "in", "Outward" = "out", "Both" = "both"),
                         selected = "in", inline = TRUE)
          ),

          # ── AXIS RANGES ────────────────────────────────────
          accordion_panel("Axis Ranges", icon = icon("arrows-left-right"),
            fluidRow(
              column(6, numericInput("xmin", "X min", value = NA, step = 0.1)),
              column(6, numericInput("xmax", "X max", value = NA, step = 0.1))
            ),
            fluidRow(
              column(6, numericInput("ymin", "Y min", value = NA, step = 0.1)),
              column(6, numericInput("ymax", "Y max", value = NA, step = 0.1))
            ),
            checkboxInput("y2_enable", "Enable secondary Y axis", value = FALSE),
            conditionalPanel("input.y2_enable",
              textInput("y2label", "Y2 axis label", value = "Temperature [\u00b0C]"),
              uiOutput("y2_series_selector")
            ),
            actionButton("reset_ranges", "Reset to auto", class = "btn-ghost w-100")
          ),

          # ── REFERENCE LINES ────────────────────────────────
          accordion_panel("Reference Lines", icon = icon("grip-lines"),
            fluidRow(
              column(6, selectInput("ref_axis", "Axis",
                                    choices = c("X (vertical)" = "x", "Y (horizontal)" = "y"))),
              column(6, numericInput("ref_value", "Value", value = NA, step = 0.1))
            ),
            textInput("ref_label", "Label", value = ""),
            fluidRow(
              column(6, selectInput("ref_linetype", "Style", choices = LINE_TYPES, selected = "dashed")),
              column(6, colourInput("ref_color", "Color", value = "#8b3a1e", showColour = "both"))
            ),
            actionButton("add_ref", "Add line", class = "btn-ghost w-100", icon = icon("plus")),
            uiOutput("ref_lines_list")
          ),

          # ── ANNOTATIONS ────────────────────────────────────
          accordion_panel("Annotations", icon = icon("comment-dots"),
            fluidRow(
              column(4, numericInput("ann_x", "X", value = NA, step = 0.1)),
              column(4, numericInput("ann_y", "Y", value = NA, step = 0.1)),
              column(4, colourInput("ann_color", "Col", value = "#8b3a1e", showColour = "background"))
            ),
            textInput("ann_text", "Text", value = ""),
            selectInput("ann_arrow", "Arrow",
                        choices = c("None" = "none", "Up-Right" = "ur", "Up-Left" = "ul",
                                    "Down-Right" = "dr", "Down-Left" = "dl"),
                        selected = "ur"),
            actionButton("add_ann", "Add annotation", class = "btn-ghost w-100", icon = icon("plus")),
            uiOutput("annotations_list")
          ),

          # ── TREND EXTRACTION (for Figure 5-type plots) ────
          accordion_panel("Trend Extraction", icon = icon("crosshairs"),
            helpText("Extract one value per series (e.g. inlet pressure) and plot against a parameter (e.g. water cut).",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            checkboxInput("extraction_enable", "Enable extraction mode", value = FALSE),
            conditionalPanel("input.extraction_enable",
              selectInput("extract_method", "Extract value",
                          choices = c("Inlet (first value)" = "first",
                                      "Outlet (last value)" = "last",
                                      "Minimum" = "min",
                                      "Maximum" = "max",
                                      "Mean" = "mean")),
              textInput("extract_x_label", "X parameter label", value = "Water Cut (%)"),
              textInput("extract_y_label", "Y parameter label", value = "Inlet Pressure [bara]"),
              uiOutput("extract_x_inputs"),
              hr(),
              checkboxInput("extract_trend", "Show trend line", value = TRUE),
              conditionalPanel("input.extract_trend",
                selectInput("extract_trend_type", "Trend type",
                            choices = c("Linear" = "lm",
                                        "LOESS smooth" = "loess"),
                            selected = "lm"),
                conditionalPanel("input.extract_trend_type == 'lm'",
                  selectInput("extract_eq_pos", "Equation position",
                              choices = c("Top Left" = "tl", "Top Right" = "tr",
                                          "Bottom Left" = "bl", "Bottom Right" = "br",
                                          "Hidden" = "none"),
                              selected = "tr")
                )
              ),
              hr(),
              checkboxInput("extract_margin", "Show margin from reference", value = FALSE),
              conditionalPanel("input.extract_margin",
                numericInput("extract_margin_ref", "Reference value", value = 32, step = 0.1),
                textInput("extract_margin_ref_label", "Reference name", value = "WAT"),
                textInput("extract_margin_panel_label", "Margin panel label",
                          value = "Thermal Margin (\u00b0C)")
              ),
              actionButton("generate_extraction", "Generate extraction plot",
                           class = "btn-academic w-100", icon = icon("chart-line"))
            )
          ),

          # ── MULTI-PANEL PLOTS (Holdup / Velocity) ─────────
          accordion_panel("Multi-Panel Plots", icon = icon("grip"),
            helpText("Create multi-panel holdup or velocity profiles combining data from multiple sheets.",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            selectInput("multi_plot_type", "Plot type",
                        choices = c("Liquid Holdup Profiles" = "holdup",
                                    "Velocity & Slip Ratio" = "velocity",
                                    "Holdup \u2014 All Years (2\u00d75)" = "holdup_all",
                                    "Velocity & Slip \u2014 Years 1\u20135" = "velocity_1_5",
                                    "Velocity & Slip \u2014 Years 6\u201310" = "velocity_6_10")),
            conditionalPanel(
              "input.multi_plot_type == 'holdup' || input.multi_plot_type == 'velocity'",
              tags$p("Select 3 cases for the 3 panels:",
                     style = "font-size:0.68rem;color:#7a7060;font-weight:600;margin-bottom:4px;"),
              fluidRow(
                column(4,
                  numericInput("mp_case1", "Case #", value = 1, min = 1, max = 10, step = 1),
                  textInput("mp_label1", "Label", value = "Year 1")
                ),
                column(4,
                  numericInput("mp_case2", "Case #", value = 5, min = 1, max = 10, step = 1),
                  textInput("mp_label2", "Label", value = "Year 5")
                ),
                column(4,
                  numericInput("mp_case3", "Case #", value = 10, min = 1, max = 10, step = 1),
                  textInput("mp_label3", "Label", value = "Year 10")
                )
              )
            ),
            conditionalPanel(
              "input.multi_plot_type == 'holdup_all' || input.multi_plot_type == 'velocity_1_5' || input.multi_plot_type == 'velocity_6_10'",
              helpText("Cases and labels are auto-assigned for appendix layouts.",
                       style = "font-size:0.62rem;color:#999;font-style:italic;")
            ),
            actionButton("generate_multi", "Generate multi-panel plot",
                         class = "btn-academic w-100", icon = icon("chart-line")),
            actionButton("exit_multi", "Exit multi-panel mode",
                         class = "btn-ghost w-100 mt-2", icon = icon("arrow-left"))
          ),

          # ── LEGEND ─────────────────────────────────────────
          accordion_panel("Legend", icon = icon("list"),
            selectInput("legend_pos", "Position",
                        choices = c("Top Left", "Top Center", "Top Right",
                                    "Mid Left", "Mid Right",
                                    "Bottom Left", "Bottom Center", "Bottom Right",
                                    "Hidden"),
                        selected = "Bottom Right"),
            numericInput("legend_size", "Text size", value = 10, min = 6, max = 20, step = 1),
            numericInput("legend_cols", "Columns", value = 1, min = 1, max = 5, step = 1)
          ),

          # ── TOP AXIS ANNOTATIONS ─────────────────────────────
          accordion_panel("Top Axis Annotations", icon = icon("arrow-up-short-wide"),
            helpText("Show associated values above the plot at each X position (e.g. mass, watercut per year).",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            checkboxInput("top_ann_enable", "Enable top-axis annotations", value = FALSE),
            conditionalPanel("input.top_ann_enable",
              uiOutput("top_ann_col_selectors"),
              numericInput("top_ann_size", "Font size", value = 3, min = 1.5, max = 8, step = 0.25),
              numericInput("top_ann_angle", "Text angle", value = 0, min = 0, max = 90, step = 15),
              colourInput("top_ann_color", "Text colour", value = "#555555", showColour = "both"),
              checkboxInput("top_ann_ticks", "Show tick marks on top axis", value = TRUE),
              hr(),
              tags$p("Manual entries (if no column selected):",
                     style = "font-size:0.65rem;color:#999;margin-bottom:6px;"),
              fluidRow(
                column(4, numericInput("top_ann_x", "X", value = NA, step = 0.1)),
                column(4, textInput("top_ann_row1", "Row 1", value = "")),
                column(4, textInput("top_ann_row2", "Row 2", value = ""))
              ),
              actionButton("add_top_ann", "Add", class = "btn-ghost w-100", icon = icon("plus")),
              uiOutput("top_ann_manual_list")
            )
          )
        ) # end accordion
      ), # end sidebar

      # ── MAIN CONTENT ─────────────────────────────────────
      layout_column_wrap(
        width = 1,

        # Toolbar
        card(
          card_header(
            class = "d-flex align-items-center gap-3 flex-wrap",
            div(class = "d-flex align-items-center gap-2 flex-grow-1",
              selectInput("journal_preset", NULL,
                          choices = names(JOURNAL_PRESETS),
                          selected = "Custom", width = "180px"),
              numericInput("export_width", "W (in)", value = 10,
                           min = 2, max = 24, step = 0.25, width = "85px"),
              numericInput("export_height", "H (in)", value = 6,
                           min = 2, max = 16, step = 0.25, width = "85px"),
              numericInput("export_dpi", "DPI", value = 300,
                           min = 72, max = 1200, step = 50, width = "80px"),
              selectInput("export_format", "Format",
                          choices = c("SVG" = "svg", "PDF" = "pdf",
                                      "PNG" = "png", "TIFF" = "tiff",
                                      "EPS" = "eps"),
                          selected = "svg", width = "80px")
            ),
            div(class = "d-flex align-items-center gap-2",
              textInput("export_filename", NULL, value = "figure_1",
                        placeholder = "filename", width = "140px"),
              downloadButton("download_plot", "Export",
                             class = "btn-export", icon = icon("download"))
            )
          )
        ),

        # Interactive preview
        card(
          card_body(
            class = "plot-container p-1",
            div(
              style = "position:relative;",
              plotlyOutput("interactive_plot", height = "620px"),
              div(style = "position:absolute;top:8px;right:12px;z-index:10;",
                actionButton("refresh_plot", "", icon = icon("sync"),
                             class = "btn btn-sm btn-ghost",
                             title = "Refresh plot")
              )
            )
          )
        )
      )
    ) # end layout_sidebar
  ), # end nav_panel

  # ═══════════════════════════════════════════════════════
  #  TAB 2: STATIC PUBLICATION PREVIEW
  # ═══════════════════════════════════════════════════════
  nav_panel("Publication Preview", icon = icon("file-pdf"),
    card(
      card_header("Exact export preview (static ggplot2 — what you'll get in the file)"),
      card_body(
        class = "plot-container text-center",
        plotOutput("static_plot", height = "650px", width = "100%")
      )
    )
  ),

  # ═══════════════════════════════════════════════════════
  #  TAB 3: DATA TABLE
  # ═══════════════════════════════════════════════════════
  nav_panel("Data", icon = icon("table"),
    card(
      card_header("Imported data"),
      card_body(
        tableOutput("data_table")
      )
    )
  ),

  # ═══════════════════════════════════════════════════════
  #  TAB 4: SLUGGING CHARACTERIZATION
  # ═══════════════════════════════════════════════════════
  nav_panel("Slugging", icon = icon("water"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 320,
        id = "slug_sidebar",
        accordion(
          id = "acc_slug",
          open = c("Data Files", "Panel Selection"),

          accordion_panel("Data Files", icon = icon("file-excel"),
            fileInput("slug_file", "Slug.xlsx",
                      accept = c(".xlsx", ".xls"),
                      placeholder = "Drop Slug.xlsx here"),
            fileInput("slugtrack_file", "Slugtracking.xlsx",
                      accept = c(".xlsx", ".xls"),
                      placeholder = "Drop Slugtracking.xlsx here"),
            actionButton("load_slug_data", "Load slug data",
                         class = "btn-academic w-100", icon = icon("database"))
          ),

          accordion_panel("Panel Selection", icon = icon("layer-group"),
            helpText("Select 3 years for multi-panel time-series plots (Figs 4.10, 4.10-B, 4.11).",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            fluidRow(
              column(4,
                selectInput("slug_panel_a", "Panel A", choices = 1:10, selected = 1),
                textInput("slug_label_a", "Label", value = "Year 1 (0% WC)")
              ),
              column(4,
                selectInput("slug_panel_b", "Panel B", choices = 1:10, selected = 4),
                textInput("slug_label_b", "Label", value = "Year 4 (50% WC)")
              ),
              column(4,
                selectInput("slug_panel_c", "Panel C", choices = 1:10, selected = 10),
                textInput("slug_label_c", "Label", value = "Year 10 (96% WC)")
              )
            )
          ),

          accordion_panel("Time Window", icon = icon("clock"),
            helpText("Select the time window for time-series plots.",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            numericInput("slug_t_start", "Start time (s)", value = 0, min = 0, step = 100),
            numericInput("slug_t_window", "Window duration (s)", value = 1800, min = 60, step = 60)
          ),

          accordion_panel("Water Cut Values", icon = icon("tint"),
            helpText("Adjust water cut (%) for each production year if needed.",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            fluidRow(
              column(6, numericInput("wc_1", "Year 1", value = 0, min = 0, max = 100)),
              column(6, numericInput("wc_2", "Year 2", value = 5, min = 0, max = 100))
            ),
            fluidRow(
              column(6, numericInput("wc_3", "Year 3", value = 25, min = 0, max = 100)),
              column(6, numericInput("wc_4", "Year 4", value = 50, min = 0, max = 100))
            ),
            fluidRow(
              column(6, numericInput("wc_5", "Year 5", value = 65, min = 0, max = 100)),
              column(6, numericInput("wc_6", "Year 6", value = 75, min = 0, max = 100))
            ),
            fluidRow(
              column(6, numericInput("wc_7", "Year 7", value = 85, min = 0, max = 100)),
              column(6, numericInput("wc_8", "Year 8", value = 92, min = 0, max = 100))
            ),
            fluidRow(
              column(6, numericInput("wc_9", "Year 9", value = 95, min = 0, max = 100)),
              column(6, numericInput("wc_10", "Year 10", value = 96.2, min = 0, max = 100))
            )
          ),

          accordion_panel("Appearance", icon = icon("palette"),
            selectInput("slug_palette", "Colour palette",
                        choices = names(PALETTES), selected = "Nature"),
            numericInput("slug_title_size", "Title font size", value = 14, min = 8, max = 28),
            numericInput("slug_label_size", "Axis label size", value = 11, min = 8, max = 24),
            numericInput("slug_text_size", "Tick label size", value = 9, min = 6, max = 20),
            sliderInput("slug_line_weight", "Line weight", min = 0.3, max = 2, value = 0.6, step = 0.1),
            selectInput("slug_grid", "Grid lines",
                        choices = c("None" = "none", "Major" = "major", "Y only" = "y"),
                        selected = "none")
          ),

          accordion_panel("Export", icon = icon("download"),
            selectInput("slug_export_preset", "Journal preset",
                        choices = names(JOURNAL_PRESETS), selected = "Custom"),
            fluidRow(
              column(4, numericInput("slug_export_w", "W (in)", value = 7, min = 2, max = 16, step = 0.25)),
              column(4, numericInput("slug_export_h", "H (in)", value = 8, min = 2, max = 16, step = 0.25)),
              column(4, numericInput("slug_export_dpi", "DPI", value = 300, min = 72, max = 1200, step = 50))
            ),
            selectInput("slug_export_fmt", "Format",
                        choices = c("SVG" = "svg", "PDF" = "pdf", "PNG" = "png",
                                    "TIFF" = "tiff", "EPS" = "eps"),
                        selected = "pdf"),
            textInput("slug_export_filename", "Filename", value = "figure_slug"),
            downloadButton("download_slug_plot", "Export", class = "btn-export w-100")
          )
        )
      ),

      # ── MAIN CONTENT: Sub-tabs for each figure ──────────
      navset_card_tab(
        id = "slug_subtab",

        nav_panel("PT PIPE-1 (Fig 4.10)",
          plotOutput("slug_plot_pt1", height = "700px")
        ),

        nav_panel("PT PIPE-7 (Fig 4.10-B)",
          plotOutput("slug_plot_pt7", height = "700px")
        ),

        nav_panel("QLT PIPE-7 (Fig 4.11)",
          plotOutput("slug_plot_qlt", height = "700px")
        ),

        nav_panel("Slug Frequency (Fig 4.12)",
          plotOutput("slug_plot_freq", height = "550px")
        ),

        nav_panel("Slug Length (Fig 4.13)",
          plotOutput("slug_plot_length", height = "550px")
        ),

        nav_panel("Amplitude Summary (Fig 4.14)",
          plotOutput("slug_plot_amplitude", height = "550px")
        ),

        nav_panel("PT PIPE-1 All Years",
          plotOutput("slug_plot_pt1_all", height = "900px")
        ),

        nav_panel("PT PIPE-7 All Years",
          plotOutput("slug_plot_pt7_all", height = "900px")
        ),

        nav_panel("QLT PIPE-7 All Years",
          plotOutput("slug_plot_qlt_all", height = "900px")
        ),

        nav_panel("Slug Length All Years",
          plotOutput("slug_plot_length_all", height = "900px")
        ),

        nav_panel("Metrics Table",
          tableOutput("slug_metrics_table")
        ),

        # ── Slugtracking Trend Plots ──────────────────────
        nav_panel("ST: PT PIPE-1",
          plotOutput("st_plot_pt1", height = "700px")
        ),

        nav_panel("ST: PT PIPE-7",
          plotOutput("st_plot_pt7", height = "700px")
        ),

        nav_panel("ST: QLT PIPE-7",
          plotOutput("st_plot_qlt", height = "700px")
        ),

        nav_panel("ST: PT PIPE-1 All Years",
          plotOutput("st_plot_pt1_all", height = "900px")
        ),

        nav_panel("ST: PT PIPE-7 All Years",
          plotOutput("st_plot_pt7_all", height = "900px")
        ),

        nav_panel("ST: QLT PIPE-7 All Years",
          plotOutput("st_plot_qlt_all", height = "900px")
        ),

        nav_panel("ST: Amplitude Summary",
          plotOutput("st_plot_amplitude", height = "550px")
        ),

        nav_panel("ST: Metrics Table",
          tableOutput("st_metrics_table")
        )
      )
    )
  ),

  # ═══════════════════════════════════════════════════════
  #  TAB 5: CONTOUR MAPS
  # ═══════════════════════════════════════════════════════
  nav_panel("Contour Maps", icon = icon("map"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 310,
        id    = "cmap_sidebar",

        accordion(
          id   = "acc_cmap",
          open = c("Interpolation", "Layers"),

          # ── INTERPOLATION ───────────────────────────────
          accordion_panel("Interpolation", icon = icon("chart-area"),
            selectInput("cmap_method", "Method",
              choices  = c("IDW (Inverse Distance Weighting)" = "idw",
                           "Kriging (Ordinary)"               = "kriging"),
              selected = "idw"),
            conditionalPanel("input.cmap_method === 'idw'",
              sliderInput("cmap_idw_power", "IDW power (p)",
                          min = 1, max = 4, value = 2, step = 0.5)
            ),
            sliderInput("cmap_grid_res", "Grid resolution (m)",
                        min = 0.5, max = 5, value = 1, step = 0.5),
            sliderInput("cmap_nlevels", "Contour levels",
                        min = 5, max = 25, value = 12, step = 1)
          ),

          # ── LAYERS ─────────────────────────────────────
          accordion_panel("Layers", icon = icon("layer-group"),
            checkboxInput("cmap_show_bg",       "Background site plan",  value = TRUE),
            conditionalPanel("input.cmap_show_bg",
              sliderInput("cmap_bg_alpha", "Background opacity",
                          min = 0.1, max = 1, value = 0.55, step = 0.05)
            ),
            checkboxInput("cmap_show_filled",   "Filled contours",       value = TRUE),
            checkboxInput("cmap_show_lines",    "Contour lines",         value = TRUE),
            checkboxInput("cmap_show_boundary", "Site boundary",         value = TRUE),
            checkboxInput("cmap_show_boreholes","Borehole markers",      value = TRUE),
            checkboxInput("cmap_show_labels",   "Borehole labels",       value = TRUE),
            checkboxInput("cmap_show_napl",     "NAPL locations",        value = TRUE),
            checkboxInput("cmap_show_flow",     "Flow direction arrows", value = FALSE)
          ),

          # ── APPEARANCE ─────────────────────────────────
          accordion_panel("Appearance", icon = icon("palette"),
            textInput("cmap_title",    "Map title",
                      value = "Groundwater Elevation (mamsl)"),
            textInput("cmap_subtitle", "Subtitle (optional)", value = ""),
            selectInput("cmap_palette", "Colour palette",
                        choices  = names(CONTOUR_PALETTES),
                        selected = "Groundwater (Blues)"),
            selectInput("cmap_line_col", "Contour line colour",
                        choices  = c("Black"   = "#000000", "Dark Blue" = "#1a3a5e",
                                     "Navy"    = "#2a4f6e", "White"     = "#ffffff",
                                     "Grey"    = "#666666"),
                        selected = "#1a3a5e"),
            sliderInput("cmap_line_wt", "Contour line weight",
                        min = 0.1, max = 1.5, value = 0.35, step = 0.05),
            sliderInput("cmap_fill_alpha", "Fill opacity",
                        min = 0.2, max = 1, value = 0.80, step = 0.05),
            sliderInput("cmap_bh_size",  "Borehole marker size",
                        min = 1, max = 6, value = 2.5, step = 0.25),
            sliderInput("cmap_lbl_size", "Label font size",
                        min = 1.5, max = 5, value = 2.5, step = 0.25),
            numericInput("cmap_title_size", "Title font size",
                         value = 14, min = 8, max = 28, step = 1),
            numericInput("cmap_axis_size", "Axis label size",
                         value = 11, min = 6, max = 20, step = 1)
          ),

          # ── EXPORT ─────────────────────────────────────
          accordion_panel("Export", icon = icon("download"),
            selectInput("cmap_preset", "Journal preset",
                        choices  = names(JOURNAL_PRESETS),
                        selected = "Custom"),
            fluidRow(
              column(6, numericInput("cmap_width",  "Width (in)",
                                     value = 10, min = 1, max = 30, step = 0.5)),
              column(6, numericInput("cmap_height", "Height (in)",
                                     value = 8,  min = 1, max = 30, step = 0.5))
            ),
            numericInput("cmap_dpi", "DPI", value = 300, min = 72, max = 1200, step = 50),
            selectInput("cmap_format", "Format",
                        choices  = c("PDF" = "pdf", "PNG" = "png",
                                     "SVG" = "svg", "TIFF" = "tiff"),
                        selected = "pdf"),
            textInput("cmap_filename", "Filename", value = "groundwater_contour"),
            downloadButton("cmap_download", "Export Map",
                           class = "btn-export w-100 mt-2")
          )
        )
      ),  # ── end sidebar ──

      # ── MAIN PANEL ───────────────────────────────────────
      card(
        full_screen = TRUE,
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
            span("Groundwater Elevation Contour Map"),
            div(class = "d-flex align-items-center gap-3",
              actionButton("cmap_refresh", "Refresh",
                           class = "btn-ghost btn-sm", icon = icon("rotate")),
              radioButtons("cmap_view", NULL,
                           choices  = c("Static" = "static", "Interactive" = "interactive"),
                           selected = "static", inline = TRUE)
            )
          )
        ),
        card_body(
          class = "plot-container",
          conditionalPanel("input.cmap_view === 'static'",
            plotOutput("cmap_static", height = "650px", width = "100%")
          ),
          conditionalPanel("input.cmap_view === 'interactive'",
            plotlyOutput("cmap_interactive", height = "650px")
          )
        )
      )
    )
  )

)


# ═══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive state ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    raw_data       = NULL,
    sheet_names    = NULL,
    series_data    = list(),
    ref_lines      = list(),
    annotations    = list(),
    top_anns       = list(),   # manual top-axis annotations
    is_flow_regime = FALSE,
    plot_counter   = 0,        # force refresh
    extraction_mode = FALSE,
    extraction_data = NULL,
    multi_panel_mode = FALSE,
    multi_panel_data = NULL,
    multi_panel_type = NULL,
    series_panel_ver = 0L      # increment to re-render series panel (structural changes only)
  )

  # ── Journal preset ─────────────────────────────────────────────────────────
  observeEvent(input$journal_preset, {
    preset <- JOURNAL_PRESETS[[input$journal_preset]]
    if (!is.null(preset) && input$journal_preset != "Custom") {
      updateNumericInput(session, "export_width",  value = preset$w)
      updateNumericInput(session, "export_height", value = preset$h)
      updateNumericInput(session, "export_dpi",    value = preset$dpi)
      updateSelectInput(session, "export_format",  selected = preset$fmt)
    }
  })

  # ══════════════════════════════════════════════════════
  #  FILE UPLOAD
  # ══════════════════════════════════════════════════════
  observeEvent(input$file_input, {
    req(input$file_input)
    f <- input$file_input
    ext <- tolower(tools::file_ext(f$name))
    tryCatch({
      if (ext == "csv") {
        df <- read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        rv$raw_data <- list(Sheet1 = df)
        rv$sheet_names <- "Sheet1"
      } else {
        sheets <- excel_sheets(f$datapath)
        rv$sheet_names <- sheets
        rv$raw_data <- setNames(
          lapply(sheets, function(s) {
            as.data.frame(read_excel(f$datapath, sheet = s, col_names = TRUE,
                                     .name_repair = "minimal"))
          }),
          sheets
        )
      }
      showNotification(
        paste0("\u2713 Loaded \"", f$name, "\" \u2014 ", length(rv$sheet_names), " sheet(s)"),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(paste0("\u2717 ", e$message), type = "error")
    })
  })

  # ── Sheet selector ──────────────────────────────────────────────────────────
  output$sheet_selector <- renderUI({
    req(rv$sheet_names)
    if (length(rv$sheet_names) > 1)
      selectInput("sheet_sel", "Sheet", choices = rv$sheet_names)
  })

  current_sheet <- reactive({
    req(rv$raw_data)
    nm <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    rv$raw_data[[nm]]
  })

  # ── Column selectors ───────────────────────────────────────────────────────
  output$column_selectors <- renderUI({
    df <- current_sheet()
    req(df)
    cols <- colnames(df)
    x_guess <- grep("length|distance|depth|x$", cols, ignore.case = TRUE, value = TRUE)
    x_default <- if (length(x_guess) > 0) x_guess[1] else cols[1]
    num_cols <- cols[vapply(df, function(c) {
      is.numeric(c) || all(!is.na(suppressWarnings(as.numeric(na.omit(c)))))
    }, logical(1))]
    y_default <- setdiff(num_cols, x_default)
    tagList(
      selectInput("col_x", "X column", choices = cols, selected = x_default),
      selectizeInput("col_y", "Y series (select multiple)", choices = cols,
                     selected = if (length(y_default) > 0) y_default else cols[min(2, length(cols))],
                     multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    )
  })

  # ── Y2 axis series selector ────────────────────────────────────────────────
  output$y2_series_selector <- renderUI({
    series <- rv$series_data
    if (length(series) == 0) return(NULL)
    labels <- vapply(series, function(s) s$label, character(1))
    selectizeInput("y2_series", "Series on Y2 axis",
                   choices = labels, multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  # ══════════════════════════════════════════════════════
  #  LOAD DATA INTO CHART
  # ══════════════════════════════════════════════════════
  observeEvent(input$load_btn, {
    df <- current_sheet()
    req(df, input$col_x, input$col_y)

    pal <- PALETTES[[input$palette]]
    series_list <- list()
    x_col <- input$col_x
    y_cols <- input$col_y
    shape_cycle <- unname(MARKER_SHAPES)

    has_regime <- any(grepl("regime|flow.?regime", y_cols, ignore.case = TRUE))
    rv$is_flow_regime <- has_regime

    for (i in seq_along(y_cols)) {
      y_col <- y_cols[i]
      x_vals <- suppressWarnings(as.numeric(df[[x_col]]))
      y_vals <- suppressWarnings(as.numeric(df[[y_col]]))
      valid <- !is.na(x_vals) & !is.na(y_vals)
      x_vals <- x_vals[valid]; y_vals <- y_vals[valid]
      if (length(x_vals) == 0) next

      # Clean label
      label <- y_col
      m <- regmatches(label, regexpr('"([^"]+)\\.ppl"', label, perl = TRUE))
      if (length(m) > 0 && nchar(m) > 0) label <- gsub('^"|"$', "", gsub("\\.ppl", "", m))

      is_regime <- grepl("regime|flow.?regime", y_col, ignore.case = TRUE)

      series_list[[length(series_list) + 1]] <- list(
        label     = label,
        color     = pal[((i - 1) %% length(pal)) + 1],
        shape     = as.integer(shape_cycle[((i - 1) %% length(shape_cycle)) + 1]),
        linetype  = "solid",
        x         = x_vals,
        y         = y_vals,
        visible   = TRUE,
        is_regime = is_regime,
        on_y2     = FALSE
      )
    }

    rv$series_data <- series_list

    # Auto-set labels
    sheet_nm <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    updateTextInput(session, "chart_title", value = sheet_nm)
    updateTextInput(session, "export_filename",
                    value = tolower(gsub("[^A-Za-z0-9]+", "_", sheet_nm)))
    if (length(y_cols) > 0) {
      yl <- gsub('"[^"]*"', "", y_cols[1])
      yl <- trimws(gsub("\\(PIPELINE\\)", "", yl))
      if (nchar(yl) > 0) updateTextInput(session, "ylabel", value = yl)
    }

    rv$plot_counter <- rv$plot_counter + 1
    rv$series_panel_ver <- rv$series_panel_ver + 1L
    showNotification(paste0("\u2713 ", length(series_list), " series loaded"), type = "message")
  })

  # ── Re-apply palette when changed ──────────────────────────────────────────
  observeEvent(input$palette, {
    pal <- PALETTES[[input$palette]]
    series <- rv$series_data
    if (length(series) > 0) {
      for (i in seq_along(series)) {
        rv$series_data[[i]]$color <- pal[((i - 1) %% length(pal)) + 1]
      }
    }
  })

  # ══════════════════════════════════════════════════════
  #  SERIES PANEL — editable names, colours, visibility
  # ══════════════════════════════════════════════════════
  output$series_panel <- renderUI({
    # Depend only on structural changes (load, visibility toggle), NOT label edits
    rv$series_panel_ver
    series <- isolate(rv$series_data)
    if (length(series) == 0)
      return(tags$p("No series loaded yet", style = "font-size:0.78rem;color:#999;font-style:italic;"))

    tagList(lapply(seq_along(series), function(i) {
      s <- series[[i]]
      div(
        style = paste0(
          "display:flex;align-items:center;gap:5px;padding:5px 8px;margin-bottom:4px;",
          "background:#f7f4ef;border:1px solid #d4cec4;border-radius:3px;",
          if (!s$visible) "opacity:0.35;" else ""
        ),
        # Colour picker (small swatch)
        colourInput(paste0("series_color_", i), NULL, value = s$color,
                    showColour = "background", palette = "limited",
                    returnName = FALSE),
        tags$style(HTML(paste0(
          "#series_color_", i, " { width:24px !important; height:24px !important; ",
          "padding:0 !important; border:1px solid #ccc !important; border-radius:2px !important; ",
          "min-height:unset !important; } ",
          "#series_color_", i, " + .input-group-addon { display:none !important; }"
        ))),
        # Editable series name
        textInput(paste0("series_name_", i), NULL, value = s$label),
        tags$style(HTML(paste0(
          "#series_name_", i, " { font-size:0.72rem !important; padding:3px 6px !important; ",
          "height:auto !important; margin:0 !important; flex:1; min-width:0; }"
        ))),
        # Toggle visibility button
        actionButton(paste0("toggle_vis_", i),
                     if (s$visible) icon("eye") else icon("eye-slash"),
                     class = "btn btn-sm btn-ghost",
                     style = "padding:2px 6px;min-width:28px;",
                     title = if (s$visible) "Hide series" else "Show series")
      )
    }))
  })

  # ── Series property observers ───────────────────────

  # Track how many series observers we've wired up so far
  rv_obs <- reactiveValues(n_series_obs = 0L)

  # Whenever the series count grows, create observers for the NEW indices only.
  # Each observeEvent is created exactly once per index (never duplicated).
  observe({
    n <- length(rv$series_data)
    prev <- isolate(rv_obs$n_series_obs)
    if (n > prev) {
      lapply((prev + 1L):n, function(i) {
        # Name edit
        observeEvent(input[[paste0("series_name_", i)]], {
          new_name <- input[[paste0("series_name_", i)]]
          if (!is.null(new_name) && nchar(trimws(new_name)) > 0 &&
              new_name != rv$series_data[[i]]$label) {
            rv$series_data[[i]]$label <- new_name
          }
        }, ignoreInit = TRUE)

        # Colour edit
        observeEvent(input[[paste0("series_color_", i)]], {
          new_col <- input[[paste0("series_color_", i)]]
          if (!is.null(new_col) && new_col != rv$series_data[[i]]$color) {
            rv$series_data[[i]]$color <- new_col
          }
        }, ignoreInit = TRUE)

        # Visibility toggle
        observeEvent(input[[paste0("toggle_vis_", i)]], {
          rv$series_data[[i]]$visible <- !rv$series_data[[i]]$visible
          rv$series_panel_ver <- rv$series_panel_ver + 1L
        }, ignoreInit = TRUE)
      })
      rv_obs$n_series_obs <- n
    }
  })

  observeEvent(input$show_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- TRUE
    rv$series_panel_ver <- rv$series_panel_ver + 1L
  })
  observeEvent(input$hide_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- FALSE
    rv$series_panel_ver <- rv$series_panel_ver + 1L
  })

  # ══════════════════════════════════════════════════════
  #  REFERENCE LINES
  # ══════════════════════════════════════════════════════
  observeEvent(input$add_ref, {
    req(input$ref_value)
    rv$ref_lines <- c(rv$ref_lines, list(list(
      axis     = input$ref_axis,
      value    = input$ref_value,
      label    = input$ref_label,
      color    = input$ref_color,
      linetype = input$ref_linetype
    )))
    updateNumericInput(session, "ref_value", value = NA)
    updateTextInput(session, "ref_label", value = "")
  })

  output$ref_lines_list <- renderUI({
    refs <- rv$ref_lines
    if (length(refs) == 0) return(NULL)
    tagList(lapply(seq_along(refs), function(i) {
      r <- refs[[i]]
      div(class = "ref-item",
        span(class = "color-swatch", style = paste0("background:", r$color)),
        span(paste0(if (r$axis == "x") "X=" else "Y=", r$value,
                    if (nchar(r$label) > 0) paste0("  \u2014 ", r$label)),
             style = "flex:1;color:#666;"),
        actionButton(paste0("rm_ref_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$ref_lines), function(i) {
      observeEvent(input[[paste0("rm_ref_", i)]], {
        rv$ref_lines <- rv$ref_lines[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ══════════════════════════════════════════════════════
  #  ANNOTATIONS
  # ══════════════════════════════════════════════════════
  observeEvent(input$add_ann, {
    req(input$ann_x, input$ann_y, nchar(input$ann_text) > 0)
    rv$annotations <- c(rv$annotations, list(list(
      x = input$ann_x, y = input$ann_y,
      text = input$ann_text, color = input$ann_color,
      arrow = input$ann_arrow
    )))
    updateNumericInput(session, "ann_x", value = NA)
    updateNumericInput(session, "ann_y", value = NA)
    updateTextInput(session, "ann_text", value = "")
  })

  output$annotations_list <- renderUI({
    anns <- rv$annotations
    if (length(anns) == 0) return(NULL)
    tagList(lapply(seq_along(anns), function(i) {
      a <- anns[[i]]
      div(class = "ref-item",
        span(class = "color-swatch", style = paste0("background:", a$color, ";border-radius:50%;")),
        span(paste0("(", a$x, ", ", a$y, ") ", a$text),
             style = "flex:1;color:#666;font-size:0.7rem;"),
        actionButton(paste0("rm_ann_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$annotations), function(i) {
      observeEvent(input[[paste0("rm_ann_", i)]], {
        rv$annotations <- rv$annotations[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ── Reset ranges ────────────────────────────────────────────────────────────
  observeEvent(input$reset_ranges, {
    updateNumericInput(session, "xmin", value = NA)
    updateNumericInput(session, "xmax", value = NA)
    updateNumericInput(session, "ymin", value = NA)
    updateNumericInput(session, "ymax", value = NA)
  })

  # ══════════════════════════════════════════════════════
  #  TREND EXTRACTION
  # ══════════════════════════════════════════════════════

  # Dynamic x-value inputs for each loaded series
  output$extract_x_inputs <- renderUI({
    series <- rv$series_data
    if (length(series) == 0)
      return(tags$p("Load data first", style = "font-size:0.75rem;color:#999;font-style:italic;"))
    tagList(
      tags$p("X parameter value for each series:",
             style = "font-size:0.68rem;color:#7a7060;font-weight:600;margin-bottom:4px;"),
      lapply(seq_along(series), function(i) {
        div(style = "display:flex;align-items:center;gap:6px;margin-bottom:3px;",
          tags$span(series[[i]]$label,
                    style = "font-size:0.72rem;flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;"),
          numericInput(paste0("extract_x_", i), NULL, value = i * 10,
                       step = 1, width = "90px")
        )
      })
    )
  })

  # Generate extraction plot
  observeEvent(input$generate_extraction, {
    series <- rv$series_data
    req(length(series) > 0)

    method <- input$extract_method %||% "first"
    x_vals <- numeric(length(series))
    y_vals <- numeric(length(series))
    labels <- character(length(series))

    for (i in seq_along(series)) {
      s <- series[[i]]
      x_vals[i] <- input[[paste0("extract_x_", i)]] %||% (i * 10)
      labels[i] <- s$label

      ord <- order(s$x)
      y_sorted <- s$y[ord]

      if (method == "first") y_vals[i] <- y_sorted[1]
      else if (method == "last") y_vals[i] <- y_sorted[length(y_sorted)]
      else if (method == "min") y_vals[i] <- min(s$y, na.rm = TRUE)
      else if (method == "max") y_vals[i] <- max(s$y, na.rm = TRUE)
      else y_vals[i] <- mean(s$y, na.rm = TRUE)
    }

    rv$extraction_data <- data.frame(
      x = x_vals, y = y_vals, label = labels, stringsAsFactors = FALSE
    )
    rv$extraction_mode <- TRUE

    # Update axis labels
    updateTextInput(session, "xlabel", value = input$extract_x_label %||% "Water Cut (%)")
    updateTextInput(session, "ylabel", value = input$extract_y_label %||% "Inlet Pressure [bara]")
    updateTextInput(session, "chart_title", value = paste0(
      switch(method, first = "Inlet", last = "Outlet", min = "Minimum",
             max = "Maximum", mean = "Mean"),
      " Value Trend"
    ))
    rv$plot_counter <- rv$plot_counter + 1
    showNotification(
      paste0("\u2713 Extracted ", length(series), " points for trend plot"),
      type = "message", duration = 4
    )
  })

  # Disable extraction mode when checkbox is unchecked
  observeEvent(input$extraction_enable, {
    if (!isTRUE(input$extraction_enable)) {
      rv$extraction_mode <- FALSE
      rv$plot_counter <- rv$plot_counter + 1
    }
  })

  # ══════════════════════════════════════════════════════
  #  TOP AXIS ANNOTATIONS
  # ══════════════════════════════════════════════════════
  output$top_ann_col_selectors <- renderUI({
    df <- current_sheet()
    req(df)
    cols <- c("(none)" = "", colnames(df))
    tagList(
      selectInput("top_ann_col1", "Row 1 column (e.g. Mass)", choices = cols),
      textInput("top_ann_label1", "Row 1 label", value = "Mass"),
      selectInput("top_ann_col2", "Row 2 column (e.g. Watercut)", choices = cols),
      textInput("top_ann_label2", "Row 2 label", value = "Watercut")
    )
  })

  observeEvent(input$add_top_ann, {
    req(input$top_ann_x)
    rv$top_anns <- c(rv$top_anns, list(list(
      x = input$top_ann_x,
      row1 = input$top_ann_row1 %||% "",
      row2 = input$top_ann_row2 %||% ""
    )))
    updateNumericInput(session, "top_ann_x", value = NA)
    updateTextInput(session, "top_ann_row1", value = "")
    updateTextInput(session, "top_ann_row2", value = "")
  })

  output$top_ann_manual_list <- renderUI({
    anns <- rv$top_anns
    if (length(anns) == 0) return(NULL)
    tagList(lapply(seq_along(anns), function(i) {
      a <- anns[[i]]
      div(class = "ref-item",
        span(paste0("x=", a$x, ": ", a$row1, " / ", a$row2),
             style = "flex:1;color:#666;font-size:0.68rem;"),
        actionButton(paste0("rm_topann_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$top_anns), function(i) {
      observeEvent(input[[paste0("rm_topann_", i)]], {
        rv$top_anns <- rv$top_anns[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ══════════════════════════════════════════════════════
  #  MULTI-PANEL PLOTS (Holdup / Velocity)
  # ══════════════════════════════════════════════════════

  observeEvent(input$generate_multi, {
    req(rv$raw_data)
    plot_type <- input$multi_plot_type

    # Helper: extract x,y for a given case from a sheet (paired columns)
    extract_case <- function(sheet_name, case_num) {
      sheet <- rv$raw_data[[sheet_name]]
      if (is.null(sheet)) return(NULL)
      x_col <- (case_num - 1) * 2 + 1
      y_col <- x_col + 1
      if (y_col > ncol(sheet)) return(NULL)
      x <- suppressWarnings(as.numeric(sheet[[x_col]]))
      y <- suppressWarnings(as.numeric(sheet[[y_col]]))
      valid <- !is.na(x) & !is.na(y)
      data.frame(x = x[valid], y = y[valid])
    }

    # ── Resolve cases & labels based on plot type ──────
    is_holdup_type  <- plot_type %in% c("holdup", "holdup_all")
    is_velocity_type <- plot_type %in% c("velocity", "velocity_1_5", "velocity_6_10")

    if (plot_type == "holdup") {
      cases  <- c(input$mp_case1, input$mp_case2, input$mp_case3)
      labels <- c(input$mp_label1, input$mp_label2, input$mp_label3)
    } else if (plot_type == "velocity") {
      cases  <- c(input$mp_case1, input$mp_case2, input$mp_case3)
      labels <- c(input$mp_label1, input$mp_label2, input$mp_label3)
    } else if (plot_type == "holdup_all") {
      cases  <- 1:10
      labels <- paste("Year", 1:10)
    } else if (plot_type == "velocity_1_5") {
      cases  <- 1:5
      labels <- paste("Year", 1:5)
    } else if (plot_type == "velocity_6_10") {
      cases  <- 6:10
      labels <- paste("Year", 6:10)
    }

    # ── Build holdup data ──────────────────────────────
    if (is_holdup_type) {
      required <- c("Liquid holdup", "Water holdup", "Oil holdup")
      missing <- setdiff(required, names(rv$raw_data))
      if (length(missing) > 0) {
        showNotification(paste("\u2717 Missing sheets:", paste(missing, collapse = ", ")),
                         type = "error")
        return()
      }

      all_data <- list()
      for (i in seq_along(cases)) {
        hol  <- extract_case("Liquid holdup", cases[i])
        holwt <- extract_case("Water holdup", cases[i])
        holhl <- extract_case("Oil holdup",   cases[i])

        if (!is.null(hol)) {
          hol$variable <- "Total Liquid (HOL)"
          hol$panel <- labels[i]
          all_data[[length(all_data) + 1]] <- hol
        }
        if (!is.null(holwt)) {
          holwt$variable <- "Water (HOLWT)"
          holwt$panel <- labels[i]
          all_data[[length(all_data) + 1]] <- holwt
        }
        if (!is.null(holhl)) {
          holhl$variable <- "Oil (HOLHL)"
          holhl$panel <- labels[i]
          all_data[[length(all_data) + 1]] <- holhl
        }
      }

      plot_df <- do.call(rbind, all_data)
      plot_df$panel <- factor(plot_df$panel, levels = labels)
      plot_df$variable <- factor(plot_df$variable,
                                  levels = c("Total Liquid (HOL)", "Water (HOLWT)", "Oil (HOLHL)"))

      rv$multi_panel_data <- plot_df
      rv$multi_panel_type <- plot_type

      title <- if (plot_type == "holdup_all") "Liquid Holdup Profiles \u2014 All Years"
               else "Liquid Holdup Profiles"
      updateTextInput(session, "chart_title", value = title)
      updateTextInput(session, "xlabel", value = "Pipeline Distance [m]")
      updateTextInput(session, "ylabel", value = "Holdup Fraction [-]")

    # ── Build velocity data ────────────────────────────
    } else if (is_velocity_type) {
      required <- c("Liquid Velocity", "Gas Velocity")
      missing <- setdiff(required, names(rv$raw_data))
      if (length(missing) > 0) {
        showNotification(paste("\u2717 Missing sheets:", paste(missing, collapse = ", ")),
                         type = "error")
        return()
      }

      all_data <- list()
      for (i in seq_along(cases)) {
        ul_raw <- extract_case("Liquid Velocity", cases[i])
        ug_raw <- extract_case("Gas Velocity",    cases[i])

        if (!is.null(ul_raw)) {
          ul_df <- ul_raw
          ul_df$variable <- "Liquid Velocity (UL)"
          ul_df$metric <- "Velocity (m/s)"
          ul_df$panel <- labels[i]
          all_data[[length(all_data) + 1]] <- ul_df
        }
        if (!is.null(ug_raw)) {
          ug_df <- ug_raw
          ug_df$variable <- "Gas Velocity (UG)"
          ug_df$metric <- "Velocity (m/s)"
          ug_df$panel <- labels[i]
          all_data[[length(all_data) + 1]] <- ug_df
        }
        # Slip ratio = UG / UL
        if (!is.null(ul_raw) && !is.null(ug_raw)) {
          merged <- merge(
            data.frame(x = ul_raw$x, y_ul = ul_raw$y),
            data.frame(x = ug_raw$x, y_ug = ug_raw$y),
            by = "x"
          )
          slip <- data.frame(
            x = merged$x,
            y = merged$y_ug / merged$y_ul,
            variable = "Slip Ratio (UG/UL)",
            metric   = "Slip Ratio (-)",
            panel    = labels[i],
            stringsAsFactors = FALSE
          )
          all_data[[length(all_data) + 1]] <- slip
        }
      }

      plot_df <- do.call(rbind, all_data)
      plot_df$panel  <- factor(plot_df$panel,  levels = labels)
      plot_df$metric <- factor(plot_df$metric, levels = c("Velocity (m/s)", "Slip Ratio (-)"))

      rv$multi_panel_data <- plot_df
      rv$multi_panel_type <- plot_type

      title <- if (plot_type == "velocity_1_5") "Phase Velocity & Slip Ratio \u2014 Years 1\u20135"
               else if (plot_type == "velocity_6_10") "Phase Velocity & Slip Ratio \u2014 Years 6\u201310"
               else "Phase Velocity & Slip Ratio"
      updateTextInput(session, "chart_title", value = title)
      updateTextInput(session, "xlabel", value = "Pipeline Distance [m]")
      updateTextInput(session, "ylabel", value = "")
    }

    rv$multi_panel_mode <- TRUE
    rv$plot_counter <- rv$plot_counter + 1
    showNotification("\u2713 Multi-panel plot generated", type = "message", duration = 4)
  })

  observeEvent(input$exit_multi, {
    rv$multi_panel_mode <- FALSE
    rv$plot_counter <- rv$plot_counter + 1
    showNotification("Exited multi-panel mode", type = "message", duration = 3)
  })

  # ── Refresh plot ────────────────────────────────────────────────────────────
  observeEvent(input$refresh_plot, { rv$plot_counter <- rv$plot_counter + 1 })

  # ══════════════════════════════════════════════════════
  #  BUILD THE GGPLOT
  # ══════════════════════════════════════════════════════
  build_plot <- reactive({
    # Touch reactive dependencies
    rv$plot_counter
    series <- rv$series_data
    visible <- Filter(function(s) s$visible, series)

    # ── Empty state ─────────────────────────────────────
    if (length(visible) == 0) {
      p <- ggplot() + theme_academic(base_size = 14) +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Import data to begin plotting",
                 size = 5.5, color = "#999999", fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), panel.border = element_blank(),
              axis.line = element_blank())
      return(p)
    }

    # ── Multi-panel mode (Holdup / Velocity) ──────────
    if (isTRUE(rv$multi_panel_mode) && !is.null(rv$multi_panel_data)) {
      mpdf <- rv$multi_panel_data
      font_size  <- input$axis_text_size %||% 10
      pal        <- PALETTES[[input$palette]]
      tick_inward <- (input$tick_dir %||% "in") != "out"
      title_sz   <- input$title_size %||% 16
      label_sz   <- input$axis_label_size %||% 12
      leg_sz     <- input$legend_size %||% 10
      lw         <- input$line_weight %||% 0.9
      grid_opt   <- input$grid_lines %||% "none"
      border_opt <- input$show_border %||% TRUE
      t_lab      <- parse_label(input$chart_title)
      x_lab      <- parse_label(input$xlabel)
      y_lab      <- parse_label(input$ylabel)
      st_lab     <- if (nchar(input$chart_subtitle %||% "") > 0) input$chart_subtitle else NULL

      # Clean academic facet strip theme (shared by both plot types)
      strip_theme <- theme(
        strip.background = element_rect(fill = "white", color = "#1a1a1a",
                                         linewidth = 0.5),
        strip.text       = element_text(size = label_sz * 0.9, face = "bold",
                                         color = "#1a1714", margin = margin(t = 4, b = 4)),
        panel.spacing    = unit(14, "pt")
      )

      if (rv$multi_panel_type %in% c("holdup", "holdup_all")) {
        var_colors    <- c("Total Liquid (HOL)" = pal[1],
                           "Water (HOLWT)"      = pal[2],
                           "Oil (HOLHL)"        = pal[3])
        var_linetypes <- c("Total Liquid (HOL)" = "solid",
                           "Water (HOLWT)"      = "dashed",
                           "Oil (HOLHL)"        = "dashed")

        # 3-panel (1x3) for main text; 10-panel (4-4-2) for appendix
        facet_ncol <- if (rv$multi_panel_type == "holdup_all") 4 else 3

        p <- ggplot(mpdf, aes(x = x, y = y, color = variable, linetype = variable)) +
          geom_line(linewidth = lw) +
          facet_wrap(~ panel, ncol = facet_ncol) +
          scale_color_manual(values = var_colors) +
          scale_linetype_manual(values = var_linetypes) +
          labs(x = x_lab, y = y_lab, title = t_lab, subtitle = st_lab,
               color = NULL, linetype = NULL) +
          theme_academic(base_size = font_size, grid = grid_opt,
                         border = border_opt, ticks_inward = tick_inward) +
          strip_theme +
          theme(
            plot.title  = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                        margin = margin(b = 8)),
            axis.title  = element_text(size = label_sz),
            axis.text   = element_text(size = font_size),
            legend.text = element_text(size = leg_sz)
          )

        # Legend: default to bottom-center outside for multi-panel
        lp <- input$legend_pos %||% "Bottom Right"
        if (lp == "Hidden") {
          p <- p + theme(legend.position = "none")
        } else {
          p <- p + theme(
            legend.position = "bottom",
            legend.justification = "center",
            legend.background = element_rect(fill = alpha("white", 0.95),
                                              color = "#cccccc", linewidth = 0.3),
            legend.margin = margin(t = 4, b = 4, l = 8, r = 8)
          )
        }

        # Override legend keys to show line swatches with correct linetypes
        p <- p + guides(
          color = guide_legend(
            nrow = 1,
            override.aes = list(linewidth = lw + 0.3)
          ),
          linetype = "none"
        )

        return(p)

      } else if (rv$multi_panel_type %in% c("velocity", "velocity_1_5", "velocity_6_10")) {
        var_colors <- c("Liquid Velocity (UL)" = pal[1],
                        "Gas Velocity (UG)"    = pal[2],
                        "Slip Ratio (UG/UL)"   = pal[3])

        p <- ggplot(mpdf, aes(x = x, y = y, color = variable)) +
          geom_line(linewidth = lw) +
          facet_grid(metric ~ panel, scales = "free_y", switch = "y") +
          scale_color_manual(values = var_colors) +
          labs(x = x_lab, y = NULL, title = t_lab, subtitle = st_lab,
               color = NULL) +
          theme_academic(base_size = font_size, grid = grid_opt,
                         border = border_opt, ticks_inward = tick_inward) +
          strip_theme +
          theme(
            plot.title  = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                        margin = margin(b = 8)),
            axis.title  = element_text(size = label_sz),
            axis.text   = element_text(size = font_size),
            legend.text = element_text(size = leg_sz),
            # Row strips on left side act as y-axis labels
            strip.placement  = "outside",
            strip.text.y.left = element_text(size = label_sz, face = "bold",
                                              angle = 90, color = "#1a1714",
                                              margin = margin(r = 6)),
            strip.background.y = element_rect(fill = "white", color = NA)
          )

        # Legend: default to bottom-center outside the plot for multi-panel
        lp <- input$legend_pos %||% "Bottom Right"
        if (lp == "Hidden") {
          p <- p + theme(legend.position = "none")
        } else {
          p <- p + theme(
            legend.position = "bottom",
            legend.justification = "center",
            legend.background = element_rect(fill = alpha("white", 0.95),
                                              color = "#cccccc", linewidth = 0.3),
            legend.margin = margin(t = 4, b = 4, l = 8, r = 8)
          )
        }

        p <- p + guides(
          color = guide_legend(
            nrow = 1,
            override.aes = list(linewidth = lw + 0.3)
          )
        )

        return(p)
      }
    }

    # ── Extraction mode plot ───────────────────────────
    if (isTRUE(rv$extraction_mode) && !is.null(rv$extraction_data)) {
      edf <- rv$extraction_data
      font_size <- input$axis_text_size %||% 10
      pal <- PALETTES[[input$palette]]
      tick_inward <- (input$tick_dir %||% "in") != "out"
      mk_size <- input$marker_size %||% 2.8
      title_sz <- input$title_size %||% 16
      label_sz <- input$axis_label_size %||% 12
      x_lab <- parse_label(input$xlabel)
      y_lab <- parse_label(input$ylabel)
      t_lab <- parse_label(input$chart_title)
      st_lab <- if (nchar(input$chart_subtitle %||% "") > 0) input$chart_subtitle else NULL
      show_margin <- isTRUE(input$extract_margin)
      trend_on <- isTRUE(input$extract_trend) && nrow(edf) >= 3
      trend_type <- input$extract_trend_type %||% "lm"
      eq_pos <- input$extract_eq_pos %||% "tr"

      # ── Build data: single panel or dual panel ──────
      if (show_margin) {
        margin_ref <- input$extract_margin_ref %||% 32
        margin_label <- input$extract_margin_panel_label %||% "Margin"
        ref_name <- input$extract_margin_ref_label %||% "Ref"
        value_label <- as.character(y_lab)

        plot_df <- rbind(
          data.frame(x = edf$x, y = edf$y,
                     panel = value_label, stringsAsFactors = FALSE),
          data.frame(x = edf$x, y = edf$y - margin_ref,
                     panel = margin_label, stringsAsFactors = FALSE)
        )
        plot_df$panel <- factor(plot_df$panel, levels = c(value_label, margin_label))

        p <- ggplot(plot_df, aes(x = x, y = y)) +
          facet_wrap(~ panel, ncol = 1, scales = "free_y") +
          theme_academic(base_size = font_size,
                         grid = input$grid_lines %||% "none",
                         border = input$show_border %||% TRUE,
                         ticks_inward = tick_inward)

        # Points
        p <- p + geom_point(size = mk_size, color = pal[1],
                            shape = 21, fill = pal[1], stroke = 0.5)

        # Trend lines (per-facet, geom_smooth handles facets automatically)
        if (trend_on) {
          if (trend_type == "lm") {
            p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                                 color = pal[2], linetype = "dashed", linewidth = 0.7)
            # Equation only on top panel (margin panel has identical slope/R²)
            if (eq_pos != "none") {
              top_df <- plot_df[plot_df$panel == value_label, ]
              if (nrow(top_df) >= 3) {
                fit <- lm(y ~ x, data = top_df)
                co <- coef(fit)
                r2 <- summary(fit)$r.squared
                sign_char <- if (co[2] >= 0) "+" else "\u2013"
                eq_label <- sprintf("y = %.3f x %s %.2f\nR\u00b2 = %.4f",
                                    co[2], sign_char, abs(co[1]), r2)
                x_rng <- range(top_df$x)
                y_rng <- range(top_df$y)
                eq_x <- if (grepl("l", eq_pos)) x_rng[1] + diff(x_rng) * 0.02
                        else x_rng[2] - diff(x_rng) * 0.02
                eq_y <- if (grepl("t", eq_pos)) y_rng[2] - diff(y_rng) * 0.02
                        else y_rng[1] + diff(y_rng) * 0.02
                eq_hjust <- if (grepl("l", eq_pos)) 0 else 1
                eq_vjust <- if (grepl("t", eq_pos)) 1 else 0
                ann_df <- data.frame(x = eq_x, y = eq_y, panel = value_label,
                                     stringsAsFactors = FALSE)
                ann_df$panel <- factor(ann_df$panel, levels = levels(plot_df$panel))
                p <- p + geom_label(data = ann_df, aes(x = x, y = y),
                  label = eq_label, hjust = eq_hjust, vjust = eq_vjust,
                  size = 3.2, color = pal[2], lineheight = 1.2,
                  fill = alpha("white", 0.92), label.size = 0.25,
                  label.padding = unit(4, "pt"), inherit.aes = FALSE)
              }
            }
          } else {
            p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                                 color = pal[2], linetype = "dashed", linewidth = 0.7,
                                 span = 0.75)
          }
        }

        # Reference line in margin panel — use annotate to avoid expanding y-axis
        margin_df <- plot_df[plot_df$panel == margin_label, ]
        margin_y_rng <- range(margin_df$y, na.rm = TRUE)
        if (0 >= margin_y_rng[1] - diff(margin_y_rng) * 0.1 &&
            0 <= margin_y_rng[2] + diff(margin_y_rng) * 0.1) {
          # Zero line is near the data — show it with geom_hline
          ref_line_df <- data.frame(
            yintercept = 0, panel = margin_label, stringsAsFactors = FALSE)
          ref_line_df$panel <- factor(ref_line_df$panel, levels = levels(plot_df$panel))
          p <- p + geom_hline(data = ref_line_df, aes(yintercept = yintercept),
                              linetype = "dashed", color = "#8b3a1e", linewidth = 0.5)
        } else {
          # Zero line is far from data — annotate the margin values instead
          # to avoid compressing the data into a narrow band
          min_margin <- min(margin_df$y, na.rm = TRUE)
          min_x <- margin_df$x[which.min(margin_df$y)]
          margin_note_df <- data.frame(x = min_x, y = min_margin,
                                       panel = margin_label, stringsAsFactors = FALSE)
          margin_note_df$panel <- factor(margin_note_df$panel, levels = levels(plot_df$panel))
          p <- p + geom_label(data = margin_note_df, aes(x = x, y = y),
            label = sprintf("Min margin: %.1f\u00b0C", min_margin),
            hjust = 0.5, vjust = 1.5, size = 3, color = "#8b3a1e",
            fill = alpha("white", 0.92), label.size = 0.25,
            label.padding = unit(3, "pt"), inherit.aes = FALSE)
        }

        # Labels
        p <- p + labs(x = x_lab, y = NULL, title = t_lab, subtitle = st_lab) +
          theme(
            plot.title    = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                         margin = margin(b = 8)),
            axis.title    = element_text(size = label_sz),
            axis.text     = element_text(size = font_size),
            strip.background = element_rect(fill = "white", color = "#1a1a1a",
                                             linewidth = 0.5),
            strip.text    = element_text(size = label_sz * 0.9, face = "bold",
                                         color = "#1a1714", margin = margin(t = 4, b = 4)),
            panel.spacing = unit(12, "pt"),
            legend.position = "none"
          )

      } else {
        # ── Single panel extraction plot ──────────────
        p <- ggplot(edf, aes(x = x, y = y)) +
          theme_academic(base_size = font_size,
                         grid = input$grid_lines %||% "none",
                         border = input$show_border %||% TRUE,
                         ticks_inward = tick_inward)

        # Scatter points
        p <- p + geom_point(size = mk_size, color = pal[1],
                            shape = 21, fill = pal[1], stroke = 0.5)

        # Trend line
        if (trend_on) {
          if (trend_type == "lm") {
            p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                                 color = pal[2], linetype = "dashed", linewidth = 0.7)
            # Equation annotation
            if (eq_pos != "none") {
              fit <- lm(y ~ x, data = edf)
              co <- coef(fit)
              r2 <- summary(fit)$r.squared
              sign_char <- if (co[2] >= 0) "+" else "\u2013"
              eq_label <- sprintf("y = %.3f x %s %.2f\nR\u00b2 = %.4f",
                                  co[2], sign_char, abs(co[1]), r2)
              x_rng <- range(edf$x)
              y_rng <- range(edf$y)
              eq_x <- if (grepl("l", eq_pos)) x_rng[1] + diff(x_rng) * 0.02
                      else x_rng[2] - diff(x_rng) * 0.02
              eq_y <- if (grepl("t", eq_pos)) y_rng[2] - diff(y_rng) * 0.02
                      else y_rng[1] + diff(y_rng) * 0.02
              eq_hjust <- if (grepl("l", eq_pos)) 0 else 1
              eq_vjust <- if (grepl("t", eq_pos)) 1 else 0
              p <- p + annotate("label",
                x = eq_x, y = eq_y,
                label = eq_label, hjust = eq_hjust, vjust = eq_vjust,
                size = 3.2, color = pal[2], lineheight = 1.2,
                fill = alpha("white", 0.92), label.size = 0.25,
                label.padding = unit(4, "pt"))
            }
          } else {
            p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                                 color = pal[2], linetype = "dashed", linewidth = 0.7,
                                 span = 0.75)
          }
        }

        # Labels
        p <- p + labs(x = x_lab, y = y_lab, title = t_lab, subtitle = st_lab) +
          theme(
            plot.title = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                       margin = margin(b = 8)),
            axis.title = element_text(size = label_sz),
            axis.text  = element_text(size = font_size),
            legend.position = "none"
          )
      }

      # Reference lines (shared for both modes)
      for (ref in rv$ref_lines) {
        if (ref$axis == "x") {
          p <- p + geom_vline(xintercept = ref$value, linetype = ref$linetype,
                              color = ref$color, linewidth = 0.6)
        } else {
          p <- p + geom_hline(yintercept = ref$value, linetype = ref$linetype,
                              color = ref$color, linewidth = 0.6)
        }
        if (nchar(ref$label) > 0) {
          p <- p + annotate("text",
            x = if (ref$axis == "x") ref$value else -Inf,
            y = if (ref$axis == "y") ref$value else Inf,
            label = ref$label, color = ref$color, size = 3.2,
            hjust = if (ref$axis == "x") -0.1 else -0.05,
            vjust = if (ref$axis == "y") -0.5 else 1.5)
        }
      }

      # Axis ranges
      x_lim <- c(if (!is.na(input$xmin)) input$xmin else NA,
                 if (!is.na(input$xmax)) input$xmax else NA)
      y_lim <- c(if (!is.na(input$ymin)) input$ymin else NA,
                 if (!is.na(input$ymax)) input$ymax else NA)
      if (!all(is.na(x_lim)))
        p <- p + scale_x_continuous(limits = x_lim, expand = expansion(mult = 0.05))
      else
        p <- p + scale_x_continuous(expand = expansion(mult = 0.05))
      if (!all(is.na(y_lim)))
        p <- p + scale_y_continuous(limits = y_lim, expand = expansion(mult = 0.05))
      else
        p <- p + scale_y_continuous(expand = expansion(mult = 0.05))

      return(p)
    }

    # ── Determine Y2 series ─────────────────────────────
    y2_labels <- if (input$y2_enable && !is.null(input$y2_series)) input$y2_series else character(0)

    # ── Build data frame ────────────────────────────────
    all_dfs <- lapply(visible, function(s) {
      data.frame(x = s$x, y = s$y, series = s$label, stringsAsFactors = FALSE)
    })
    plot_df <- do.call(rbind, all_dfs)
    plot_df$series <- factor(plot_df$series,
                             levels = vapply(visible, function(s) s$label, character(1)))

    # Maps
    color_map <- setNames(vapply(visible, function(s) s$color, character(1)),
                          vapply(visible, function(s) s$label, character(1)))
    shape_map <- setNames(
      vapply(visible, function(s) as.integer(s$shape), integer(1)),
      vapply(visible, function(s) s$label, character(1))
    )

    # ── Base plot ───────────────────────────────────────
    font_size <- input$axis_text_size %||% 10
    tick_inward <- (input$tick_dir %||% "in") != "out"
    p <- ggplot(plot_df, aes(x = x, y = y, color = series, shape = series)) +
      theme_academic(base_size = font_size,
                     grid = input$grid_lines %||% "none",
                     border = input$show_border %||% TRUE,
                     ticks_inward = tick_inward)

    # Handle "both" tick direction (inward + outward)
    if ((input$tick_dir %||% "in") == "both") {
      p <- p + theme(axis.ticks.length = unit(-4, "pt"))
      # Add outward ticks via a second axis with no labels
      # This is handled by minor axis overlay later
    }

    # ── Flow regime bands ───────────────────────────────
    if (rv$is_flow_regime) {
      regime_s <- Filter(function(s) s$is_regime, visible)
      if (length(regime_s) > 0) {
        rs <- regime_s[[1]]
        ord <- order(rs$x)
        for (j in seq_len(length(ord) - 1)) {
          code <- as.character(round(rs$y[ord[j]]))
          fr <- FLOW_REGIMES[[code]]
          if (!is.null(fr))
            p <- p + annotate("rect", xmin = rs$x[ord[j]], xmax = rs$x[ord[j+1]],
                              ymin = -Inf, ymax = Inf, fill = fr$fill, alpha = 0.08)
        }
      }
    }

    # ── Reference lines ─────────────────────────────────
    for (ref in rv$ref_lines) {
      if (ref$axis == "x") {
        p <- p + geom_vline(xintercept = ref$value, linetype = ref$linetype,
                            color = ref$color, linewidth = 0.6)
      } else {
        p <- p + geom_hline(yintercept = ref$value, linetype = ref$linetype,
                            color = ref$color, linewidth = 0.6)
      }
      if (nchar(ref$label) > 0) {
        p <- p + annotate("text",
          x = if (ref$axis == "x") ref$value else -Inf,
          y = if (ref$axis == "y") ref$value else Inf,
          label = ref$label, color = ref$color, size = 3.2,
          hjust = if (ref$axis == "x") -0.1 else -0.05,
          vjust = if (ref$axis == "y") -0.5 else 1.5)
      }
    }

    # ── Curves ──────────────────────────────────────────
    curve_type <- input$curve_type %||% "loess"
    lt <- input$line_type %||% "solid"
    lw <- input$line_weight %||% 0.9

    if (curve_type != "none") {
      for (s in visible) {
        sdf <- data.frame(x = s$x, y = s$y)
        sdf <- sdf[order(sdf$x), ]
        if (nrow(sdf) < 2) next

        if (curve_type == "loess" && nrow(sdf) >= 4) {
          span_val <- input$loess_span %||% 0.35
          p <- p + geom_smooth(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                               method = "loess", formula = y ~ x, se = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               span = span_val, show.legend = FALSE)
        } else if (curve_type == "spline" && nrow(sdf) >= 4) {
          # Natural cubic spline interpolation
          n_interp <- max(200, nrow(sdf) * 10)
          xnew <- seq(min(sdf$x), max(sdf$x), length.out = n_interp)
          sp <- tryCatch(splinefun(sdf$x, sdf$y, method = "natural"),
                         error = function(e) NULL)
          if (!is.null(sp)) {
            spline_df <- data.frame(x = xnew, y = sp(xnew))
            p <- p + geom_line(data = spline_df, aes(x = x, y = y), inherit.aes = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               show.legend = FALSE)
          } else {
            p <- p + geom_line(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               show.legend = FALSE)
          }
        } else if (curve_type == "step") {
          p <- p + geom_step(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                             color = s$color, linetype = lt, linewidth = lw,
                             show.legend = FALSE)
        } else {
          # Linear or fallback
          p <- p + geom_line(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                             color = s$color, linetype = lt, linewidth = lw,
                             show.legend = FALSE)
        }
      }
    }

    # ── Markers ─────────────────────────────────────────
    mk_mode <- input$marker_mode %||% "all"
    mk_size <- input$marker_size %||% 2.8

    if (mk_mode != "none") {
      # Build a combined marker data frame with series factor for legend mapping
      marker_dfs <- list()
      for (s in visible) {
        sdf <- data.frame(x = s$x, y = s$y, series = s$label)
        sdf <- sdf[order(sdf$x), ]

        if (mk_mode == "nth") {
          nth <- input$marker_nth %||% 10
          idx <- seq(1, nrow(sdf), by = nth)
          idx <- sort(unique(c(1, idx, nrow(sdf))))
          sdf <- sdf[idx, ]
        } else if (mk_mode == "ends") {
          sdf <- sdf[c(1, nrow(sdf)), ]
        }
        marker_dfs[[length(marker_dfs) + 1]] <- sdf
      }
      marker_df <- do.call(rbind, marker_dfs)
      marker_df$series <- factor(marker_df$series,
                                  levels = vapply(visible, function(s) s$label, character(1)))

      # Use mapped aesthetics so the legend is generated automatically
      # Map fill = series so shapes 21-25 get filled interiors
      p <- p + geom_point(data = marker_df,
                          aes(x = x, y = y, color = series, shape = series, fill = series),
                          size = mk_size, stroke = 0.5)
    } else {
      # No markers — use invisible line layer so legend shows colored line swatches
      p <- p + geom_line(data = plot_df,
                         aes(x = x, y = y, color = series),
                         linewidth = 0, alpha = 0, show.legend = TRUE)
    }

    # ── Annotations ─────────────────────────────────────
    for (ann in rv$annotations) {
      x_rng <- range(plot_df$x, na.rm = TRUE)
      y_rng <- range(plot_df$y, na.rm = TRUE)
      dx <- diff(x_rng) * 0.04
      dy <- diff(y_rng) * 0.06

      # Arrow direction
      arr <- ann$arrow %||% "ur"
      nudge_x <- dx * ifelse(grepl("l", arr), -1, 1)
      nudge_y <- dy * ifelse(grepl("d", arr), -1, 1)

      if (arr != "none") {
        p <- p + annotate("segment",
          x = ann$x, y = ann$y,
          xend = ann$x + nudge_x * 0.6, yend = ann$y + nudge_y * 0.6,
          color = ann$color, linewidth = 0.4,
          arrow = arrow(length = unit(4, "pt"), type = "closed"))
      }
      p <- p + annotate("point", x = ann$x, y = ann$y,
                        size = 3.5, color = ann$color, shape = 16)
      p <- p + annotate("label", x = ann$x + nudge_x, y = ann$y + nudge_y,
                        label = ann$text, color = ann$color,
                        fill = alpha("white", 0.92), label.size = 0.25,
                        size = 3.2, label.padding = unit(3, "pt"))
    }

    # ── Scales ──────────────────────────────────────────
    p <- p + scale_color_manual(values = color_map) +
             scale_shape_manual(values = shape_map)

    # Build proper legend: line swatch + marker for each series
    # Use fill map for shapes 21-25 (filled shapes use fill, not color)
    fill_vals <- setNames(
      vapply(visible, function(s) {
        if (s$shape %in% FILLED_SHAPES) s$color else NA_character_
      }, character(1)),
      vapply(visible, function(s) s$label, character(1))
    )

    p <- p + scale_fill_manual(values = fill_vals, guide = "none")

    # Legend override: show colored line + correct per-series marker shape
    legend_overrides <- list(
      size = mk_size + 0.5,
      stroke = 0.5,
      linetype = lt,
      linewidth = lw,
      shape = vapply(visible, function(s) as.integer(s$shape), integer(1)),
      fill = vapply(visible, function(s) {
        if (s$shape %in% FILLED_SHAPES) s$color else NA_character_
      }, character(1))
    )

    if (mk_mode == "none") {
      legend_overrides$shape <- NA
      legend_overrides$size <- 0
      legend_overrides$alpha <- 1
      legend_overrides$linewidth <- lw
    }

    p <- p + guides(
      color = guide_legend(
        ncol = input$legend_cols %||% 1,
        override.aes = legend_overrides
      ),
      shape = "none"
    )

    # ── Labels ──────────────────────────────────────────
    title_sz <- input$title_size %||% 16
    label_sz <- input$axis_label_size %||% 12
    leg_sz   <- input$legend_size %||% 10

    x_lab <- parse_label(input$xlabel)
    y_lab <- parse_label(input$ylabel)
    t_lab <- parse_label(input$chart_title)
    st_lab <- if (nchar(input$chart_subtitle %||% "") > 0) input$chart_subtitle else NULL

    p <- p + labs(x = x_lab, y = y_lab, title = t_lab, subtitle = st_lab)

    # Font size overrides
    p <- p + theme(
      plot.title    = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
      axis.title    = element_text(size = label_sz),
      axis.text     = element_text(size = font_size),
      legend.text   = element_text(size = leg_sz)
    )

    # ── Axis ranges ─────────────────────────────────────
    x_lim <- c(if (!is.na(input$xmin)) input$xmin else NA,
               if (!is.na(input$xmax)) input$xmax else NA)
    y_lim <- c(if (!is.na(input$ymin)) input$ymin else NA,
               if (!is.na(input$ymax)) input$ymax else NA)

    # ── Smart X axis breaks: use integer breaks when data are integer-like ────
    x_all <- plot_df$x
    x_is_integer <- all(x_all == round(x_all), na.rm = TRUE) &&
                    diff(range(x_all, na.rm = TRUE)) <= 50
    x_breaks_fn <- if (x_is_integer) {
      x_range <- range(x_all, na.rm = TRUE)
      # Use every integer as a break
      function(lim) seq(ceiling(lim[1]), floor(lim[2]), by = max(1, round(diff(lim)/20)))
    } else {
      waiver()
    }
    x_labels_fn <- if (x_is_integer) {
      function(x) as.character(as.integer(x))
    } else {
      waiver()
    }

    # Only add x scale here if top-axis annotations won't override it later
    top_ann_active <- isTRUE(input$top_ann_enable) && isTRUE(input$top_ann_ticks)
    if (!top_ann_active) {
      if (!all(is.na(x_lim)))
        p <- p + scale_x_continuous(limits = x_lim, expand = expansion(mult = 0.02),
                                    breaks = x_breaks_fn, labels = x_labels_fn)
      else
        p <- p + scale_x_continuous(expand = expansion(mult = 0.02),
                                    breaks = x_breaks_fn, labels = x_labels_fn)
    }

    if (!all(is.na(y_lim)))
      p <- p + scale_y_continuous(limits = y_lim, expand = expansion(mult = 0.02))
    else
      p <- p + scale_y_continuous(expand = expansion(mult = 0.02))

    # ── Top-axis annotations ─────────────────────────────
    if (isTRUE(input$top_ann_enable)) {
      top_ann_sz    <- input$top_ann_size %||% 3
      top_ann_angle <- input$top_ann_angle %||% 0
      top_ann_col   <- input$top_ann_color %||% "#555555"
      top_ann_hjust <- if (top_ann_angle > 0) 0.5 else 0.5
      top_ann_vjust <- 1

      # Build top annotation data from columns or manual entries
      top_df <- NULL

      col1 <- input$top_ann_col1 %||% ""
      col2 <- input$top_ann_col2 %||% ""
      label1 <- input$top_ann_label1 %||% ""
      label2 <- input$top_ann_label2 %||% ""

      df <- tryCatch(current_sheet(), error = function(e) NULL)

      if (!is.null(df) && (nchar(col1) > 0 || nchar(col2) > 0)) {
        x_col <- input$col_x
        if (!is.null(x_col) && x_col %in% colnames(df)) {
          x_vals <- suppressWarnings(as.numeric(df[[x_col]]))

          rows1 <- if (nchar(col1) > 0 && col1 %in% colnames(df)) as.character(df[[col1]]) else rep("", length(x_vals))
          rows2 <- if (nchar(col2) > 0 && col2 %in% colnames(df)) as.character(df[[col2]]) else rep("", length(x_vals))

          valid <- !is.na(x_vals)
          if (any(valid)) {
            top_df <- data.frame(
              x = x_vals[valid],
              row1 = rows1[valid],
              row2 = rows2[valid],
              stringsAsFactors = FALSE
            )
            # Deduplicate by x (take first occurrence)
            top_df <- top_df[!duplicated(top_df$x), ]
          }
        }
      }

      # Add manual entries
      manual_anns <- rv$top_anns
      if (length(manual_anns) > 0) {
        manual_df <- data.frame(
          x = vapply(manual_anns, function(a) a$x, numeric(1)),
          row1 = vapply(manual_anns, function(a) a$row1, character(1)),
          row2 = vapply(manual_anns, function(a) a$row2, character(1)),
          stringsAsFactors = FALSE
        )
        top_df <- if (is.null(top_df)) manual_df else rbind(top_df, manual_df)
      }

      if (!is.null(top_df) && nrow(top_df) > 0) {
        y_upper <- max(plot_df$y, na.rm = TRUE)
        y_range <- diff(range(plot_df$y, na.rm = TRUE))

        has_row1 <- any(nchar(top_df$row1) > 0)
        has_row2 <- any(nchar(top_df$row2) > 0)
        both_rows <- has_row1 && has_row2

        # Build combined label: stack row1 / row2 with padding for readability
        top_df$combined <- mapply(function(r1, r2) {
          parts <- c()
          if (nchar(r1) > 0) parts <- c(parts, trimws(r1))
          if (nchar(r2) > 0) parts <- c(parts, trimws(r2))
          paste(parts, collapse = "\n")
        }, top_df$row1, top_df$row2, USE.NAMES = FALSE)

        # Header title for the top axis
        header_parts <- c()
        if (nchar(label1) > 0) header_parts <- c(header_parts, label1)
        if (nchar(label2) > 0) header_parts <- c(header_parts, label2)
        top_title <- paste(header_parts, collapse = "  /  ")

        # Use sec_axis for top tick marks
        if (isTRUE(input$top_ann_ticks)) {
          top_breaks <- top_df$x
          top_labels <- top_df$combined
          p <- p + scale_x_continuous(
            limits = if (!all(is.na(x_lim))) x_lim else NULL,
            expand = expansion(mult = 0.02),
            breaks = x_breaks_fn, labels = x_labels_fn,
            sec.axis = sec_axis(
              transform = ~ .,
              name = top_title,
              breaks = top_breaks,
              labels = top_labels
            )
          )
          # Increase lineheight when both rows are present so they don't overlap
          lh <- if (both_rows) 1.35 else 1.0
          p <- p + theme(
            axis.text.x.top = element_text(
              size = top_ann_sz * 2.5, color = top_ann_col,
              angle = top_ann_angle, hjust = top_ann_hjust,
              vjust = 0, lineheight = lh,
              margin = margin(b = 4)
            ),
            axis.title.x.top = element_text(
              size = top_ann_sz * 2.8, color = top_ann_col,
              face = "italic", margin = margin(b = 2)
            ),
            axis.ticks.x.top = element_line(color = "#999999", linewidth = 0.3),
            axis.ticks.length.x.top = unit(3, "pt"),
            # Extra top margin so the two-row labels don't clip
            plot.margin = margin(t = if (both_rows) 8 else 5, r = 12, b = 12, l = 12)
          )
        } else {
          # No tick marks — annotate text above plot area with clip off
          for (k in seq_len(nrow(top_df))) {
            p <- p + annotate("text",
              x = top_df$x[k], y = Inf,
              label = top_df$combined[k],
              color = top_ann_col, size = top_ann_sz,
              angle = top_ann_angle, hjust = 0.5, vjust = -0.3,
              lineheight = if (both_rows) 1.2 else 0.85)
          }
          if (nchar(top_title) > 0) {
            # Add title annotation above everything
            x_mid <- mean(range(top_df$x, na.rm = TRUE))
            p <- p + annotate("text",
              x = x_mid, y = Inf,
              label = top_title,
              color = top_ann_col, size = top_ann_sz * 1.1,
              fontface = "italic", hjust = 0.5, vjust = -1.8)
          }
          p <- p + coord_cartesian(clip = "off") +
            theme(plot.margin = margin(t = if (both_rows) 40 else 30, r = 12, b = 12, l = 12))
        }
      }
    }

    # ── Legend position ─────────────────────────────────
    lp <- input$legend_pos %||% "Bottom Right"
    if (lp == "Hidden") {
      p <- p + theme(legend.position = "none")
    } else {
      pos <- leg_pos(lp)
      jst <- leg_just(lp)
      p <- p + theme(
        legend.position = pos,
        legend.justification = jst,
        legend.position.inside = pos
      )
    }

    p
  })

  # ══════════════════════════════════════════════════════
  #  SLUGGING CHARACTERIZATION
  # ══════════════════════════════════════════════════════

  # Reactive storage for slug data
  slug_rv <- reactiveValues(
    slug_data = NULL,       # list of sheets from Slug.xlsx
    slugtrack_data = NULL,  # list of sheets from Slugtracking.xlsx
    loaded = FALSE
  )

  # Load slug data
  observeEvent(input$load_slug_data, {
    tryCatch({
      # Load Slug.xlsx
      slug_path <- if (!is.null(input$slug_file)) input$slug_file$datapath else "Slug.xlsx"
      if (file.exists(slug_path)) {
        sheets <- excel_sheets(slug_path)
        slug_rv$slug_data <- setNames(
          lapply(sheets, function(s) {
            as.data.frame(read_excel(slug_path, sheet = s, col_names = TRUE,
                                     .name_repair = "minimal"))
          }), sheets)
      }

      # Load Slugtracking.xlsx
      st_path <- if (!is.null(input$slugtrack_file)) input$slugtrack_file$datapath else "Slugtracking.xlsx"
      if (file.exists(st_path)) {
        sheets2 <- excel_sheets(st_path)
        slug_rv$slugtrack_data <- setNames(
          lapply(sheets2, function(s) {
            as.data.frame(read_excel(st_path, sheet = s, col_names = TRUE,
                                     .name_repair = "minimal"))
          }), sheets2)
      }

      slug_rv$loaded <- TRUE
      showNotification("\u2713 Slug data loaded successfully", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste0("\u2717 Error loading slug data: ", e$message), type = "error")
    })
  })

  # Helper: extract time-series for a case from a slug sheet (paired columns)
  extract_slug_case <- function(data_list, sheet_name, case_num, t_start, t_end) {
    sheet <- data_list[[sheet_name]]
    if (is.null(sheet)) return(NULL)
    x_col <- (case_num - 1) * 2 + 1
    y_col <- x_col + 1
    if (y_col > ncol(sheet)) return(NULL)
    t <- suppressWarnings(as.numeric(sheet[[x_col]]))
    v <- suppressWarnings(as.numeric(sheet[[y_col]]))
    valid <- !is.na(t) & !is.na(v)
    t <- t[valid]; v <- v[valid]
    # Apply time window
    in_window <- t >= t_start & t <= t_end
    if (sum(in_window) == 0) return(NULL)
    data.frame(time = t[in_window], value = v[in_window])
  }

  # Helper: get water cuts from inputs
  slug_water_cuts <- reactive({
    vapply(1:10, function(i) input[[paste0("wc_", i)]] %||% WATER_CUTS[i], numeric(1))
  })

  # Helper: get selected panels
  slug_panels <- reactive({
    list(
      cases = as.integer(c(input$slug_panel_a, input$slug_panel_b, input$slug_panel_c)),
      labels = c(input$slug_label_a, input$slug_label_b, input$slug_label_c)
    )
  })

  # Update panel labels when panel selection changes
  observeEvent(input$slug_panel_a, {
    wc <- slug_water_cuts()
    yr <- as.integer(input$slug_panel_a)
    updateTextInput(session, "slug_label_a",
                    value = paste0("Year ", yr, " (", round(wc[yr]), "% WC)"))
  })
  observeEvent(input$slug_panel_b, {
    wc <- slug_water_cuts()
    yr <- as.integer(input$slug_panel_b)
    updateTextInput(session, "slug_label_b",
                    value = paste0("Year ", yr, " (", round(wc[yr]), "% WC)"))
  })
  observeEvent(input$slug_panel_c, {
    wc <- slug_water_cuts()
    yr <- as.integer(input$slug_panel_c)
    updateTextInput(session, "slug_label_c",
                    value = paste0("Year ", yr, " (", round(wc[yr]), "% WC)"))
  })

  # Slug export preset
  observeEvent(input$slug_export_preset, {
    preset <- JOURNAL_PRESETS[[input$slug_export_preset]]
    if (!is.null(preset) && input$slug_export_preset != "Custom") {
      updateNumericInput(session, "slug_export_w", value = preset$w)
      updateNumericInput(session, "slug_export_h", value = preset$h)
      updateNumericInput(session, "slug_export_dpi", value = preset$dpi)
      updateSelectInput(session, "slug_export_fmt", selected = preset$fmt)
    }
  })

  # ── Common slug plot builder ────────────────────────────
  # Build 3-panel stacked time-series plot
  build_slug_timeseries <- function(data_list, sheet_name, y_label, title,
                                     panels, t_start, t_window, pal, opts,
                                     ref_line = NULL, var_symbol = "P",
                                     ann_pos = "top_right") {
    t_end <- t_start + t_window
    all_data <- list()

    for (i in seq_along(panels$cases)) {
      d <- extract_slug_case(data_list, sheet_name, panels$cases[i], t_start, t_end)
      if (!is.null(d)) {
        d$panel <- panels$labels[i]
        all_data[[length(all_data) + 1]] <- d
      }
    }

    if (length(all_data) == 0) {
      return(ggplot() + theme_academic(base_size = 12) +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No data found in sheet: ", sheet_name),
                 size = 5, color = "#999", fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), panel.border = element_blank()))
    }

    plot_df <- do.call(rbind, all_data)
    plot_df$panel <- factor(plot_df$panel, levels = panels$labels)

    # Compute per-panel stats
    stats_df <- do.call(rbind, lapply(split(plot_df, plot_df$panel), function(pd) {
      data.frame(
        panel = pd$panel[1],
        mean_val = mean(pd$value, na.rm = TRUE),
        sd_val = sd(pd$value, na.rm = TRUE),
        min_val = min(pd$value, na.rm = TRUE),
        max_val = max(pd$value, na.rm = TRUE),
        pp_amp = max(pd$value, na.rm = TRUE) - min(pd$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    stats_df$panel <- factor(stats_df$panel, levels = panels$labels)

    # Assign color by year index so 3-panel colors match the all-years grid
    color_map <- setNames(
      pal[((panels$cases - 1) %% length(pal)) + 1],
      panels$labels
    )

    p <- ggplot(plot_df, aes(x = time, y = value, color = panel)) +
      geom_line(linewidth = opts$lw, show.legend = FALSE) +
      facet_wrap(~ panel, ncol = 1, scales = "free_y") +
      scale_color_manual(values = color_map) +
      theme_academic(base_size = opts$label_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
        strip.text = element_text(size = opts$label_size - 1, face = "bold",
                                  color = "#1a1714", margin = margin(4, 0, 4, 0)),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8))
      ) +
      labs(x = "Time (s)", y = y_label, title = title)

    # Add mean line and annotation per panel
    # Build annotation label with correct variable symbol (P or Q) and subscript pp
    stats_df$ann_label <- sprintf(
      "Mean: %.2f\n\u0394%s\u209a\u209a: %.2f\nMax: %.2f\n\u03c3: %.3f",
      stats_df$mean_val, var_symbol, stats_df$pp_amp, stats_df$max_val, stats_df$sd_val
    )

    # Annotation position — auto-detect best corner per panel
    # For each panel, check which corner (TL, TR, BL, BR) is farthest from data
    if (ann_pos == "auto") {
      corner_results <- do.call(rbind, lapply(levels(plot_df$panel), function(pnl) {
        pd <- plot_df[plot_df$panel == pnl, ]
        st <- stats_df[stats_df$panel == pnl, ]
        t_range <- range(pd$time, na.rm = TRUE)
        y_range <- range(pd$value, na.rm = TRUE)
        # Normalise time and value to [0,1] for fair distance comparison
        t_norm <- (pd$time - t_range[1]) / max(diff(t_range), 1e-9)
        y_norm <- (pd$value - y_range[1]) / max(diff(y_range), 1e-9)
        # Four candidate corners in normalised space
        corners <- list(
          top_left     = c(0, 1),
          top_right    = c(1, 1),
          bottom_left  = c(0, 0),
          bottom_right = c(1, 0)
        )
        # For each corner, compute mean distance to all data points
        best <- names(which.max(vapply(corners, function(cn) {
          mean(sqrt((t_norm - cn[1])^2 + (y_norm - cn[2])^2))
        }, numeric(1))))
        is_right  <- grepl("right", best)
        is_bottom <- grepl("bottom", best)
        y_pad <- diff(y_range) * 0.05
        data.frame(
          panel = pnl,
          ann_x = if (is_right) t_range[2] - diff(t_range) * 0.02 else t_range[1] + diff(t_range) * 0.02,
          ann_y = if (is_bottom) y_range[1] + y_pad else y_range[2] - y_pad,
          ann_hjust = if (is_right) 1 else 0,
          ann_vjust = if (is_bottom) 0 else 1,
          stringsAsFactors = FALSE
        )
      }))
      corner_results$panel <- factor(corner_results$panel, levels = levels(plot_df$panel))
      stats_df <- merge(stats_df, corner_results, by = "panel")
    } else {
      stats_df$ann_x <- if (grepl("right", ann_pos)) t_start + t_window * 0.98 else t_start + t_window * 0.02
      stats_df$ann_y <- if (grepl("bottom", ann_pos)) stats_df$min_val else stats_df$max_val
      stats_df$ann_hjust <- if (grepl("right", ann_pos)) 1 else 0
      stats_df$ann_vjust <- if (grepl("bottom", ann_pos)) 0 else 1
    }

    # Add per-panel color to stats_df for mean lines and labels
    stats_df$panel_color <- color_map[as.character(stats_df$panel)]

    p <- p +
      geom_hline(data = stats_df, aes(yintercept = mean_val),
                 linetype = "dashed", color = stats_df$panel_color, linewidth = 0.4) +
      geom_label(data = stats_df,
                 aes(x = ann_x, y = ann_y,
                     label = ann_label, hjust = ann_hjust, vjust = ann_vjust),
                 size = 2.5,
                 color = stats_df$panel_color,
                 fill = alpha("white", 0.92), label.size = 0.2,
                 label.padding = unit(3, "pt"), lineheight = 1.2)

    # Optional reference line
    if (!is.null(ref_line)) {
      p <- p + geom_hline(yintercept = ref_line$value, linetype = "dotted",
                           color = ref_line$color, linewidth = 0.5)
    }

    p
  }

  # ── Fig 4.10: PT PIPE 1 ────────────────────────────────
  output$slug_plot_pt1 <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slug_data, "PT PIPE 1 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Pipeline Inlet (PIPE-1)",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, ann_pos = "bottom_right"
    )
  }, res = 96)

  # ── Fig 4.10-B: PT PIPE 7 ──────────────────────────────
  output$slug_plot_pt7 <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slug_data, "PT PIPE 7 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Separator Inlet (PIPE-7)",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, ann_pos = "top_right"
    )
  }, res = 96)

  # ── Fig 4.11: QLT PIPE 7 ───────────────────────────────
  output$slug_plot_qlt <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slug_data, "QLT PIPE 7 Trend",
      "Liquid Flow Rate (m\u00b3/d)",
      "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7)",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, var_symbol = "Q", ann_pos = "auto"
    )
  }, res = 96)

  # ── Appendix: All Years (2×5 grid) ──────────────────────
  # Shared builder for 10-panel plots
  build_slug_all_years <- function(data_list, sheet_name, y_label, title,
                                    t_start, t_window, pal, opts,
                                    var_symbol = "P") {
    wc <- slug_water_cuts()
    t_end <- t_start + t_window
    all_data <- list()
    labels <- paste0("Year ", 1:10, " (", round(wc), "% WC)")

    for (i in 1:10) {
      d <- extract_slug_case(data_list, sheet_name, i, t_start, t_end)
      if (!is.null(d)) {
        d$panel <- labels[i]
        all_data[[length(all_data) + 1]] <- d
      }
    }

    if (length(all_data) == 0) {
      return(ggplot() + theme_academic(base_size = 12) +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No data found in sheet: ", sheet_name),
                 size = 5, color = "#999", fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), panel.border = element_blank()))
    }

    plot_df <- do.call(rbind, all_data)
    plot_df$panel <- factor(plot_df$panel, levels = labels)

    # Per-panel stats
    stats_df <- do.call(rbind, lapply(split(plot_df, plot_df$panel), function(pd) {
      data.frame(
        panel = pd$panel[1],
        mean_val = mean(pd$value, na.rm = TRUE),
        sd_val = sd(pd$value, na.rm = TRUE),
        min_val = min(pd$value, na.rm = TRUE),
        max_val = max(pd$value, na.rm = TRUE),
        pp_amp = max(pd$value, na.rm = TRUE) - min(pd$value, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    stats_df$panel <- factor(stats_df$panel, levels = labels)
    stats_df$ann_label <- sprintf(
      "\u0394%s\u209a\u209a: %.2f\n\u03c3: %.3f",
      var_symbol, stats_df$pp_amp, stats_df$sd_val
    )

    # Auto-detect best corner per panel
    corner_results <- do.call(rbind, lapply(levels(plot_df$panel), function(pnl) {
      pd <- plot_df[plot_df$panel == pnl, ]
      st <- stats_df[stats_df$panel == pnl, ]
      t_range <- range(pd$time, na.rm = TRUE)
      y_range <- range(pd$value, na.rm = TRUE)
      t_norm <- (pd$time - t_range[1]) / max(diff(t_range), 1e-9)
      y_norm <- (pd$value - y_range[1]) / max(diff(y_range), 1e-9)
      corners <- list(
        top_left = c(0, 1), top_right = c(1, 1),
        bottom_left = c(0, 0), bottom_right = c(1, 0)
      )
      best <- names(which.max(vapply(corners, function(cn) {
        mean(sqrt((t_norm - cn[1])^2 + (y_norm - cn[2])^2))
      }, numeric(1))))
      is_right  <- grepl("right", best)
      is_bottom <- grepl("bottom", best)
      y_pad <- diff(y_range) * 0.05
      data.frame(
        panel = pnl,
        ann_x = if (is_right) t_range[2] - diff(t_range) * 0.02 else t_range[1] + diff(t_range) * 0.02,
        ann_y = if (is_bottom) y_range[1] + y_pad else y_range[2] - y_pad,
        ann_hjust = if (is_right) 1 else 0,
        ann_vjust = if (is_bottom) 0 else 1,
        stringsAsFactors = FALSE
      )
    }))
    corner_results$panel <- factor(corner_results$panel, levels = levels(plot_df$panel))
    stats_df <- merge(stats_df, corner_results, by = "panel")

    # Assign one color per panel (matching reference profile matrix style)
    panel_levels <- levels(plot_df$panel)
    color_map <- setNames(
      pal[((seq_along(panel_levels) - 1) %% length(pal)) + 1],
      panel_levels
    )

    # Add per-panel color to stats_df for mean lines
    stats_df$panel_color <- color_map[as.character(stats_df$panel)]

    p <- ggplot(plot_df, aes(x = time, y = value, color = panel)) +
      geom_line(linewidth = opts$lw * 0.8, show.legend = FALSE) +
      facet_wrap(~ panel, ncol = 5, scales = "fixed") +
      scale_color_manual(values = color_map) +
      geom_hline(data = stats_df, aes(yintercept = mean_val),
                 linetype = "dashed", color = stats_df$panel_color, linewidth = 0.3) +
      geom_label(data = stats_df,
                 aes(x = ann_x, y = ann_y,
                     label = ann_label, hjust = ann_hjust, vjust = ann_vjust),
                 size = 2.2, color = stats_df$panel_color,
                 fill = alpha("white", 0.92), label.size = 0.15,
                 label.padding = unit(3, "pt"), lineheight = 1.1,
                 show.legend = FALSE) +
      theme_academic(base_size = opts$label_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
        strip.text = element_text(size = opts$label_size - 1, face = "bold",
                                  color = "#1a1714", margin = margin(4, 0, 4, 0)),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 10))
      ) +
      labs(x = "Time (s)", y = y_label, title = title)

    p
  }

  output$slug_plot_pt1_all <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slug_data, "PT PIPE 1 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
  }, res = 96)

  output$slug_plot_pt7_all <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slug_data, "PT PIPE 7 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
  }, res = 96)

  output$slug_plot_qlt_all <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slug_data, "QLT PIPE 7 Trend",
      "Liquid Flow Rate (m\u00b3/d)",
      "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
      var_symbol = "Q")
  }, res = 96)

  # ── Appendix: Slug Length All Years ─────────────────────
  output$slug_plot_length_all <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    wc <- slug_water_cuts()

    sheet <- slug_rv$slugtrack_data[["LSLEXP PIPE 6 Trend"]]
    if (is.null(sheet)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "LSLEXP PIPE 6 Trend sheet not found", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    all_data <- list()
    for (yr in 1:10) {
      y_col <- (yr - 1) * 2 + 2
      if (y_col > ncol(sheet)) next
      v <- suppressWarnings(as.numeric(sheet[[y_col]]))
      v <- v[!is.na(v) & v > 0]
      if (length(v) == 0) next
      all_data[[length(all_data) + 1]] <- data.frame(
        year_label = paste0("Year ", yr, "\n(", round(wc[yr]), "% WC)"),
        year = yr, length = v, stringsAsFactors = FALSE
      )
    }

    if (length(all_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "No slug length data available", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    plot_df <- do.call(rbind, all_data)
    plot_df$year_label <- factor(plot_df$year_label,
      levels = paste0("Year ", 1:10, "\n(", round(wc), "% WC)"))

    p <- ggplot(plot_df, aes(x = year_label, y = length)) +
      geom_violin(fill = alpha(pal[1], 0.15), color = pal[1], linewidth = 0.4) +
      geom_boxplot(width = 0.15, fill = alpha(pal[2], 0.3), color = pal[2],
                   outlier.size = 0.8, outlier.alpha = 0.4) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
                   fill = pal[3], color = "black", stroke = 0.4) +
      theme_academic(base_size = opts$text_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
        axis.title = element_text(size = opts$label_size),
        axis.text = element_text(size = opts$text_size),
        axis.text.x = element_text(lineheight = 1.1)
      ) +
      labs(x = NULL, y = "Slug Body Length (m)",
           title = "Slug Body Length Distribution \u2014 All Years")

    p
  }, res = 96)

  # ── Fig 4.12: Slug Frequency ────────────────────────────
  output$slug_plot_freq <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    wc <- slug_water_cuts()

    # Extract NSLUG for each case: slug frequency = max(NSLUG) / total_time * 3600
    sheet <- slug_rv$slugtrack_data[["NSLUG Trend"]]
    if (is.null(sheet)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "NSLUG Trend sheet not found", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    freq_data <- data.frame(year = integer(), wc = numeric(), freq = numeric())
    for (i in 1:10) {
      x_col <- (i - 1) * 2 + 1
      y_col <- x_col + 1
      if (y_col > ncol(sheet)) next
      t <- suppressWarnings(as.numeric(sheet[[x_col]]))
      n <- suppressWarnings(as.numeric(sheet[[y_col]]))
      valid <- !is.na(t) & !is.na(n)
      t <- t[valid]; n <- n[valid]
      if (length(t) < 2) next

      total_time_s <- max(t) - min(t)
      # Slug frequency: count how many new slugs appear per hour
      # NSLUG is cumulative count, so frequency = delta_NSLUG / delta_time * 3600
      delta_n <- max(n) - min(n)
      slugs_per_hour <- if (total_time_s > 0) delta_n / total_time_s * 3600 else 0

      freq_data <- rbind(freq_data, data.frame(year = i, wc = wc[i], freq = slugs_per_hour))
    }

    if (nrow(freq_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "No NSLUG data available", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    p <- ggplot(freq_data, aes(x = wc, y = freq)) +
      geom_col(fill = pal[1], color = "black", width = 4, linewidth = 0.3) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
                  color = pal[2], linetype = "dashed", linewidth = 0.7) +
      geom_point(size = 2.5, color = pal[1], shape = 21, fill = pal[1], stroke = 0.5) +
      geom_text(aes(label = paste0("Y", year)), vjust = -0.8, size = 2.8, color = "#555") +
      theme_academic(base_size = opts$text_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
        axis.title = element_text(size = opts$label_size),
        axis.text = element_text(size = opts$text_size)
      ) +
      labs(x = "Water Cut (%)", y = "Slug Frequency (slugs/hour)",
           title = "Slug Frequency Trend Over Field Life") +
      scale_x_continuous(breaks = wc, expand = expansion(mult = 0.08))

    p
  }, res = 96)

  # ── Fig 4.13: Slug Body Length ──────────────────────────
  output$slug_plot_length <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    wc <- slug_water_cuts()

    sheet <- slug_rv$slugtrack_data[["LSLEXP PIPE 6 Trend"]]
    if (is.null(sheet)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "LSLEXP PIPE 6 Trend sheet not found", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    # Selected years for box plot: 1, 3, 5, 7, 10
    sel_years <- c(1, 3, 5, 7, 10)
    all_data <- list()
    for (yr in sel_years) {
      x_col <- (yr - 1) * 2 + 1
      y_col <- x_col + 1
      if (y_col > ncol(sheet)) next
      v <- suppressWarnings(as.numeric(sheet[[y_col]]))
      v <- v[!is.na(v) & v > 0]  # exclude zero-length entries
      if (length(v) == 0) next
      all_data[[length(all_data) + 1]] <- data.frame(
        year_label = paste0("Year ", yr, "\n(", wc[yr], "% WC)"),
        year = yr,
        length = v,
        stringsAsFactors = FALSE
      )
    }

    if (length(all_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "No slug length data available", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    plot_df <- do.call(rbind, all_data)
    plot_df$year_label <- factor(plot_df$year_label,
      levels = paste0("Year ", sel_years, "\n(", wc[sel_years], "% WC)"))

    p <- ggplot(plot_df, aes(x = year_label, y = length)) +
      geom_violin(fill = alpha(pal[1], 0.15), color = pal[1], linewidth = 0.4) +
      geom_boxplot(width = 0.15, fill = alpha(pal[2], 0.3), color = pal[2],
                   outlier.size = 1, outlier.alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                   fill = pal[3], color = "black", stroke = 0.5) +
      theme_academic(base_size = opts$text_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
        axis.title = element_text(size = opts$label_size),
        axis.text = element_text(size = opts$text_size),
        axis.text.x = element_text(lineheight = 1.1)
      ) +
      labs(x = NULL, y = "Slug Body Length (m)",
           title = "Slug Body Length Distribution")

    p
  }, res = 96)

  # ── Fig 4.14: Oscillation Amplitude Summary ─────────────
  output$slug_plot_amplitude <- renderPlot({
    req(slug_rv$loaded)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    wc <- slug_water_cuts()

    # Calculate peak-to-peak amplitudes for all 10 years
    # Use full data range (720s window as per spec) or available data
    amp_data <- data.frame(year = integer(), wc = numeric(),
                           pt1_amp = numeric(), pt7_amp = numeric(), qlt_amp = numeric())

    for (i in 1:10) {
      pt1_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 1 Trend", i, 0, 7200)
      pt7_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 7 Trend", i, 0, 7200)
      qlt_d <- extract_slug_case(slug_rv$slug_data, "QLT PIPE 7 Trend", i, 0, 7200)

      pt1_amp <- if (!is.null(pt1_d)) max(pt1_d$value) - min(pt1_d$value) else NA
      pt7_amp <- if (!is.null(pt7_d)) max(pt7_d$value) - min(pt7_d$value) else NA
      qlt_amp <- if (!is.null(qlt_d)) max(qlt_d$value) - min(qlt_d$value) else NA

      amp_data <- rbind(amp_data, data.frame(
        year = i, wc = wc[i],
        pt1_amp = pt1_amp, pt7_amp = pt7_amp, qlt_amp = qlt_amp
      ))
    }

    # Reshape for grouped bar chart
    # Left axis: PT amplitudes (bara); Right axis: QLT amplitude (m3/d)
    bar_df <- rbind(
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$pt1_amp, metric = "Pressure in PIPE-1 (bara)",
                 stringsAsFactors = FALSE),
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$pt7_amp, metric = "Pressure in PIPE-7 (bara)",
                 stringsAsFactors = FALSE),
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$qlt_amp, metric = "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)",
                 stringsAsFactors = FALSE)
    )
    bar_df <- bar_df[!is.na(bar_df$value), ]

    if (nrow(bar_df) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "No amplitude data available", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    # Since QLT values are much larger than PT, use faceted layout
    # with free y-scales instead of dual-axis (cleaner for publication)
    bar_df$metric <- factor(bar_df$metric,
      levels = c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)",
                 "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
    bar_df$year_label <- paste0("Y", bar_df$year)
    bar_df$year_label <- factor(bar_df$year_label, levels = paste0("Y", 1:10))

    p <- ggplot(bar_df, aes(x = year_label, y = value, fill = metric)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7,
               color = "black", linewidth = 0.2) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = setNames(pal[1:3],
        c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))) +
      theme_academic(base_size = opts$text_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
        strip.text = element_text(size = opts$label_size * 0.85, face = "bold",
                                   color = "#1a1714", margin = margin(t = 3, b = 3)),
        panel.spacing = unit(12, "pt"),
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
        axis.title = element_text(size = opts$label_size),
        axis.text = element_text(size = opts$text_size),
        legend.position = "none"
      ) +
      labs(x = "Production Year", y = "Peak-to-Peak Amplitude",
           title = "Oscillation Amplitude Summary Across Field Life")

    p
  }, res = 96)

  # ── Metrics Table ───────────────────────────────────────
  output$slug_metrics_table <- renderTable({
    req(slug_rv$loaded)
    wc <- slug_water_cuts()

    metrics <- data.frame(
      Year = integer(), `Water Cut (%)` = numeric(),
      `PT1 Mean (bara)` = numeric(), `PT1 Std (bar)` = numeric(), `PT1 P-P (bar)` = numeric(),
      `PT7 Mean (bara)` = numeric(), `PT7 Std (bar)` = numeric(), `PT7 P-P (bar)` = numeric(),
      `QLT Mean (m3/d)` = numeric(), `QLT Std (m3/d)` = numeric(), `QLT P-P (m3/d)` = numeric(),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    for (i in 1:10) {
      pt1_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 1 Trend", i, 0, 7200)
      pt7_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 7 Trend", i, 0, 7200)
      qlt_d <- extract_slug_case(slug_rv$slug_data, "QLT PIPE 7 Trend", i, 0, 7200)

      row <- data.frame(
        Year = i,
        `Water Cut (%)` = wc[i],
        `PT1 Mean (bara)` = if (!is.null(pt1_d)) round(mean(pt1_d$value), 2) else NA,
        `PT1 Std (bar)` = if (!is.null(pt1_d)) round(sd(pt1_d$value), 3) else NA,
        `PT1 P-P (bar)` = if (!is.null(pt1_d)) round(max(pt1_d$value) - min(pt1_d$value), 3) else NA,
        `PT7 Mean (bara)` = if (!is.null(pt7_d)) round(mean(pt7_d$value), 2) else NA,
        `PT7 Std (bar)` = if (!is.null(pt7_d)) round(sd(pt7_d$value), 3) else NA,
        `PT7 P-P (bar)` = if (!is.null(pt7_d)) round(max(pt7_d$value) - min(pt7_d$value), 3) else NA,
        `QLT Mean (m3/d)` = if (!is.null(qlt_d)) round(mean(qlt_d$value), 1) else NA,
        `QLT Std (m3/d)` = if (!is.null(qlt_d)) round(sd(qlt_d$value), 1) else NA,
        `QLT P-P (m3/d)` = if (!is.null(qlt_d)) round(max(qlt_d$value) - min(qlt_d$value), 1) else NA,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      metrics <- rbind(metrics, row)
    }

    metrics
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%", digits = 3)

  # ══════════════════════════════════════════════════════
  #  SLUGTRACKING TREND PLOTS
  # ══════════════════════════════════════════════════════

  # ── ST: PT PIPE-1 (3-panel) ──────────────────────────
  output$st_plot_pt1 <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slugtrack_data, "PT PIPE 1 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, ann_pos = "bottom_right"
    )
  }, res = 96)

  # ── ST: PT PIPE-7 (3-panel) ──────────────────────────
  output$st_plot_pt7 <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slugtrack_data, "PT PIPE 7 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, ann_pos = "top_right"
    )
  }, res = 96)

  # ── ST: QLT PIPE-7 (3-panel) ─────────────────────────
  output$st_plot_qlt <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_timeseries(
      slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
      "Liquid Flow Rate (m\u00b3/d)",
      "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
      slug_panels(),
      input$slug_t_start %||% 0,
      input$slug_t_window %||% 1800,
      pal, opts, var_symbol = "Q", ann_pos = "auto"
    )
  }, res = 96)

  # ── ST: PT PIPE-1 All Years ──────────────────────────
  output$st_plot_pt1_all <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 1 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
  }, res = 96)

  # ── ST: PT PIPE-7 All Years ──────────────────────────
  output$st_plot_pt7_all <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 7 Trend",
      "Pressure (bara)",
      "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
  }, res = 96)

  # ── ST: QLT PIPE-7 All Years ─────────────────────────
  output$st_plot_qlt_all <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    build_slug_all_years(slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
      "Liquid Flow Rate (m\u00b3/d)",
      "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
      input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
      var_symbol = "Q")
  }, res = 96)

  # ── ST: Oscillation Amplitude Summary ────────────────
  output$st_plot_amplitude <- renderPlot({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )
    wc <- slug_water_cuts()

    amp_data <- data.frame(year = integer(), wc = numeric(),
                           pt1_amp = numeric(), pt7_amp = numeric(), qlt_amp = numeric())

    for (i in 1:10) {
      pt1_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 1 Trend", i, 0, 7200)
      pt7_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 7 Trend", i, 0, 7200)
      qlt_d <- extract_slug_case(slug_rv$slugtrack_data, "QLT PIPE 7 Trend", i, 0, 7200)

      pt1_amp <- if (!is.null(pt1_d)) max(pt1_d$value) - min(pt1_d$value) else NA
      pt7_amp <- if (!is.null(pt7_d)) max(pt7_d$value) - min(pt7_d$value) else NA
      qlt_amp <- if (!is.null(qlt_d)) max(qlt_d$value) - min(qlt_d$value) else NA

      amp_data <- rbind(amp_data, data.frame(
        year = i, wc = wc[i],
        pt1_amp = pt1_amp, pt7_amp = pt7_amp, qlt_amp = qlt_amp
      ))
    }

    bar_df <- rbind(
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$pt1_amp, metric = "Pressure in PIPE-1 (bara)",
                 stringsAsFactors = FALSE),
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$pt7_amp, metric = "Pressure in PIPE-7 (bara)",
                 stringsAsFactors = FALSE),
      data.frame(year = amp_data$year, wc = amp_data$wc,
                 value = amp_data$qlt_amp, metric = "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)",
                 stringsAsFactors = FALSE)
    )
    bar_df <- bar_df[!is.na(bar_df$value), ]

    if (nrow(bar_df) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
        label = "No amplitude data available", size = 5, color = "#999") +
        xlim(0, 1) + ylim(0, 1) + theme_void())
    }

    bar_df$metric <- factor(bar_df$metric,
      levels = c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)",
                 "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
    bar_df$year_label <- paste0("Y", bar_df$year)
    bar_df$year_label <- factor(bar_df$year_label, levels = paste0("Y", 1:10))

    p <- ggplot(bar_df, aes(x = year_label, y = value, fill = metric)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7,
               color = "black", linewidth = 0.2) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = setNames(pal[1:3],
        c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))) +
      theme_academic(base_size = opts$text_size, grid = opts$grid,
                     border = TRUE, ticks_inward = TRUE) +
      theme(
        strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
        strip.text = element_text(size = opts$label_size * 0.85, face = "bold",
                                   color = "#1a1714", margin = margin(t = 3, b = 3)),
        panel.spacing = unit(12, "pt"),
        plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
        axis.title = element_text(size = opts$label_size),
        axis.text = element_text(size = opts$text_size),
        legend.position = "none"
      ) +
      labs(x = "Production Year", y = "Peak-to-Peak Amplitude",
           title = "Oscillation Amplitude Summary \u2014 Slugtracking")

    p
  }, res = 96)

  # ── ST: Metrics Table ────────────────────────────────
  output$st_metrics_table <- renderTable({
    req(slug_rv$loaded, slug_rv$slugtrack_data)
    wc <- slug_water_cuts()

    metrics <- data.frame(
      Year = integer(), `Water Cut (%)` = numeric(),
      `PT1 Mean (bara)` = numeric(), `PT1 Std (bar)` = numeric(), `PT1 P-P (bar)` = numeric(),
      `PT7 Mean (bara)` = numeric(), `PT7 Std (bar)` = numeric(), `PT7 P-P (bar)` = numeric(),
      `QLT Mean (m3/d)` = numeric(), `QLT Std (m3/d)` = numeric(), `QLT P-P (m3/d)` = numeric(),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    for (i in 1:10) {
      pt1_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 1 Trend", i, 0, 7200)
      pt7_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 7 Trend", i, 0, 7200)
      qlt_d <- extract_slug_case(slug_rv$slugtrack_data, "QLT PIPE 7 Trend", i, 0, 7200)

      row <- data.frame(
        Year = i,
        `Water Cut (%)` = wc[i],
        `PT1 Mean (bara)` = if (!is.null(pt1_d)) round(mean(pt1_d$value), 2) else NA,
        `PT1 Std (bar)` = if (!is.null(pt1_d)) round(sd(pt1_d$value), 3) else NA,
        `PT1 P-P (bar)` = if (!is.null(pt1_d)) round(max(pt1_d$value) - min(pt1_d$value), 3) else NA,
        `PT7 Mean (bara)` = if (!is.null(pt7_d)) round(mean(pt7_d$value), 2) else NA,
        `PT7 Std (bar)` = if (!is.null(pt7_d)) round(sd(pt7_d$value), 3) else NA,
        `PT7 P-P (bar)` = if (!is.null(pt7_d)) round(max(pt7_d$value) - min(pt7_d$value), 3) else NA,
        `QLT Mean (m3/d)` = if (!is.null(qlt_d)) round(mean(qlt_d$value), 1) else NA,
        `QLT Std (m3/d)` = if (!is.null(qlt_d)) round(sd(qlt_d$value), 1) else NA,
        `QLT P-P (m3/d)` = if (!is.null(qlt_d)) round(max(qlt_d$value) - min(qlt_d$value), 1) else NA,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      metrics <- rbind(metrics, row)
    }

    metrics
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", width = "100%", digits = 3)

  # ── Slug plot builder for active tab (used by export) ───
  build_slug_active_plot <- reactive({
    req(slug_rv$loaded)
    tab <- input$slug_subtab
    pal <- PALETTES[[input$slug_palette %||% "Nature"]]
    opts <- list(
      lw = input$slug_line_weight %||% 0.6,
      title_size = input$slug_title_size %||% 14,
      label_size = input$slug_label_size %||% 11,
      text_size = input$slug_text_size %||% 9,
      grid = input$slug_grid %||% "none"
    )

    if (tab == "PT PIPE-1 (Fig 4.10)") {
      build_slug_timeseries(slug_rv$slug_data, "PT PIPE 1 Trend",
        "Pressure (bara)", "Pressure Oscillations at Pipeline Inlet (PIPE-1)",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        ann_pos = "bottom_right")
    } else if (tab == "PT PIPE-7 (Fig 4.10-B)") {
      build_slug_timeseries(slug_rv$slug_data, "PT PIPE 7 Trend",
        "Pressure (bara)", "Pressure Oscillations at Separator Inlet (PIPE-7)",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        ann_pos = "top_right")
    } else if (tab == "QLT PIPE-7 (Fig 4.11)") {
      build_slug_timeseries(slug_rv$slug_data, "QLT PIPE 7 Trend",
        "Liquid Flow Rate (m\u00b3/d)",
        "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7)",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        var_symbol = "Q", ann_pos = "auto")
    } else if (tab == "PT PIPE-1 All Years") {
      build_slug_all_years(slug_rv$slug_data, "PT PIPE 1 Trend",
        "Pressure (bara)",
        "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
    } else if (tab == "PT PIPE-7 All Years") {
      build_slug_all_years(slug_rv$slug_data, "PT PIPE 7 Trend",
        "Pressure (bara)",
        "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
    } else if (tab == "QLT PIPE-7 All Years") {
      build_slug_all_years(slug_rv$slug_data, "QLT PIPE 7 Trend",
        "Liquid Flow Rate (m\u00b3/d)",
        "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        var_symbol = "Q")
    } else if (tab == "ST: PT PIPE-1") {
      build_slug_timeseries(slug_rv$slugtrack_data, "PT PIPE 1 Trend",
        "Pressure (bara)", "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        ann_pos = "bottom_right")
    } else if (tab == "ST: PT PIPE-7") {
      build_slug_timeseries(slug_rv$slugtrack_data, "PT PIPE 7 Trend",
        "Pressure (bara)", "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        ann_pos = "top_right")
    } else if (tab == "ST: QLT PIPE-7") {
      build_slug_timeseries(slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
        "Liquid Flow Rate (m\u00b3/d)",
        "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
        slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        var_symbol = "Q", ann_pos = "auto")
    } else if (tab == "ST: PT PIPE-1 All Years") {
      build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 1 Trend",
        "Pressure (bara)",
        "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
    } else if (tab == "ST: PT PIPE-7 All Years") {
      build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 7 Trend",
        "Pressure (bara)",
        "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
    } else if (tab == "ST: QLT PIPE-7 All Years") {
      build_slug_all_years(slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
        "Liquid Flow Rate (m\u00b3/d)",
        "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
        input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
        var_symbol = "Q")
    } else {
      NULL
    }
  })

  # ── Slug Export ─────────────────────────────────────────
  output$download_slug_plot <- downloadHandler(
    filename = function() {
      paste0(input$slug_export_filename, ".", input$slug_export_fmt)
    },
    content = function(file) {
      # Re-render the active tab's plot
      tab <- input$slug_subtab
      pal <- PALETTES[[input$slug_palette %||% "Nature"]]
      opts <- list(
        lw = input$slug_line_weight %||% 0.6,
        title_size = input$slug_title_size %||% 14,
        label_size = input$slug_label_size %||% 11,
        text_size = input$slug_text_size %||% 9,
        grid = input$slug_grid %||% "none"
      )
      wc <- slug_water_cuts()

      p <- if (tab == "PT PIPE-1 (Fig 4.10)") {
        build_slug_timeseries(slug_rv$slug_data, "PT PIPE 1 Trend",
          "Pressure (bara)", "Pressure Oscillations at Pipeline Inlet (PIPE-1)",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          ann_pos = "bottom_right")
      } else if (tab == "PT PIPE-7 (Fig 4.10-B)") {
        build_slug_timeseries(slug_rv$slug_data, "PT PIPE 7 Trend",
          "Pressure (bara)", "Pressure Oscillations at Separator Inlet (PIPE-7)",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          ann_pos = "top_right")
      } else if (tab == "QLT PIPE-7 (Fig 4.11)") {
        build_slug_timeseries(slug_rv$slug_data, "QLT PIPE 7 Trend",
          "Liquid Flow Rate (m\u00b3/d)",
          "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7)",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          var_symbol = "Q", ann_pos = "auto")
      } else if (tab == "PT PIPE-1 All Years") {
        build_slug_all_years(slug_rv$slug_data, "PT PIPE 1 Trend",
          "Pressure (bara)",
          "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
      } else if (tab == "PT PIPE-7 All Years") {
        build_slug_all_years(slug_rv$slug_data, "PT PIPE 7 Trend",
          "Pressure (bara)",
          "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
      } else if (tab == "QLT PIPE-7 All Years") {
        build_slug_all_years(slug_rv$slug_data, "QLT PIPE 7 Trend",
          "Liquid Flow Rate (m\u00b3/d)",
          "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          var_symbol = "Q")
      } else if (tab == "Slug Length All Years") {
        # Rebuild slug length all-years plot inline
        sheet <- slug_rv$slugtrack_data[["LSLEXP PIPE 6 Trend"]]
        if (is.null(sheet)) return()
        all_data <- list()
        for (yr in 1:10) {
          y_col <- (yr - 1) * 2 + 2
          if (y_col > ncol(sheet)) next
          v <- suppressWarnings(as.numeric(sheet[[y_col]]))
          v <- v[!is.na(v) & v > 0]
          if (length(v) == 0) next
          all_data[[length(all_data) + 1]] <- data.frame(
            year_label = paste0("Year ", yr, "\n(", round(wc[yr]), "% WC)"),
            year = yr, length = v)
        }
        if (length(all_data) == 0) return()
        plot_df <- do.call(rbind, all_data)
        plot_df$year_label <- factor(plot_df$year_label,
          levels = paste0("Year ", 1:10, "\n(", round(wc), "% WC)"))
        ggplot(plot_df, aes(x = year_label, y = length)) +
          geom_violin(fill = alpha(pal[1], 0.15), color = pal[1], linewidth = 0.4) +
          geom_boxplot(width = 0.15, fill = alpha(pal[2], 0.3), color = pal[2],
                       outlier.size = 0.8, outlier.alpha = 0.4) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
                       fill = pal[3], color = "black", stroke = 0.4) +
          theme_academic(base_size = opts$text_size, grid = opts$grid, border = TRUE, ticks_inward = TRUE) +
          theme(plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5),
                axis.title = element_text(size = opts$label_size), axis.text = element_text(size = opts$text_size),
                axis.text.x = element_text(lineheight = 1.1)) +
          labs(x = NULL, y = "Slug Body Length (m)",
               title = "Slug Body Length Distribution \u2014 All Years")
      } else if (tab == "Slug Frequency (Fig 4.12)") {
        # Rebuild frequency plot
        sheet <- slug_rv$slugtrack_data[["NSLUG Trend"]]
        if (is.null(sheet)) return()
        freq_data <- data.frame(year = integer(), wc = numeric(), freq = numeric())
        for (i in 1:10) {
          x_col <- (i - 1) * 2 + 1; y_col <- x_col + 1
          if (y_col > ncol(sheet)) next
          t <- suppressWarnings(as.numeric(sheet[[x_col]]))
          n <- suppressWarnings(as.numeric(sheet[[y_col]]))
          valid <- !is.na(t) & !is.na(n); t <- t[valid]; n <- n[valid]
          if (length(t) < 2) next
          total_time_s <- max(t) - min(t)
          delta_n <- max(n) - min(n)
          slugs_per_hour <- if (total_time_s > 0) delta_n / total_time_s * 3600 else 0
          freq_data <- rbind(freq_data, data.frame(year = i, wc = wc[i], freq = slugs_per_hour))
        }
        if (nrow(freq_data) == 0) return()
        ggplot(freq_data, aes(x = wc, y = freq)) +
          geom_col(fill = pal[1], color = "black", width = 4, linewidth = 0.3) +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
                      color = pal[2], linetype = "dashed", linewidth = 0.7) +
          geom_point(size = 2.5, color = pal[1], shape = 21, fill = pal[1], stroke = 0.5) +
          geom_text(aes(label = paste0("Y", year)), vjust = -0.8, size = 2.8, color = "#555") +
          theme_academic(base_size = opts$text_size, grid = opts$grid, border = TRUE, ticks_inward = TRUE) +
          theme(plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5),
                axis.title = element_text(size = opts$label_size), axis.text = element_text(size = opts$text_size)) +
          labs(x = "Water Cut (%)", y = "Slug Frequency (slugs/hour)",
               title = "Slug Frequency Trend Over Field Life") +
          scale_x_continuous(breaks = wc, expand = expansion(mult = 0.08))
      } else if (tab == "Slug Length (Fig 4.13)") {
        sheet <- slug_rv$slugtrack_data[["LSLEXP PIPE 6 Trend"]]
        if (is.null(sheet)) return()
        sel_years <- c(1, 3, 5, 7, 10)
        all_data <- list()
        for (yr in sel_years) {
          y_col <- (yr - 1) * 2 + 2
          if (y_col > ncol(sheet)) next
          v <- suppressWarnings(as.numeric(sheet[[y_col]]))
          v <- v[!is.na(v) & v > 0]
          if (length(v) == 0) next
          all_data[[length(all_data) + 1]] <- data.frame(
            year_label = paste0("Year ", yr, "\n(", wc[yr], "% WC)"), year = yr, length = v)
        }
        if (length(all_data) == 0) return()
        plot_df <- do.call(rbind, all_data)
        plot_df$year_label <- factor(plot_df$year_label,
          levels = paste0("Year ", sel_years, "\n(", wc[sel_years], "% WC)"))
        ggplot(plot_df, aes(x = year_label, y = length)) +
          geom_violin(fill = alpha(pal[1], 0.15), color = pal[1], linewidth = 0.4) +
          geom_boxplot(width = 0.15, fill = alpha(pal[2], 0.3), color = pal[2],
                       outlier.size = 1, outlier.alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                       fill = pal[3], color = "black", stroke = 0.5) +
          theme_academic(base_size = opts$text_size, grid = opts$grid, border = TRUE, ticks_inward = TRUE) +
          theme(plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5),
                axis.title = element_text(size = opts$label_size), axis.text = element_text(size = opts$text_size)) +
          labs(x = NULL, y = "Slug Body Length (m)", title = "Slug Body Length Distribution")
      } else if (tab == "Amplitude Summary (Fig 4.14)") {
        amp_data <- data.frame(year = integer(), wc = numeric(),
                               pt1_amp = numeric(), pt7_amp = numeric(), qlt_amp = numeric())
        for (i in 1:10) {
          pt1_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 1 Trend", i, 0, 7200)
          pt7_d <- extract_slug_case(slug_rv$slug_data, "PT PIPE 7 Trend", i, 0, 7200)
          qlt_d <- extract_slug_case(slug_rv$slug_data, "QLT PIPE 7 Trend", i, 0, 7200)
          amp_data <- rbind(amp_data, data.frame(
            year = i, wc = wc[i],
            pt1_amp = if (!is.null(pt1_d)) max(pt1_d$value) - min(pt1_d$value) else NA,
            pt7_amp = if (!is.null(pt7_d)) max(pt7_d$value) - min(pt7_d$value) else NA,
            qlt_amp = if (!is.null(qlt_d)) max(qlt_d$value) - min(qlt_d$value) else NA))
        }
        bar_df <- rbind(
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$pt1_amp, metric = "Pressure in PIPE-1 (bara)"),
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$pt7_amp, metric = "Pressure in PIPE-7 (bara)"),
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$qlt_amp, metric = "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
        bar_df <- bar_df[!is.na(bar_df$value), ]
        if (nrow(bar_df) == 0) return()
        bar_df$metric <- factor(bar_df$metric, levels = c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
        bar_df$year_label <- factor(paste0("Y", bar_df$year), levels = paste0("Y", 1:10))
        ggplot(bar_df, aes(x = year_label, y = value, fill = metric)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.2) +
          facet_wrap(~ metric, ncol = 1, scales = "free_y") +
          scale_fill_manual(values = setNames(pal[1:3],
            c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))) +
          theme_academic(base_size = opts$text_size, grid = opts$grid, border = TRUE, ticks_inward = TRUE) +
          theme(strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
                strip.text = element_text(size = opts$label_size * 0.85, face = "bold", margin = margin(t = 3, b = 3)),
                plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5),
                axis.title = element_text(size = opts$label_size), axis.text = element_text(size = opts$text_size),
                legend.position = "none") +
          labs(x = "Production Year", y = "Peak-to-Peak Amplitude",
               title = "Oscillation Amplitude Summary Across Field Life")
      } else if (tab == "ST: PT PIPE-1") {
        build_slug_timeseries(slug_rv$slugtrack_data, "PT PIPE 1 Trend",
          "Pressure (bara)", "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          ann_pos = "bottom_right")
      } else if (tab == "ST: PT PIPE-7") {
        build_slug_timeseries(slug_rv$slugtrack_data, "PT PIPE 7 Trend",
          "Pressure (bara)", "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          ann_pos = "top_right")
      } else if (tab == "ST: QLT PIPE-7") {
        build_slug_timeseries(slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
          "Liquid Flow Rate (m\u00b3/d)",
          "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking",
          slug_panels(), input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          var_symbol = "Q", ann_pos = "auto")
      } else if (tab == "ST: PT PIPE-1 All Years") {
        build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 1 Trend",
          "Pressure (bara)",
          "Pressure Oscillations at Pipeline Inlet (PIPE-1) \u2014 Slugtracking All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
      } else if (tab == "ST: PT PIPE-7 All Years") {
        build_slug_all_years(slug_rv$slugtrack_data, "PT PIPE 7 Trend",
          "Pressure (bara)",
          "Pressure Oscillations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts)
      } else if (tab == "ST: QLT PIPE-7 All Years") {
        build_slug_all_years(slug_rv$slugtrack_data, "QLT PIPE 7 Trend",
          "Liquid Flow Rate (m\u00b3/d)",
          "Liquid Flow Rate Fluctuations at Separator Inlet (PIPE-7) \u2014 Slugtracking All Years",
          input$slug_t_start %||% 0, input$slug_t_window %||% 1800, pal, opts,
          var_symbol = "Q")
      } else if (tab == "ST: Amplitude Summary") {
        amp_data <- data.frame(year = integer(), wc = numeric(),
                               pt1_amp = numeric(), pt7_amp = numeric(), qlt_amp = numeric())
        for (i in 1:10) {
          pt1_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 1 Trend", i, 0, 7200)
          pt7_d <- extract_slug_case(slug_rv$slugtrack_data, "PT PIPE 7 Trend", i, 0, 7200)
          qlt_d <- extract_slug_case(slug_rv$slugtrack_data, "QLT PIPE 7 Trend", i, 0, 7200)
          amp_data <- rbind(amp_data, data.frame(
            year = i, wc = wc[i],
            pt1_amp = if (!is.null(pt1_d)) max(pt1_d$value) - min(pt1_d$value) else NA,
            pt7_amp = if (!is.null(pt7_d)) max(pt7_d$value) - min(pt7_d$value) else NA,
            qlt_amp = if (!is.null(qlt_d)) max(qlt_d$value) - min(qlt_d$value) else NA))
        }
        bar_df <- rbind(
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$pt1_amp, metric = "Pressure in PIPE-1 (bara)"),
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$pt7_amp, metric = "Pressure in PIPE-7 (bara)"),
          data.frame(year = amp_data$year, wc = amp_data$wc, value = amp_data$qlt_amp, metric = "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
        bar_df <- bar_df[!is.na(bar_df$value), ]
        if (nrow(bar_df) == 0) return()
        bar_df$metric <- factor(bar_df$metric, levels = c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))
        bar_df$year_label <- factor(paste0("Y", bar_df$year), levels = paste0("Y", 1:10))
        ggplot(bar_df, aes(x = year_label, y = value, fill = metric)) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.2) +
          facet_wrap(~ metric, ncol = 1, scales = "free_y") +
          scale_fill_manual(values = setNames(pal[1:3],
            c("Pressure in PIPE-1 (bara)", "Pressure in PIPE-7 (bara)", "Total Liquid Flowrate in PIPE-7 (m\u00b3/d)"))) +
          theme_academic(base_size = opts$text_size, grid = opts$grid, border = TRUE, ticks_inward = TRUE) +
          theme(strip.background = element_rect(fill = "white", color = "#1a1a1a", linewidth = 0.5),
                strip.text = element_text(size = opts$label_size * 0.85, face = "bold", margin = margin(t = 3, b = 3)),
                plot.title = element_text(size = opts$title_size, face = "bold", hjust = 0.5),
                axis.title = element_text(size = opts$label_size), axis.text = element_text(size = opts$text_size),
                legend.position = "none") +
          labs(x = "Production Year", y = "Peak-to-Peak Amplitude",
               title = "Oscillation Amplitude Summary \u2014 Slugtracking")
      } else {
        NULL
      }

      if (is.null(p)) {
        showNotification("Select a figure tab to export", type = "warning")
        return()
      }

      w <- input$slug_export_w %||% 7
      h <- input$slug_export_h %||% 8
      dpi <- input$slug_export_dpi %||% 300
      fmt <- input$slug_export_fmt %||% "pdf"

      if (fmt == "eps") {
        ggsave(file, plot = p, width = w, height = h, device = cairo_ps, bg = "white")
      } else {
        ggsave(file, plot = p, width = w, height = h, dpi = dpi,
               device = fmt, bg = "white")
      }
    }
  )

  # ══════════════════════════════════════════════════════
  #  RENDER PLOTS
  # ══════════════════════════════════════════════════════

  # Interactive (plotly) preview
  output$interactive_plot <- renderPlotly({
    p <- build_plot()
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToAdd = list("hoverclosest", "hovercompare"),
        modeBarButtonsToRemove = list("lasso2d", "select2d"),
        displaylogo = FALSE
      ) %>%
      plotly::layout(
        hoverlabel = list(bgcolor = "white", font = list(family = "Inter", size = 12)),
        legend = list(font = list(family = "Inter", size = 11))
      )
  })

  # Static ggplot2 preview (publication exact)
  output$static_plot <- renderPlot({
    build_plot()
  }, res = 96, execOnResize = TRUE)

  # ══════════════════════════════════════════════════════
  #  DATA TABLE
  # ══════════════════════════════════════════════════════
  output$data_table <- renderTable({
    df <- current_sheet()
    req(df)
    head(df, 100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%")

  # ══════════════════════════════════════════════════════
  #  EXPORT
  # ══════════════════════════════════════════════════════
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$export_filename, ".", input$export_format)
    },
    content = function(file) {
      p <- build_plot()
      w   <- input$export_width
      h   <- input$export_height
      dpi <- input$export_dpi
      fmt <- input$export_format

      if (fmt == "eps") {
        ggsave(file, plot = p, width = w, height = h, device = cairo_ps, bg = "white")
      } else {
        ggsave(file, plot = p, width = w, height = h, dpi = dpi,
               device = fmt, bg = "white")
      }
    }
  )

  # ════════════════════════════════════════════════════════
  #  CONTOUR MAPS
  # ════════════════════════════════════════════════════════

  # ── Sync export preset → width / height / dpi / format ──────────────────────
  observeEvent(input$cmap_preset, {
    p <- JOURNAL_PRESETS[[input$cmap_preset]]
    if (!is.null(p)) {
      updateNumericInput(session, "cmap_width",  value = p$w)
      updateNumericInput(session, "cmap_height", value = p$h)
      updateNumericInput(session, "cmap_dpi",    value = p$dpi)
      updateSelectInput(session,  "cmap_format", selected = p$fmt)
    }
  })

  # ── Interpolation grid (expensive — only reruns when interpolation params change) ──
  contour_grid <- reactive({
    req(GW_DATA)
    gw     <- GW_DATA
    method <- input$cmap_method
    res    <- input$cmap_grid_res
    power  <- if (method == "idw") input$cmap_idw_power else 2

    pad  <- 8
    xseq <- seq(floor(min(gw$Easting)  - pad), ceiling(max(gw$Easting)  + pad), by = res)
    yseq <- seq(floor(min(gw$Northing) - pad), ceiling(max(gw$Northing) + pad), by = res)

    # sp objects
    sp_bh <- SpatialPointsDataFrame(
      coords = gw[, c("Easting", "Northing")],
      data   = data.frame(GW_elev = gw[["GW_Elevation_mamsl"]])
    )

    grd <- expand.grid(Easting = xseq, Northing = yseq)
    coordinates(grd) <- ~Easting + Northing
    gridded(grd)     <- TRUE

    pred <- if (method == "kriging") {
      vfit <- tryCatch({
        v <- gstat::variogram(GW_elev ~ 1, sp_bh)
        gstat::fit.variogram(v, gstat::vgm("Sph"))
      }, error = function(e) gstat::vgm(0.5, "Sph", 50, 0.05))
      kg <- gstat::gstat(formula = GW_elev ~ 1, data = sp_bh, model = vfit)
      predict(kg, grd)
    } else {
      gstat::idw(GW_elev ~ 1, sp_bh, grd, idp = power)
    }

    df           <- as.data.frame(pred)
    df$GW_elev   <- df$var1.pred
    df
  }) |> bindCache(input$cmap_method, input$cmap_idw_power, input$cmap_grid_res)

  # ── Build the ggplot contour map ─────────────────────────────────────────────
  build_contour_map <- reactive({
    req(GW_DATA)
    input$cmap_refresh   # manual refresh trigger

    gw      <- GW_DATA
    napl    <- NAPL_DATA
    pred_df <- contour_grid()
    pal     <- colorRampPalette(CONTOUR_PALETTES[[input$cmap_palette]])(256)

    pad      <- 8
    x_lo     <- min(gw$Easting)  - pad;  x_hi <- max(gw$Easting)  + pad
    y_lo     <- min(gw$Northing) - pad;  y_hi <- max(gw$Northing) + pad
    z_lo     <- min(pred_df$GW_elev, na.rm = TRUE)
    z_hi     <- max(pred_df$GW_elev, na.rm = TRUE)
    brks     <- pretty(c(z_lo, z_hi), n = input$cmap_nlevels)

    p <- ggplot()

    # ── Layer 1: background site plan ────────────────────
    if (isTRUE(input$cmap_show_bg) && !is.null(BOREHOLE_PNG)) {
      alpha   <- input$cmap_bg_alpha
      bg      <- BOREHOLE_PNG
      # Blend with white to control opacity (works for RGB arrays)
      if (alpha < 1) bg <- alpha * bg + (1 - alpha)
      p <- p + annotation_raster(
        bg,
        xmin = IMG_EXTENT$xmin, xmax = IMG_EXTENT$xmax,
        ymin = IMG_EXTENT$ymin, ymax = IMG_EXTENT$ymax,
        interpolate = TRUE
      )
    }

    # ── Layer 2: filled contour surface ──────────────────
    if (isTRUE(input$cmap_show_filled)) {
      p <- p + geom_raster(
        data        = pred_df,
        aes(x = Easting, y = Northing, fill = GW_elev),
        interpolate = TRUE,
        alpha       = input$cmap_fill_alpha
      ) + scale_fill_gradientn(
        colours = pal,
        limits  = c(z_lo, z_hi),
        name    = "GW Elevation\n(mamsl)",
        guide   = guide_colorbar(
          barheight    = unit(6, "cm"),
          barwidth     = unit(0.45, "cm"),
          title.hjust  = 0.5,
          label.theme  = element_text(size = 8),
          title.theme  = element_text(size = 9, face = "bold")
        )
      )
    }

    # ── Layer 3: contour lines ────────────────────────────
    if (isTRUE(input$cmap_show_lines)) {
      p <- p + geom_contour(
        data      = pred_df,
        aes(x = Easting, y = Northing, z = GW_elev),
        breaks    = brks,
        colour    = input$cmap_line_col,
        linewidth = input$cmap_line_wt
      )
    }

    # ── Layer 4: site boundary ────────────────────────────
    if (isTRUE(input$cmap_show_boundary)) {
      p <- p + geom_path(
        data      = SITE_BOUNDARY,
        aes(x = E, y = N),
        colour    = "#1a1714",
        linewidth = 0.65,
        linetype  = "solid"
      )
    }

    # ── Layer 5: flow direction arrows ────────────────────
    if (isTRUE(input$cmap_show_flow)) {
      arrows <- compute_flow_arrows(gw)
      p <- p + geom_segment(
        data = arrows,
        aes(x = E, y = N, xend = E + dE, yend = N + dN),
        arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
        colour    = "#2a4f6e",
        linewidth = 0.45,
        alpha     = 0.85
      )
    }

    # ── Layer 6: all borehole markers ────────────────────
    if (isTRUE(input$cmap_show_boreholes)) {
      is_napl_bh <- if (!is.null(napl) && nrow(napl) > 0)
                      gw$Borehole %in% napl$Borehole
                    else
                      rep(FALSE, nrow(gw))

      # Regular boreholes
      reg_bh <- gw[!is_napl_bh, ]
      if (nrow(reg_bh) > 0) {
        p <- p + geom_point(
          data   = reg_bh,
          aes(x = Easting, y = Northing),
          shape  = 21, fill = "white", colour = "#1a1714",
          size   = input$cmap_bh_size, stroke = 0.65
        )
      }

      # NAPL boreholes (distinct marker)
      if (isTRUE(input$cmap_show_napl) && any(is_napl_bh)) {
        napl_bh <- gw[is_napl_bh, ]
        p <- p + geom_point(
          data   = napl_bh,
          aes(x = Easting, y = Northing),
          shape  = 24, fill = "#8b3a1e", colour = "#1a1714",
          size   = input$cmap_bh_size + 0.8, stroke = 0.7
        )
      }
    }

    # ── Layer 7: borehole ID labels ───────────────────────
    if (isTRUE(input$cmap_show_labels)) {
      p <- p + ggrepel::geom_text_repel(
        data              = gw,
        aes(x = Easting, y = Northing, label = Borehole),
        size              = input$cmap_lbl_size,
        fontface          = "bold",
        colour            = "#1a1714",
        bg.colour         = "white",
        bg.r              = 0.12,
        segment.colour    = "#555555",
        segment.size      = 0.3,
        segment.linetype  = "dashed",
        min.segment.length = 0.25,
        box.padding       = 0.35,
        point.padding     = 0.2,
        max.overlaps      = Inf,
        seed              = 42L
      )
    }

    # ── North arrow ───────────────────────────────────────
    nx <- x_hi - 2.5;  ny_base <- y_hi - 7
    p <- p +
      annotate("segment",
               x = nx, xend = nx, y = ny_base, yend = ny_base + 5,
               arrow     = arrow(length = unit(0.22, "cm"), type = "closed"),
               colour    = "#1a1714", linewidth = 0.75) +
      annotate("text",
               x = nx, y = ny_base - 1.8, label = "N",
               size = 3.5, fontface = "bold", colour = "#1a1714")

    # ── Scale bar ─────────────────────────────────────────
    sb_x0 <- x_lo + 1.5;  sb_y  <- y_lo + 2.5
    p <- p +
      annotate("segment",
               x = sb_x0, xend = sb_x0 + 20, y = sb_y, yend = sb_y,
               colour = "#1a1714", linewidth = 0.7) +
      annotate("segment",
               x = sb_x0,      xend = sb_x0,      y = sb_y - 0.6, yend = sb_y + 0.6,
               colour = "#1a1714", linewidth = 0.5) +
      annotate("segment",
               x = sb_x0 + 20, xend = sb_x0 + 20, y = sb_y - 0.6, yend = sb_y + 0.6,
               colour = "#1a1714", linewidth = 0.5) +
      annotate("text",
               x = sb_x0 + 10, y = sb_y + 2, label = "20 m",
               size = 3, colour = "#1a1714")

    # ── Coordinate system and theme ───────────────────────
    p <- p +
      coord_fixed(ratio = 1, xlim = c(x_lo, x_hi), ylim = c(y_lo, y_hi),
                  expand = FALSE) +
      labs(
        title    = input$cmap_title,
        subtitle = if (nchar(trimws(input$cmap_subtitle)) > 0)
                     input$cmap_subtitle else NULL,
        x        = "Easting (m)",
        y        = "Northing (m)"
      ) +
      theme_academic(base_size = input$cmap_axis_size,
                     grid = "none", border = TRUE) +
      theme(
        plot.title      = element_text(size  = input$cmap_title_size,
                                       face  = "bold", hjust = 0.5),
        plot.subtitle   = element_text(size  = rel(0.85), face = "italic",
                                       hjust = 0.5, colour = "#666666"),
        legend.position  = "right",
        legend.key.size  = unit(0.9, "lines"),
        legend.text      = element_text(size = 8),
        legend.title     = element_text(size = 9, face = "bold"),
        axis.title       = element_text(size = rel(0.9))
      )

    # ── Manual legend for NAPL marker ────────────────────
    if (isTRUE(input$cmap_show_napl) && !is.null(napl) && nrow(napl) > 0 &&
        isTRUE(input$cmap_show_boreholes)) {
      p <- p + annotate("point",
        x = x_lo - 999, y = y_lo - 999,    # off-canvas dummy
        shape = 24, fill = "#8b3a1e", colour = "#1a1714", size = 3)
    }

    p
  })

  # ── Render: static plot ──────────────────────────────────────────────────────
  output$cmap_static <- renderPlot({
    build_contour_map()
  }, res = 96, execOnResize = TRUE)

  # ── Render: interactive plotly ───────────────────────────────────────────────
  output$cmap_interactive <- renderPlotly({
    p <- build_contour_map()
    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      plotly::config(
        displayModeBar  = TRUE,
        displaylogo     = FALSE,
        modeBarButtonsToRemove = list("lasso2d", "select2d")
      ) |>
      plotly::layout(
        hoverlabel = list(bgcolor = "white",
                          font    = list(family = "Inter", size = 12))
      )
  })

  # ── Export handler ───────────────────────────────────────────────────────────
  output$cmap_download <- downloadHandler(
    filename = function() {
      paste0(input$cmap_filename, ".", input$cmap_format)
    },
    content  = function(file) {
      p   <- build_contour_map()
      w   <- input$cmap_width
      h   <- input$cmap_height
      dpi <- input$cmap_dpi
      fmt <- input$cmap_format
      if (fmt == "svg") {
        ggsave(file, plot = p, width = w, height = h, device = "svg",  bg = "white")
      } else {
        ggsave(file, plot = p, width = w, height = h, dpi = dpi,
               device = fmt, bg = "white")
      }
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
#  LAUNCH
# ═══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
