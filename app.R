# =============================================================================
# Gorham Fire Department — Station Optimization Dashboard
# ALY 6980 Capstone, Group C
#
# Reads a precomputed scenario bundle (shiny_bundle.json) produced by
# export_scenarios.py, and lets the sponsor explore station configurations
# interactively. No optimization runs here — sliders select precomputed
# scenarios, so the app responds instantly.
# =============================================================================

# install.packages(c("shiny", "bslib", "leaflet", "jsonlite", "dplyr", "scales", "rsconnect"))

library(shiny)
library(bslib)
library(leaflet)
library(jsonlite)
library(dplyr)
library(scales)
library(rsconnect)

# ── DATA SOURCE ──────────────────────────────────────────────────────────────
# Replace with your raw GitHub URL after committing the bundle.
BUNDLE_URL <- "https://raw.githubusercontent.com/Ithorian123/gofd-dashboard/refs/heads/main/shiny_bundle.json"

# Load once at startup (works for both a URL and a local path).
bundle <- jsonlite::fromJSON(BUNDLE_URL, simplifyVector = FALSE)

meta          <- bundle$meta
stations_meta <- bundle$stations_meta
scenarios     <- bundle$scenarios
baseline      <- bundle$baseline

# Fast lookup: candidate grid index -> (lon, lat, name)
station_lookup <- do.call(rbind, lapply(stations_meta, function(s) {
  data.frame(idx = s$idx, lon = s$lon, lat = s$lat,
             name = ifelse(is.null(s$name), NA_character_, s$name),
             stringsAsFactors = FALSE)
}))

dev_scale_steps <- unlist(meta$dev_scale_steps)
station_counts  <- unlist(meta$station_counts)

# ── PALETTE ──────────────────────────────────────────────────────────────────
# Fire-service inspired: deep engine red as the single accent against a
# disciplined slate / off-white system. Coverage uses a green→amber→red ramp.
C <- list(
  red    = "#B11A21",   # engine red — primary accent
  redDk  = "#7E1318",
  ink    = "#1C2230",   # near-black slate for text + headers
  slate  = "#3A4355",
  mute   = "#7B8494",
  line   = "#E2E6EC",
  panel  = "#FFFFFF",
  bg     = "#F4F6F9",
  good   = "#1A8A5A",   # within target
  mid    = "#E0A21A",   # marginal
  bad    = "#C0392B"    # over target
)

cov_pal <- function(x) {
  # x = effective minutes; bins tuned to NFPA-style thresholds.
  # Domain spans the full bin range so values up to the cap never fall outside it.
  colorBin(palette = c(C$good, "#7DBE3C", C$mid, "#E06B1A", C$bad),
           domain = c(0, 100), bins = c(0, 4, 8, 12, 20, 100),
           na.color = "#BBBBBB")(x)
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Gorham Fire Department — Station Optimization",
  theme = bs_theme(
    version = 5,
    bg = C$bg, fg = C$ink, primary = C$red,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter Tight"),
    "border-radius" = "0.5rem"
  ),
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 320,
    class = "p-3",
    
    div(class = "fw-bold text-uppercase",
        style = paste0("font-size:0.72rem; letter-spacing:0.08em; color:", C$mute, ";"),
        "Scenario controls"),
    
    # Number of stations
    div(style = "margin-top:0.6rem;",
        tags$label("Number of stations", class = "fw-semibold",
                   style = paste0("color:", C$ink, ";")),
        sliderInput("n_stations", NULL,
                    min = min(station_counts), max = max(station_counts),
                    value = 4, step = 1, ticks = TRUE, width = "100%")
    ),
    
    # Staffing
    div(style = "margin-top:0.4rem;",
        tags$label("Staffed stations", class = "fw-semibold",
                   style = paste0("color:", C$ink, ";")),
        radioButtons("staff", NULL,
                     choiceNames = c("Central only",
                                     "Central + 1 optimally chosen"),
                     choiceValues = c(1, 2), selected = 1)
    ),
    
    hr(style = paste0("border-color:", C$line, ";")),
    
    # Development projections
    div(
      tags$label("Include approved developments", class = "fw-semibold",
                 style = paste0("color:", C$ink, ";")),
      div(style = paste0("font-size:0.78rem; color:", C$mute, "; margin-bottom:0.3rem;"),
          sprintf("%s and %s",
                  names(meta$developments)[1], names(meta$developments)[2])),
      checkboxInput("dev_on", "Project future call volume from new units",
                    value = TRUE)
    ),
    
    conditionalPanel(
      condition = "input.dev_on == true",
      div(style = "margin-top:0.2rem;",
          tags$label("Projected demand scale", class = "fw-semibold",
                     style = paste0("color:", C$ink, ";")),
          div(style = paste0("font-size:0.78rem; color:", C$mute, "; margin-bottom:0.2rem;"),
              "100% = Gorham's observed call rate per dwelling"),
          # Slider bounds adapt to what's in the bundle. A quick export only has
          # the 100% step, so the slider locks there until the full sweep is run.
          sliderInput("dev_scale", NULL,
                      min = round(min(dev_scale_steps) * 100),
                      max = round(max(dev_scale_steps) * 100),
                      value = 100, step = 25,
                      post = "%", width = "100%"),
          if (length(dev_scale_steps) <= 1)
            div(style = paste0("font-size:0.72rem; color:", C$mute, "; font-style:italic;"),
                "Scale sweep not yet loaded — showing 100% projection.")
      )
    ),
    
    hr(style = paste0("border-color:", C$line, ";")),
    
    # Map coloring toggle
    div(
      tags$label("Map shading", class = "fw-semibold",
                 style = paste0("color:", C$ink, ";")),
      radioButtons("map_mode", NULL,
                   choiceNames = c("Response time (coverage)",
                                   "Station assignment"),
                   choiceValues = c("coverage", "assignment"),
                   selected = "coverage")
    ),
    
    checkboxInput("show_baseline", "Overlay current 6 stations", value = FALSE),
    
    div(style = paste0("margin-top:auto; font-size:0.7rem; color:", C$mute,
                       "; padding-top:1rem;"),
        sprintf("Every parcel reachable within %d min. Volunteer turnout penalty: %.1f min.",
                meta$coverage_minutes, meta$turnout_penalty_min))
  ),
  
  # ── Main area ──────────────────────────────────────────────────────────────
  # Top: stat cards. Middle: map. Bottom: comparison vs current.
  layout_columns(
    col_widths = 12,
    div(
      # Stat cards row
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Avg response time",
          value = textOutput("vb_avg"),
          showcase = NULL,
          theme = value_box_theme(bg = C$ink, fg = "#FFFFFF"),
          p(textOutput("vb_avg_delta"), class = "mb-0",
            style = "font-size:0.78rem; opacity:0.85;")
        ),
        value_box(
          title = "Within 8 minutes",
          value = textOutput("vb_p8"),
          theme = value_box_theme(bg = C$panel, fg = C$ink),
          p(textOutput("vb_p8_delta"), class = "mb-0",
            style = paste0("font-size:0.78rem; color:", C$mute, ";"))
        ),
        value_box(
          title = "Within 12 minutes",
          value = textOutput("vb_p12"),
          theme = value_box_theme(bg = C$panel, fg = C$ink),
          p(textOutput("vb_p12_delta"), class = "mb-0",
            style = paste0("font-size:0.78rem; color:", C$mute, ";"))
        ),
        value_box(
          title = "Stations staffed",
          value = textOutput("vb_staffed"),
          theme = value_box_theme(bg = C$red, fg = "#FFFFFF"),
          p("of selected configuration", class = "mb-0",
            style = "font-size:0.78rem; opacity:0.85;")
        )
      )
    )
  ),
  
  # Map card
  card(
    full_screen = TRUE,
    height = 480,
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
          span("Station configuration & coverage", class = "fw-semibold"),
          span(textOutput("map_caption", inline = TRUE),
               style = paste0("font-size:0.8rem; color:", C$mute, ";")))
    ),
    leafletOutput("map", height = "100%")
  ),
  
  # Legend + station list
  div(class = "mt-3",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(span("How to read the map", class = "fw-semibold")),
          uiOutput("legend_ui")
        ),
        card(
          card_header(span("Open stations this scenario", class = "fw-semibold")),
          tableOutput("station_table")
        )
      )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Resolve the slider state to a precomputed scenario key.
  current_key <- reactive({
    N  <- as.integer(input$n_stations)
    st <- as.integer(input$staff)
    if (isTRUE(input$dev_on)) {
      scl <- as.integer(input$dev_scale)
      sprintf("dev1_s%03d_N%d_st%d", scl, N, st)
    } else {
      sprintf("dev0_N%d_st%d", N, st)
    }
  })
  
  scn <- reactive({
    key <- current_key()
    s <- scenarios[[key]]
    if (is.null(s)) {
      showNotification(
        "This combination wasn't precomputed. Pick another setting.",
        type = "warning", duration = 4)
      req(FALSE)
    }
    s
  })
  
  # Parcel rows -> data frame  (lon, lat, eff_min, assigned_idx)
  parcels_df <- reactive({
    rows <- scn()$parcels
    m <- do.call(rbind, lapply(rows, function(r) unlist(r)))
    df <- as.data.frame(m)
    names(df) <- c("lon", "lat", "eff", "assigned")
    df
  })
  
  open_stations_df <- reactive({
    idx <- unlist(scn()$open_idx)
    staffed <- unlist(scn()$staffed_idx)
    df <- station_lookup[station_lookup$idx %in% idx, ]
    df$staffed <- df$idx %in% staffed
    df$label <- ifelse(is.na(df$name),
                       paste0("Proposed site (grid ", df$idx, ")"),
                       df$name)
    df
  })
  
  # ── Stat cards ──────────────────────────────────────────────────────────────
  m_overall <- reactive(scn()$metrics$overall)
  b_overall <- baseline$metrics$overall
  
  fmt_min <- function(x) sprintf("%.1f min", x)
  delta_txt <- function(now, base, unit = "min", invert = TRUE) {
    d <- now - base
    better <- if (invert) d < 0 else d > 0
    arrow <- if (d < 0) "▼" else if (d > 0) "▲" else "—"
    sprintf("%s %+.1f %s vs current", arrow, d, unit)
  }
  
  output$vb_avg       <- renderText(fmt_min(m_overall()$avg))
  output$vb_avg_delta <- renderText(delta_txt(m_overall()$avg, b_overall$avg))
  output$vb_p8        <- renderText(sprintf("%.1f%%", m_overall()$pct_8))
  output$vb_p8_delta  <- renderText(sprintf("current: %.1f%%", b_overall$pct_8))
  output$vb_p12       <- renderText(sprintf("%.1f%%", m_overall()$pct_12))
  output$vb_p12_delta <- renderText(sprintf("current: %.1f%%", b_overall$pct_12))
  output$vb_staffed   <- renderText(length(unlist(scn()$staffed_idx)))
  
  output$map_caption <- renderText({
    sprintf("%d stations  ·  %s",
            scn()$N,
            if (input$dev_on) paste0("developments at ", input$dev_scale, "%")
            else "current demand only")
  })
  
  # ── Map ─────────────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     preferCanvas = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = meta$map_center[[2]], lat = meta$map_center[[1]], zoom = 11)
  })
  
  observe({
    df  <- parcels_df()
    st  <- open_stations_df()
    mode <- input$map_mode
    
    # Parcel point colors
    if (mode == "coverage") {
      cols <- cov_pal(df$eff)
    } else {
      # color by assigned station
      assigned_ids <- sort(unique(df$assigned))
      pal <- colorFactor(
        palette = c("#B11A21", "#1F6FB2", "#1A8A5A", "#E0A21A",
                    "#7B4FB7", "#0F8C8C", "#C0392B", "#555555"),
        domain = assigned_ids)
      cols <- pal(df$assigned)
    }
    
    proxy <- leafletProxy("map") |>
      clearGroup("parcels") |>
      clearGroup("stations") |>
      clearGroup("baseline")
    
    proxy <- proxy |>
      addCircleMarkers(data = df, lng = ~lon, lat = ~lat,
                       radius = 3, stroke = FALSE, fillOpacity = 0.55,
                       fillColor = cols, group = "parcels",
                       label = ~sprintf("%.1f min", eff))
    
    # Baseline overlay (current stations) if requested
    if (isTRUE(input$show_baseline)) {
      b_idx <- unlist(baseline$open_idx)
      bdf <- station_lookup[station_lookup$idx %in% b_idx, ]
      proxy <- proxy |>
        addCircleMarkers(data = bdf, lng = ~lon, lat = ~lat,
                         radius = 9, color = "#FFFFFF", weight = 2,
                         fillColor = C$mute, fillOpacity = 0.9,
                         group = "baseline",
                         label = ~ifelse(is.na(name), "Current station", name))
    }
    
    # Open stations for this scenario — staffed are larger / filled red,
    # unstaffed are hollow.
    staffed_df  <- st[st$staffed, , drop = FALSE]
    unstaffed_df <- st[!st$staffed, , drop = FALSE]
    
    if (nrow(unstaffed_df) > 0) {
      proxy <- proxy |>
        addCircleMarkers(data = unstaffed_df, lng = ~lon, lat = ~lat,
                         radius = 9, color = C$redDk, weight = 3,
                         fillColor = "#FFFFFF", fillOpacity = 1,
                         group = "stations",
                         label = ~paste0(label, " (volunteer)"))
    }
    if (nrow(staffed_df) > 0) {
      proxy <- proxy |>
        addCircleMarkers(data = staffed_df, lng = ~lon, lat = ~lat,
                         radius = 13, color = "#FFFFFF", weight = 3,
                         fillColor = C$red, fillOpacity = 1,
                         group = "stations",
                         label = ~paste0(label, " (staffed)"))
    }
  })
  
  # ── Legend ──────────────────────────────────────────────────────────────────
  output$legend_ui <- renderUI({
    if (input$map_mode == "coverage") {
      bins <- list(
        c("≤ 4 min",  C$good),
        c("4–8 min",  "#7DBE3C"),
        c("8–12 min", C$mid),
        c("12–20 min","#E06B1A"),
        c("> 20 min", C$bad)
      )
      div(
        p(class = "mb-2", style = paste0("color:", C$slate, ";"),
          "Each dot is a sampled property, colored by its estimated response time (drive time plus a turnout penalty when the assigned station is volunteer)."),
        div(class = "d-flex flex-wrap gap-3",
            lapply(bins, function(b) {
              div(class = "d-flex align-items-center gap-2",
                  span(style = sprintf("width:14px;height:14px;border-radius:3px;background:%s;display:inline-block;", b[2])),
                  span(b[1], style = "font-size:0.85rem;"))
            })
        ),
        div(class = "d-flex align-items-center gap-3 mt-3",
            div(class = "d-flex align-items-center gap-2",
                span(style = sprintf("width:16px;height:16px;border-radius:50%%;background:%s;border:2px solid #fff;box-shadow:0 0 0 1px %s;display:inline-block;", C$red, C$redDk)),
                span("Staffed station", style = "font-size:0.85rem;")),
            div(class = "d-flex align-items-center gap-2",
                span(style = sprintf("width:14px;height:14px;border-radius:50%%;background:#fff;border:2px solid %s;display:inline-block;", C$redDk)),
                span("Volunteer station", style = "font-size:0.85rem;")))
      )
    } else {
      div(
        p(class = "mb-0", style = paste0("color:", C$slate, ";"),
          "Each dot is a sampled property, colored by which open station it is assigned to. Use this view to see each station's coverage territory.")
      )
    }
  })
  
  # ── Station table ────────────────────────────────────────────────────────────
  output$station_table <- renderTable({
    st <- open_stations_df()
    data.frame(
      Station = st$label,
      Role    = ifelse(st$staffed, "Staffed", "Volunteer"),
      Type    = ifelse(is.na(st$name), "Proposed new site", "Existing"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
}

shinyApp(ui, server)
