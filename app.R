# app.R <U+2014> Ultra-thin top bar, brand on left, compact nav on right

# ---- Packages & options ----
library(shiny)
library(bslib)

options(shiny.autoreload = TRUE)
options(shiny.autoreload.pattern = "\\.(r|R|csv|json|css|js)$")

# ---- Load globals & pages ----
if (file.exists("global.R")) source("global.R")

# Explicitly and visibly re-source, errors will appear in console and interrupt
reload_pages <- function() {
  if (!dir.exists("pages")) {
    return(invisible())
  }
  page_files <- list.files("pages", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  for (f in page_files) {
    message("sourcing: ", normalizePath(f))
    sys.source(f, envir = globalenv(), chdir = TRUE)
  }
}
reload_pages()

# ---- Ultra-thin Top Bar (no image) ----
topbar <- function() {
  div(
    class = "topbar d-flex align-items-center justify-content-between",
    # Left: logo + title
    div(
      class = "d-flex align-items-center gap-2",
      tags$img(src = "two-children-hiking.png", class = "brand-logo", alt = "BIOEXPLORER: Melbourne Summer Journey logo"),
      span(class = "topbar-brand", "BIOEXPLORER"),
      span(class = "topbar-brand2", "Melbounrne Summer Journey")
    ),
    # Right: compact buttons
    div(
      class = "nav nav-pills topbar-tabs",
      actionLink("go_home", "Home", class = "nav-link active"),
      actionLink("go_bio", "Biodiversity", class = "nav-link"),
      actionLink("go_plant", "Plant", class = "nav-link"),
      actionLink("go_butterfly", "Butterfly", class = "nav-link"),
      actionLink("go_bird", "Bird", class = "nav-link")
    )
  )
}


# ---- UI ----
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty",
    bg = "#fff",
    fg = "#1f2b3a",
    primary = "#2a9d8f"
  ),

  # Styles & behavior scripts
  tags$head(
    # Load fonts (before CSS)
    # Preconnect (optional but recommended)
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "true"),

    # Load fonts (required)
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Schoolbell&display=swap",
      rel = "stylesheet"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css"),
    tags$script(src = "index.js")
  ), setUpTableauInShiny(),

  # Ultra-thin top row
  div(class = "container-fluid px-100", topbar()),

  # Hidden actual tab content
  div(
    class = "container-fluid tab-body",
    tabsetPanel(
      id = "mainNav", type = "hidden",
      tabPanel("Home", map_home_ui("home")),
      tabPanel("Biodiversity", biodiversity_map_ui("biodiversity")),
      tabPanel("Plant", plant_map_ui("plant")),
      tabPanel("Butterfly", butterfly_map_ui("butterfly")),
      tabPanel("Bird", bird_map_ui("bird")) # New Home page
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Four modules
  biodiversity_map_server("biodiversity")
  plant_map_server("plant")
  butterfly_map_server("butterfly")
  bird_map_server("bird")
  map_home_server("home") # Start Home page

  # Right buttons <U+2192> switch to actual tab
  observeEvent(input$go_bio, {
    updateTabsetPanel(session, "mainNav", "Biodiversity")
  })
  observeEvent(input$go_plant, {
    updateTabsetPanel(session, "mainNav", "Plant")
  })
  observeEvent(input$go_butterfly, {
    updateTabsetPanel(session, "mainNav", "Butterfly")
  })
  observeEvent(input$go_bird, {
    updateTabsetPanel(session, "mainNav", "Bird")
  })
  observeEvent(input$go_home, {
    updateTabsetPanel(session, "mainNav", "Home")
  })


  # Synchronize "active" style
  observeEvent(input$mainNav, {
    btn_ids <- c(
      Home = "go_home",
      Biodiversity = "go_bio",
      Plant = "go_plant",
      Butterfly = "go_butterfly",
      Bird = "go_bird"
    )
    for (nm in names(btn_ids)) {
      session$sendCustomMessage("toggleActive", list(
        id = btn_ids[[nm]],
        active = identical(input$mainNav, nm)
      ))
    }
  })
}

# ---- Run ----
shinyApp(ui, server, options = list(launch.browser = TRUE))
