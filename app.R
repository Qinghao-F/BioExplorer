# app.R — robust + responsive, with collapsible navbar on narrow screens

source("global.R")
options(shiny.autoreload = TRUE)

# 自动加载 pages 下所有 map.R
page_files <- list.files("pages", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
invisible(lapply(page_files, function(f) try(source(f), silent = TRUE)))

# 占位模块（防止未实现时报错）
empty_module_ui <- function(id, title = "Empty Module") {
  ns <- NS(id)
  tagList(
    h3(title),
    div(
      style = "height:70vh; min-height:320px; width:100%;
               display:flex; align-items:center; justify-content:center;
               border:1px dashed rgba(255,255,255,0.15); border-radius:12px; color:#9fb0c3;",
      "This module is empty."
    )
  )
}
empty_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

ensure_module <- function(ui_name, server_name, title) {
  if (!exists(ui_name, mode = "function")) {
    assign(ui_name, function(id) empty_module_ui(id, paste0(title, " (empty)")), envir = .GlobalEnv)
  }
  if (!exists(server_name, mode = "function")) {
    assign(server_name, function(id) empty_module_server(id), envir = .GlobalEnv)
  }
}
ensure_module("biodiversity_map_ui", "biodiversity_map_server", "Biodiversity")
ensure_module("butterfly_map_ui", "butterfly_map_server", "Butterfly")
ensure_module("flower_map_ui", "flower_map_server", "Flower")
ensure_module("garden_map_ui", "garden_map_server", "Garden")

# [CHANGE] 加入 collapsible = TRUE 和 fluid = TRUE
ui <- navbarPage(
  id = "mainNav",
  title = "GardenApp",
  theme = bslib::bs_theme(version = 5, bootswatch = "darkly"),
  collapsible = TRUE, # ✅ 窄屏时会出现汉堡按钮
  fluid = TRUE, # ✅ 内容随屏幕宽度变化
  tabPanel("Biodiversity", biodiversity_map_ui("biodiversity")),
  tabPanel("Butterfly", butterfly_map_ui("butterfly")),
  tabPanel("Flower", flower_map_ui("flower")),
  tabPanel("Garden", garden_map_ui("garden"))
)

server <- function(input, output, session) {
  biodiversity_map_server("biodiversity")
  butterfly_map_server("butterfly")
  flower_map_server("flower")
  garden_map_server("garden")
}

shinyApp(ui, server)
