library(shiny)
library(readr)
library(dplyr)
library(tidyr)

# === Data Preparation ===
# Get the project root directory
project_root <- rprojroot::find_root(rprojroot::has_file("app.R"))
origin <- read_csv(file.path(project_root, "pages/butterfly/butterfly-origin.csv"), show_col_types = FALSE)
species <- read_csv(file.path(project_root, "pages/butterfly/Binfo.csv"), show_col_types = FALSE)

count_columns <- setdiff(names(origin), c("Site", "Address"))

color_palette <- c(
  "Little blue butterfly group" = "#89c4f4",
  "Cabbage white" = "#7fc9a7",
  "Hesperiidae group" = "#9dd2a8",
  "Small grass-yellow" = "#d6e8a3",
  "Australian painted lady" = "#94cfc2",
  "Dingy swallowtail" = "#6ab0bf",
  "Yellow admiral" = "#f2c84b",
  "Macleay's swallowtail" = "#f4a261",
  "Meadow argus" = "#f48b7a"
)

authoritative_colors <- tibble(
  species_group = names(color_palette),
  color = unname(color_palette)
)

butterfly_data <- origin %>%
  pivot_longer(
    cols = all_of(count_columns),
    names_to = "species_group",
    values_to = "count"
  ) %>%
  left_join(species, by = "species_group") %>%
  left_join(authoritative_colors, by = "species_group") %>%
  mutate(
    count = coalesce(count, 0),
    common_name = coalesce(common_name, species_group),
    latin_name = coalesce(latin_name, species_group),
    description = coalesce(description, "Description pending."),
    image_path = coalesce(image_path, ""),
    color = coalesce(color, "#3a9d7a")
  ) %>%
  rename(
    park_name = Site,
    address = Address,
    display_name = common_name
  )

parks <- sort(unique(butterfly_data$park_name))

butterfly_map_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$style(HTML("
      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        background: #f5faf6;
        color: #143021;
        font-family: 'Rubik', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .app-shell {
        display: flex;
        height: 100vh;
      }
      .left-pane {
        flex: 4;
        position: relative;
        padding: 72px 56px 48px 56px;
        display: flex;
        flex-direction: column;
      }
      .right-pane {
        flex: 3;
        background: #ffffff;
        border-left: 1px solid rgba(55, 136, 83, 0.18);
        padding: 48px 40px;
        display: flex;
        flex-direction: column;
        gap: 20px;
        overflow-y: auto;
        box-shadow: inset 12px 0 24px rgba(20, 60, 34, 0.06);
      }
      .title-row {
        display: flex;
        align-items: center;
        gap: 24px;
        margin-bottom: 36px;
      }
      .title-row h1 {
        font-size: 42px;
        letter-spacing: 0.08em;
        margin: 0;
        color: #0f4c2b;
      }
      .park-selector {
        background: #ffffff;
        border: 1px solid rgba(62, 153, 97, 0.4);
        border-radius: 12px;
        padding: 12px 16px;
        box-shadow: 0 12px 28px rgba(24, 94, 54, 0.18);
        min-width: 220px;
      }
      #viz-wrapper {
        flex: 1;
        position: relative;
        border-radius: 20px;
        background: linear-gradient(160deg, rgba(213, 248, 230, 0.9), rgba(234, 250, 241, 0.94));
        border: 1px dashed rgba(62, 153, 97, 0.25);
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
      }
      #viz-canvas {
        width: 100%;
        height: 100%;
      }
      .detail-heading {
        font-size: 18px;
        color: #1f7a49;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }
      .detail-title {
        font-size: 28px;
        margin: 0;
        color: #0f4c2b;
      }
      .detail-subtitle {
        font-size: 15px;
        color: #2a6b44;
        font-style: italic;
      }
      .detail-text {
        font-size: 14px;
        line-height: 1.6;
        color: #2b4b37;
      }
      .detail-image {
        width: 100%;
        height: 340px;
        object-fit: cover;
        object-position: center;
        border-radius: 12px;
        background: rgba(21, 85, 45, 0.08);
        border: 1px solid rgba(62, 153, 97, 0.2);
      }
      .detail-meta {
        font-size: 14px;
        color: #2b4b37;
      }
      .detail-meta strong {
        color: #1f7a49;
      }
      .species-node text {
        fill: rgba(20, 48, 33, 0.85);
        font-size: 14px;
        text-anchor: middle;
        dominant-baseline: hanging;
        pointer-events: none;
      }
      .species-node circle.cluster-halo {
        fill: none;
        stroke-width: 0;
        stroke: transparent;
        transition: all 0.2s ease;
      }
      .species-node circle.cluster-hitarea {
        fill: transparent;
        transition: fill 0.2s ease;
      }
      .species-node:hover circle.cluster-hitarea {
        fill: rgba(31, 122, 73, 0.08);
      }
      .species-node:hover circle.cluster-halo {
        stroke-width: 0;
        stroke: transparent;
      }
      .species-node.inactive circle.cluster-halo {
        stroke: transparent;
        stroke-width: 0;
      }
      .species-node.inactive text {
        fill: rgba(20, 48, 33, 0.45);
      }
      .species-node.inactive:hover circle.cluster-halo {
        stroke: transparent;
        stroke-width: 0;
      }
      .species-node.active circle.cluster-halo {
        stroke: transparent;
        stroke-width: 0;
      }
      .species-node.active text {
        fill: #1f7a49;
        font-weight: 600;
      }
      .species-node.active circle.cluster-hitarea {
        fill: rgba(31, 122, 73, 0.15);
      }
      .species-node.active:hover circle.cluster-hitarea {
        fill: rgba(31, 122, 73, 0.15);
      }
    ")),
      tags$script(src = "https://d3js.org/d3.v7.min.js")
    ),
    div(
      class = "app-shell",
      div(
        class = "left-pane",
        div(
          class = "title-row",
          h1("Butterfly"),
          div(
            class = "park-selector",
            selectInput(ns("park"), "Choose a park", choices = c("Choose a park" = "", parks), selected = "")
          )
        ),
        div(
          id = "viz-wrapper",
          tags$svg(id = "viz-canvas")
        )
      ),
      div(
        class = "right-pane",
        span(id = "detail-park", class = "detail-heading", "Select a park"),
        span(id = "detail-title", class = "detail-title", "Select a butterfly"),
        span(id = "detail-latin", class = "detail-subtitle", ""),
        img(id = "detail-image", class = "detail-image", src = "", alt = "Butterfly image"),
        div(
          id = "detail-description", class = "detail-text",
          "Choose a park and butterfly on the left to see details here."
        ),
        div(id = "detail-count", class = "detail-meta", "")
      )
    ),
    tags$script(HTML("
    (function() {
      const GRID_COLUMNS = 3;
      const MAX_DOTS = 80;
      let svg = null;
      let speciesData = [];
      let dotsData = [];
      let dotSimulation = null;
      let selectedSpecies = null;
      let userHasClicked = false;

      function ensureSvg() {
        const container = document.getElementById('viz-wrapper');
        if (!container) return null;
        const width = container.clientWidth;
        const height = container.clientHeight;
        if (!svg) {
          svg = d3.select('#viz-canvas');
        }
        svg.attr('width', width).attr('height', height);
        return { width, height };
      }

      function layoutSpecies(data) {
        const dims = ensureSvg();
        if (!dims) return [];
        const { width, height } = dims;
        const paddingX = width * 0.18;
        const paddingY = height * 0.18;
        const innerWidth = Math.max(width - paddingX * 2, 200);
        const innerHeight = Math.max(height - paddingY * 2, 200);

        const rows = Math.max(Math.ceil(data.length / GRID_COLUMNS), 1);
        const rowSpacing = rows > 1 ? innerHeight / (rows - 1) : 0;

        return data.map((d, index) => {
          const row = Math.floor(index / GRID_COLUMNS);
          const col = index % GRID_COLUMNS;
          const itemsInRow = Math.min(GRID_COLUMNS, data.length - row * GRID_COLUMNS);
          const colSpacing = itemsInRow > 1 ? innerWidth / (itemsInRow - 1) : 0;
          const rowWidth = colSpacing * Math.max(itemsInRow - 1, 0);
          const startX = paddingX + (innerWidth - rowWidth) / 2;
          const centerX = itemsInRow > 1 ? startX + col * colSpacing : width / 2;
          const centerY = rows > 1 ? paddingY + row * rowSpacing : height / 2;
          const haloRadius = 40 + Math.sqrt(d.count || 0) * 4;
          return Object.assign({}, d, { centerX, centerY, haloRadius });
        });
      }

      function buildDots(speciesList) {
        const dots = [];
        speciesList.forEach(spec => {
          const total = spec.count || 0;
          if (total === 0) return; // Skip species with no data
          const dotCount = Math.min(total, MAX_DOTS);
          const radius = Math.max(4, Math.min(9, spec.haloRadius * 0.18));
          for (let i = 0; i < dotCount; i++) {
            dots.push({
              id: `${spec.species_group}_${i}`,
              species_group: spec.species_group,
              display_name: spec.display_name,
              color: spec.color || '#3a9d7a',
              targetX: spec.centerX,
              targetY: spec.centerY,
              radius,
              weight: i < total ? 1 : 0
            });
          }
        });
        return dots;
      }

      function updateDetails(node) {
        const parkEl = document.getElementById('detail-park');
        const titleEl = document.getElementById('detail-title');
        const latinEl = document.getElementById('detail-latin');
        const imageEl = document.getElementById('detail-image');
        const descEl = document.getElementById('detail-description');
        const countEl = document.getElementById('detail-count');

        if (!node) {
          parkEl.textContent = 'Select a park';
          titleEl.textContent = 'Select a butterfly';
          latinEl.textContent = '';
          descEl.textContent = 'Choose a park and butterfly on the left to see details here.';
          countEl.textContent = '';
          imageEl.style.display = 'none';
          return;
        }

        parkEl.textContent = node.park_name || '';
        titleEl.textContent = node.display_name || node.species_group;
        latinEl.textContent = node.latin_name || '';
        descEl.textContent = node.description || 'Description pending.';
        countEl.innerHTML = `<strong>Relative sightings:</strong> ${node.count || 0}`;

        if (node.image_path) {
          imageEl.src = node.image_path;
          imageEl.style.display = 'block';
        } else {
          imageEl.style.display = 'none';
        }
      }

      function renderSpecies(speciesList, hideCircles = false) {
        const nodes = svg.selectAll('g.species-node')
          .data(hideCircles ? [] : speciesList, d => d.species_group);

        nodes.exit().remove();

        const enter = nodes.enter()
          .append('g')
          .attr('class', 'species-node')
          .style('cursor', 'pointer')
          .on('click', function(event, d) {
            userHasClicked = true;
            selectedSpecies = d.display_name;
            updateDetails(d);
            // Update active state for all nodes
            svg.selectAll('g.species-node')
              .classed('active', node => node.display_name === selectedSpecies);
          });

        enter.append('circle')
          .attr('class', 'cluster-halo')
          .attr('r', 0)
          .attr('stroke', d => d.color || '#3a9d7a');

        enter.append('circle')
          .attr('class', 'cluster-hitarea')
          .attr('r', 0)
          .attr('fill', 'transparent')
          .style('cursor', 'pointer');

        enter.append('text')
          .attr('y', 38)
          .text(d => d.display_name);

        const merged = enter.merge(nodes);

        merged
          .classed('inactive', d => (d.count || 0) === 0)
          .classed('active', d => {
            return userHasClicked && d.display_name === selectedSpecies;
          })
          .transition()
          .duration(400)
          .attr('transform', d => `translate(${d.centerX}, ${d.centerY})`);

        merged.select('circle.cluster-halo')
          .transition()
          .duration(400)
          .attr('r', d => hideCircles ? 0 : d.haloRadius)
          .attr('stroke', d => d.color || '#3a9d7a');

        merged.select('circle.cluster-hitarea')
          .transition()
          .duration(400)
          .attr('r', d => hideCircles ? 0 : d.haloRadius * 1.2);

        merged.select('text')
          .text(d => d.display_name);
      }

      function renderDots(speciesList) {
        dotsData = buildDots(speciesList);

        const dotGroups = svg.selectAll('circle.dot')
          .data(dotsData, d => d.id);

        dotGroups.exit().remove();

        dotGroups.enter()
          .append('circle')
          .attr('class', 'dot')
          .attr('r', 0)
          .attr('fill', d => d.color)
          .attr('cx', d => d.targetX)
          .attr('cy', d => d.targetY)
          .transition()
          .duration(200)
          .attr('r', d => d.radius);

        svg.selectAll('circle.dot')
          .transition()
          .duration(200)
          .attr('fill', d => d.color)
          .attr('r', d => d.radius);

        if (!dotSimulation) {
          dotSimulation = d3.forceSimulation(dotsData)
            .alphaMin(0.05)
            .alphaDecay(0.02)
            .velocityDecay(0.18)
            .force('x', d3.forceX().strength(0.18).x(d => d.targetX))
            .force('y', d3.forceY().strength(0.18).y(d => d.targetY))
            .force('cluster', d3.forceManyBody().strength(4))
            .force('collide', d3.forceCollide().radius(d => d.radius * 1.3))
            .on('tick', () => {
              svg.selectAll('circle.dot')
                .attr('cx', d => d.x)
                .attr('cy', d => d.y);
            });
        } else {
          dotSimulation.nodes(dotsData);
          dotSimulation.force('x').strength(0.18).x(d => d.targetX);
          dotSimulation.force('y').strength(0.18).y(d => d.targetY);
          dotSimulation.force('collide').radius(d => d.radius * 1.3);
          dotSimulation.alpha(0.7).restart();
        }
      }

      function render(data, hasSelection = true) {
        speciesData = layoutSpecies(data);
        if (!speciesData.length) {
          updateDetails(null);
          return;
        }

        renderSpecies(speciesData, !hasSelection);
        if (hasSelection) {
          renderDots(speciesData);
        } else {
          // Clear dots when no park selected
          svg.selectAll('circle.dot').remove();
        }

        // Bring species labels to front
        svg.selectAll('g.species-node').raise();

        if (hasSelection) {
          if (!selectedSpecies || !speciesData.some(d => d.display_name === selectedSpecies)) {
            selectedSpecies = null;
          }
          const active = selectedSpecies ? speciesData.find(d => d.display_name === selectedSpecies) : null;
          updateDetails(active);
        } else {
          selectedSpecies = null;
          updateDetails(null);
        }
      }

      Shiny.addCustomMessageHandler('updateButterflies', function(message) {
        const nodes = message.nodes || [];
        const hasSelection = message.hasSelection !== false;
        render(nodes, hasSelection);
      });

      let resizeTimer;
      window.addEventListener('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(function() {
          if (speciesData.length) {
            const savedData = speciesData.map(d => ({
              park_name: d.park_name,
              species_group: d.species_group,
              display_name: d.display_name,
              latin_name: d.latin_name,
              count: d.count,
              color: d.color,
              description: d.description,
              image_path: d.image_path
            }));
            render(savedData);
          }
        }, 200);
      });
    })();
  "))
  )
}

butterfly_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_store <- reactiveVal(butterfly_data)

    observeEvent(input$park,
      {
        if (is.null(input$park) || input$park == "") {
          # No park selected - send all unique butterfly names with hasSelection = FALSE
          nodes <- data_store() %>%
            group_by(species_group, display_name, latin_name, color, description, image_path) %>%
            summarise(count = 0, .groups = "drop") %>%
            mutate(park_name = "") %>%
            select(park_name, species_group, display_name, latin_name, count, color, description, image_path)

          nodes_list <- lapply(seq_len(nrow(nodes)), function(i) as.list(nodes[i, ]))
          session$sendCustomMessage("updateButterflies", list(nodes = nodes_list, hasSelection = FALSE))
        } else {
          # Park selected - send filtered data
          nodes <- data_store() %>%
            filter(park_name == input$park) %>%
            select(park_name, species_group, display_name, latin_name, count, color, description, image_path)

          nodes_list <- lapply(seq_len(nrow(nodes)), function(i) as.list(nodes[i, ]))
          session$sendCustomMessage("updateButterflies", list(nodes = nodes_list, hasSelection = TRUE))
        }
      },
      ignoreNULL = FALSE
    )
  })
}
