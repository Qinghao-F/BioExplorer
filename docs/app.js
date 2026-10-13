const DATA_ROOT = 'data'
const VIEWS = ['overview', 'biodiversity', 'plants', 'butterflies', 'birds']
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
const PALETTE = {
  Animalia: '#e88f6e',
  Plantae: '#2c7a64',
  Other: '#e8bd5e',
  garden: '#123c33'
}

const cache = new Map()
let activeView = 'overview'
let activeMap = null

function escapeHtml(value) {
  return String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#039;')
}

function formatNumber(value) {
  return new Intl.NumberFormat('en-AU').format(Number(value) || 0)
}

async function loadData(name) {
  if (!cache.has(name)) {
    cache.set(
      name,
      fetch(`${DATA_ROOT}/${name}.json`).then((response) => {
        if (!response.ok) throw new Error(`Could not load ${name}`)
        return response.json()
      })
    )
  }
  return cache.get(name)
}

function destroyMap() {
  if (activeMap) {
    activeMap.remove()
    activeMap = null
  }
}

function setLoading() {
  destroyMap()
  document.querySelector('#view-root').innerHTML = `
    <div class="loading-state">
      <span class="loading-leaf">⌁</span>
      <p>Loading this layer of the ecosystem…</p>
    </div>
  `
}

function showError(error) {
  console.error(error)
  document.querySelector('#view-root').innerHTML = `
    <div class="error-state">
      <p>We couldn't load this dataset.</p>
      <button class="button button-secondary" type="button" data-retry>Try again</button>
    </div>
  `
  document.querySelector('[data-retry]')?.addEventListener('click', () => renderView(activeView))
}

function updateNavigation(view) {
  document.querySelectorAll('[data-view]').forEach((element) => {
    const selected = element.dataset.view === view
    element.classList.toggle('is-active', selected)
    if (element.getAttribute('role') === 'tab') element.setAttribute('aria-selected', String(selected))
  })
}

async function switchView(view, { scroll = true, replace = false } = {}) {
  if (!VIEWS.includes(view)) view = 'overview'
  activeView = view
  updateNavigation(view)
  if (replace) history.replaceState(null, '', `#${view}`)
  else if (location.hash !== `#${view}`) history.pushState(null, '', `#${view}`)
  await renderView(view)
  if (scroll) document.querySelector('#explorer').scrollIntoView({ behavior: 'smooth', block: 'start' })
}

async function renderView(view) {
  setLoading()
  try {
    if (view === 'overview') await renderOverview()
    if (view === 'biodiversity') await renderBiodiversity()
    if (view === 'plants') await renderPlants()
    if (view === 'butterflies') await renderButterflies()
    if (view === 'birds') await renderBirds()
  } catch (error) {
    showError(error)
  }
}

function explorerLayout(side, content, contentClass = 'map-wrap') {
  return `
    <div class="explorer-layout">
      <aside class="side-panel side-panel-scroll">${side}</aside>
      <div class="${contentClass}">${content}</div>
    </div>
  `
}

function stat(label, value) {
  return `<div class="panel-stat"><strong>${escapeHtml(formatNumber(value))}</strong><span>${escapeHtml(label)}</span></div>`
}

function mapFallback() {
  return `<div class="empty-message">The basemap could not load. The rest of the explorer is still available.</div>`
}

function createMap(elementId, center = [-37.8136, 144.9631], zoom = 12) {
  if (!window.L) {
    document.querySelector(`#${elementId}`).innerHTML = mapFallback()
    return null
  }

  activeMap = L.map(elementId, {
    center,
    zoom,
    zoomControl: true,
    preferCanvas: true,
    scrollWheelZoom: false
  })

  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 19,
    attribution: '&copy; OpenStreetMap contributors'
  }).addTo(activeMap)

  requestAnimationFrame(() => activeMap?.invalidateSize())
  return activeMap
}

function observationPopup(item) {
  return `
    <div class="popup-card">
      <h4>${escapeHtml(item.name)}</h4>
      ${item.scientific ? `<p><em>${escapeHtml(item.scientific)}</em></p>` : ''}
      <p>${escapeHtml(item.taxa)} · ${escapeHtml(item.kingdom)}</p>
      <p>${escapeHtml(item.date || 'Date unavailable')}</p>
    </div>
  `
}

async function renderOverview() {
  const [summary, observations, plants] = await Promise.all([
    loadData('summary'),
    loadData('observations'),
    loadData('plants')
  ])

  const side = `
    <p class="panel-kicker">Citywide snapshot</p>
    <h3 class="view-title">Nature across Melbourne</h3>
    <p class="view-copy">Start with the whole picture. Coloured points are biodiversity records; dark rings mark surveyed garden sites.</p>
    <div class="panel-stats">
      ${stat('observations', summary.observations)}
      ${stat('species', summary.observedSpecies)}
      ${stat('plant records', summary.plantRecords)}
      ${stat('bird species', summary.birdSpecies)}
    </div>
    <p class="filter-label">Jump to a dataset</p>
    <div class="location-list">
      <button class="location-button" data-view="biodiversity"><strong>Biodiversity records</strong><span>Search by species, kingdom and month</span></button>
      <button class="location-button" data-view="plants"><strong>Garden plants</strong><span>Compare planting diversity by location</span></button>
      <button class="location-button" data-view="butterflies"><strong>Butterfly surveys</strong><span>See species totals across eight sites</span></button>
      <button class="location-button" data-view="birds"><strong>Bird communities</strong><span>Compare sightings and seasonality</span></button>
    </div>
    <div class="legend">
      <span><i style="background:${PALETTE.Animalia}"></i>Animals</span>
      <span><i style="background:${PALETTE.Plantae}"></i>Plants</span>
      <span><i style="background:${PALETTE.garden}"></i>Gardens</span>
    </div>
  `

  document.querySelector('#view-root').innerHTML = explorerLayout(side, '<div id="overview-map" class="map"></div><div class="map-message">Select a point to identify it</div>')
  bindViewButtons()
  const map = createMap('overview-map')
  if (!map) return

  const renderer = L.canvas({ padding: 0.4 })
  observations.forEach((item) => {
    const color = PALETTE[item.kingdom] ?? PALETTE.Other
    L.circleMarker([item.lat, item.lon], {
      renderer,
      radius: 4,
      weight: 1,
      color: '#ffffff',
      fillColor: color,
      fillOpacity: 0.78
    }).bindPopup(observationPopup(item)).addTo(map)
  })

  plants.sites.filter((site) => site.lat && site.lon).forEach((site) => {
    L.circleMarker([site.lat, site.lon], {
      radius: 10,
      weight: 3,
      color: PALETTE.garden,
      fillColor: '#cfe77f',
      fillOpacity: 0.95
    }).bindPopup(`
      <div class="popup-card">
        <h4>${escapeHtml(site.name)}</h4>
        <p>${formatNumber(site.speciesCount)} plant species · ${formatNumber(site.records)} records</p>
        <p>${escapeHtml(site.address)}</p>
      </div>
    `).addTo(map)
  })
}

async function renderBiodiversity() {
  const observations = await loadData('observations')
  const kingdoms = [...new Set(observations.map((row) => row.kingdom))].sort()

  const side = `
    <p class="panel-kicker">Observation explorer</p>
    <h3 class="view-title">Biodiversity sightings</h3>
    <p class="view-copy">Filter observations by name, biological kingdom or month. The map updates instantly in your browser.</p>
    <div class="control-group">
      <label for="bio-search">Species or group</label>
      <input id="bio-search" type="search" placeholder="Try bird, egret or Acacia…" autocomplete="off" />
    </div>
    <div class="filter-row">
      <div class="control-group">
        <label for="bio-kingdom">Kingdom</label>
        <select id="bio-kingdom">
          <option value="">All kingdoms</option>
          ${kingdoms.map((name) => `<option>${escapeHtml(name)}</option>`).join('')}
        </select>
      </div>
      <div class="control-group">
        <label for="bio-month">Month</label>
        <select id="bio-month">
          <option value="">All months</option>
          ${MONTHS.map((name, index) => `<option value="${index + 1}">${name}</option>`).join('')}
        </select>
      </div>
    </div>
    <div class="panel-stats" id="bio-stats"></div>
    <div class="legend">
      ${kingdoms.map((name) => `<span><i style="background:${PALETTE[name] ?? PALETTE.Other}"></i>${escapeHtml(name)}</span>`).join('')}
    </div>
  `

  document.querySelector('#view-root').innerHTML = explorerLayout(side, '<div id="biodiversity-map" class="map"></div><div id="bio-map-message" class="map-message"></div>')
  const map = createMap('biodiversity-map')
  if (!map) return
  const layer = L.layerGroup().addTo(map)
  const renderer = L.canvas({ padding: 0.4 })

  function update() {
    const search = document.querySelector('#bio-search').value.trim().toLowerCase()
    const kingdom = document.querySelector('#bio-kingdom').value
    const month = Number(document.querySelector('#bio-month').value) || null
    const filtered = observations.filter((item) => {
      const haystack = `${item.name} ${item.scientific} ${item.taxa}`.toLowerCase()
      return (!search || haystack.includes(search)) && (!kingdom || item.kingdom === kingdom) && (!month || item.month === month)
    })

    layer.clearLayers()
    filtered.forEach((item) => {
      L.circleMarker([item.lat, item.lon], {
        renderer,
        radius: 5,
        weight: 1,
        color: '#fff',
        fillColor: PALETTE[item.kingdom] ?? PALETTE.Other,
        fillOpacity: 0.84
      }).bindPopup(observationPopup(item)).addTo(layer)
    })

    const unique = new Set(filtered.map((item) => item.name)).size
    document.querySelector('#bio-stats').innerHTML = `${stat('matching records', filtered.length)}${stat('species shown', unique)}`
    document.querySelector('#bio-map-message').textContent = `${formatNumber(filtered.length)} records on the map`
  }

  document.querySelectorAll('#bio-search, #bio-kingdom, #bio-month').forEach((control) => {
    control.addEventListener(control.tagName === 'INPUT' ? 'input' : 'change', update)
  })
  update()
}

function bars(entries, colour = null) {
  if (!entries?.length) return '<p class="empty-message">No records for this selection.</p>'
  const max = Math.max(...entries.map((entry) => entry.count), 1)
  return `<div class="bar-list">${entries.map((entry) => `
    <div class="bar-row">
      <span class="bar-label" title="${escapeHtml(entry.name)}">${escapeHtml(entry.name)}</span>
      <span class="bar-track"><span class="bar-fill" style="width:${Math.max(2, (entry.count / max) * 100)}%;${colour ? `background:${colour}` : ''}"></span></span>
      <span class="bar-value">${formatNumber(entry.count)}</span>
    </div>
  `).join('')}</div>`
}

async function renderPlants() {
  const plants = await loadData('plants')
  const firstSite = plants.sites[0]

  const side = `
    <p class="panel-kicker">Garden inventory</p>
    <h3 class="view-title">Plants and places</h3>
    <p class="view-copy">Compare garden diversity, then browse a visual selection of plants documented in the inventory.</p>
    <div class="panel-stats">
      ${stat('plant records', plants.totalRecords)}
      ${stat('unique plants', plants.totalSpecies)}
    </div>
    <p class="filter-label">Garden locations</p>
    <div class="location-list" id="plant-sites">
      ${plants.sites.map((site, index) => `
        <button class="location-button ${index === 0 ? 'is-active' : ''}" data-site="${escapeHtml(site.name)}">
          <strong>${escapeHtml(site.name)}</strong>
          <span>${formatNumber(site.speciesCount)} species · ${formatNumber(site.records)} records</span>
        </button>
      `).join('')}
    </div>
  `

  const content = `
    <div id="plant-site-detail"></div>
    <div class="content-header" style="margin-top:28px">
      <div><h3>Featured plants</h3><p>Images load only as you scroll.</p></div>
      <span class="content-badge">${formatNumber(plants.featured.length)} illustrated species</span>
    </div>
    <div class="plant-grid">
      ${plants.featured.map((plant) => `
        <article class="plant-card">
          <img src="${plant.image}" alt="${escapeHtml(plant.botanical)}" loading="lazy" decoding="async" />
          <div>
            <h4>${escapeHtml(plant.botanical)}</h4>
            <p>${escapeHtml(plant.common || plant.form || 'Garden plant')} · ${escapeHtml(plant.origin)}</p>
          </div>
        </article>
      `).join('')}
    </div>
  `

  document.querySelector('#view-root').innerHTML = explorerLayout(side, content, 'content-canvas')

  function selectSite(name) {
    const site = plants.sites.find((entry) => entry.name === name) ?? firstSite
    document.querySelectorAll('[data-site]').forEach((button) => button.classList.toggle('is-active', button.dataset.site === site.name))
    document.querySelector('#plant-site-detail').innerHTML = `
      <div class="content-header">
        <div><h3>${escapeHtml(site.name)}</h3><p>${escapeHtml(site.address || `${site.category} in Melbourne`)}</p></div>
        <span class="content-badge">${formatNumber(site.speciesCount)} species</span>
      </div>
      <div class="chart-card">
        <div class="chart-title"><h4>Most recorded plants</h4><span>${site.nativeShare}% native records</span></div>
        ${bars(site.topSpecies)}
      </div>
    `
  }

  document.querySelectorAll('[data-site]').forEach((button) => button.addEventListener('click', () => selectSite(button.dataset.site)))
  selectSite(firstSite.name)
}

async function renderButterflies() {
  const butterflies = await loadData('butterflies')
  const totals = butterflies.species.map((species) => ({ name: species.name, count: species.total }))

  const side = `
    <p class="panel-kicker">Butterfly survey</p>
    <h3 class="view-title">Butterflies by site</h3>
    <p class="view-copy">Compare nine surveyed groups across Melbourne parks and gardens.</p>
    <div class="panel-stats">
      ${stat('total sightings', butterflies.total)}
      ${stat('survey sites', butterflies.sites.length)}
    </div>
    <div class="control-group">
      <label for="butterfly-site">Survey location</label>
      <select id="butterfly-site">
        <option value="all">All locations</option>
        ${butterflies.sites.map((site) => `<option value="${escapeHtml(site.name)}">${escapeHtml(site.name)}</option>`).join('')}
      </select>
    </div>
    <div id="butterfly-site-note" class="view-copy"></div>
  `

  const content = `
    <div class="chart-card">
      <div class="chart-title"><h4 id="butterfly-chart-title">Sightings across all locations</h4><span>survey count</span></div>
      <div id="butterfly-bars">${bars(totals, 'linear-gradient(90deg, #e88f6e, #e8bd5e)')}</div>
    </div>
    <div class="content-header" style="margin-top:28px">
      <div><h3>Species guide</h3><p>Survey groups and their scientific names.</p></div>
    </div>
    <div class="species-grid">
      ${butterflies.species.map((species) => `
        <article class="species-card">
          ${species.image ? `<img src="${escapeHtml(species.image)}" alt="${escapeHtml(species.common || species.name)}" loading="lazy" decoding="async" referrerpolicy="no-referrer" />` : ''}
          <div class="species-card-content">
            <h4>${escapeHtml(species.common || species.name)}</h4>
            <p><em>${escapeHtml(species.scientific)}</em> · ${formatNumber(species.total)} sightings</p>
            <p class="description">${escapeHtml(species.description)}</p>
          </div>
        </article>
      `).join('')}
    </div>
  `

  document.querySelector('#view-root').innerHTML = explorerLayout(side, content, 'content-canvas')

  function update() {
    const selected = document.querySelector('#butterfly-site').value
    const site = butterflies.sites.find((entry) => entry.name === selected)
    const entries = site ? site.counts : totals
    document.querySelector('#butterfly-bars').innerHTML = bars(entries, 'linear-gradient(90deg, #e88f6e, #e8bd5e)')
    document.querySelector('#butterfly-chart-title').textContent = site ? site.name : 'Sightings across all locations'
    document.querySelector('#butterfly-site-note').textContent = site ? `${site.total} sightings · ${site.address}` : 'Showing totals from all survey locations.'
  }

  document.querySelector('#butterfly-site').addEventListener('change', update)
  update()
}

function lineChart(values) {
  const width = 620
  const height = 180
  const padX = 28
  const padTop = 14
  const padBottom = 28
  const max = Math.max(...values, 1)
  const points = values.map((value, index) => {
    const x = padX + (index / 11) * (width - padX * 2)
    const y = padTop + (1 - value / max) * (height - padTop - padBottom)
    return { x, y, value, label: MONTHS[index] }
  })
  const polyline = points.map((point) => `${point.x},${point.y}`).join(' ')
  const area = `${padX},${height - padBottom} ${polyline} ${width - padX},${height - padBottom}`
  return `
    <svg class="line-chart" viewBox="0 0 ${width} ${height}" role="img" aria-label="Monthly sightings line chart">
      ${[0, 0.5, 1].map((ratio) => `<line class="grid-line" x1="${padX}" x2="${width - padX}" y1="${padTop + ratio * (height - padTop - padBottom)}" y2="${padTop + ratio * (height - padTop - padBottom)}" />`).join('')}
      <polygon class="area" points="${area}" />
      <polyline class="data-line" points="${polyline}" />
      ${points.map((point) => `<circle class="dot" cx="${point.x}" cy="${point.y}" r="4"><title>${point.label}: ${point.value}</title></circle>`).join('')}
      ${points.map((point, index) => index % 2 === 0 ? `<text x="${point.x}" y="${height - 8}" text-anchor="middle">${point.label}</text>` : '').join('')}
    </svg>
  `
}

async function renderBirds() {
  const birds = await loadData('birds')
  const firstSite = birds.sites[0]

  const side = `
    <p class="panel-kicker">Bird survey</p>
    <h3 class="view-title">Bird communities</h3>
    <p class="view-copy">Select a survey location to compare common species and seasonal activity.</p>
    <div class="panel-stats">
      ${stat('survey records', birds.totalRecords)}
      ${stat('bird species', birds.totalSpecies)}
    </div>
    <p class="filter-label">Survey locations</p>
    <div class="location-list" id="bird-sites">
      ${birds.sites.map((site, index) => `
        <button class="location-button ${index === 0 ? 'is-active' : ''}" data-bird-site="${escapeHtml(site.name)}">
          <strong>${escapeHtml(site.name)}</strong>
          <span>${formatNumber(site.speciesCount)} species · ${formatNumber(site.sightings)} sightings</span>
        </button>
      `).join('')}
    </div>
  `

  const content = '<div id="bird-detail"></div>'
  document.querySelector('#view-root').innerHTML = explorerLayout(side, content, 'content-canvas')

  function selectSite(name) {
    const site = birds.sites.find((entry) => entry.name === name) ?? firstSite
    document.querySelectorAll('[data-bird-site]').forEach((button) => button.classList.toggle('is-active', button.dataset.birdSite === site.name))
    document.querySelector('#bird-detail').innerHTML = `
      <div class="content-header">
        <div><h3>${escapeHtml(site.name)}</h3><p>${formatNumber(site.records)} survey records across ${formatNumber(site.speciesCount)} species.</p></div>
        <span class="content-badge">${formatNumber(site.sightings)} sightings</span>
      </div>
      <div class="chart-card">
        <div class="chart-title"><h4>Most frequently recorded</h4><span>weighted survey count</span></div>
        ${bars(site.topSpecies, 'linear-gradient(90deg, #2c7a64, #74b9a0)')}
      </div>
      <div class="chart-card">
        <div class="chart-title"><h4>Activity through the year</h4><span>monthly sightings</span></div>
        ${lineChart(site.months)}
      </div>
    `
  }

  document.querySelectorAll('[data-bird-site]').forEach((button) => button.addEventListener('click', () => selectSite(button.dataset.birdSite)))
  selectSite(firstSite.name)
}

function bindViewButtons() {
  document.querySelectorAll('[data-view]').forEach((button) => {
    if (button.dataset.bound) return
    button.dataset.bound = 'true'
    button.addEventListener('click', (event) => {
      if (button.tagName === 'BUTTON') event.preventDefault()
      switchView(button.dataset.view)
    })
  })
}

async function loadSummary() {
  try {
    const summary = await loadData('summary')
    document.querySelectorAll('[data-summary]').forEach((element) => {
      element.textContent = formatNumber(summary[element.dataset.summary])
    })
  } catch (error) {
    console.warn('Summary data unavailable', error)
  }
}

document.addEventListener('DOMContentLoaded', async () => {
  bindViewButtons()
  loadSummary()
  const requested = location.hash.replace('#', '')
  const initial = VIEWS.includes(requested) ? requested : 'overview'
  await switchView(initial, { scroll: false, replace: !VIEWS.includes(requested) })
})

window.addEventListener('popstate', () => {
  const requested = location.hash.replace('#', '')
  switchView(VIEWS.includes(requested) ? requested : 'overview', { scroll: false, replace: true })
})
