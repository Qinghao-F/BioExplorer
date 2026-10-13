import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const outputDir = path.join(root, 'docs', 'data')

function parseCsv(text) {
  const rows = []
  let row = []
  let field = ''
  let quoted = false

  for (let i = 0; i < text.length; i += 1) {
    const char = text[i]
    if (quoted) {
      if (char === '"' && text[i + 1] === '"') {
        field += '"'
        i += 1
      } else if (char === '"') {
        quoted = false
      } else {
        field += char
      }
    } else if (char === '"') {
      quoted = true
    } else if (char === ',') {
      row.push(field)
      field = ''
    } else if (char === '\n') {
      row.push(field.replace(/\r$/, ''))
      if (row.some((value) => value !== '')) rows.push(row)
      row = []
      field = ''
    } else {
      field += char
    }
  }

  if (field || row.length) {
    row.push(field.replace(/\r$/, ''))
    rows.push(row)
  }

  const [headers, ...values] = rows
  return values.map((cells) => Object.fromEntries(headers.map((header, index) => [header.replace(/^\uFEFF/, ''), cells[index] ?? ''])))
}

function readCsv(relativePath) {
  return parseCsv(fs.readFileSync(path.join(root, relativePath), 'utf8'))
}

function writeJson(name, value) {
  fs.mkdirSync(outputDir, { recursive: true })
  fs.writeFileSync(path.join(outputDir, name), `${JSON.stringify(value)}\n`)
}

function number(value) {
  const parsed = Number(value)
  return Number.isFinite(parsed) ? parsed : null
}

function clean(value) {
  return String(value ?? '').trim()
}

function monthFromDate(value) {
  const match = clean(value).match(/^\d{4}[/-](\d{1,2})/)
  return match ? Number(match[1]) : null
}

function titleCase(value) {
  return clean(value)
    .toLowerCase()
    .replace(/\b\w/g, (letter) => letter.toUpperCase())
}

function normaliseSite(value) {
  return clean(value)
    .toLowerCase()
    .replace(/women'?s?/g, 'women')
    .replace(/[^a-z0-9]+/g, '')
}

function topEntries(map, limit = 8) {
  return [...map.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .slice(0, limit)
    .map(([name, count]) => ({ name, count }))
}

const biodiversityRows = readCsv('dataset/biodiversity.csv')
const observations = biodiversityRows
  .map((row) => ({
    name: clean(row['common name']) || clean(row.species) || clean(row.taxa) || 'Unknown species',
    scientific: [clean(row.genus), clean(row.species)].filter(Boolean).join(' '),
    kingdom: titleCase(row.kingdom || 'Unknown'),
    taxa: titleCase(row.taxa || 'Other'),
    date: clean(row['sighting date']),
    month: monthFromDate(row['sighting date']),
    lat: number(row.latitude),
    lon: number(row.longitude),
    location: clean(row.location)
  }))
  .filter((row) => row.lat !== null && row.lon !== null)

writeJson('observations.json', observations)

const gardenRows = readCsv('dataset/garden_site.csv')
const plantRows = readCsv('dataset/plant.csv')
const plantsBySite = new Map()
const allPlantSpecies = new Map()

for (const row of plantRows) {
  const site = clean(row.Site)
  const botanical = clean(row['Botanical name'])
  if (!site || !botanical) continue

  if (!plantsBySite.has(site)) {
    plantsBySite.set(site, {
      records: 0,
      species: new Map(),
      native: 0,
      exotic: 0,
      scoreTotal: 0,
      scoreCount: 0
    })
  }

  const group = plantsBySite.get(site)
  group.records += 1
  group.species.set(botanical, (group.species.get(botanical) ?? 0) + 1)
  allPlantSpecies.set(botanical, (allPlantSpecies.get(botanical) ?? 0) + 1)

  const origin = clean(row.Origin).toLowerCase()
  if (origin.includes('native')) group.native += 1
  if (origin.includes('exotic')) group.exotic += 1

  const score = number(row['TOTAL SCORE /26 Beds - /15 Mulch'])
  if (score !== null) {
    group.scoreTotal += score
    group.scoreCount += 1
  }
}

const siteLookup = new Map(gardenRows.map((row) => [normaliseSite(row.Site), row]))
const plantSites = [...plantsBySite.entries()]
  .map(([name, group]) => {
    const location = siteLookup.get(normaliseSite(name)) ?? {}
    return {
      name,
      category: titleCase(location.Category || 'Garden'),
      address: clean(location.Address),
      lat: number(location.Lat),
      lon: number(location.Lon),
      records: group.records,
      speciesCount: group.species.size,
      nativeShare: group.records ? Math.round((group.native / group.records) * 100) : 0,
      averageScore: group.scoreCount ? Math.round((group.scoreTotal / group.scoreCount) * 10) / 10 : null,
      topSpecies: topEntries(group.species, 7)
    }
  })
  .sort((a, b) => b.speciesCount - a.speciesCount)

const imageDirectory = path.join(root, 'www', 'species')
const featuredPlants = fs.readdirSync(imageDirectory)
  .filter((name) => /\.(jpe?g)$/i.test(name))
  .map((filename) => {
    const botanical = filename.replace(/\.(jpe?g)$/i, '')
    const match = plantRows.find((row) => clean(row['Botanical name']).toLowerCase() === botanical.toLowerCase())
    return {
      botanical,
      common: clean(match?.['Common name']),
      origin: titleCase(match?.Origin || 'Unknown'),
      form: titleCase(match?.Form || ''),
      records: allPlantSpecies.get(botanical) ?? 0,
      image: `assets/plants/${encodeURIComponent(filename.replace(/\.jpeg$/i, '.jpg').replace(/\.JPG$/, '.jpg'))}`
    }
  })
  .sort((a, b) => b.records - a.records)

writeJson('plants.json', {
  sites: plantSites,
  featured: featuredPlants,
  totalSpecies: allPlantSpecies.size,
  totalRecords: plantRows.length
})

const birdRows = readCsv('dataset/garden.csv')
const birdsBySite = new Map()
const birdSpecies = new Set()

for (const row of birdRows) {
  const site = clean(row.site_name) || clean(row.loc1_desc) || 'Other location'
  const species = clean(row['Common Name']) || clean(row['Scientific Name']) || 'Unknown bird'
  const count = number(row['Sighting Count']) ?? 0
  birdSpecies.add(species)

  if (!birdsBySite.has(site)) {
    birdsBySite.set(site, {
      lat: number(row.lat),
      lon: number(row.lon),
      records: 0,
      sightings: 0,
      species: new Map(),
      months: new Map()
    })
  }

  const group = birdsBySite.get(site)
  group.records += 1
  group.sightings += count
  group.species.set(species, (group.species.get(species) ?? 0) + Math.max(count, 1))
  const month = monthFromDate(row['Sighting Date'])
  if (month) group.months.set(month, (group.months.get(month) ?? 0) + count)
}

const birdSites = [...birdsBySite.entries()]
  .map(([name, group]) => ({
    name,
    lat: group.lat,
    lon: group.lon,
    records: group.records,
    sightings: group.sightings,
    speciesCount: group.species.size,
    topSpecies: topEntries(group.species, 8),
    months: Array.from({ length: 12 }, (_, index) => group.months.get(index + 1) ?? 0)
  }))
  .sort((a, b) => b.sightings - a.sightings)

writeJson('birds.json', {
  sites: birdSites,
  totalRecords: birdRows.length,
  totalSpecies: birdSpecies.size,
  totalSightings: birdSites.reduce((sum, site) => sum + site.sightings, 0)
})

const butterflyRows = readCsv('pages/butterfly/butterfly-origin.csv')
const butterflyInfoRows = readCsv('pages/butterfly/Binfo.csv')
const butterflySpeciesNames = Object.keys(butterflyRows[0] ?? {}).filter((name) => !['Site', 'Address'].includes(name))
const infoLookup = new Map(butterflyInfoRows.map((row) => [clean(row.species_group), row]))
const butterflyTotals = new Map(butterflySpeciesNames.map((name) => [name, 0]))

const butterflySites = butterflyRows.map((row) => {
  const counts = butterflySpeciesNames.map((name) => {
    const count = number(row[name]) ?? 0
    butterflyTotals.set(name, (butterflyTotals.get(name) ?? 0) + count)
    return { name, count }
  })
  return {
    name: clean(row.Site),
    address: clean(row.Address),
    total: counts.reduce((sum, entry) => sum + entry.count, 0),
    counts
  }
})

const butterflySpecies = butterflySpeciesNames
  .map((name) => {
    const info = infoLookup.get(name) ?? {}
    return {
      name,
      common: clean(info.common_name),
      scientific: clean(info.latin_name),
      description: clean(info.description),
      image: clean(info.image_path),
      total: butterflyTotals.get(name) ?? 0
    }
  })
  .sort((a, b) => b.total - a.total)

writeJson('butterflies.json', {
  sites: butterflySites,
  species: butterflySpecies,
  total: butterflySpecies.reduce((sum, species) => sum + species.total, 0)
})

writeJson('summary.json', {
  observations: observations.length,
  observedSpecies: new Set(observations.map((row) => row.name)).size,
  plantRecords: plantRows.length,
  plantSpecies: allPlantSpecies.size,
  birdRecords: birdRows.length,
  birdSpecies: birdSpecies.size,
  butterflySites: butterflyRows.length
})

console.log(`Wrote GitHub Pages data to ${path.relative(root, outputDir)}`)
