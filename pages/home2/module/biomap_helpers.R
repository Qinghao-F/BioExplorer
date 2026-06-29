# pages/biodiversity/module/biomap_helpers.R
# Helpers to build Leaflet label & popup HTML (vectorized)


# Helper utilities
bio_nz <- function(x) ifelse(!is.na(x) & nzchar(x), x, "\u2014")

bio_nice_date <- function(x) {
  if (is.null(x)) {
    return("\u2014")
  }
  parsed <- suppressWarnings(as.Date(x))
  out <- ifelse(is.na(parsed), x, format(parsed, "%Y-%m-%d"))
  bio_nz(out)
}


# Hover label (hover)
# Hover label (hover) - use template rendering (consistent with popup)
bio_build_labels <- function(
    display_name, taxa, kingdom, location, lat, lon,
    template_rel = "pages/biodiversity/module/templates/bio_hover.html") {
  esc <- htmltools::htmlEscape
  hasv <- function(x) !is.na(x) & nzchar(x)

  # Unify length to avoid recycling/out of bounds
  n <- min(
    length(display_name), length(taxa), length(kingdom),
    length(location), length(lat), length(lon)
  )
  if (n == 0) {
    return(character(0))
  }

  # Locate template
  BIO_BASE <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("app.R")),
    error = function(e) getwd()
  )
  template_path <- file.path(BIO_BASE, template_rel)

  # Fallback to old inline HTML when template doesn't exist
  if (!file.exists(template_path)) {
    tip_sub <- paste0(
      ifelse(hasv(taxa), esc(taxa), "\u2014"),
      ifelse(hasv(kingdom), paste0(" <U+00B7> ", esc(kingdom)), "")
    )
    tip_meta <- ifelse(hasv(location),
      esc(location),
      sprintf("%.4f, %.4f", lat, lon)
    )
    return(paste0(
      "<div class='bio-tip'>",
      "<div class='tip-name'>", esc(display_name), "</div>",
      "<div class='tip-sub'>", tip_sub, "</div>",
      "<div class='tip-meta'>", tip_meta, "</div>",
      "</div>"
    ))
  }

  # <U+2705> Render with template one by one
  vapply(seq_len(n), function(i) {
    # Pre-assemble conditional logic on R side, template only receives simple variables
    taxa_sub <- if (hasv(taxa[i]) && hasv(kingdom[i])) {
      sprintf("%s <U+00B7> %s", esc(taxa[i]), esc(kingdom[i]))
    } else if (hasv(taxa[i])) {
      esc(taxa[i])
    } else if (hasv(kingdom[i])) {
      esc(kingdom[i])
    } else {
      "\u2014"
    }

    meta_i <- if (hasv(location[i])) {
      esc(location[i])
    } else {
      sprintf("%.4f, %.4f", lat[i], lon[i])
    }

    tag <- htmltools::htmlTemplate(
      template_path,
      display_name = esc(display_name[i]),
      taxa_sub     = taxa_sub,
      meta         = meta_i
    )
    htmltools::doRenderTags(tag)
  }, FUN.VALUE = character(1))
}



# Click popup (click)
bio_build_popups <- function(
    display_name, species, taxa, kingdom, phylum, class_, order_, family, genus,
    sighting, datares, notes, location, lat, lon,
    commonname = NULL, # <U+2705> New: optional
    template_rel = "pages/biodiversity/module/templates/bio_popup.html") {
  esc <- htmltools::htmlEscape

  # Fallback commonname (use display_name if not provided, otherwise species)
  if (is.null(commonname)) {
    commonname <- ifelse(!is.na(display_name) & nzchar(display_name), display_name, species)
  }

  # Unify length to avoid recycling/out of bounds
  n <- min(
    length(display_name), length(species), length(taxa), length(kingdom), length(phylum),
    length(class_), length(order_), length(family), length(genus),
    length(sighting), length(datares), length(notes), length(location),
    length(lat), length(lon), length(commonname)
  )
  if (n == 0) {
    return(character(0))
  }

  # Find template
  BIO_BASE <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("app.R")),
    error = function(e) getwd()
  )
  template_path <- file.path(BIO_BASE, template_rel)

  if (!file.exists(template_path)) {
    # Template missing: fallback to old inline construction
    return(paste0(
      "<div class='bio-popup'>",
      "<div class='bp-header'><div class='bp-title'>", esc(display_name), "</div>",
      "<div class='bp-sub'>", esc(bio_nz(species)), "</div></div>",
      "</div>"
    ))
  }

  # Render as string
  vapply(seq_len(n), function(i) {
    tag <- htmltools::htmlTemplate(
      template_path,
      display_name = esc(display_name[i]),
      species      = esc(bio_nz(species[i])),
      taxa         = esc(bio_nz(taxa[i])),
      kingdom      = esc(bio_nz(kingdom[i])),
      phylum       = esc(bio_nz(phylum[i])),
      class_       = esc(bio_nz(class_[i])),
      order_       = esc(bio_nz(order_[i])),
      family       = esc(bio_nz(family[i])),
      genus        = esc(bio_nz(genus[i])),
      sighting     = esc(bio_nice_date(sighting[i])),
      datares      = esc(bio_nz(datares[i])),
      notes        = esc(bio_nz(notes[i])),
      location     = esc(bio_nz(location[i])),
      lat          = sprintf("%.5f", lat[i]),
      lon          = sprintf("%.5f", lon[i]),
      commonname   = esc(bio_nz(commonname[i])) # <U+2705> Pass to template
    )
    htmltools::doRenderTags(tag)
  }, FUN.VALUE = character(1))
}
