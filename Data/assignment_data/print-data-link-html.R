##### Make HTML code for the download links on the website

created_files <- character()

for (a in c("WVS7/wvs7", "EVS2017/evs2017", "ESS10/ess10")) {

  d <- datawizard::data_read(paste0("Data/assignment_data/", a, ".sav"))
  
  country_codes <- sjlabelled::get_labels(d$country_iso3, drop.unused = TRUE)
  country_names <- sjlabelled::get_labels(d$country,      drop.unused = TRUE)
  
  if (length(country_codes) == 0) next 
  
  short_name <- sub(".*/", "", a)

  out_file <- file.path("Data/assignment_data", paste0("_", short_name, "-country-links.md"))

  lines <- character(length(country_codes) + 1)
  lines[1] <- paste0("<!-- generated from ", a, ".sav -->")

  for (i in seq_along(country_codes)) {
    link <- paste0('/Data/assignment_data/', a, '_', country_codes[i], '.sav')
    label <- htmltools::htmlEscape(country_names[i])
    line <- paste0('<li><a class="dropdown-item" href="', link, '">', label, '</a></li>')
    # comment out UK-focused datasets"
    if (grepl("_GBR\\.sav$", link, ignore.case = FALSE)) {
      line <- paste0("<!-- ", line, " -->")
    }
    lines[i + 1] <- line  
  }

  writeLines(lines, con = out_file, useBytes = TRUE)
  created_files <- c(created_files, out_file)
}

 