#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(stringr)
  library(dplyr)
})

if (!requireNamespace("pdftools", quietly = TRUE)) {
  stop("Package 'pdftools' is required. Install with install.packages('pdftools').", call. = FALSE)
}

pdf_path <- Sys.getenv("WSU_FACULTY_ROSTER_PDF", unset = file.path("project", "data", "wsufacultyroster.pdf"))
output_path <- Sys.getenv("WSU_PROFESSOR_OUTPUT", unset = file.path("project", "data", "wsu_professors.json"))

if (!file.exists(pdf_path)) {
  stop(sprintf("Faculty roster PDF not found at %s", pdf_path), call. = FALSE)
}

message(sprintf("Reading PDF: %s", pdf_path))
text_pages <- pdftools::pdf_text(pdf_path)

lines <- unlist(strsplit(text_pages, "\n", fixed = TRUE), use.names = FALSE)
clean <- str_trim(str_replace_all(lines, "\\s+", " "))
clean <- clean[nzchar(clean)]

email_pattern <- "[A-Za-z0-9._%+-]+@wsu\\.edu"
phone_pattern <- "\\b\\d{3}[-. ]?\\d{3}[-. ]?\\d{4}\\b"
campus_pattern <- "Pullman|Vancouver|Tri-?Cities|Spokane|Everett|Global Campus"

entries <- lapply(clean[grepl(email_pattern, clean, ignore.case = TRUE)], function(line) {
  emails <- str_extract_all(line, email_pattern, simplify = TRUE)
  emails <- emails[emails != ""]
  if (!length(emails)) return(NULL)

  # remove first email to parse name/details
  email <- emails[1]
  rest <- str_trim(str_replace(line, email, ""))

  name_guess <- str_extract(rest, "^[A-Za-z\\.'-]+,?\\s+[A-Za-z\\.'-]+")
  phone <- str_extract(line, phone_pattern)
  campus <- str_extract(line, campus_pattern)

  # department/title guess: drop name and phone from rest
  dept <- rest
  if (!is.na(name_guess)) dept <- str_trim(str_replace(dept, fixed(name_guess), ""))
  if (!is.na(phone)) dept <- str_trim(str_replace(dept, phone, ""))

  list(
    name = ifelse(is.na(name_guess), "", name_guess),
    email = tolower(email),
    phone = ifelse(is.na(phone), "", phone),
    campus = ifelse(is.na(campus), "", campus),
    department = dept,
    title = "",
    raw_line = line
  )
})

entries <- entries[!vapply(entries, is.null, logical(1))]
if (!length(entries)) {
  stop("No entries with @wsu.edu emails were found. Check the PDF format.", call. = FALSE)
}

prof_df <- bind_rows(entries)
prof_df <- prof_df %>%
  mutate(
    name = str_trim(name),
    department = str_trim(department),
    title = str_trim(title),
    email = tolower(email)
  ) %>%
  filter(nzchar(email)) %>%
  distinct(email, .keep_all = TRUE)

output <- list(
  source = basename(pdf_path),
  updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  professor_count = nrow(prof_df),
  professors = prof_df
)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write_json(output, output_path, pretty = TRUE, auto_unbox = TRUE)

message(sprintf("Saved %d professors to %s", nrow(prof_df), normalizePath(output_path)))
