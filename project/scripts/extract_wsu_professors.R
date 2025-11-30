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

synthesize_email <- function(name) {
  if (!nzchar(name)) return("")

  sanitize <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

  if (grepl(",", name, fixed = TRUE)) {
    parts <- strsplit(name, ",", fixed = TRUE)[[1]]
    last <- str_trim(parts[1])
    remaining <- str_trim(paste(parts[-1], collapse = " "))
    first_tokens <- strsplit(remaining, "\\s+")[[1]]
    first_tokens <- first_tokens[nzchar(first_tokens)]
    first <- if (length(first_tokens)) first_tokens[1] else ""
  } else {
    tokens <- strsplit(name, "\\s+")[[1]]
    tokens <- tokens[nzchar(tokens)]
    first <- if (length(tokens)) tokens[1] else ""
    last <- if (length(tokens)) tokens[length(tokens)] else ""
  }

  first <- sanitize(first)
  last <- sanitize(last)
  if (!nzchar(first) || !nzchar(last)) return("")
  sprintf("%s.%s@wsu.edu", first, last)
}

# Column helpers (used as a fallback when spacing collapses)
locate_columns <- function(pages) {
  for (page in pages) {
    lines <- strsplit(page, "\n", fixed = TRUE)[[1]]
    header_idx <- which(grepl("\\bName\\b\\s+Title\\s+WSU Academic Area", lines))
    if (!length(header_idx)) next
    header_line <- lines[header_idx[1]]
    starts <- c(
      regexpr("Name", header_line)[1],
      regexpr("Title", header_line)[1],
      regexpr("WSU Academic Area", header_line)[1],
      regexpr("Highest Degree", header_line)[1],
      regexpr("Conferring", header_line)[1]
    )
    if (any(starts < 0)) next
    starts <- pmax(1, starts)
    ends <- c(starts[-1] - 1, nchar(header_line) + 50)
    return(list(starts = starts, ends = ends))
  }
  NULL
}

slice_columns <- function(line, starts, ends) {
  max_len <- max(ends)
  padded <- str_pad(line, max_len, side = "right")
  fields <- mapply(function(s, e) substr(padded, s, e), starts, ends, SIMPLIFY = FALSE)
  vapply(fields, function(x) str_squish(x), character(1))
}

append_field <- function(existing, addition) {
  if (!nzchar(addition)) return(existing)
  if (!nzchar(existing)) return(addition)
  paste(existing, addition)
}

split_degree_institution <- function(degree, institution) {
  degree <- str_squish(degree)
  institution <- str_squish(institution)

  if (nzchar(institution)) return(list(degree, institution))

  pattern <- "(Doctor of [A-Za-z ]+|Master of [A-Za-z ]+|Bachelor of [A-Za-z ]+|Juris Doctor|PhD|Ph\\.D\\.|EdD|MBA|MFA)"
  m <- regexpr(pattern, degree, ignore.case = TRUE, perl = TRUE)
  if (m[1] == -1) return(list(degree, institution))

  deg_part <- substr(degree, m[1], m[1] + attr(m, "match.length") - 1)
  tail_part <- str_squish(substr(degree, m[1] + attr(m, "match.length"), nchar(degree)))
  if (nzchar(tail_part) && !nzchar(institution)) {
    institution <- tail_part
  }

  list(str_squish(deg_part), institution)
}

parse_page <- function(page_text, cols = NULL) {
  lines <- strsplit(page_text, "\n", fixed = TRUE)[[1]]
  entries <- list()
  current <- NULL
  data_started <- FALSE

  for (line in lines) {
    if (!nzchar(str_squish(line))) next

    # Skip page headers/titles
    if (grepl("Washington State University Faculty", line, ignore.case = TRUE)) next
    if (grepl("\\bName\\b\\s+Title\\s+WSU Academic Area", line)) next
    if (grepl("Page\\s+\\d+\\s+-\\s+Appendix", line, ignore.case = TRUE)) next

    parts <- strsplit(line, "\\s{2,}")[[1]]
    parts <- trimws(parts)
    first_idx <- suppressWarnings(min(which(nzchar(parts))))
    first_token <- if (is.finite(first_idx)) parts[first_idx] else ""
    if (!nzchar(first_token)) next

    starts_new_row <- grepl("^[A-Z][A-Za-z\\.'-]+,\\s+", first_token)

    # Drop leading empties for easier column alignment on new rows
    if (starts_new_row && first_idx > 1) {
      parts <- parts[first_idx:length(parts)]
    }

    if (!data_started && !starts_new_row) next
    data_started <- TRUE

    if (starts_new_row) {
      if (!is.null(current)) {
        entries <- append(entries, list(current))
      }
      if (length(parts) == 1 && !is.null(cols)) {
        fields <- slice_columns(line, cols$starts, cols$ends)
        current <- list(
          name = fields[1],
          title = fields[2],
          area = fields[3],
          degree = fields[4],
          institution = fields[5],
          raw = str_squish(line)
        )
      } else {
        parts <- c(parts, rep("", max(0, 5 - length(parts))))
        current <- list(
          name = parts[1],
          title = ifelse(length(parts) >= 2, parts[2], ""),
          area = ifelse(length(parts) >= 3, parts[3], ""),
          degree = ifelse(length(parts) >= 4, parts[4], ""),
          institution = ifelse(length(parts) >= 5, parts[5], ""),
          raw = str_squish(line)
        )
      }
    } else if (!is.null(current)) {
      # Continuation lines: first token is blank, remaining tokens map from the end (area -> degree -> institution)
      tokens <- parts[-1]
      if (length(tokens)) {
        inst_add <- ""
        degree_add <- ""
        area_add <- ""

        if (length(tokens) == 1) {
          # Heuristic: single token continuation goes to institution if it looks like one, else area
          if (nzchar(current$institution) && grepl("University of$", current$institution)) {
            inst_add <- tokens[1]
          } else if (grepl("University|College|Institute|School|State|Center", tokens[1], ignore.case = TRUE)) {
            inst_add <- tokens[1]
          } else {
            area_add <- tokens[1]
          }
        } else if (length(tokens) == 2) {
          last <- tokens[2]
          first <- tokens[1]
          if (nzchar(current$institution) && grepl("University of$", current$institution)) {
            area_add <- first
            inst_add <- last
          } else {
            if (grepl("Doctor|Master|PhD|Ph\\.D", first, ignore.case = TRUE)) {
              degree_add <- first
            } else {
              area_add <- first
            }
            if (grepl("University|College|Institute|School|State|Center", last, ignore.case = TRUE)) {
              inst_add <- last
            } else if (!nzchar(degree_add)) {
              degree_add <- last
            } else {
              inst_add <- last
            }
          }
        } else {
          inst_add <- tokens[length(tokens)]
          degree_add <- tokens[length(tokens) - 1]
          area_add <- paste(tokens[1:(length(tokens) - 2)], collapse = " ")
        }

        if (nzchar(area_add)) current$area <- append_field(current$area, area_add)
        if (nzchar(degree_add)) current$degree <- append_field(current$degree, degree_add)
        if (nzchar(inst_add)) current$institution <- append_field(current$institution, inst_add)
      }

      current$raw <- append_field(current$raw, str_squish(line))
    }
  }

  if (!is.null(current)) {
    entries <- append(entries, list(current))
  }

  entries
}

col_positions <- locate_columns(text_pages)
entries <- unlist(lapply(text_pages, function(pg) parse_page(pg, cols = col_positions)), recursive = FALSE)

entries <- entries[!vapply(entries, is.null, logical(1))]
if (!length(entries)) {
  stop("No faculty entries were parsed from the PDF. Check the PDF format.", call. = FALSE)
}

prof_df <- bind_rows(lapply(entries, function(x) {
  tibble(
    name = x$name,
    title = x$title,
    department = x$area,
    degree = x$degree,
    conferring_institution = x$institution,
    raw_line = x$raw
  )
})) %>%
  mutate(
    name = str_squish(name),
    title = str_squish(title),
    department = str_squish(department),
    degree = str_squish(degree),
    conferring_institution = str_squish(conferring_institution)
  ) %>%
  filter(grepl("professor", title, ignore.case = TRUE))

if (!nrow(prof_df)) {
  stop("No professor rows found after filtering titles containing 'Professor'.", call. = FALSE)
}

# Clean departments that captured degree text, and split combined degree/institution fields
if (nrow(prof_df)) {
  cleaned <- lapply(seq_len(nrow(prof_df)), function(i) {
    dept <- prof_df$department[i]
    degree <- prof_df$degree[i]
    inst <- prof_df$conferring_institution[i]

    if (grepl("Doctor of|Master of|Bachelor of", dept, ignore.case = TRUE)) {
      extra <- str_squish(sub(".*?(Doctor of.*)", "\\1", dept, perl = TRUE))
      dept <- str_squish(sub("(Doctor of.*)", "", dept, perl = TRUE))
      degree <- append_field(degree, extra)
    }

    split <- split_degree_institution(degree, inst)
    dept_clean <- str_squish(dept)
    inst_clean <- str_squish(split[[2]])

    # If institution ended with a comma and department ends with a trailing city token, reattach it
    if (grepl(",$", inst_clean) && grepl("\\b[A-Za-z]+$", dept_clean)) {
      trailing_token <- sub(".*\\b([A-Za-z]+)$", "\\1", dept_clean, perl = TRUE)
      if (!grepl("College|University|Campus", trailing_token, ignore.case = TRUE)) {
        dept_clean <- str_squish(sub(paste0("\\b", trailing_token, "$"), "", dept_clean))
        inst_clean <- str_squish(paste(inst_clean, trailing_token))
      }
    }

    list(
      department = dept_clean,
      degree = str_squish(split[[1]]),
      conferring_institution = inst_clean
    )
  })

  prof_df$department <- vapply(cleaned, function(x) x$department, character(1))
  prof_df$degree <- vapply(cleaned, function(x) x$degree, character(1))
  prof_df$conferring_institution <- vapply(cleaned, function(x) x$conferring_institution, character(1))
}

prof_df <- prof_df %>%
  mutate(
    email = vapply(name, synthesize_email, character(1)),
    phone = "",
    campus = "",
    bio = str_squish(paste(
      ifelse(nzchar(degree), paste0("Highest Degree: ", degree), ""),
      ifelse(nzchar(conferring_institution), paste0("Conferring Institution: ", conferring_institution), "")
    ))
  ) %>%
  filter(nzchar(email)) %>%
  distinct(email, .keep_all = TRUE) %>%
  select(name, department, title, email, phone, campus, raw_line, bio)

output <- list(
  source = basename(pdf_path),
  updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  professor_count = nrow(prof_df),
  professors = prof_df
)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write_json(output, output_path, pretty = TRUE, auto_unbox = TRUE)

message(sprintf("Saved %d professors to %s", nrow(prof_df), normalizePath(output_path)))
