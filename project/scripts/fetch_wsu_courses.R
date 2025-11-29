#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

catalog_base <- "https://catalog.wsu.edu/api/Data"
campus <- Sys.getenv("WSU_CAMPUS", unset = "Pullman")
output_path <- Sys.getenv(
  "WSU_COURSE_OUTPUT",
  unset = file.path("project", "data", "wsu_courses.json")
)
subject_list_path <- Sys.getenv(
  "WSU_SUBJECT_LIST_PATH",
  unset = file.path("project", "data", "subjectList.json")
)
u_token <- Sys.getenv("WSU_CATALOG_U_TOKEN", unset = "")
query_params <- if (nzchar(u_token)) list(u = u_token) else NULL

safe_get <- function(url, query = NULL, ...) {
  resp <- tryCatch(
    GET(url, query = query, ...),
    error = function(err) {
      stop(sprintf("Request failed for %s: %s", url, err$message), call. = FALSE)
    }
  )
  stop_for_status(resp)
  content(resp, as = "text", encoding = "UTF-8")
}

dropdown_url <- sprintf("%s/LoadCoursesDropDowns/%s", catalog_base, campus)
message(sprintf("Fetching subject list from %s", dropdown_url))
dropdown_raw <- tryCatch(
  safe_get(dropdown_url, query = query_params),
  error = function(err) {
    warning(err$message)
    NA_character_
  }
)

subjects_tbl <- NULL

if (!is.na(dropdown_raw)) {
  dropdown <- fromJSON(dropdown_raw, simplifyVector = TRUE)
  subjects_tbl <- dropdown$subjects %||% dropdown$Subjects %||%
    dropdown$subjectList %||% dropdown$SubjectList
}

if (is.null(subjects_tbl) && file.exists(subject_list_path)) {
  message(sprintf("Falling back to subject list file: %s", subject_list_path))
  fallback_raw <- readChar(subject_list_path, file.info(subject_list_path)$size)
  fallback_json <- fromJSON(fallback_raw, simplifyVector = TRUE)
  subjects_tbl <- fallback_json$subjectList %||% fallback_json$subjects %||%
    fallback_json$Subjects %||% fallback_json
}

if (is.null(subjects_tbl)) {
  stop("Unable to locate `subjects` array in LoadCoursesDropDowns response or fallback file.")
}

subject_codes_raw <- subjects_tbl$subject %||% subjects_tbl$Subject %||%
  subjects_tbl$id %||% subjects_tbl$ID %||% subjects_tbl$value
if (is.null(subject_codes_raw)) {
  stop("`subjects` payload does not contain a recognizable subject code column.")
}

subject_df <- tibble(
  subject_raw = subject_codes_raw,
  subject_code = trimws(subject_codes_raw),
  prefix = subjects_tbl$prefix %||% subjects_tbl$Prefix %||% subjects_tbl$id,
  title = subjects_tbl$prefixTitle %||% subjects_tbl$Title %||%
    subjects_tbl$title %||% subjects_tbl$prefix
) %>%
  filter(!is.na(subject_code) & nzchar(subject_code))

if (!nrow(subject_df)) {
  stop("No subject codes returned by the catalog API.")
}

message(sprintf("Found %d subjects; downloading course lists…", nrow(subject_df)))

course_endpoint <- function(subject_raw) {
  encoded <- utils::URLencode(subject_raw, reserved = TRUE)
  sprintf("%s/GetCoursesBySubject/%s/%s", catalog_base, encoded, campus)
}

all_courses <- vector("list", nrow(subject_df))
for (idx in seq_len(nrow(subject_df))) {
  subj_raw <- subject_df$subject_raw[idx]
  subj_code <- subject_df$subject_code[idx]
  url <- course_endpoint(subj_raw)
  message(sprintf("[%03d/%03d] %s -> %s", idx, nrow(subject_df), subj_code, url))
  course_raw <- tryCatch(
    safe_get(url, query = query_params),
    error = function(err) {
      warning(err$message)
      return(NULL)
    }
  )
  if (is.null(course_raw)) next

  parsed <- fromJSON(course_raw, simplifyVector = TRUE, flatten = TRUE)
  if (!length(parsed)) next
  parsed$subject_code <- subj_code
  parsed$subject_raw <- subj_raw
  all_courses[[idx]] <- parsed
  Sys.sleep(0.2)
}

course_df <- bind_rows(all_courses)

if (!nrow(course_df)) {
  stop("No courses were returned by the catalog API.")
}

clean_courses <- course_df %>%
  mutate(
    course_number = as.character(number),
    course_id = trimws(sprintf("%s %s", subject_code, course_number)),
    title = trimws(ifelse(!is.na(longTitle) & nzchar(longTitle), longTitle, shortTitle)),
    department = trimws(prefixTitle %||% prefix),
    description = description,
    credits = creditsPhrase,
    typically_offered = typicallyOffered
  ) %>%
  arrange(course_id, desc(yearEnd), desc(termEnd)) %>%
  distinct(course_id, .keep_all = TRUE) %>%
  select(
    course_id,
    subject_code,
    course_number,
    title,
    department,
    description,
    credits,
    typically_offered,
    everything()
  )

output <- list(
  campus = campus,
  updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  subject_count = nrow(subject_df),
  course_count = nrow(clean_courses),
  subjects = subject_df,
  courses = clean_courses
)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write_json(output, output_path, pretty = TRUE, auto_unbox = TRUE)

message(sprintf("Saved %d courses to %s", nrow(clean_courses), normalizePath(output_path)))
