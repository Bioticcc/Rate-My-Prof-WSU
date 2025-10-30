#FOR PUSHING TO THE GITHUB REPO, FOLLOW THESE STEPS:
#git status #whats changed
#git add . #all files or specific (specific is prolly safer)
#git commit -m "your message here" #commit plus message
#git push #pushes changes to repo!

#FOR PULLING FROM THE MAIN REPO, FOLLOW THESE STEPS:
#git status
#git pull origin main


#When running from VScode, this is how you do it:
#(Ctrl + ~) to open terminal for project.
#cd project (unless you are already in that folder)
#run the command: R (this will open R terminal similair to the ghci thing from 355)
#run the command: shiny::runApp("project")
#when you want to close the app, click on terminal and press: (Ctrl C)
#close the newly opened browser tab!

# -------- global.R --------

# General options (optional but handy)
options(shiny.autoreload = TRUE)
options(shiny.reactlog = FALSE)
# If you use bslib Google fonts:
# options(bslib.fonts.download = "yes")

# Core
library(shiny)

# UI polish / widgets
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinyvalidate)
library(DT)

# Authentication
library(shinymanager)

# Data / persistence
library(DBI)
library(RSQLite)
# library(RPostgres)   # keep only if you actually use Postgres
library(pool)
library(dplyr)
library(dbplyr)
library(scrypt) # password hashing support

# Data wrangling & export
library(tidyr)
library(readr)
library(jsonlite)

# Plots (if you use them)
library(ggplot2)
library(plotly)

# ---- Globals / config / helpers go here ----
APP_TITLE <- "Rate-My-Prof-WSU"

# Example: paths, constants, small helper functions
data_dir <- "data"
credentials_db_path <- "credentials.sqlite"
credentials_passphrase <- "change-this-passphrase"
reviews_db_path <- file.path(data_dir, "reviews.sqlite")
VALID_REVIEW_TYPES <- c("Course", "Professor")

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Example helper (available in both ui.R and server.R)
is_admin <- function(auth) isTRUE(auth$user_info$admin)

# ensure_credentials_store() creates an encrypted SQLite credential store when the app boots.
ensure_credentials_store <- function(db_path = credentials_db_path,
                                     passphrase = credentials_passphrase) {
  changed <- FALSE

  if (!file.exists(db_path)) {
    shinymanager::create_db(
      credentials_data = data.frame(
        user = character(),
        password = character(),
        start = as.POSIXct(character()),
        expire = as.POSIXct(character()),
        admin = logical(),
        verified = logical(),
        is_hashed_password = logical(),
        created_at = character(),
        stringsAsFactors = FALSE
      ),
      sqlite_path = db_path,
      passphrase = passphrase
    )
    changed <- TRUE
  }

  creds <- tryCatch(
    shinymanager::read_db_decrypt(db_path, passphrase = passphrase),
    error = function(err) {
      warning(sprintf("Unable to read credentials db: %s", err$message))
      data.frame(stringsAsFactors = FALSE)
    }
  )

  if (!nrow(creds)) {
    if ("verified" %ni% names(creds)) {
      creds$verified <- logical()
    }
    if ("created_at" %ni% names(creds)) {
      creds$created_at <- character()
    }
  }

  if ("verified" %ni% names(creds)) {
    creds$verified <- TRUE
    changed <- TRUE
  } else if (!is.logical(creds$verified)) {
    creds$verified <- tolower(as.character(creds$verified)) %in% c("true", "t", "1", "yes")
    changed <- TRUE
  }

  if ("created_at" %ni% names(creds)) {
    creds$created_at <- NA_character_
    changed <- TRUE
  }

  if ("is_hashed_password" %ni% names(creds)) {
    creds$is_hashed_password <- TRUE
    changed <- TRUE
  }

  if ("start" %in% names(creds) && !inherits(creds$start, "POSIXt")) {
    creds$start <- suppressWarnings(as.POSIXct(creds$start, tz = "UTC"))
    changed <- TRUE
  }

  if ("expire" %in% names(creds) && !inherits(creds$expire, "POSIXt")) {
    creds$expire <- suppressWarnings(as.POSIXct(creds$expire, tz = "UTC"))
    changed <- TRUE
  }

  if (changed) {
    shinymanager::write_db_encrypt(
      conn = db_path,
      value = creds,
      passphrase = passphrase
    )
  }
}

# register_user() adds a new credential row after validating uniqueness and hashing the password.
register_user <- function(username,
                          password,
                          db_path = credentials_db_path,
                          passphrase = credentials_passphrase) {
  ensure_credentials_store(db_path, passphrase)

  creds <- shinymanager::read_db_decrypt(db_path, passphrase = passphrase)

  if ("created_at" %ni% names(creds)) {
    creds$created_at <- NA_character_
  }

  if (any(creds$user == username, na.rm = TRUE)) {
    return(list(ok = FALSE, message = "That username is already in use."))
  }

  hashed_password <- scrypt::hashPassword(password)
  created_stamp   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  new_cred <- dplyr::bind_rows(
    creds,
    dplyr::tibble(
      user = username,
      password = hashed_password,
      start = as.POSIXct(NA),
      expire = as.POSIXct(NA),
      admin = FALSE,
      is_hashed_password = TRUE,
      verified = TRUE,
      created_at = created_stamp
    )
  )

  shinymanager::write_db_encrypt(
    conn = db_path,
    value = new_cred,
    passphrase = passphrase
  )

  list(ok = TRUE, message = "Account created! You can sign in now.")
}

# get_user_profile() returns basic profile metadata to populate the header dropdown.
get_user_profile <- function(username,
                             db_path = credentials_db_path,
                             passphrase = credentials_passphrase) {
  ensure_credentials_store(db_path, passphrase)

  creds <- shinymanager::read_db_decrypt(db_path, passphrase = passphrase)

  if (!nrow(creds) || !any(creds$user == username, na.rm = TRUE)) {
    return(NULL)
  }

  if ("created_at" %ni% names(creds)) {
    creds$created_at <- NA_character_
  }

  if ("verified" %ni% names(creds)) {
    creds$verified <- TRUE
  }

  row <- creds[match(username, creds$user), , drop = FALSE]

  user_reviews <- fetch_user_reviews(username)

  list(
    username = row$user,
    created_at = row$created_at,
    password_hash = row$password,
    verified = isTRUE(row$verified),
    reviews_published = nrow(user_reviews),
    reviews = user_reviews
  )
}

# ensure_review_store() prepares a simple SQLite database for user reviews.
ensure_review_store <- function(db_path = reviews_db_path) {
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS reviews (
       review_id INTEGER PRIMARY KEY,
       author TEXT NOT NULL,
       review_type TEXT NOT NULL,
       subject TEXT NOT NULL,
       title TEXT NOT NULL,
       body TEXT NOT NULL,
       created_at TEXT NOT NULL
     )"
  )
}

# create_review() writes a single review row.
create_review <- function(author, review_type, subject, title, body,
                          db_path = reviews_db_path) {
  ensure_review_store(db_path)

  review_type <- match.arg(review_type, VALID_REVIEW_TYPES)
  now_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(
    con,
    "INSERT INTO reviews (author, review_type, subject, title, body, created_at)
     VALUES (:author, :review_type, :subject, :title, :body, :created_at)",
    params = list(
      author = author,
      review_type = review_type,
      subject = subject,
      title = title,
      body = body,
      created_at = now_stamp
    )
  )
}

# fetch_user_reviews() retrieves all reviews authored by a user ordered newest first.
fetch_user_reviews <- function(author, db_path = reviews_db_path) {
  ensure_review_store(db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  df <- DBI::dbGetQuery(
    con,
    "SELECT review_id, review_type, subject, title, body, created_at
     FROM reviews
     WHERE author = :author
     ORDER BY datetime(created_at) DESC",
    params = list(author = author)
  )

  tibble::as_tibble(df)
}

# list_reviews_for_subject() returns reviews for a given subject/type pair.
list_reviews_for_subject <- function(review_type, subject,
                                     db_path = reviews_db_path) {
  ensure_review_store(db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  df <- DBI::dbGetQuery(
    con,
    "SELECT review_id, author, title, body, created_at
     FROM reviews
     WHERE review_type = :review_type AND subject = :subject
     ORDER BY datetime(created_at) DESC",
    params = list(review_type = review_type, subject = subject)
  )

  tibble::as_tibble(df)
}

# Utility to mimic magrittr's %ni% without loading the pipe just for this helper.
`%ni%` <- function(x, table) !(match(x, table, nomatch = 0) > 0)

ensure_credentials_store()
ensure_review_store()
