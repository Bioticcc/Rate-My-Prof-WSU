#Server is where we keep any functional aspects, like making reviews, publishing them,
#getting the list of WSU professors, authentication eventually, logging in, and more    


library(shinymanager)

server <- function(input, output, session) {

  ensure_credentials_store()

  # 1) Credentials checker 
  check_creds <- function(user, password) {
    ensure_credentials_store()

    creds <- shinymanager::read_db_decrypt(
      credentials_db_path,
      passphrase = credentials_passphrase
    )

    # normalize potential whitespace to mirror registration rules
    creds$user <- trimws(as.character(creds$user))
    if ("start" %in% names(creds) && !inherits(creds$start, "POSIXt")) {
      creds$start <- suppressWarnings(as.POSIXct(creds$start, tz = "UTC"))
    }
    if ("expire" %in% names(creds) && !inherits(creds$expire, "POSIXt")) {
      creds$expire <- suppressWarnings(as.POSIXct(creds$expire, tz = "UTC"))
    }
    if ("admin" %in% names(creds) && !is.logical(creds$admin)) {
      creds$admin <- tolower(as.character(creds$admin)) %in% c("true", "t", "1", "yes")
    }
    if ("verified" %in% names(creds) && !is.logical(creds$verified)) {
      creds$verified <- tolower(as.character(creds$verified)) %in% c("true", "t", "1", "yes")
    }
    if ("is_hashed_password" %in% names(creds) && !is.logical(creds$is_hashed_password)) {
      creds$is_hashed_password <- tolower(as.character(creds$is_hashed_password)) %in% c("true", "t", "1", "yes")
    }
    user_clean <- trimws(if (is.null(user)) "" else as.character(user))
    password_clean <- if (is.null(password)) "" else password

    checker <- shinymanager::check_credentials(creds)
    checker(user_clean, password_clean)
  }

  # 2) Start the login module
  auth <- callModule(
    shinymanager::auth_server,
    "login",
    check_credentials = check_creds
  )
  # auth_server() returns reactive values; auth$result becomes TRUE after login

  auth_mode     <- reactiveVal("login")
  signup_status <- reactiveVal(NULL)
  auth_result   <- reactive({ auth$result })
  active_user   <- reactive({
    req(isTRUE(auth_result()))
    auth$user
  })
  user_verified <- reactive({
    req(isTRUE(auth_result()))
    info <- auth$user_info
    if (is.null(info$verified)) TRUE else isTRUE(info$verified)
  })
  user_is_admin <- reactive({
    isTRUE(auth_result()) && isTRUE(auth$user_info$admin)
  })
  reviews_updated <- reactiveVal(Sys.time())
  # value_or_empty() normalizes NULL input values to empty strings for validation.
  value_or_empty <- function(x) if (is.null(x)) "" else x
  review_state <- reactiveValues(
    type = VALID_REVIEW_TYPES[1],
    subject = NULL,
    source = "home"
  )
  review_feedback <- reactiveVal(NULL)
  first_non_empty <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    if (!length(vals)) {
      return("")
    }
    vals <- vals[!is.na(vals)]
    vals <- trimws(as.character(vals))
    vals <- vals[nzchar(vals)]
    if (length(vals)) vals[1] else ""
  }

  # 3) Control overlay visibility
  showLogin   <- reactiveVal(FALSE)

  observeEvent(input$loginB, {
    auth_mode("login")
    showLogin(TRUE)
  })   # open overlay when user clicks Login

  observeEvent(input$open_auth_header, {
    auth_mode("login")
    showLogin(TRUE)
  })

  observeEvent(input$backHome, {
    showLogin(FALSE)
    auth_mode("login")
  })   # close overlay manually

  observeEvent(auth_result(), {
    if (isTRUE(auth_result())) {
      showLogin(FALSE)        # auto-close overlay after successful login
      auth_mode("login")
    }
  })

  # 4) ConditionalPanel driver: only show when overlay is open AND not authenticated
  output$displayLogin <- reactive({
    !isTRUE(auth_result()) && isTRUE(showLogin())
  })
  outputOptions(output, "displayLogin", suspendWhenHidden = FALSE)

  observeEvent(input$auth_tab_login, {
    auth_mode("login")
  })

  observeEvent(input$auth_tab_signup, {
    auth_mode("signup")
    showLogin(TRUE)
  })

  observeEvent(input$auth_link_signup, {
    auth_mode("signup")
    showLogin(TRUE)
  })

  observeEvent(input$auth_link_login, {
    auth_mode("login")
  })

  observeEvent(input$open_signup_header, {
    auth_mode("signup")
    showLogin(TRUE)
  })

  observeEvent(auth_mode(), {
    if (!identical(auth_mode(), "signup")) {
      signup_status(NULL)
    }
  })

  observeEvent(input$submit_signup, {
    username <- trimws(value_or_empty(input$signup_username))
    password <- value_or_empty(input$signup_password)
    confirm  <- value_or_empty(input$signup_confirm)
    admin_secret <- trimws(value_or_empty(input$signup_admin_password))

    if (!nzchar(username) || nchar(username) < 3) {
      signup_status(list(type = "error", text = "Choose a username with at least 3 characters."))
      return()
    }

    if (!nzchar(password) || nchar(password) < 6) {
      signup_status(list(type = "error", text = "Passwords need at least 6 characters."))
      return()
    }

    if (!identical(password, confirm)) {
      signup_status(list(type = "error", text = "Passwords do not match, try again."))
      return()
    }

    wants_admin <- nzchar(admin_secret)
    admin_flag <- FALSE
    if (wants_admin) {
      if (!identical(admin_secret, ADMIN_SIGNUP_SECRET)) {
        signup_status(list(type = "error", text = "Admin password is incorrect. Leave blank for a regular account."))
        return()
      }
      admin_flag <- TRUE
    }

    result <- tryCatch(
      register_user(username, password, admin = admin_flag),
      error = function(err) {
        fallback <- err$message
        if (is.null(fallback) || !nzchar(fallback)) {
          fallback <- "Unable to register right now."
        }
        list(ok = FALSE, text = fallback)
      }
    )

    if (!isTRUE(result$ok)) {
      message_text <- result$message
      if (is.null(message_text) || !nzchar(message_text)) {
        message_text <- result$text
      }
      if (is.null(message_text) || !nzchar(message_text)) {
        message_text <- "Registration failed, please retry."
      }
      signup_status(list(type = "error", text = message_text))
      return()
    }

    success_message <- result$message
    if (isTRUE(admin_flag)) {
      success_message <- paste(success_message, "Admin privileges granted.")
    }
    signup_status(list(type = "success", text = success_message))
    showNotification("Account created! Log in from the Login tab.", type = "message", duration = 5)
    auth_mode("login")
    updateTextInput(session, "signup_username", value = "")
    updateTextInput(session, "signup_password", value = "")
    updateTextInput(session, "signup_confirm", value = "")
    updateTextInput(session, "signup_admin_password", value = "")
  })

  output$auth_modal_tabs <- renderUI({
    mode <- auth_mode()
    div(
      class = "auth-tabs",
      actionLink(
        inputId = "auth_tab_login",
        label   = "Login",
        class   = paste("auth-tab", if (identical(mode, "login")) "active" else "")
      ),
      actionLink(
        inputId = "auth_tab_signup",
        label   = "Sign up",
        class   = paste("auth-tab", if (identical(mode, "signup")) "active" else "")
      )
    )
  })

  output$signup_feedback <- renderUI({
    status <- signup_status()
    if (is.null(status)) {
      return(NULL)
    }

    div(
      class = paste("signup-feedback", status$type),
      status$text
    )
  })

  output$auth_modal_body <- renderUI({
    if (identical(auth_mode(), "signup")) {
      return(
        div(
          class = "auth-modal-body signup",
          textInput("signup_username", "Username"),
          passwordInput("signup_password", "Password"),
          passwordInput("signup_confirm", "Confirm password"),
          passwordInput("signup_admin_password", "Admin password (optional)"),
          div(class = "auth-note", tags$small("Enter the admin password only if you were given one; otherwise leave blank.")),
          uiOutput("signup_feedback"),
          actionButton("submit_signup", "Create account", class = "btn btn-primary auth-action"),
          div(
            class = "auth-switcher",
            span("Already have an account?"),
            actionLink("auth_link_login", "Back to login", class = "auth-link")
          )
        )
      )
    }

    ns <- NS("login")

    tagList(
      div(
        class = "auth-modal-body login",
        textInput(ns("user_id"), "Username"),
        passwordInput(ns("user_pwd"), "Password"),
        div(id = ns("result_auth"), class = "auth-feedback"),
        actionButton(ns("go_auth"), "Login", class = "btn btn-primary auth-action"),
        div(class = "auth-note", tags$small("Forgot password? Contact admin.")),
        div(
          class = "auth-switcher",
          span("New here?"),
          actionLink("auth_link_signup", "Create an account", class = "auth-link")
        ),
        tags$div(id = ns("update_shinymanager_language"), class = "shiny-html-output")
      ),
      tags$script("bindEnter('login-');")
    )
  })

  output$user_profile_header <- renderUI({
    reviews_updated()

    dark_toggle_btn <- tags$button(
      id = "darkModeToggle",
      type = "button",
      class = "btn btn-outline-light btn-sm dark-mode-toggle",
      `aria-pressed` = "false",
      tags$span(class = "toggle-icon", "🌙"),
      tags$span(class = "toggle-label", "Dark Mode")
    )

    if (!isTRUE(auth_result())) {
      return(
        div(
          class = "user-profile-header",
          dark_toggle_btn,
          actionButton("open_signup_header", "Sign up", class = "btn btn-primary btn-sm"),
          actionButton("open_auth_header", "Login", class = "btn btn-outline-light btn-sm")
        )
      )
    }

    profile <- get_user_profile(active_user())
    if (is.null(profile)) {
      return(NULL)
    }

    joined_label <- tryCatch(
      {
        parsed <- as.POSIXct(profile$created_at, tz = "UTC")
        if (is.na(parsed)) {
          NULL
        } else {
          format(parsed, "%B %d, %Y")
        }
      },
      error = function(...) NULL
    )

    password_hint <- profile$password_hash
    if (isTRUE(nzchar(password_hint))) {
      password_hint <- paste0(substr(password_hint, 1, 20), "…")
    } else {
      password_hint <- "Encrypted"
    }
    status_label <- if (isTRUE(profile$verified)) "Verified" else "Not verified"

    dropdown <- shinyWidgets::dropdownButton(
      circle = FALSE,
      size = "sm",
      right = TRUE,
      width = "280px",
      icon = icon("user-circle"),
      status = "default",
      tooltip = shinyWidgets::tooltipOptions(title = "View profile"),
      label = tags$span(class = "user-profile-label", profile$username),
      div(
        class = "user-profile-card",
        tags$h4(profile$username),
        tags$p(
          class = "user-profile-subtitle",
          if (is.null(joined_label)) "Member profile" else sprintf("Member since %s", joined_label)
        ),
        tags$dl(
          class = "user-profile-details",
          tags$dt("Username"),
          tags$dd(profile$username),
          tags$dt("Password"),
          tags$dd(tags$code(password_hint)),
          tags$dt("Role"),
          tags$dd(if (isTRUE(profile$admin)) "Admin" else "Member"),
          tags$dt("Published reviews"),
          tags$dd(profile$reviews_published),
          tags$dt("Likes received"),
          tags$dd(profile$total_likes),
          tags$dt("Status"),
          tags$dd(status_label)
        ),
        tags$p(
          class = "user-profile-note",
          if (isTRUE(profile$verified)) {
            "Passwords are stored securely — view and manage your reviews below."
          } else {
            "Account awaiting verification — reviews can be drafted once verified."
          }
        ),
        div(
          class = "user-profile-actions",
          actionButton("view_profile", "View profile", class = "btn btn-primary btn-sm view-profile-btn", width = "100%")
        )
      )
  )

  div(
    class = "user-profile-header logged-in",
    dark_toggle_btn,
    dropdown
  )
})

  show_user_profile_modal <- function(username) {
    profile <- get_user_profile(username)
    if (is.null(profile)) {
      showNotification("Profile not found.", type = "error", duration = 4)
      return(invisible(NULL))
    }

    created_display <- tryCatch(
      {
        parsed <- as.POSIXct(profile$created_at, tz = "UTC")
        if (is.na(parsed)) "Not recorded" else format(parsed, "%B %d, %Y at %I:%M %p %Z")
      },
      error = function(...) "Not recorded"
    )

    reviews <- profile$reviews
    current_user <- if (isTRUE(auth_result())) active_user() else NULL
    is_owner <- !is.null(current_user) && identical(current_user, profile$username)
    stats <- if (!is.null(reviews) && nrow(reviews)) review_like_stats(reviews$review_id, current_user) else data.frame()
    review_cards <- list()

    if (!is.null(reviews) && nrow(reviews) > 0) {
      review_cards <- lapply(seq_len(nrow(reviews)), function(idx) {
        review <- reviews[idx, , drop = FALSE]
        created <- tryCatch(
          {
            parsed <- as.POSIXct(review$created_at, tz = "UTC")
            if (is.na(parsed)) "Unknown date" else format(parsed, "%b %d, %Y")
          },
          error = function(...) "Unknown date"
        )
        like_btn <- render_like_button(review$review_id, stats)
        delete_btn <- render_delete_button(review$review_id, review$author, allow_delete = is_owner)

        div(
          class = "profile-review-card",
          div(
            class = "profile-review-top",
            span(class = "profile-review-chip", review$review_type),
            span(class = "profile-review-date", created)
          ),
          h4(class = "profile-review-title", review$title),
          p(class = "profile-review-subject", review$subject),
          p(class = "profile-review-body", review$body),
          div(class = "subject-review-actions", like_btn, delete_btn)
        )
      })
    } else {
      review_cards <- list(
        div(
          class = "profile-review-empty",
          icon("file-alt"),
          p("No reviews published yet. Start by writing your first review!")
        )
      )
    }

    showModal(
      modalDialog(
        title = NULL,
        easyClose = TRUE,
        size = "l",
        class = "cougar-modal profile-shell",
        footer = modalButton("Close"),
        div(
          class = "profile-modal",
          div(
            class = "profile-hero",
            div(
              class = "profile-identity",
              span(
                class = paste("profile-status-pill", if (isTRUE(profile$verified)) "verified" else "pending"),
                if (isTRUE(profile$verified)) "Verified reviewer" else "Awaiting verification"
              ),
              h3(profile$username),
              span(class = "profile-subline", sprintf("Joined %s", created_display))
            ),
            div(
              class = "profile-stats",
              div(class = "profile-stat",
                  span(class = "label", "Reviews"),
                  span(class = "value", profile$reviews_published)
              ),
              div(class = "profile-stat",
                  span(class = "label", "Likes"),
                  span(class = "value", profile$total_likes)
              ),
              div(class = "profile-stat",
                  span(class = "label", "Status"),
                  span(class = "value", if (isTRUE(profile$verified)) "Verified" else "Pending")
              )
            )
          ),
          div(
            class = "profile-review-section",
            h4("Reviews"),
            div(
              class = "profile-modal-reviews",
              review_cards
            )
          )
        )
      )
    )
  }

  observeEvent(input$view_profile, {
    req(isTRUE(auth_result()))
    show_user_profile_modal(active_user())
  })

  observeEvent(input$view_profile_from_username, {
    payload <- input$view_profile_from_username
    username <- tryCatch(payload$user, error = function(...) NULL)
    req(!is.null(username), nzchar(username))
    show_user_profile_modal(username)
  })

  # --- Courses tab logic (driven by exported WSU catalog data) ---
  live_courses_path <- file.path(data_dir, "wsu_courses.json")
  mock_courses_path <- file.path(data_dir, "courses_mock.json")

  courses_payload <- tryCatch(
    {
      if (file.exists(live_courses_path)) {
        jsonlite::fromJSON(live_courses_path, simplifyVector = TRUE)
      } else if (file.exists(mock_courses_path)) {
        list(courses = jsonlite::fromJSON(mock_courses_path, simplifyDataFrame = TRUE))
      } else {
        list()
      }
    },
    error = function(err) {
      warning(sprintf("Unable to read catalog data: %s", err$message))
      list()
    }
  )

  courses_df <- tryCatch(
    {
      if (!is.null(courses_payload$courses)) {
        as.data.frame(courses_payload$courses, stringsAsFactors = FALSE)
      } else {
        data.frame(stringsAsFactors = FALSE)
      }
    },
    error = function(err) {
      warning(sprintf("Unable to parse course data: %s", err$message))
      data.frame(stringsAsFactors = FALSE)
    }
  )

  subjects_df <- tryCatch(
    {
      if (!is.null(courses_payload$subjects)) {
        as.data.frame(courses_payload$subjects, stringsAsFactors = FALSE)
      } else {
        data.frame(stringsAsFactors = FALSE)
      }
    },
    error = function(err) {
      warning(sprintf("Unable to parse subject data: %s", err$message))
      data.frame(stringsAsFactors = FALSE)
    }
  )

  if (!nrow(subjects_df) && nrow(courses_df)) {
    subjects_df <- unique(courses_df[, c("subject_code", "department"), drop = FALSE])
    names(subjects_df) <- c("subject_code", "title")
  }
  if ("subject_code" %in% names(subjects_df)) {
    subjects_df$subject_code <- trimws(as.character(subjects_df$subject_code))
  }
  if (!"title" %in% names(subjects_df)) {
    subjects_df$title <- subjects_df$prefix
  }
  subjects_df$title <- trimws(as.character(subjects_df$title))
  if ("subject_code" %in% names(subjects_df)) {
    missing_title <- !nzchar(subjects_df$title) & nzchar(subjects_df$subject_code)
    subjects_df$title[missing_title] <- subjects_df$subject_code[missing_title]
  }
  if (nrow(subjects_df)) {
    subjects_df <- subjects_df[!is.na(subjects_df$subject_code) & nzchar(subjects_df$subject_code), , drop = FALSE]
  }

  text_cols <- c(
    "course_id", "title", "department", "description", "subject_code",
    "credits", "creditsPhrase", "typically_offered", "typicallyOffered",
    "requisitePhrase", "recommendedPhrase", "prefixTitle", "prefix"
  )
  for (col in text_cols) {
    if (!col %in% names(courses_df)) {
      courses_df[[col]] <- ""
    } else {
      courses_df[[col]] <- as.character(courses_df[[col]])
    }
  }
  if ("subject_code" %in% names(courses_df)) {
    courses_df$subject_code <- trimws(courses_df$subject_code)
  }
  if ("course_id" %in% names(courses_df)) {
    courses_df$course_id <- trimws(courses_df$course_id)
  }

  if ("prefixTitle" %in% names(courses_df)) {
    missing_dept <- !nzchar(courses_df$department) & nzchar(courses_df$prefixTitle)
    courses_df$department[missing_dept] <- courses_df$prefixTitle[missing_dept]
  }
  missing_dept <- !nzchar(courses_df$department)
  courses_df$department[missing_dept] <- courses_df$subject_code[missing_dept]

  if (nrow(courses_df)) {
    courses_df$subject_label <- sprintf("%s — %s", courses_df$course_id, courses_df$title)
  } else {
    courses_df$subject_label <- character()
  }

  course_subjects <- if (nrow(courses_df)) sort(unique(courses_df$subject_label)) else character(0)

  subject_choices <- if (nrow(subjects_df)) {
    labels <- ifelse(
      !is.na(subjects_df$title) & nzchar(subjects_df$title),
      sprintf("%s — %s", subjects_df$subject_code, subjects_df$title),
      subjects_df$subject_code
    )
    values <- subjects_df$subject_code
    valid <- !is.na(values) & nzchar(values)
    values <- values[valid]
    labels <- labels[valid]
    stats::setNames(values, labels)
  } else {
    character(0)
  }

  default_subject_choice <- if ("CPT_S" %in% subjects_df$subject_code) {
    "CPT_S"
  } else if (length(subject_choices)) {
    unname(subject_choices[1])
  } else {
    ""
  }

  courses_loaded <- reactiveVal(FALSE)
  professors_loaded <- reactiveVal(FALSE)
  courses_loaded(TRUE)

  output$download_courses_json <- downloadHandler(
    filename = function() {
      sprintf("wsu_courses_%s.json", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      source_path <- if (file.exists(live_courses_path)) live_courses_path else mock_courses_path
      req(file.exists(source_path))
      file.copy(source_path, file, overwrite = TRUE)
    }
  )

  observe({
    if (!length(subject_choices)) {
      updateSelectizeInput(session, "course_subject", choices = list(), selected = NULL, server = TRUE)
    } else {
      selected_val <- if (nzchar(default_subject_choice)) default_subject_choice else NULL
      updateSelectizeInput(
        session,
        "course_subject",
        choices = subject_choices,
        selected = selected_val,
        server = TRUE
      )
    }
  })

  # --- Professors tab data (live PDF scrape fallback to mock) ---
  professors_live_path <- file.path(data_dir, "wsu_professors.json")
  professors_mock_path <- file.path(data_dir, "professors_mock.json")
  professors_payload <- tryCatch(
    {
      if (file.exists(professors_live_path)) {
        jsonlite::fromJSON(professors_live_path, simplifyVector = TRUE)
      } else if (file.exists(professors_mock_path)) {
        list(professors = jsonlite::fromJSON(professors_mock_path, simplifyDataFrame = TRUE))
      } else {
        list()
      }
    },
    error = function(err) {
      warning(sprintf("Unable to read professors data: %s", err$message))
      list()
    }
  )

  professors_df <- tryCatch(
    {
      if (!is.null(professors_payload$professors)) {
        as.data.frame(professors_payload$professors, stringsAsFactors = FALSE)
      } else {
        data.frame(stringsAsFactors = FALSE)
      }
    },
    error = function(err) {
      warning(sprintf("Unable to parse professor data: %s", err$message))
      data.frame(stringsAsFactors = FALSE)
    }
  )

  prof_text_cols <- c("name", "department", "title", "email", "phone", "campus", "raw_line", "bio")
  for (col in prof_text_cols) {
    if (!col %in% names(professors_df)) {
      professors_df[[col]] <- ""
    } else {
      professors_df[[col]] <- as.character(professors_df[[col]])
    }
  }
  # Generate placeholder emails when missing by using first/last tokens from name.
  synthesize_email <- function(name) {
    tokens <- unlist(strsplit(tolower(gsub("[^A-Za-z\\s]", " ", name)), "\\s+"))
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) >= 2) {
      first <- tokens[1]
      last <- tokens[length(tokens)]
      return(sprintf("%s.%s@wsu.edu", first, last))
    }
    ""
  }
  professors_df$name <- trimws(professors_df$name)
  missing_name <- !nzchar(professors_df$name) & nzchar(professors_df$email)
  professors_df$name[missing_name] <- professors_df$email[missing_name]
  professors_df$department <- trimws(professors_df$department)
  professors_df$title <- trimws(professors_df$title)
  professors_df$campus <- trimws(professors_df$campus)
  professors_df$bio <- trimws(professors_df$bio)
  professors_df$email <- ifelse(
    nzchar(professors_df$email),
    professors_df$email,
    vapply(professors_df$name, synthesize_email, character(1))
  )

  if (nrow(professors_df)) {
    professors_df$subject_label <- ifelse(nzchar(professors_df$name), professors_df$name, professors_df$email)
  } else {
    professors_df$subject_label <- character()
  }

  # Attach review counts from the reviews store (for ranking)
  prof_counts <- tryCatch(
    {
      ensure_review_store(reviews_db_path)
      con <- DBI::dbConnect(RSQLite::SQLite(), reviews_db_path)
      on.exit(DBI::dbDisconnect(con))
      DBI::dbGetQuery(
        con,
        "SELECT subject AS subject_label, COUNT(*) AS review_count
         FROM reviews
         WHERE review_type = 'Professor'
         GROUP BY subject"
      )
    },
    error = function(e) data.frame()
  )
  professors_df$review_count <- 0L
  if (nrow(prof_counts)) {
    professors_df$review_count <- prof_counts$review_count[match(professors_df$subject_label, prof_counts$subject_label)]
    professors_df$review_count[is.na(professors_df$review_count)] <- 0L
  }
  professors_loaded(TRUE)

  professor_subjects <- if (nrow(professors_df)) sort(unique(professors_df$subject_label)) else character(0)

  subject_choices_for_type <- function(type) {
    if (identical(type, "Professor")) {
      professor_subjects
    } else {
      course_subjects
    }
  }

  review_like_stats <- function(review_ids, current_user = NULL) {
    ids <- unique(as.integer(review_ids))
    ids <- ids[!is.na(ids)]
    if (!length(ids)) {
      return(data.frame(review_id = integer(), like_count = integer(), liked = logical()))
    }

    ensure_review_store(reviews_db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), reviews_db_path)
    on.exit(DBI::dbDisconnect(con))

    ids_sql <- paste(ids, collapse = ",")

    counts <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT review_id, COUNT(*) AS like_count
         FROM review_likes
         WHERE review_id IN (%s)
         GROUP BY review_id",
        ids_sql
      )
    )

    liked_df <- data.frame()
    if (!is.null(current_user)) {
      liked_df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT review_id FROM review_likes
           WHERE review_id IN (%s) AND user = :user",
          ids_sql
        ),
        params = list(user = current_user)
      )
    }

    result <- data.frame(
      review_id   = ids,
      like_count  = integer(length(ids)),
      liked       = rep(FALSE, length(ids))
    )

    if (nrow(counts)) {
      result$like_count[match(counts$review_id, result$review_id)] <- as.integer(counts$like_count)
    }
    if (nrow(liked_df)) {
      result$liked[match(liked_df$review_id, result$review_id)] <- TRUE
    }

    result
  }

  render_like_button <- function(review_id, stats_df) {
    if (is.null(review_id) || is.na(review_id)) return(NULL)
    row_idx <- match(review_id, stats_df$review_id)
    like_count <- if (!is.na(row_idx)) stats_df$like_count[[row_idx]] else 0L
    liked <- if (!is.na(row_idx)) isTRUE(stats_df$liked[[row_idx]]) else FALSE

    tags$button(
      type = "button",
      class = paste("review-like-btn", if (liked) "liked" else ""),
      onclick = sprintf(
        "event.stopPropagation(); Shiny.setInputValue('toggle_review_like', {id: %d, ts: Date.now()}, {priority: 'event'})",
        as.integer(review_id)
      ),
      icon(if (liked) "thumbs-up" else "thumbs-o-up"),
      span(class = "like-count", like_count)
    )
  }

  render_delete_button <- function(review_id, author, allow_delete = FALSE) {
    if (is.null(review_id) || is.na(review_id)) return(NULL)
    current_user <- tryCatch(if (isTRUE(auth_result())) active_user() else NULL, error = function(...) NULL)
    admin_mode <- isTRUE(user_is_admin())
    can_delete <- admin_mode || isTRUE(allow_delete)
    if (!can_delete) return(NULL)
    if (!admin_mode && (is.null(current_user) || !identical(current_user, author))) return(NULL)
    tags$button(
      type = "button",
      class = "review-delete-btn",
      onclick = sprintf(
        "event.stopPropagation(); Shiny.setInputValue('delete_review', {id: %d, ts: Date.now()}, {priority: 'event'})",
        as.integer(review_id)
      ),
      icon("trash"),
      span("Delete")
    )
  }

  author_link_tag <- function(username) {
    if (is.null(username) || !nzchar(username)) return(NULL)
    payload <- jsonlite::toJSON(
      list(user = username, ts = as.integer(round(as.numeric(Sys.time()) * 1000))),
      auto_unbox = TRUE
    )
    tags$a(
      href = "#",
      class = "review-author-tag",
      onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('view_profile_from_username', %s, {priority: 'event'})", payload),
      paste0("@", username)
    )
  }

  sort_reviews_for_display <- function(df) {
    if (is.null(df) || !nrow(df)) {
      return(df)
    }
    if (!"rating_count" %in% names(df)) {
      df$rating_count <- 0
    }
    if ("created_at" %in% names(df)) {
      df$created_at_parsed <- suppressWarnings(as.POSIXct(df$created_at, tz = "UTC"))
    } else {
      df$created_at_parsed <- NA
    }
    df[order(-as.numeric(df$rating_count), -as.numeric(df$created_at_parsed)), , drop = FALSE]
  }

  latest_review_author <- function(review_type, subject) {
    reviews <- tryCatch(list_reviews_for_subject(review_type, subject), error = function(e) data.frame())
    if (!nrow(reviews)) {
      return(NA_character_)
    }
    reviews <- sort_reviews_for_display(reviews)
    reviews$author[[1]]
  }

  render_subject_reviews <- function(review_type, subject) {
    reviews <- tryCatch(list_reviews_for_subject(review_type, subject), error = function(e) data.frame())
    if (!nrow(reviews)) {
      return(div(class = "subject-review-empty", p("No reviews yet for this subject.")))
    }
    current_user <- tryCatch(if (isTRUE(auth_result())) active_user() else NULL, error = function(...) NULL)
    stats <- review_like_stats(reviews$review_id, current_user)
    reviews$rating_count <- stats$like_count[match(reviews$review_id, stats$review_id)]
    reviews$rating_count[is.na(reviews$rating_count)] <- 0
    reviews$liked_by_user <- stats$liked[match(reviews$review_id, stats$review_id)]
    reviews <- sort_reviews_for_display(reviews)
    cards <- lapply(seq_len(nrow(reviews)), function(idx) {
      rv <- reviews[idx, , drop = FALSE]
      created <- as.character(rv$created_at)
      created_fmt <- if (!is.null(created) && nzchar(created)) {
        format(as.POSIXct(created, tz = "UTC"), "%b %d, %Y")
      } else {
        ""
      }
      like_btn <- render_like_button(rv$review_id, stats)
      delete_btn <- render_delete_button(
        rv$review_id,
        rv$author,
        allow_delete = isTRUE(!is.null(current_user) && identical(current_user, rv$author))
      )
      div(
        class = "subject-review-card",
        div(
          class = "subject-review-meta",
          author_link_tag(rv$author),
          if (nzchar(created_fmt)) span(class = "subject-review-date", created_fmt)
        ),
        h4(class = "subject-review-title", rv$title),
        p(class = "subject-review-body", rv$body),
        div(class = "subject-review-actions", like_btn, delete_btn)
      )
    })

    div(
      class = "subject-review-list",
      h4("Reviews"),
      do.call(tagList, cards)
    )
  }

  open_review_modal <- function(default_type = VALID_REVIEW_TYPES[1],
                                default_subject = NULL,
                                source = "home") {
    if (!isTRUE(auth_result())) {
      showNotification("Log in to write a review.", type = "warning", duration = 4)
      showLogin(TRUE)
      auth_mode("login")
      return(invisible(NULL))
    }

    if (!isTRUE(user_verified())) {
      showNotification("Your account must be verified before publishing reviews.", type = "error", duration = 5)
      return(invisible(NULL))
    }

    review_state$type <- default_type
    review_state$subject <- default_subject
    review_state$source <- source
    review_feedback(NULL)

    showModal(
      modalDialog(
        title = "Publish a review",
        easyClose = TRUE,
        size = "l",
        class = "cougar-modal review-modal-dialog",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_review", "Publish review", class = "btn btn-primary")
        ),
        div(
          class = "review-modal",
          fluidRow(
            class = "review-modal-row",
            column(
              width = 6,
              class = "review-modal-column",
              selectInput("review_type", "Review type", choices = VALID_REVIEW_TYPES, selected = default_type)
            ),
            column(
              width = 6,
              class = "review-modal-column",
              uiOutput("review_subject_ui")
            )
          ),
          div(
            class = "review-modal-row",
            textInput("review_title", "Review title", width = "100%")
          ),
          div(
            class = "review-modal-row",
            textAreaInput(
              "review_body",
              "Review body",
              rows = 10,
              width = "100%",
              placeholder = "Share specific insights future Cougs will care about."
            )
          ),
          uiOutput("review_feedback")
        )
      )
    )

    session$onFlushed(function() {
      selected_subject <- isolate(review_state$subject)
      updateSelectInput(session, "review_type", selected = default_type)
      updateTextInput(session, "review_title", value = "")
      updateTextAreaInput(session, "review_body", value = "")
      updateSelectizeInput(session, "review_subject", selected = selected_subject, server = FALSE)
    }, once = TRUE)
  }

  output$review_subject_ui <- renderUI({
    type_input <- input$review_type
    if (!is.null(type_input)) {
      review_state$type <- type_input
    }
    type <- if (!is.null(type_input)) type_input else review_state$type
    choices <- subject_choices_for_type(type)
    if (!length(choices)) {
      review_state$subject <- NULL
      return(
        div(
          class = "review-subject-empty",
          p("No subjects available. Try selecting a different review type.")
        )
      )
    }
    selected <- review_state$subject
    chosen <- NULL
    if (!is.null(selected) && selected %in% choices) {
      chosen <- selected
    } else if (!is.null(selected) && length(choices)) {
      chosen <- choices[1]
    }
    review_state$subject <- chosen

    source <- review_state$source
    placeholder <- NULL
    if (identical(source, "home")) {
      placeholder <- if (identical(type, "Course")) {
        "Type a course code or title (e.g., CPT_S 355)"
      } else {
        "Type a professor name"
      }
    }

    selectizeInput(
      "review_subject",
      "Review subject",
      choices = choices,
      selected = chosen,
      options = list(
        placeholder = placeholder,
        closeAfterSelect = TRUE,
        highlight = TRUE
      )
    )
  })

  # --- Home tab: top reviews & professors ---
  fetch_top_reviews <- function(limit = 3, db_path = reviews_db_path) {
    ensure_review_store(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT review_id, author, review_type, subject, title, body, created_at
         FROM reviews
         ORDER BY datetime(created_at) DESC
         LIMIT %d",
        as.integer(limit)
      )
    )
  }

  fetch_top_professors <- function(limit = 3, db_path = reviews_db_path) {
    ensure_review_store(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    counts <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT subject AS name, COUNT(*) AS review_count
         FROM reviews
         WHERE review_type = 'Professor'
         GROUP BY subject
         ORDER BY review_count DESC, subject ASC
         LIMIT %d",
        as.integer(limit)
      )
    )

    counts
  }

  output$home_top_reviews <- renderUI({
    reviews_updated()
    reviews <- tryCatch(fetch_top_reviews(3), error = function(e) data.frame())

    if (!nrow(reviews)) {
      return(p(class = "muted", "No reviews yet — be the first to publish one."))
    }

    current_user <- if (isTRUE(auth_result())) active_user() else NULL
    stats <- review_like_stats(reviews$review_id, current_user)

    cards <- lapply(seq_len(nrow(reviews)), function(idx) {
      rv <- reviews[idx, , drop = FALSE]
      author_tag <- author_link_tag(rv$author)
      like_btn <- render_like_button(rv$review_id, stats)
      delete_btn <- render_delete_button(
        rv$review_id,
        rv$author,
        allow_delete = isTRUE(!is.null(current_user) && identical(current_user, rv$author))
      )
      div(
        class = "home-top-card",
        h4(rv$title),
        div(class = "meta",
            author_tag,
            sprintf("%s • %s", rv$review_type, rv$subject)),
        p(class = "body", rv$body),
        div(class = "subject-review-actions", like_btn, delete_btn)
      )
    })

    div(class = "home-top-cards", do.call(tagList, cards))
  })

  output$home_top_professors <- renderUI({
    reviews_updated()
    counts <- tryCatch(fetch_top_professors(3), error = function(e) data.frame())

    if (!nrow(counts)) {
      return(p(class = "muted", "No professor reviews yet — start by writing one."))
    }

    cards <- lapply(seq_len(nrow(counts)), function(idx) {
      row <- counts[idx, , drop = FALSE]
      prof <- professors_df[match(row$name, professors_df$subject_label), , drop = FALSE]
      prof <- prof[1, , drop = FALSE]
      dept <- first_non_empty(prof$department, prof$campus)
      title <- first_non_empty(prof$title)
      latest_author <- latest_review_author("Professor", row$name)
      author_tag <- author_link_tag(latest_author)
      div(
        class = "home-top-card",
        h4(first_non_empty(prof$name, row$name)),
        div(class = "meta",
            author_tag,
            sprintf("%d review%s • %s", row$review_count, ifelse(row$review_count == 1, "", "s"),
                    ifelse(nzchar(title), title, "Professor"))),
        if (nzchar(dept)) p(class = "body", dept)
      )
    })

    div(class = "home-top-cards", do.call(tagList, cards))
  })

  output$review_feedback <- renderUI({
    status <- review_feedback()
    if (is.null(status)) {
      return(NULL)
    }

    div(
      class = paste("review-feedback", status$type),
      icon(if (identical(status$type, "error")) "exclamation-triangle" else "info-circle"),
      span(status$text)
    )
  })

  observeEvent(input$review_type, {
    prev_type <- isolate(review_state$type)
    review_state$type <- input$review_type
    if (!identical(prev_type, input$review_type)) {
      review_state$subject <- NULL
    }
    review_feedback(NULL)
  })

  observeEvent(input$review_subject, {
    review_state$subject <- input$review_subject
  })

  observeEvent(input$open_home_review, {
    open_review_modal(VALID_REVIEW_TYPES[1], NULL, source = "home")
  })

  observeEvent(input$open_review_from_course, {
    info <- input$open_review_from_course
    req(!is.null(info))
    open_review_modal("Course", info$subject, source = "course")
  })

  observeEvent(input$open_review_from_professor, {
    info <- input$open_review_from_professor
    req(!is.null(info))
    open_review_modal("Professor", info$subject, source = "professor")
  })

  observeEvent(input$submit_review, {
    req(isTRUE(auth_result()))

    if (!isTRUE(user_verified())) {
      review_feedback(list(type = "error", text = "Only verified accounts can publish reviews."))
      return()
    }

    review_type <- input$review_type
    if (is.null(review_type) || !review_type %in% VALID_REVIEW_TYPES) {
      review_type <- review_state$type
    }

    subject_choices <- subject_choices_for_type(review_type)
    subject <- input$review_subject
    if (is.null(subject) || !subject %in% subject_choices) {
      review_feedback(list(type = "error", text = "Please choose a subject for your review."))
      return()
    }

    title <- trimws(value_or_empty(input$review_title))
    body  <- trimws(value_or_empty(input$review_body))

    if (!nzchar(title) || nchar(title) < 3) {
      review_feedback(list(type = "error", text = "Add a short title (at least 3 characters)."))
      return()
    }

    if (!nzchar(body) || nchar(body) < 20) {
      review_feedback(list(type = "error", text = "Share a bit more detail (20+ characters)."))
      return()
    }

    result <- tryCatch(
      {
        create_review(
          author = active_user(),
          review_type = review_type,
          subject = subject,
          title = title,
          body = body
        )
        TRUE
      },
      error = function(err) {
        message <- err$message
        if (is.null(message) || !nzchar(message)) {
          message <- "Unable to publish the review right now."
        }
        review_feedback(list(type = "error", text = message))
        FALSE
      }
    )

    if (!isTRUE(result)) {
      return()
    }

    removeModal()
    showNotification("Review published! Thanks for helping fellow Cougs.", type = "message", duration = 5)
    reviews_updated(Sys.time())
  })

  observeEvent(input$toggle_review_like, {
    if (!isTRUE(auth_result())) {
      showNotification("Log in to like reviews.", type = "warning", duration = 4)
      return()
    }

    payload <- input$toggle_review_like
    review_id <- tryCatch(as.integer(payload$id), error = function(...) NA_integer_)
    if (is.na(review_id)) {
      return()
    }

    result <- tryCatch(
      toggle_review_like(review_id, active_user()),
      error = function(err) list(ok = FALSE, message = err$message)
    )

    if (!isTRUE(result$ok)) {
      msg <- if (!is.null(result$message) && nzchar(result$message)) result$message else "Unable to update like."
      showNotification(msg, type = "error", duration = 4)
      return()
    }

    reviews_updated(Sys.time())
  })

  observeEvent(input$delete_review, {
    if (!isTRUE(auth_result())) {
      showNotification("Log in to delete your reviews.", type = "warning", duration = 4)
      return()
    }

    payload <- input$delete_review
    review_id <- tryCatch(as.integer(payload$id), error = function(...) NA_integer_)
    if (is.na(review_id)) {
      return()
    }

    result <- tryCatch(
      {
        delete_review(review_id, active_user(), is_admin = user_is_admin())
        list(ok = TRUE)
      },
      error = function(err) list(ok = FALSE, message = err$message)
    )

    if (!isTRUE(result$ok)) {
      msg <- if (!is.null(result$message) && nzchar(result$message)) result$message else "Unable to delete review."
      showNotification(msg, type = "error", duration = 4)
      return()
    }

    showNotification("Review deleted.", type = "message", duration = 4)
    reviews_updated(Sys.time())
    try({
      show_user_profile_modal(active_user())
    }, silent = TRUE)
  })

  filtered_professors <- reactive({
    data <- professors_df

    if (!nrow(data)) {
      return(data)
    }

    query <- input$professor_search
    if (!is.null(query)) {
      query <- trimws(tolower(query))
      if (nzchar(query)) {
        keep <- grepl(query, tolower(data$name)) |
          grepl(query, tolower(data$title)) |
          grepl(query, tolower(data$department)) |
          grepl(query, tolower(data$bio))
        data <- data[keep, , drop = FALSE]
      }
    }

    sort_option <- input$professor_sort
    if (is.null(sort_option) || !nzchar(sort_option)) {
      sort_option <- "name"
    }

    if (nrow(data)) {
      if (identical(sort_option, "department")) {
        data <- data[order(tolower(data$department), tolower(data$name)), , drop = FALSE]
      } else if (identical(sort_option, "title")) {
        data <- data[order(tolower(data$title), tolower(data$name)), , drop = FALSE]
      } else {
        data <- data[order(tolower(data$name), tolower(data$department)), , drop = FALSE]
      }
    }

    # Limit to top 10 by review count (using stored counts if available), unless a search is active
    if (is.null(input$professor_search) || !nzchar(trimws(input$professor_search))) {
      if ("review_count" %in% names(data)) {
        data <- data[order(-as.numeric(data$review_count), tolower(data$name)), , drop = FALSE]
      }
      data <- head(data, 10)
    }

    data
  })

  output$professors_list <- renderUI({
    data <- filtered_professors()

    if (!nrow(data)) {
      return(
        div(
          class = "professor-empty",
          h3("No professors found"),
          p("Try a different search or sort option.")
        )
      )
    }

    professor_cards <- lapply(seq_len(nrow(data)), function(idx) {
      prof <- data[idx, , drop = FALSE]
      label <- as.character(prof$subject_label)
      name <- first_non_empty(prof$name, prof$email)
      dept <- first_non_empty(prof$department, prof$campus)
      title <- first_non_empty(prof$title)
      email <- prof$email
      card_js <- sprintf(
        "Shiny.setInputValue('selected_professor', %s, {priority: 'event'})",
        jsonlite::toJSON(label, auto_unbox = TRUE)
      )

      review_payload <- list(
        type = "Professor",
        subject = label
      )

      review_js <- sprintf(
        "event.stopPropagation(); Shiny.setInputValue('open_review_from_professor', %s, {priority: 'event'})",
        jsonlite::toJSON(review_payload, auto_unbox = TRUE)
      )

      div(
        class = "professor-card card",
        onclick = card_js,
        h3(class = "professor-name", name),
        if (nzchar(title)) span(class = "professor-title", title),
        if (nzchar(dept)) span(class = "professor-dept", dept),
        if (nzchar(prof$bio)) p(class = "professor-bio", prof$bio),
        if (!nzchar(prof$bio) && nzchar(prof$raw_line)) {
          p(class = "professor-bio muted", prof$raw_line)
        },
        div(
          class = "professor-actions",
          if (nzchar(email)) {
            tags$a(
              href = sprintf("mailto:%s", email),
              class = "btn btn-link professor-contact",
              title = "Email instructor",
              icon("envelope")
            )
          },
          tags$button(
            type = "button",
            class = "btn btn-sm btn-primary review-card-btn",
            onclick = review_js,
            icon("pen"), " Write review"
          )
        )
      )
    })

    do.call(tagList, professor_cards)
  })

  # Filter and sort courses reactively so the mock layer can be swapped out later.
  filtered_courses <- reactive({
    data <- courses_df

    if (!nrow(data)) {
      return(data)
    }

    subject_choice <- input$course_subject
    if (!is.null(subject_choice) && nzchar(subject_choice)) {
      data <- data[data$subject_code == subject_choice, , drop = FALSE]
    }

    query <- input$course_search
    if (!is.null(query)) {
      query <- trimws(tolower(query))
      if (nzchar(query)) {
        keep <- grepl(query, tolower(data$course_id), fixed = FALSE) |
          grepl(query, tolower(data$title), fixed = FALSE) |
          grepl(query, tolower(data$department), fixed = FALSE)
        data <- data[keep, , drop = FALSE]
      }
    }

    sort_option <- input$course_sort
    if (is.null(sort_option) || !nzchar(sort_option)) {
      sort_option <- "course_id"
    }

    if (nrow(data)) {
      if (identical(sort_option, "title")) {
        data <- data[order(tolower(data$title), tolower(data$course_id)), , drop = FALSE]
      } else {
        data <- data[order(tolower(data$course_id), tolower(data$title)), , drop = FALSE]
      }
    }

    data
  })

  output$courses_list <- renderUI({
    data <- filtered_courses()

    if (!nrow(data)) {
      return(
        div(
          class = "course-empty card",
          h3("No courses found"),
          p("Try a different subject or search term to see catalog results.")
        )
      )
    }

    course_cards <- lapply(seq_len(nrow(data)), function(idx) {
      course <- as.list(data[idx, , drop = FALSE])

      course_id <- as.character(course$course_id)
      title     <- as.character(course$title)
      dept      <- first_non_empty(course$department, course$prefixTitle, course$subject_code)
      credits_display <- first_non_empty(course$credits, course$creditsPhrase)
      offered_display <- first_non_empty(course$typically_offered, course$typicallyOffered)
      prereq_display  <- first_non_empty(course$requisitePhrase)
      recommend_display <- first_non_empty(course$recommendedPhrase)

      onclick_js <- sprintf(
        "Shiny.setInputValue('selected_course', %s, {priority: 'event'})",
        jsonlite::toJSON(course_id, auto_unbox = TRUE)
      )

      review_payload <- list(
        type = "Course",
        subject = course$subject_label
      )

      review_js <- sprintf(
        "event.stopPropagation(); Shiny.setInputValue('open_review_from_course', %s, {priority: 'event'})",
        jsonlite::toJSON(review_payload, auto_unbox = TRUE)
      )

      div(
        class = "course-card card",
        onclick = onclick_js,
        div(
          class = "course-meta",
          span(class = "course-code", course_id),
          span(class = "course-dept", dept)
        ),
        h3(class = "course-title", title),
        p(class = "course-description", course$description),
        div(
          class = "course-stats",
          span(
            class = "course-credits",
            sprintf("Credits: %s", if (nzchar(credits_display)) credits_display else "See catalog")
          ),
          span(
            class = "course-offering",
            sprintf("Typically offered: %s", if (nzchar(offered_display)) offered_display else "Varies")
          )
        ),
        if (nzchar(prereq_display) || nzchar(recommend_display)) {
          div(
            class = "course-prereqs",
            if (nzchar(prereq_display)) span(class = "course-prereq-text", prereq_display),
            if (nzchar(recommend_display)) span(class = "course-recommend-text", recommend_display)
          )
        },
        div(
          class = "course-actions",
          tags$button(
            type = "button",
            class = "btn btn-sm btn-primary review-card-btn",
            onclick = review_js,
            icon("pen"), " Write review"
          )
        )
      )
    })

    do.call(tagList, course_cards)
  })

  observeEvent(input$selected_course, {
    req(nrow(courses_df) > 0)

    selection <- input$selected_course
    req(!is.null(selection), nzchar(selection))

    course_row <- courses_df[courses_df$course_id == selection, , drop = FALSE]
    req(nrow(course_row) == 1)

    course <- as.list(course_row[1, , drop = FALSE])
    dept <- first_non_empty(course$department, course$prefixTitle, course$subject_code)
    credits_display <- first_non_empty(course$credits, course$creditsPhrase)
    offered_display <- first_non_empty(course$typically_offered, course$typicallyOffered)
    prereq_display <- first_non_empty(course$requisitePhrase)
    recommend_display <- first_non_empty(course$recommendedPhrase)
    description_text <- if (nzchar(course$description)) course$description else "Description unavailable."
    reviews_block <- render_subject_reviews("Course", course$subject_label)

    showModal(
      modalDialog(
        title = sprintf("%s — %s", course$course_id, course$title),
        easyClose = TRUE,
        fade = TRUE,
        class = "cougar-modal",
        footer = modalButton("Close"),
        div(
          class = "course-detail-modal",
          p(span(class = "detail-label", "Department: "), dept),
          p(span(class = "detail-label", "Credits: "),
            if (nzchar(credits_display)) credits_display else "See advisor for details"),
          if (nzchar(offered_display)) {
            p(span(class = "detail-label", "Typically offered: "), offered_display)
          },
          p(description_text),
          if (nzchar(prereq_display)) {
            p(span(class = "detail-label", "Prerequisites: "), prereq_display)
          },
          if (nzchar(recommend_display)) {
            p(span(class = "detail-label", "Recommended preparation: "), recommend_display)
          },
          p(class = "course-detail-note",
            "Data sourced from the downloaded catalog snapshot; verify details with the official WSU schedule."),
          reviews_block
        )
      )
    )
  })

  observeEvent(input$selected_professor, {
    req(nrow(professors_df) > 0)

    selection <- input$selected_professor
    req(!is.null(selection), nzchar(selection))

    prof_row <- professors_df[professors_df$subject_label == selection, , drop = FALSE]
    req(nrow(prof_row) >= 1)

    prof <- as.list(prof_row[1, , drop = FALSE])
    name <- first_non_empty(prof$name, prof$email, selection)
    dept <- first_non_empty(prof$department, prof$campus)
    title <- first_non_empty(prof$title, "Professor")
    bio <- prof$bio
    email <- prof$email
    reviews_block <- render_subject_reviews("Professor", selection)

    showModal(
      modalDialog(
        title = name,
        easyClose = TRUE,
        fade = TRUE,
        class = "cougar-modal",
        footer = modalButton("Close"),
        div(
          class = "professor-detail-modal",
          p(span(class = "detail-label", "Title: "), title),
          if (nzchar(dept)) p(span(class = "detail-label", "Department: "), dept),
          if (nzchar(email)) p(span(class = "detail-label", "Contact: "),
                               tags$a(href = sprintf("mailto:%s", email), email)),
          if (nzchar(bio)) p(bio),
          reviews_block
        )
      )
    )
  })

  output$courses_loaded <- renderText({
    courses_loaded()
  })
  outputOptions(output, "courses_loaded", suspendWhenHidden = FALSE)

  output$professors_loaded <- renderText({
    professors_loaded()
  })
  outputOptions(output, "professors_loaded", suspendWhenHidden = FALSE)

}
