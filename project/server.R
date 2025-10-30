#Server is where we keep any functional aspects, like making reviews, publishing them,
#getting the list of WSU professors, authentication eventually, logging in, and more    


library(shinymanager)

server <- function(input, output, session) {

  # 1) Credentials checker 
  check_creds <- check_credentials(
    db         = "credentials.sqlite",
    passphrase = "change-this-passphrase" #still gotta change this
  )

  # 2) Start the login module
  auth <- callModule(
    shinymanager::auth_server,
    "login",
    check_credentials = check_creds
  )
  # auth_server() returns reactive values; auth$result becomes TRUE after login

  # 3) Control overlay visibility
  showLogin   <- reactiveVal(FALSE)
  auth_result <- reactive({ auth$result })

  observeEvent(input$loginB,   { showLogin(TRUE)  })   # open overlay when user clicks Login
  observeEvent(input$backHome, { showLogin(FALSE) })   # close overlay manually
  observeEvent(auth_result(), {
    if (isTRUE(auth_result())) showLogin(FALSE)        # auto-close overlay after successful login
  })

  # 4) ConditionalPanel driver: only show when overlay is open AND not authenticated
  output$displayLogin <- reactive({
    isTRUE(showLogin()) && !isTRUE(auth_result())
  })
  outputOptions(output, "displayLogin", suspendWhenHidden = FALSE)

  # --- Courses tab logic (mock data driven) ---
  courses_path <- file.path(data_dir, "courses_mock.json")
  courses_df <- tryCatch(
    {
      if (file.exists(courses_path)) {
        df <- jsonlite::fromJSON(courses_path, simplifyDataFrame = TRUE)
        as.data.frame(df, stringsAsFactors = FALSE)
      } else {
        data.frame(stringsAsFactors = FALSE)
      }
    },
    error = function(err) {
      warning(sprintf("Unable to read %s: %s", courses_path, err$message))
      data.frame(stringsAsFactors = FALSE)
    }
  )

  # Filter and sort courses reactively so the mock layer can be swapped out later.
  filtered_courses <- reactive({
    data <- courses_df

    if (!nrow(data)) {
      return(data)
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
          p("Try a different search term or remove filters to see the available mock courses.")
        )
      )
    }

    course_cards <- lapply(seq_len(nrow(data)), function(idx) {
      course <- as.list(data[idx, , drop = FALSE])

      course_id <- as.character(course$course_id)
      title     <- as.character(course$title)
      dept      <- as.character(course$department)
      rating    <- suppressWarnings(as.numeric(course$average_rating))
      rating_display <- if (length(rating) && is.finite(rating)) {
        sprintf("%.1f", rating)
      } else {
        "N/A"
      }

      reviews_val <- suppressWarnings(as.integer(course$review_count))
      reviews_display <- if (length(reviews_val) && !is.na(reviews_val)) {
        format(reviews_val, big.mark = ",")
      } else {
        "0"
      }

      onclick_js <- sprintf(
        "Shiny.setInputValue('selected_course', %s, {priority: 'event'})",
        jsonlite::toJSON(course_id, auto_unbox = TRUE)
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
          span(class = "course-rating", sprintf("Avg rating: %s", rating_display)),
          span(class = "course-reviews", sprintf("Reviews: %s", reviews_display))
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
    rating_val <- suppressWarnings(as.numeric(course$average_rating))
    avg_display <- if (length(rating_val) && is.finite(rating_val)) {
      sprintf("%.1f / 5.0", rating_val)
    } else {
      "N/A"
    }
    review_val <- suppressWarnings(as.integer(course$review_count))
    review_display <- if (length(review_val) && !is.na(review_val)) {
      format(review_val, big.mark = ",")
    } else {
      "0"
    }

    showModal(
      modalDialog(
        title = sprintf("%s — %s", course$course_id, course$title),
        easyClose = TRUE,
        fade = TRUE,
        footer = modalButton("Close"),
        div(
          class = "course-detail-modal",
          p(span(class = "detail-label", "Department: "), course$department),
          p(course$description),
          div(
            class = "course-detail-stats",
            span(class = "course-detail-rating", sprintf("Average rating: %s", avg_display)),
            span(class = "course-detail-reviews", sprintf("Reviews: %s", review_display))
          ),
          p(class = "course-detail-note", "Placeholder data – connect to live course and review data in a future update.")
        )
      )
    )
  })

}
