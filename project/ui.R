#UI.R is where most of the site will be, at least for the early stages. Here we are simply setting up
#A simple,default look for the websites various UI elements, making them exist. 
#A good way to think of server and ui is as backend and frontend files. Server handles backend, UI frontend.

# here we make a theme object using bslib.
theme <- bslib::bs_theme(
  version   = 5,
  base_font = bslib::font_google("Inter"),
  primary   = "#981E32",   # WSU crimson
  secondary = "#7a1828",
  bg        = "#f7f7f9",
  fg        = "#1f2937"
)

# MAIN UI SECTION
ui <- tagList(
  #this lets us add our style.css sheet to the rest of teh website through html tags.
  tags$head(
    # Load Inter font from Google for cleaner typography
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML("console.log('Custom CSS loaded!');")),
    tags$script(HTML("
      (function() {
        var storageKey = 'wsu-rmp-dark-mode';

        function getPreference() {
          var stored = null;
          try {
            stored = localStorage.getItem(storageKey);
          } catch (err) {
            stored = null;
          }
          if (stored === null) {
            stored = (window.matchMedia &&
                      window.matchMedia('(prefers-color-scheme: dark)').matches) ? 'true' : 'false';
          }
          return stored;
        }

        function applyPreferenceToBody() {
          var body = document.body;
          if (!body) return;
          if (getPreference() === 'true') {
            body.classList.add('dark-mode');
          } else {
            body.classList.remove('dark-mode');
          }
        }

        function syncToggle(toggle) {
          if (!toggle) return;
          var body = document.body;
          var isDark = body.classList.contains('dark-mode');
          toggle.classList.toggle('active', isDark);
          toggle.setAttribute('aria-pressed', isDark);
          var labelNode = toggle.querySelector('.toggle-label');
          if (labelNode) {
            labelNode.textContent = isDark ? 'Light Mode' : 'Dark Mode';
          }
          var iconNode = toggle.querySelector('.toggle-icon');
          if (iconNode) {
            iconNode.textContent = isDark ? '☀️' : '🌙';
          }
        }

        function bindToggle(toggle) {
          if (!toggle || toggle.dataset.bound === 'true') return;
          toggle.dataset.bound = 'true';
          syncToggle(toggle);
          toggle.addEventListener('click', function() {
            var body = document.body;
            body.classList.toggle('dark-mode');
            var isDark = body.classList.contains('dark-mode');
            try {
              localStorage.setItem(storageKey, isDark ? 'true' : 'false');
            } catch (err) {
              /* noop */
            }
            syncToggle(toggle);
          });
        }

        function initialiseToggle() {
          applyPreferenceToBody();
          bindToggle(document.getElementById('darkModeToggle'));
        }

        if (document.readyState !== 'loading') {
          applyPreferenceToBody();
        } else {
          document.addEventListener('DOMContentLoaded', applyPreferenceToBody, { once: true });
        }

        document.addEventListener('DOMContentLoaded', function() {
          initialiseToggle();
          var observer = new MutationObserver(function(mutations) {
            for (var i = 0; i < mutations.length; i++) {
              var mutation = mutations[i];
              if (!mutation.addedNodes) continue;
              for (var j = 0; j < mutation.addedNodes.length; j++) {
                var node = mutation.addedNodes[j];
                if (node.nodeType !== 1) continue;
                if (node.id === 'darkModeToggle' || (node.querySelector && node.querySelector('#darkModeToggle'))) {
                  bindToggle(document.getElementById('darkModeToggle'));
                  return;
                }
              }
            }
          });
          observer.observe(document.body, { childList: true, subtree: true });
        });
      })();
    "))
  ),
  uiOutput("user_profile_header"),
  
  #creates a navigation bar that we can add too later. 
  navbarPage(
    title    = "Rate-My-Prof-WSU",
    id       = "menu",
    selected = "home",
    theme    = theme, 

    # Home tab
    tabPanel(
      title = "Home",
      value = "home",

      # Main hero section
      div(class = "hero",
          div(class = "hero-content",
              h1("Find and share insights on WSU professors"),
              p(class = "subtext",
                "Search, rate, and review to help Cougs pick the right courses."),
              div(
                class = "hero-actions",
                actionButton("loginB", "Register / Login", class = "btn btn-primary btn-lg"),
                actionButton("open_home_review", "Write a Review", class = "btn btn-outline-light btn-lg write-review-btn")
              )
          )
      ),

      # Spotlight sections
      div(
        class = "home-spotlights",
        div(
          class = "card home-top-list",
          h3("Top Reviews"),
          p(class = "subtext", "Most recent reviews from the community."),
          uiOutput("home_top_reviews")
        ),
        div(
          class = "card home-top-list",
          h3("Top Professors"),
          p(class = "subtext", "Most reviewed professors right now."),
          uiOutput("home_top_professors")
        )
      )
    ),


    tabPanel(
      title = "Professors",
      value = "professors",
      div(
        class = "professors-tab",
        div(
          class = "card professors-intro",
          h2("Professors and their reviews"),
          p("Browse WSU faculty, explore quick bios, and jump into writing a review for an instructor."),
          div(
            class = "professors-controls",
            div(
              class = "professors-search",
              textInput(
                inputId = "professor_search",
                label = "Search professors",
                placeholder = "Search by name, title, or department"
              )
            ),
            div(
              class = "professors-sort",
              selectInput(
                inputId = "professor_sort",
                label = "Sort by",
                choices = c("Name" = "name", "Department" = "department", "Title" = "title"),
                selected = "name"
              )
            )
          )
        ),
        div(
          class = "card professors-list-card",
          conditionalPanel(
            condition = "output.professors_loaded != 'TRUE'",
            div(class = "loading-indicator", span(class = "spinner"), span("Loading professors..."))
          ),
          conditionalPanel(
            condition = "output.professors_loaded == 'TRUE'",
            uiOutput("professors_list", container = div, class = "professors-grid")
          )
        )
      )
    ),

    tabPanel(
      title = "Courses",
      value = "courses",
      div(
        class = "courses-tab",
        div(
          class = "card courses-intro",
          h2("Explore WSU Courses"),
          p("Browse available classes, filter by subject, and tap a course card to see catalog details."),
          div(
            class = "courses-intro-controls",
            div(
              class = "courses-subject",
              selectizeInput(
                inputId = "course_subject",
                label = "Subject",
                choices = NULL,
                options = list(
                  placeholder = "Start typing a subject (e.g., CPT_S)",
                  plugins = list("clear_button")
                )
              )
            ),
            div(
              class = "courses-search",
              textInput(
                inputId   = "course_search",
                label     = "Search courses",
                placeholder = "Search by course code, title, or department"
              )
            ),
            div(
              class = "courses-sort",
              selectInput(
                inputId = "course_sort",
                label   = "Sort by",
                choices = c("Course Code" = "course_id", "Title" = "title"),
                selected = "course_id"
              )
            ),
            div(
              class = "courses-download",
              downloadButton("download_courses_json", "Download catalog JSON", class = "btn btn-outline-secondary")
            )
          )
        ),
        div(
          class = "card courses-list-card",
          conditionalPanel(
            condition = "output.courses_loaded != 'TRUE'",
            div(class = "loading-indicator", span(class = "spinner"), span("Loading courses..."))
          ),
          conditionalPanel(
            condition = "output.courses_loaded == 'TRUE'",
            uiOutput("courses_list", container = div, class = "courses-grid")
          )
        )
      )
    ),

    # Info tab (About + Feedback consolidated)
    tabPanel(
      title = "Info",
      value = "info",
      div(
        class = "card",
        h2("About Rate-My-Prof-WSU"),
        p("A community-driven tool for WSU students to find the right classes with confidence."),
        p("This project is not affiliated with Washington State University but aims to follow WSU-inspired branding.")
      ),
      div(
        class = "card",
        h2("Meet the devs"),
        tags$ul(
          tags$li(HTML("Adam Ward — <a href='mailto:adamward.bio@gmail.com'>adamward.bio@gmail.com</a> — <a href='https://github.com/Bioticcc' target='_blank'>github.com/Bioticcc</a>")),
          tags$li(HTML("Jaydon Devictoria — <a href='mailto:jaydon.devictoria@wsu.edu'>jaydon.devictoria@wsu.edu</a> — <a href='https://github.com/ThaRealJdion' target='_blank'>github.com/ThaRealJdion</a>")),
          tags$li(HTML("David AcLeon — <a href='mailto:david.acostaleon@wsu.edu'>david.acostaleon@wsu.edu</a> — <a href='https://github.com/davidacleon' target='_blank'>github.com/davidacleon</a>"))
        )
      )
    )
  ), #end of navBarPage

  conditionalPanel(
  condition = "output.displayLogin",
  tags$div(
    class = "overlay-root",
    tags$div(
      class = "overlay-card",
      tags$div(
        class = "auth-modal-header",
        tags$h2("Account Access"),
        uiOutput("auth_modal_tabs")
      ),
      uiOutput("auth_modal_body"),
      actionButton("backHome", "Back to Home", class = "btn btn-primary")
    )
  )
)



  
) #end of UI
