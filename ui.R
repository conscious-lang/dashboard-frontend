header <- dashboardHeader(title = "Concious Language Project")

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem("Dashboard",  tabName = "dash",    icon = icon("dashboard")),
              menuItem("History",    tabName = "history", icon = icon("chart-line")),
              menuItem("Tables",     tabName = "tables",  icon = icon("table")),
              menuItem("Repo Files", tabName = "files",   icon = icon("search")),
              menuItem("About",      tabName = "about",   icon = icon("info"))
  ),
  selectInput('side_org','Org','All','All'),
  selectInput('side_repo','Repo','All','All')
)

body <- dashboardBody(
  tabItems(
    # Dashbord - dash -------------------------------------------------------
    tabItem(tabName = "dash",
            fluidRow(
              infoBoxOutput('blacklist', width = 3),
              infoBoxOutput('whitelist', width = 3),
              infoBoxOutput('master',    width = 3),
              infoBoxOutput('slave',     width = 3)
            ),
            fluidRow(
              box(title = 'Blacklist' ,status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("plot1", height = 290)),
              box(title = 'Whitelist', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("plot2", height = 290))
            ),
            fluidRow(
              box(title = 'Master', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("plot3", height = 290)),
              box(title = 'Slave', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("plot4", height = 290))
            )
    ),
    # History - history -------------------------------------------------------
    tabItem(tabName = "history",
            h2("Time-series history of total counts"),
            fluidRow(
              box(title = 'Blacklist' ,status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("hist1", height = 290)),
              box(title = 'Whitelist', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("hist2", height = 290))
            ),
            fluidRow(
              box(title = 'Master', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("hist3", height = 290)),
              box(title = 'Slave', status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("hist4", height = 290))
            )
    ),
    # Tables - tables -------------------------------------------------------
    tabItem(tabName = "tables",
            h2("Searchable / Sortable tables of counts & changes"),
            fluidRow(
              tabBox(width=12, id = 'table_tabs',
                tabPanel("Current counts",
                         p("Counts of each search in the latest 'git clone'"),
                         DT::dataTableOutput('table')
                ),
                tabPanel("Change from last run",
                         p("Changes in count since the previous 'git clone'"),
                         DT::dataTableOutput('deltas')
                )
              )
            )
    ),
    # Repo Files - files -------------------------------------------------------
    tabItem(tabName = "files",
            h2("Word counts per file in a given repo"),
            fluidRow(
              box(title = 'Word', width = 3,
                  selectInput('word',NULL,
                              c('blacklist','whitelist','master','slave'))
              ),
              box(width = 2,
                  actionButton('search', 'Search'))
            ),
            DT::dataTableOutput('filetable')
    ),
    # About - about -------------------------------------------------------
    # Hat-tip to Garrick Aden-Buie for the style of this about page
    # https://garrickadenbuie.com"
    tabItem(tabName = "about",
      fluidRow(
        # About - Header ------------------------------------------------
        box(
          status = "primary",
          width = "12",
          h2(
            class = "text-center",
            'It is ', em('not'), 'the goal of this dashboard to shame anyone'
          ),
          p(
            class = "text-center",
            'Rather, it is indended to help those who wish to work on this',
            'project to know where to focus their attention and time.'
          ),
        ),
        # About - Methodology ------------------------------------------------
        box(
          title = "Methodology",
          status = "danger",
          width = "6",
          strong("Individual counts"),
          p("Once per week (Monday, 5am UTC) we run a script which takes a list",
            "of repos, and clones them all locally. For each repo, and for each",
            "word (regular expression) of interest, we run this command:"),
          pre("ag -c $word $repo_path"),
          p("Which results in a count for that term. We use `ag` because it is",
            "much faster than `grep` and because it correctly counts multiple",
            "matches per line (e.g. 'master of master' would be 1 hit in grep,",
            "but 2 hits in ag)."),
          strong("Excluded directories"),
          p("We don't exclude", strong('any'), "directories from the ag process.",
            "There are 3 main reasons for this:"),
          tags$ul(
            tags$li(strong('Simple methodology:')),
            p('If we exlude "vendor" or "src" then we start playing whack-a-mole',
              'with a proliferation of language-specific places-to-put-libraries.',
              'Keeping it simple makes it easy for others replicate our results'),
            tags$li(strong('Influencing upstream libraries')),
            p('Our goal is to improve as many projects as we can. If we simply',
              "accept the argument that \"this isn't my code\" then we're",
              "missing the chance to positively impact many other projects by",
              "working with those upstreams"),
            tags$li(strong('Nowhere to hide (code)')),
            p("It is inevitable that this project will provoke debate, and that's",
              "fine. But *we* still want to see what's happening out there, and if",
              "we exclude directories, then developers", strong('may'), 'decide',
              "to put code in those directories", strong('just to avoid indexing'))
          ),
          p('This approach has a significant consequence: ',
            strong('It is quite possible that a repo will have instances of these',
                   'terms that it cannot remove.'),
            'Perhaps because of long deprecation cycles, integration with other',
            "projects, or internal APIs, this can happen, and it's fine. We'll",
            "say again the message at the top of the page:",
            strong('The goal of this dashboard is not to shame anyone')
          ),
        ),
        # About - Dashboard -----------------------------------------
        box(
          title = "Dashboard Tools",
          status = "primary",
          width = "6",
          p('There are 4 main tabs',
            tags$ul(
              tags$li('Dashboard has the current data for the repos'),
              tags$li('History explores the timeline data for the repos'),
              tags$li('Tables lets you get exact values for a given repo'),
              tags$li('Files lets you get exact values for a given file in a repo')
            )
          ),
          p('The Org & Repo selectors on the left sidebar will allow you to filter',
            'any of the graphs to a given subset'),
          p('All graphs can be downloaded, hover for a second and the download',
            'button should appear at the bottom-left of the image')
        ),
        # About - Tools -----------------------------------------
        box(
          title = "About tooling",
          status = "primary",
          width = "6",
          tags$p(
            class = "text-center",
            tags$a(
              href = "https://www.r-project.org",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "https://www.r-project.org/logo/Rlogo.svg",
                       style = "max-width: 150px;"
              )
            ),
            tags$a(
              href = "https://rstudio.com",
              target = "_blank",
              tags$img(class = "image-responsive",
                       src = "RStudio.svg",
                       style = "max-width: 150px; margin-left: 2em;"
              )
            ),
          ),
          tags$p(
            "This dashboard was built in",
            tags$a(href = "https://r-project.org", target = "_blank", "R"),
            "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
            tags$strong("shiny,"),
            tags$strong("shinydashboard,"),
            tags$strong("plotly,"),
            "the", tags$strong("tidyverse,"),
            "and many more packages."
          ),
          tags$p(
            'You can view the source for this dashboard at',
            tags$a(href = "https://github.com/conscious-lang/dashboard-frontend", target = "_blank", "GitHub/conscious-lang/dashboard-frontend"),
          ),
          tags$p(
            'You can view the source for the backend scraper at',
            tags$a(href = "https://github.com/conscious-lang/dashboard-backend", target = "_blank", "GitHub/conscious-lang/dashboard-backend"),
          )
        )
      )
    ),
    tabItem(tabName = "method",
    )
  )
)

function(request) {
  dashboardPage(
    header,
    sidebar,
    body
  )
}
