header <- dashboardHeader(title = "Concious Language Project")

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem("Dashboard",  tabName = "dash",    icon = icon("dashboard")),
              menuItem("History",    tabName = "history", icon = icon("chart-line")),
              menuItem("Tables",     tabName = "tables",  icon = icon("table")),
              menuItem("Repo Files", tabName = "files",   icon = icon("search"))
  ),
  selectInput('side_org','Org','All','All'),
  selectInput('side_repo','Repo','All','All')
)

body <- dashboardBody(
  tabItems(
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
