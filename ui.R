header <- dashboardHeader(title = "Concious Language Project")

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'menu',
              menuItem("Dashboard",  tabName = "dash",    icon = icon("dashboard")),
              menuItem("History",    tabName = "history", icon = icon("chart-line")),
              menuItem("Tables",     tabName = "tables",  icon = icon("table")),
              menuItem("Deltas",     tabName = "deltas",  icon = icon("arrows-alt-v")),
              menuItem("Repo Files", tabName = "files",   icon = icon("search"))
  )
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
                  plotOutput("plot1", height = 290)),
              box(title = 'Whitelist', status = "primary",
                  solidHeader = TRUE,
                  plotOutput("plot2", height = 290))
            ),
            fluidRow(
              box(title = 'Master', status = "primary",
                  solidHeader = TRUE,
                  plotOutput("plot3", height = 290)),
              box(title = 'Slave', status = "primary",
                  solidHeader = TRUE,
                  plotOutput("plot4", height = 290))
            )
    ),
    tabItem(tabName = "history",
            h2("Time-series history of total counts"),
            p("TBD - we don't have historical data yet")
    ),
    tabItem(tabName = "tables",
            h2("Searchable / Sortable table of counts"),
            DT::dataTableOutput('table')
    ),
    tabItem(tabName = "deltas",
            h2("Searchable / Sortable table of changes"),
            DT::dataTableOutput('deltas')
    ),
    tabItem(tabName = "files",
            h2("Word counts per file in a given repo"),
            fluidRow(
              box(title = 'Repo', width = 6,
                  selectInput('repo',NULL,c('Please wait'))
              ),
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
