library(pins)
library(tidyverse)

server <- function(input, output, session) {

# Reactive Vals -----------------------------------------------------------

  # Should be pin_reactive, but datafile boards aren't
  # handling ETAG cache well right now
  h <- reactiveVal(
    pin_get('cl_hist', board = 'conscious_lang')
  )
  d <- reactiveVal()
  org_repo <- reactiveVal(list(org = 'All', repo = 'All'))

# ObserveEvents -----------------------------------------------------------

  observeEvent(h(), {
    tmp <- h() %>%
      filter(date == max(date)) %>%
      select(-date)
    d(tmp)
  })

  observeEvent(d(), {
    orgs <- d() %>%
      pull(org) %>%
      unique() %>%
      sort()
    orgs <- c('All',orgs)

    updateSelectInput(session, 'side_org',
                      choices = orgs, selected = input$side_org)
  })

  observeEvent(input$side_org, {
    repos <- if (input$side_org == 'All') {
      d() %>%
        mutate(label = glue('{org}/{repo}')) %>%
        pull(label) %>%
        unique() %>%
        sort()
    } else {
      d() %>%
        filter(org == input$side_org) %>%
        pull(repo) %>%
        unique() %>%
        sort()
    }
    repos <- c('All',repos)

    updateSelectInput(session, 'side_repo',
                      choices = repos, selected = 'All')

    # Detected an update to Org, stash it
    o <- org_repo()
    o$org <- input$side_org ; o$repo <- 'All'
    org_repo(o)
  })

  observeEvent(input$side_repo, {
    # Cleanly store org/repo even when "all" selected
    or <- org_repo()
    if (str_detect(input$side_repo,'/')) {
      split <- str_split_fixed(input$side_repo,'/',2)
      or$org <- split[1]
      or$repo <- split[2]
    } else {
      # Org should already be set in the other observer
      or$repo <- input$side_repo
    }
    org_repo(or)
  })

# Bar graphs --------------------------------------------------------------

  output$plot1 <- renderGirafe({
    bar_plot(d(),blacklist)
  })

  output$plot2 <- renderGirafe({
    bar_plot(d(),whitelist)
  })

  output$plot3 <- renderGirafe({
    bar_plot(d(),master)
  })

  output$plot4 <- renderGirafe({
    bar_plot(d(),slave)
  })


# History Graphs ----------------------------------------------------------

  output$hist1 <- renderGirafe({
    line_plot(h(),blacklist)
  })

  output$hist2 <- renderGirafe({
    line_plot(h(),whitelist)
  })

  output$hist3 <- renderGirafe({
    line_plot(h(),master)
  })

  output$hist4 <- renderGirafe({
    line_plot(h(),slave)
  })


# Info Boxes --------------------------------------------------------------

  output$blacklist <- renderInfoBox({
    infoBox("Blacklist", sum(d()$blacklist),
            subtitle = 'Total in all repos',
            icon = icon('dice-one'), color = 'red')
  })
  output$whitelist <- renderInfoBox({
    infoBox("Whitelist", sum(d()$whitelist),
            subtitle = 'Total in all repos',
            icon = icon('dice-two'), color = 'yellow')
  })
  output$master <- renderInfoBox({
    infoBox("Master", sum(d()$master),
            subtitle = 'Total in all repos',
            icon = icon('dice-three'), color = 'blue')
  })
  output$slave <- renderInfoBox({
    infoBox("Slave", sum(d()$slave),
            subtitle = 'Total in all repos',
            icon = icon('dice-four'), color = 'purple')
  })


# Tables ------------------------------------------------------------------

  output$table <- DT::renderDataTable({

    or <- org_repo()
    d() %>%
      filter(if (or$org  == 'All') TRUE else org  == or$org) %>%
      filter(if (or$repo == 'All') TRUE else repo == or$repo) %>%
      arrange(url) %>%
      rowwise() %>%
      mutate(url = map_chr(url, ~ toString(htmltools::tags$a(href=url,url))),
             total = sum(c_across(where(is.integer)))) %>%
      relocate(total, .after = url) %>%
      ungroup() %>%
      select(-org,-repo) %>% # redundant for display purposes
      arrange(-total) %>%
      DT::datatable(escape=F)
  })

  output$deltas <- DT::renderDataTable({
    or <- org_repo()
    h() %>%
      filter(if (or$org  == 'All') TRUE else org  == or$org) %>%
      filter(if (or$repo == 'All') TRUE else repo == or$repo) %>%
      arrange(date) %>%
      group_by(url) %>%
      summarise(across(where(is.numeric), ~{last(.x) - nth(.x,-2L)},
                       .names = "delta_{.col}")) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(url = map_chr(url, ~ toString(htmltools::tags$a(href=url,url))),
             total = sum(c_across(where(is.numeric)))) %>%
      relocate(total, .after = url) %>%
      arrange(total) %>%
      DT::datatable(escape=F)
  })

  output$filetable <- DT::renderDataTable({
    input$search # depend on go button

    httr::GET(url  = 'http://dev.stats.eng.ansible.com:7033',
              path = '/search',
              query = list(repo = input$repo, word = input$word)
    ) -> res

    httr::content(res,'parsed')

  })
}
