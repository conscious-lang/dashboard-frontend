library(pins)
library(tidyverse)

server <- function(input, output, session) {

# Reactive Vals -----------------------------------------------------------

  # Should be pin_reactive, but datafile boards aren't
  # handling ETAG cache well right now
  raw <- reactiveVal(
    pin_get('cl_hist', board = 'conscious_lang')
  )
  h <- reactiveVal()
  d <- reactiveVal()
  org_repo <- reactiveVal(list(org = 'All', repo = 'All'))

# ObserveEvents -----------------------------------------------------------

  observeEvent(raw(), {
    # if raw updates, reset d() and h()
    h(raw())
    d(raw() %>%
      filter(date == max(date)) %>%
      select(-date))

    # now get the org list from raw()
    orgs <- raw() %>%
      filter(date == max(date)) %>%
      pull(org) %>%
      unique() %>%
      sort()
    orgs <- c('All',orgs)

    updateSelectInput(session, 'side_org',
                      choices = orgs, selected = input$side_org)
  })

  observeEvent(input$side_org, {
    repos <- if (input$side_org == 'All') {
      raw() %>%
        filter(date == max(date)) %>%
        mutate(label = glue('{org}/{repo}')) %>%
        pull(label) %>%
        unique() %>%
        sort()
    } else {
      raw() %>%
        filter(date == max(date)) %>%
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

  observeEvent(org_repo(),{
    or <- org_repo()
    tmp <- raw() %>%
      filter(if (or$org  == 'All') TRUE else org  == or$org) %>%
      filter(if (or$repo == 'All') TRUE else repo == or$repo)
    h(tmp)
    tmp <- tmp %>%
      filter(date == max(date)) %>%
      select(-date)
    d(tmp)
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

    d() %>%
      arrange(url) %>%
      rowwise() %>%
      mutate(url = map_chr(url, ~ toString(htmltools::tags$a(href=url,url))),
             total = sum(c_across(where(is.numeric)))) %>%
      relocate(total, .after = url) %>%
      ungroup() %>%
      select(-org,-repo) %>% # redundant for display purposes
      arrange(-total) %>%
      DT::datatable(escape=F)
  })

  output$deltas <- DT::renderDataTable({
    h() %>%
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
    if (input$side_org == 'All') {
      return(data.frame(message = 'Please select a specific Org'))
    }

    if (input$side_repo == 'All') {
      return(data.frame(message = 'Please select a specific Repo'))
    }

    repo = paste(input$side_org, input$side_repo, sep = '/')
    httr::GET(url  = 'http://dev.stats.eng.ansible.com:7033',
              path = '/search',
              query = list(repo = repo, word = input$word)
    ) -> res

    httr::content(res,'parsed')

  })
}
