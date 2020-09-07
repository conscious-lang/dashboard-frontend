library(pins)
library(tidyverse)

server <- function(input, output, session) {

  output$plot1 <- renderGirafe({
    bar_plot(d,blacklist)
  })

  output$plot2 <- renderGirafe({
    bar_plot(d,whitelist)
  })

  output$plot3 <- renderGirafe({
    bar_plot(d,master)
  })

  output$plot4 <- renderGirafe({
    bar_plot(d,slave)
  })

  output$blacklist <- renderInfoBox({
    infoBox("Blacklist", sum(d$blacklist),
            subtitle = 'Total in all repos',
            icon = icon('dice-one'), color = 'red')
  })
  output$whitelist <- renderInfoBox({
    infoBox("Whitelist", sum(d$whitelist),
            subtitle = 'Total in all repos',
            icon = icon('dice-two'), color = 'yellow')
  })
  output$master <- renderInfoBox({
    infoBox("Master", sum(d$master),
            subtitle = 'Total in all repos',
            icon = icon('dice-three'), color = 'blue')
  })
  output$slave <- renderInfoBox({
    infoBox("Slave", sum(d$slave),
            subtitle = 'Total in all repos',
            icon = icon('dice-four'), color = 'purple')
  })

  output$table <- DT::renderDataTable({
    d %>%
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
    h %>%
      arrange(date) %>%
      group_by(url) %>%
      summarise(across(where(is.integer), ~{last(.x) - nth(.x,-2L)},
                       .names = "delta_{.col}")) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(url = map_chr(url, ~ toString(htmltools::tags$a(href=url,url))),
             total = sum(c_across(where(is.integer)))) %>%
      relocate(total, .after = url) %>%
      arrange(total) %>%
      DT::datatable(escape=F)
  })

  observe({
    choices <- d %>%
      mutate(label = glue('{org}/{repo}')) %>%
      arrange(label) %>%
      dplyr::pull(label) # thanks, git2r::pull ...

    updateSelectInput(session, 'repo', choices = choices, selected = NULL)
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
