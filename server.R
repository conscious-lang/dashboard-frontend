library(pins)
library(tidyverse)

bar_plot <- function(d,word) {
  word_str <- quo_name(enquo(word))
  d %>%
    mutate(label = glue('{org}/{repo}')) %>%
    arrange(-{{word}}) %>%
    slice(1:10) %>%
    ggplot(aes(fct_reorder(label,{{word}}),{{word}})) +
    geom_col(fill = '#CB333B') +
    coord_flip() +
    labs(title = NULL,
         caption = glue('Results of "sum(ag -c {word_str} $repo)"'),
         x = 'Repo', y = 'Count') +
    theme(text = element_text(size = 18))
}

clean_ag_output <- function(string) {
  match <- str_extract(string,':[0-9]*$')
  name  <- str_remove(string, match) %>% str_remove('^:')
  value <- str_remove(match,':') %>% as.integer()
  data.frame(File = name, Count = value)
}


server <- function(input, output, session) {
  d <- pin_reactive('cl_results', board = 'local')

  output$plot1 <- renderPlot({
    bar_plot(d(),blacklist)
  })

  output$plot2 <- renderPlot({
    bar_plot(d(),whitelist)
  })

  output$plot3 <- renderPlot({
    bar_plot(d(),master)
  })

  output$plot4 <- renderPlot({
    bar_plot(d(),slave)
  })

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

  output$table <- DT::renderDataTable({
    d() %>% select(-pull) %>% arrange(url)
  })

  observe({
    choices <- d() %>%
      mutate(label = glue('{org}/{repo}')) %>%
      arrange(label) %>%
      dplyr::pull(label) # thanks, git2r::pull ...

    updateSelectInput(session, 'repo', choices = choices, selected = NULL)
  })

  output$filetable <- DT::renderDataTable({
    input$search # depend on go button

    # change dir so we don't print the filepath
    setwd(here('clones',isolate(input$repo)))

    # This is very ugly, but ag returns exit 1 on match-not-found
    suppressWarnings(
      system2('ag', c('--ackmate','-c', isolate(input$word), '.'),
              stdout = TRUE)
    ) -> res

    req(res)

    res %>%
      map_dfr(clean_ag_output) %>%
      arrange(-Count)

  })
}
