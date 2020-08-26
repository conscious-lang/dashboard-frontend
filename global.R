library(shinydashboard)
library(glue)
library(here)

pins::board_register_datatxt(name = "conscious_lang",
                             url = "http://dev.stats.eng.ansible.com/data.txt")

d <- pin_get('cl_results', board = 'conscious_lang')
h <- pin_get('cl_hist', board = 'conscious_lang')
