# Cron script to clone the necessary git repos

library(pins)
library(tidyverse)
library(git2r)
library(here)

setwd(here())

pins::board_register_local(name = 'conscious_lang', cache = '/tmp')

projects <- pin_get('cl_projects', board = 'conscious_lang') %>%
  # temporary, github only, other styles to come
  filter(str_detect(repo,'github.com')) %>%
  # split up the path so we can use it for things
  mutate(url   = repo,
         path  = str_split(url,'/'),
         parts = map_int(path, ~{ .x %>%
                                    unlist() %>%
                                    length() })
  ) %>%
  filter(parts == 5) %>% # habndle orgs (4 parts) later
  mutate(org  = map_chr(path, ~{ unlist(.x) %>%
                                   tail(2) %>%
                                   head(1) }),
         repo = map_chr(path, ~{ unlist(.x) %>%
                                   tail(1) })
  ) %>%
  select(url, org, repo)

# Holding area
if (!dir.exists(here('clones'))) { dir.create(here('clones'))}

# Create any necessary org dirs
for (dir in unique(projects$org)) {
  if (!dir.exists(here('clones',dir))) { dir.create(here('clones',dir))}
}

# Wrapper to clone or pull depending on the repo presence
pull_or_clone <- function(url, path) {
  # Do we need to switch to shallow clone?
  if (dir.exists(path)) {
    git2r::pull(path)
  } else {
    system2('git', c('clone', '--depth', '1', '--quiet', url, path))
  }
}
# Do it nicely, don't break the loop
safe_pull_or_clone = possibly(pull_or_clone, otherwise = NA)

# Clone repos
library(furrr)
plan(multiprocess, workers=4)
projects <- projects %>%
  mutate(pull = future_map2(url,
                     str_c('clones',org,repo,sep='/'),
                     safe_pull_or_clone,
                     .progress = interactive()))

count_words <- function(org, repo, word) {
  # Search path for this repo
  path = here('clones',org,repo)

  # This is very ugly, but ag returns exit 1 on match-not-found
  suppressWarnings(
    system2('ag',c('-c', word, path), stdout = TRUE, stderr = FALSE)
  ) -> res

  # Ag2 vs Ag1
  if (length(res) > 0 && str_detect(res[1],':')) {
    #AG1 returns paths too
    res %>%
      str_extract(':[0-9]*$') %>%
      str_remove(':') -> res
  }

  res %>%
    as.integer() %>%
    sum() %>%
    return()
}

# Note the failures
projects %>%
  filter(is.na(pull)) %>%
  mutate(pull = dir.exists(here('clones',org,repo))) %>%
  select(url, pull) -> failures

# Count words in clone
projects %>%
  mutate(blacklist = future_map2_int(org, repo, count_words, 'blacklist', .progress = interactive()),
         whitelist = future_map2_int(org, repo, count_words, 'whitelist', .progress = interactive()),
         master    = future_map2_int(org, repo, count_words, 'master',    .progress = interactive()),
         slave     = future_map2_int(org, repo, count_words, 'slave',     .progress = interactive())
  ) -> projects

projects <- projects %>% filter(blacklist + whitelist + master + slave > 0)

pin(failures,name='cl_fails', board = 'conscious_lang')
pin(projects,name='cl_results', board = 'conscious_lang')
