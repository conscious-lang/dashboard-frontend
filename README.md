# Conscious Language Dashboard - backend

This repo contains the source for the [RShiny](https://shiny.rstudio.com/)
dashboard used by the Conscious Language project. You can view the dashboard
itself at
[https://stats.eng.ansible.com/apps/ConsciousLanguage](https://stats.eng.ansible.com/apps/ConsciousLanguage)

# Installation

## OS dependencies

Short version: There is an Ansible playbook for the necessary dependencies, run
that first

```
ansible-playbook playbook.yml
```

(This was written for a Debian host, but essentially it installs some
dependencies, and sets up LighTTPD to run the API)

## R dependencies

In the root of the project, use [Renv](https://rstudio.github.io/renv) to get the deps:

```
Rscript -e 'renv::restore()'
```

# Input Data

The dashboard loads data from the [backend](https://github.com/conscious-lang/dashboard-backend) so set that up first. Once the API and LighTTPD setup is ready, you can set this up

# Configuration

There are two calls to the backend, one in `global.R`, the other in `server.R`.
Both reference `dev.stats.eng.ansible.com` which is not in public DNS (the
dashboard has this host in `/etc/hosts`) so you can either set that name
yourself (in hosts) or change the source to point elsewhere.

# Execution

This is a Shiny app, so to run it you will either need to:

1. Run it locally in R (or RStudio)
  * clone the project ; cd to it
  * open it in R
  * call 'shiny::runApp()'
2. Host it on a Shiny server
  * This could be a Shiny Server, a Docker + ShinyProxy setup, RSConnect, or a hosted option like `shinyapps.io`
  * Setting up these options is beyond the scope of this README

# Contributing

Help is very welcome at all levels - please do open Issues or Pull Requests as
required, even for minor things!
