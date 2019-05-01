
# Shiny GEM

Shiny GEM is a data analysis app written in R Shiny.

*Now available from DockerHub!*


- [Documentation](#documentation)
- [How to Run](#how-to-run)
  - [ShinyApps.io](#shinyappsio)
  - [Docker](#docker)
  - [From Source](#from-source)


## Documentation

https://www.donaldmellenbruch.com/doc/shinygem

> Simplified documentation is on the agenda.

## How to Run

### ShinyApps.io

The easiest way to demo the app is from [ShinyApps.io](https://dmellenbruch.shinyapps.io/Shiny_GEM/). Though, for increased security, consider setting up a local instance with Docker or from source.

https://dmellenbruch.shinyapps.io/Shiny_GEM/

### Docker

* Download image from DockerHub:

```bash
docker pull dm3ll3n/shiny-gem
```

* Run locally in a background container:

```bash
docker run -d --restart unless-stopped -p 127.0.0.1:3838:3838 dm3ll3n/shiny-gem
```

### From Source

Clone the source code, install dependencies, and launch locally.

```bash
git clone https://github.com/dm3ll3n/Shiny-GEM.git

cd Shiny-GEM

Rscript "install-requirements.R"

R -e "shiny::runApp(host='127.0.0.1', port=3838)"
```
