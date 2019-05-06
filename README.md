
# Shiny GEM

Shiny GEM is a data analysis app written in R Shiny.

- [Documentation](#documentation)
- [Ways to Run](#ways-to-run)
  - [ShinyApps.io](#shinyappsio)
  - [Docker](#docker)
  - [From Source](#from-source)
  - [ShinyStudio](#shinystudio)


## Documentation

https://www.donaldmellenbruch.com/doc/shinygem

> Simplified documentation is on the agenda.

## Ways to Run

Shiny GEM is available on ShinyApps.io, DockerHub, GitHub, and from within ShinyStudio. Take your pick!

### ShinyApps.io

The easiest way to demo the app is from [ShinyApps.io](https://dmellenbruch.shinyapps.io/Shiny_GEM/).

https://dmellenbruch.shinyapps.io/Shiny_GEM/

For increased performance and security, consider another method below.

### Docker

* Download image from DockerHub:

```bash
docker pull dm3ll3n/shiny-gem
```

* Run locally in a background container:

```bash
docker run -d --restart unless-stopped -p 127.0.0.1:3838:3838 dm3ll3n/shiny-gem
```

Shiny GEM will now be available in a browser at `http://localhost:3838`.

### From Source

Clone the source code, install dependencies, and launch locally.

```bash
git clone https://github.com/dm3ll3n/Shiny-GEM.git

cd Shiny-GEM

Rscript "install-requirements.R"

R -e "shiny::runApp(host='127.0.0.1', port=3838)"
```

Shiny GEM will now be available in a browser at `http://localhost:3838`.

### ShinyStudio

Shiny GEM is included as an example app in the [ShinyStudio](https://github.com/dm3ll3n/ShinyStudio) Docker stack. First, follow the [setup instructions](https://github.com/dm3ll3n/ShinyStudio#how-to-get-it) for ShinyStudio. Afterward: 

* Navigate to ShinyStudio at `http://localhost:8080`.
* Open RStudio.
* Use RStudio's file browser to open `shiny-examples/Shiny-GEM/app.R`.
* Run the app within RStudio.

Optionally, copy the directory `shiny-examples/Shiny-GEM` to `__ShinyStudio__/_apps` in order to serve Shiny GEM from the "Apps & Reports" page.
