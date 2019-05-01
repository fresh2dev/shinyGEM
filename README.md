
## [Run hosted instance on ShinyApps.io](https://dmellenbruch.shinyapps.io/Shiny_GEM/)

*or*

Run local instance with Docker:

* Download image from DockerHub:

```bash
docker pull dm3ll3n/shiny-gem
```

* Run locally in a background container:

```bash
docker run --rm -d -p 3838:3838 --name shiny-gem dm3ll3n/shiny-gem
```

* Stop container:

```bash
docker stop shiny-gem
```

[Read the documentation](https://www.donaldmellenbruch.com/doc/shinygem)

> Simplified documentation is on the agenda.
