dashboardBody(
  # https://stackoverflow.com/a/36471739
  tags$style(type = "text/css", "div.nav-tabs-custom {height: 110vh !important;}"),
  tags$style(type = "text/css", "#count_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", "#trends_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(
    type = "text/css",
    "#distribution_plot {height: calc(110vh - 150px) !important;}"
  ),
  tags$style(type = "text/css", "#pairs_plot {height: calc(110vh - 150px) !important;}"),
  tags$style(type = "text/css", ".box {border: 2px solid #3c8dbc !important;}"),
  tags$style(
    type = "text/css",
    "hr.blackline { margin-top: 0px; border-top: 2px solid #808080 !important;}"
  ),
  # https://stackoverflow.com/a/40098855
  tags$head(tags$style(
    HTML(".shiny-split-layout > div {overflow: visible;}")
  )),
  tags$head(
    tags$style(type = "text/css", "text {font-family: Segoe UI,sans-serif}")
  ),

  # https://stackoverflow.com/a/32244289
  tags$style(
    "
.nav-tabs {
  background-color: #3c8dbc;
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
  background-color: transparent;
  color: #FFF;
  border-color: transparent;
  border-left-color: #FFF;
  border-right-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
  border-top-color: #FFF;
  color: #FFF;
}"),

  tabItems(
    tabItem(
      tabName = 'load_dataset',

      source('ui/body/load_and_transform.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/source_dataset.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/group_options.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/grouped_dataset.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/plot_options.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/plots.R', local = TRUE)$value,

      hr(class = 'blackline'),

      source('ui/body/selected_dataset.R', local = TRUE)$value
    )
  )
)
