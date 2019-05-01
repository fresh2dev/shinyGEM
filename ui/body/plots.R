fluidRow(box(
  width = 12,
  collapsible = TRUE,
  tabBox(
    id = 'tabContainer',
    width = 12,
    tabPanel(
      'Distribution',
      icon = icon('bar-chart'),
      shiny::splitLayout(
        shiny::splitLayout(
          downloadButton('download_dist_plot', label = 'Download'),
          radioButtons(
            'distGeom',
            label = 'Geom:',
            choices = c('Histogram', 'Boxplot'),
            selected = 'Histogram',
            inline = T
          ),
          checkboxInput('showRug', label = 'Show Rug', value = TRUE)
        ),
        conditionalPanel(
          'input.distGeom=="Histogram"',
          radioButtons(
            'histValuesAs',
            label = 'Values As:',
            choices = c('Frequency', '% of Grand Total', '% of Group Total'),
            selected = 'Frequency',
            inline = T
          )
        ),
        conditionalPanel(
          'input.distGeom=="Histogram"',
          numericInput(
            'bins',
            'Bins',
            min = 2,
            value = 30,
            step = 1,
            width = '50%'
          )
        ),
        verbatimTextOutput('distPlotMessage', placeholder = TRUE)
      ),
      hr(),
      ggiraphOutput('distribution_plot') %>% withSpinner(type =
                                                           6, color = '#3c8dbc')
    ),
    tabPanel(
      'Composition',
      icon = icon('pie-chart'),
      shiny::splitLayout(
        downloadButton('download_count_plot', label = 'Download'),
        radioButtons(
          'countGeom',
          label = 'Geom:',
          choices = c('Bars', 'Bars Stacked', 'Pie'),
          selected = 'Bars',
          inline = T
        ),
        conditionalPanel(
          'input.countGeom != "Pie"',
          radioButtons(
            'countValuesAs',
            label = 'Values As:',
            choices = c('Values', '% of Grand Total', '% of Group Total'),
            selected = 'Values',
            inline = T
          )
        ),
        verbatimTextOutput('countPlotMessage', placeholder = TRUE)
      ),
      hr(),
      ggiraphOutput('count_plot', width = '100%') %>% withSpinner(type =
                                                                    6, color = '#3c8dbc')
    ),
    tabPanel(
      'Trends',
      icon = icon('line-chart'),
      shiny::splitLayout(
        downloadButton('download_trend_plot', label = 'Download'),
        radioButtons(
          'trendGeom',
          label = 'Geom:',
          choices = c('Lines', 'Bars'),
          selected = 'Lines',
          inline = T
        ),
        radioButtons(
          'trendValuesAs',
          label = 'Values As:',
          choices = c('Values', '% of Grand Total', '% of Group Total'),
          selected = 'Values',
          inline = T
        ),
        verbatimTextOutput('trendPlotMessage', placeholder = TRUE)
      ),
      hr(),
      ggiraphOutput('trends_plot') %>% withSpinner(type =
                                                     6, color = '#3c8dbc')
    ),
    tabPanel(
      'Feature Pairs',
      icon = icon('exchange'),
      shiny::splitLayout(
        downloadButton('download_pairs_plot', label = 'Download'),
        selectizeInput(
          'featureX',
          label = 'X Feature',
          choices = NULL,
          selected = NA,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          'featureY',
          label = 'Y Feature',
          choices = NULL,
          selected = NA,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # box(background = 'light-blue', actionButton('viewPairsPlot', 'View / Refresh', icon=icon('refresh'), width='100%')),
        verbatimTextOutput('pairsPlotMessage', placeholder = T)
      ),
      hr(),
      ggiraphOutput('pairs_plot') %>% withSpinner(type =
                                                    6, color = '#3c8dbc')
    )
  )
))
