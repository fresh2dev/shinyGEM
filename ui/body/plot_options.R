fluidRow(
  box(
    width = 12,
    collapsible = TRUE,
    title = 'Plot Options',
    solidHeader = TRUE,
    status = 'primary',
    box(
      width = 2,
      radioButtons(
        'plotValues',
        label = 'Values to Plot:',
        inline = TRUE,
        choices = c('values', '# observations'),
        selected = '# observations'
      ),
      hr(),
      radioButtons(
        'legendPos',
        label = 'Legend Position:',
        inline = TRUE,
        choices = c('bottom', 'right'),
        selected = 'bottom'
      ),
      hr(),
      checkboxInput('reactivity', label = 'Auto-Refresh', value = TRUE),
      conditionalPanel(
        'input.reactivity==0',
        actionButton('submit', label = 'Refresh')
      )
    ),
    box(
      width = 3,
      checkboxGroupInput(
        'plotParams',
        label = 'Parameters:',
        inline = FALSE,
        choices = c(
          'Swap Groups & Features' = 'swap',
          'Facet Features' = 'facetFeatures',
          'Rotate' = 'rotate',
          'Fix X Limits' = 'fixLimsX',
          'Fix Y Limits' = 'fixLimsY',
          'Show Data Labels' = 'showLabels'
        ),
        selected = c('fixLimsX', 'fixLimsY')
      ),
      shiny::splitLayout(
        numericInput(
          'n_x_breaks',
          'Desired X breaks:',
          min = 1,
          value = 31,
          step = 1
        ),
        numericInput(
          'n_y_breaks',
          'Desired Y breaks:',
          min = 1,
          value = 10,
          step = 1
        )
      )
    ),
    box(
      width = 3,
      # height='85px',
      tags$label(class = 'control-label', 'Labels:'),
      # shiny::splitLayout(
      textInput(
        'title',
        label = NA,
        value = NA,
        placeholder = 'Title'
      ),
      textInput(
        'subtitle',
        label = NA,
        value = NA,
        placeholder = 'Subtitle'
      ),
      textInput(
        'caption',
        label = NA,
        value = NA,
        placeholder = 'Caption'
      ),
      shiny::splitLayout(
        textInput(
          'xlab',
          label = NA,
          value = NA,
          placeholder = 'x-label'
        ),
        textInput(
          'ylab',
          label = NA,
          value = NA,
          placeholder = 'y-label'
        )
      ),
      radioButtons(
        'angleX',
        label = 'Text Angle (X):',
        choices = c(0, 35, 45, 90),
        inline = T,
        selected = 35
      )
      # )
    ),
    box(
      width = 4,
      selectizeInput(
        'plotTheme',
        'Plot Theme:',
        choices = all_themes <-
          list(
            'Stock Themes' = ls('package:ggplot2') %>% .[grepl(., pattern = theme_pattern, perl =
                                                                 T)] %>%
              sub(
                pattern = theme_pattern,
                replacement = '',
                perl = T
              ),
            'GG Themes' = ls('package:ggthemes') %>% .[grepl(., pattern =
                                                               theme_pattern, perl = T)] %>%
              sub(
                pattern = theme_pattern,
                replacement = '',
                perl = T
              )
          ),
        selected = 'gray',
        multiple = TRUE,
        options = list(maxItems = 1)
      ),

      selectizeInput(
        'plotColors',
        'Plot Colors:',
        choices = list(
          'GG Colors' = ls('package:ggthemes') %>% .[grepl(., pattern = '(?<!shape|linetype|gradient)\\_pal$', perl = T)] %>%
            sub(
              pattern = '_pal',
              replacement = '',
              fixed = T
            ),
          'Gradients' = colors(distinct = T) %>% .[!grepl(., pattern =
                                                            '\\d')]
        ),
        selected = 'gdocs',
        multiple = TRUE,
        options = list(maxItems = 1)
      ),

      shiny::splitLayout(
        numericInput(
          'plotHeight',
          label = 'Plot Image Height (in.)',
          min = 3,
          max = 99,
          value = 10
        ),
        numericInput(
          'plotWidth',
          label = 'Plot Image Width (in.)',
          min = 3,
          max = 99,
          value = 16
        )
      )
    )
  )
)
