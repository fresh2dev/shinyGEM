dashboardSidebar(width = 150,
                 sidebarMenu(
                   id = 'sb_dataset',
                   menuItem(
                     'Home',
                     tabName = 'load_dataset',
                     icon = icon('home'),
                     selected = TRUE
                   ),
                   menuItem(
                     'Documentation',
                     icon = icon('book'),
                     href = 'https://www.donaldmellenbruch.com/doc/shinygem/',
                     newtab = TRUE
                   ),
                   menuItem(
                     'GitHub',
                     icon = icon('github'),
                     href = 'https://github.com/dm3ll3n/Shiny-GEM',
                     newtab = TRUE
                   )
                 ))
