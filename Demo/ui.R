pageWithSidebar(
  headerPanel('Portfolio activo'),
  sidebarPanel(
    selectInput('cluster',
                'Seleccionar cluster',
                c('Asia Emergente','Europa','USA sectores','Asia Sub','USA core')),
    uiOutput('options'),
    actionButton("rfs", "Actualizar datos"),
    tags$br(),
    actionButton("create", "Crear portafolios")
  ),
  mainPanel(
    plotOutput('plot1')
  )
)