ui <- shinyUI(fluidPage(

  titlePanel("HiCvideRe"),

  sidebarLayout(

    sidebarPanel(

      fileInput('file1',
                'Choose TSV File',
                accept=c("text/tab-separated-values",
                         ".tsv")),

      uiOutput('selection')

    ),

    mainPanel(

      plotOutput('plot_matrix', inline = T)

    )

  )

))
