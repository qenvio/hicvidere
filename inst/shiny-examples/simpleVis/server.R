server <- shinyServer(function(input, output, session) {

  output$selection <- renderUI({

    if (is.null(input$file1))
      return(NULL)

    info <- read.delim(input$file1$datapath, stringsAsFactors = F, head = F)

    list(selectInput("hic_id", "Select HiC experiment", info$V1, selectize = T),
         textInput("region", "Region", "chr6:146000000-150000000"),
         checkboxInput("rotate", "Rotate diagonal", TRUE),
         sliderInput("size", "Size of the image (pixels)", 200, 2000, 800, 50, ticks = F))

  })

  select_size <- reactive({

    if(is.null(input$size))
      return(1)

    input$size

  })

  output$plot_matrix <- renderPlot({

    if (is.null(input$file1))
      return(NULL)

    info <- read.delim(input$file1$datapath, stringsAsFactors = F, head = F)

    path <- info$V2[match(input$hic_id, info$V1)]

    if (length(path) == 0)
      return(NULL)

    contacts <- get_contacts(path, input$region)

    plot_contacts(contacts, input$rotate)

  }, width = select_size, height = select_size)

})
