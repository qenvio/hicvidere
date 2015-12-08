server <- shinyServer(function(input, output, session) {

  output$selection <- renderUI({

    if (is.null(input$file1))
      return(NULL)

    info <- read.delim(input$file1$datapath, stringsAsFactors = F, head = F)

    list(selectInput("hic_id", "Select HiC experiment", info$V1, selectize = T),
         selectInput("hic_id_2", "Select HiC experiment to compare with",
                     c("none", info$V1), selected = NULL, selectize = T),
         textInput("region", "Region", "chr6:146000000-150000000"),
         numericInput("resolution", "Resolution (Kbp)", 500, 10, 5000, 10),
         sliderInput("size", "Size of the image (pixels)", 200, 2000, 600, 50, ticks = F))

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

    if(input$hic_id_2 == "none") {
    
        contacts <- make_contacts(path, input$region, input$resolution * 1000)

        id1 <- id2 <- NULL
        
        #return(plot_contacts_rotated(log10(contacts + .9)))

    }else{
        
        path_2 <- info$V2[match(input$hic_id_2, info$V1)]

        contacts_1 <- make_contacts(path, input$region, input$resolution * 1000)

        contacts_2 <- make_contacts(path_2, input$region, input$resolution * 1000)
        
        contacts <- combine_contacts(contacts_2, contacts_1)
        
        id1 <- input$hic_id
        id2 <- input$hic_id_2

        #return(plot_contacts_rotated(log10(contacts + .9), 1, m2))

    }

    plot_contacts_rotated(contacts, m1 = id2, m2 = id1)

  }, width = select_size, height = select_size)
    

})
