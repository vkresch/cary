# Load data
som_model <- load_model(model_name)

# Plot UMatrix
cardist2d <- plotly_som_umatrix(som_model, source="carsomUmatrix2d")
cardist3d <- plotly_som_umatrix3d(som_model)

cars[,makemodel:=paste(make, model, version)]
cars[,registration := as.Date(paste0("01/", registration), format="%d/%m/%y")]

data_train <- readRDS(sprintf("encoded/%s_data_train.rds", model_name))

# Price plotting
carpriceMean <- plotly_som_price(som_model, cars, 'mean', "", source="carsomUmatrix2d")
carpriceStd <- plotly_som_price(som_model, cars, 'std', "", source="carsomUmatrix2d")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  setBookmarkExclude(c("cordBookmark", "wordcategoryBookmark"))

  # Trigger bookmarking with either button
  observeEvent(input$carBookmark, {
    session$doBookmark()
  })

  updateSelectInput(session, 
                    "propertySelection", 
                    label="Select property plot",
                    choices = c("umatrix", "price_mean", "price_std",colnames(som_model$data[[1]])))

  # =======================================================================
  # CARSOM
  # =======================================================================
  # Initialize values
  predicted_node <- 1
  user_text_input <- ""
  set_selection <<- FALSE
  d_old <- NULL
  title_search_text_input <- ""
  use_query <- reactiveValues(value = FALSE)

  predicted_node <- reactive({
    d <- event_data("plotly_click", source="carsomUmatrix2d")
    d2 <- event_data("plotly_brushed", source = "carsomUmatrix2d")

    # Check if selection changed
    if(!is.null(d) && !is.null(d_old)){
      if(d$y == d_old$y && d$x == d_old$x){
        set_selection <<- FALSE
      } else {
        set_selection <<- TRUE
      }
    } else if(!is.null(d2) && is.null(d)) {
      set_selection <<- FALSE
    }

    # Box/Lasso selection
    if(!is.null(d2) && !set_selection) {
      f_node <- future({
        y_vector <- round(d2$y)
        y_vector <- y_vector[y_vector >= 0]
        y_seq <- seq(min(y_vector), max(y_vector))
        y_seq <- y_seq[som_model$grid$ydim>=y_seq]

        x_vector <- round(d2$x)
        x_vector <- x_vector[x_vector >= 0]
        x_seq <- seq(min(x_vector), max(x_vector))
        x_seq <- x_seq[som_model$grid$xdim>=x_seq]
        pp <- expand.grid(y=y_seq,x =x_seq)
        som_model$grid$xdim * pp$y + pp$x + 1
      })
      set_selection <<- TRUE
      return(f_node)
    }

    # One node selection
    if (is.null(d)){
      if(title_search_text_input!="" && !use_query$value){
        # Regex keyword search
        f_node <- future({
          search_node_lookup_regex(som_model, cars, title_search_text_input) 
          })
      } else {
        # Initial
        f_node <- future({1})
      }
      
    } else {
      # Clicked node
      f_node <- future({som_model$grid$xdim * d$y + d$x + 1})
    }
    d_old <<- d
    return(f_node)
  })

  observeEvent(input$runsearchQuery, {
    title_search_text_input <<- input$searchTitleCar
    js$resetClick()
    js$resetBrushed()
    use_query$value <- TRUE
    use_query$value <- FALSE
  })

  output$carsomUmatrix3d <- renderPlotly({
    cardist3d
  })

  output$carsomUmatrix2d <- renderPlotly({
      predicted_node() %...T>%
        {. ->> node_clicked } %...>%
        node2coords(som_model) %...>%
        {
          x_s <- .[[1]]
          y_s <- .[[2]]
          if(input$propertySelection=="umatrix"){
            som_plot <- cardist2d
          } else if(input$propertySelection=="price_mean") {
            som_plot <- carpriceMean
          } else if(input$propertySelection=="price_std") {
            som_plot <- carpriceStd
          } else {
            som_plot <- plotly_som_property(som_model, data_train, input$propertySelection, source="carsomUmatrix2d")
          }
          som_plot %>% add_trace(x=x_s, y=y_s,
                                 name = 'BMU',
                                 mode = 'lines+markers',
                                 type="scatter",
                                 marker=list(size=10,
                                             line=list(color="black", width=2),
                                             color='rgb(255,69,0)',
                                             symbol="hexagon"),
                                 line = list(color = 'white',
                                             width = 0.0,
                                             dash = 'dot'),
                                 hovertemplate=sprintf("Node: %s", node_clicked))
      }
  })

  output$som_cars <- DT::renderDataTable({
    predicted_node() %...T>%
      {
        . ->> cov_node
        if(length(cov_node)>1){
          cov_node <<- input$searchTitleCar
          if(cov_node == "") cov_node <<- "manual"
        }
        } %...>%
      select_node_number(som_model) %...>%
      {
        selection <- .
        cars_selected <- cars[sha %in% selection] # Replace url with sha
        setorder(data.table("Registration"=cars_selected$registration,
                            "Price"=cars_selected$price_euro,
                            "Power"=cars_selected$power_kW,
                            "Driven"=cars_selected$driven_km,
                  "Title"=createCarLink(cars_selected)),
                 -Registration)
        }
  }, escape = FALSE,
  server = FALSE, extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = list('copy',
                   list(extend='csv', filename=sprintf("cord_node_%s", cov_node)),
                   list(extend='excel', filename=sprintf("cord_node_%s", cov_node)),
                   list(extend='pdf', filename=sprintf("cord_node_%s", cov_node))
                   )
  )
  )
  
  output$all_cars <- DT::renderDataTable({
    setorder(data.table("Registration"=cars$registration,
                        # "Parsed"=cars$date,
                        "Price"=cars$price_euro,
                        "Power"=cars$power_kW,
                        "Driven"=cars$driven_km,
                        "Make Model"=createCarLink(cars)),
             -Registration)
  }, escape = FALSE)
})
