coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

plot_bar_chart <- function(df, attribute="price_euro"){
  # Create a bar plot for each makemodel
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,8,4,2)) # increase y-axis margin.
  barplot(df[, attribute], main=attribute, names.arg = df[,'makemodel'], horiz=TRUE, cex.names=0.6)
}

plot_som_price <- function(somObj, originalData, calc='mean', label='make'){
  # Calculate the price mean for each node
  num_nodes <- som_model$grid$xdim * som_model$grid$ydim
  node_price_mean <- c()
  node_price_std <- c()
  for (nodeid in 1:num_nodes){
    node <- originalData[rownames(data.frame(somObj$data)[which(somObj$unit.classif==nodeid),]),]
    node_price_mean <- append(node_price_mean, mean(node$price_euro))
    node_price_std <- append(node_price_std, sd(node$price_euro))
  }
  
  # Plot price
  par(mfrow = c(1, 1))
  par(oma=c(1,1,1,1))
  
  if(calc=='mean'){
    plot(somObj, type = "property", property = node_price_mean, shape = 'straight', main = paste("price", calc), border=NA, palette.name = coolBlueHotRed)
  } else if(calc=='std') {
    plot(somObj, type = "property", property = node_price_std, shape = 'straight', main = paste("price", calc), border=NA, palette.name = coolBlueHotRed)
  } 
  par(new=TRUE)
  par(oma=c(1,5,1,1))
  plot(somObj, type="mapping" , main=paste("price", calc),  shape="straight", border = NA, labels=originalData[rownames(data.frame(somObj$data)), label])
  
}

plot_som_property <- function(somObj, originalData, attribute_number=1, label='make'){
  scaled_var <- scale(originalData[,attribute_number])
  if (length(unique(getCodes(somObj)[,attribute_number]* attr(scaled_var, 'scaled:scale') + attr(scaled_var, 'scaled:center')))!=1){
    par(mfrow = c(1, 1))
    par(oma=c(1,1,1,1))
    plot(somObj, type = "property", property = getCodes(somObj)[,attribute_number]* attr(scaled_var, 'scaled:scale') + attr(scaled_var, 'scaled:center'), main=colnames(getCodes(somObj))[attribute_number], shape = 'straight', border=NA, palette.name = coolBlueHotRed)
    par(new=TRUE)
    par(oma=c(1,5,1,1))
    plot(somObj, type="mapping" , main=colnames(getCodes(somObj))[attribute_number],  shape="straight", border = NA, labels=originalData[rownames(data.frame(somObj$data)),label])
  }
}

plotly_som_property <- function(somObj, originalData, attribute_number, label='make', source=""){
  scaled_var <- scale(originalData[,attribute_number])
  if (length(unique(getCodes(somObj)[,attribute_number]* attr(scaled_var, 'scaled:scale') + attr(scaled_var, 'scaled:center')))!=1){
    node_nums <- t(matrix(seq(1:(somObj$grid$xdim*somObj$grid$ydim)), nrow = somObj$grid$xdim, ncol = somObj$grid$ydim))
    propmatrix <- getProperty(somObj, attribute_number, scaled_var)
    property_plot <- plot_ly(z = ~propmatrix,
                            colorscale = 'Portland',
                            type = "contour",
                            name = attribute_number,
                            text=node_nums,
                            hovertemplate = 'Node: %{text}<extra></extra>' ,
                            line = list(smoothing = 0.85),
                            source=source) %>% colorbar(title = attribute_number) %>% config(displaylogo = FALSE)
  }
  return(property_plot)
}

plotly_som_price <- function(somObj, originalData, calc='mean', label='make', source=""){
  # Calculate the price mean for each node
  num_nodes <- somObj$grid$xdim * somObj$grid$ydim
  node_price_mean <- c()
  node_price_std <- c()
  for (nodeid in 1:num_nodes){
    node <- originalData[originalData$sha %in% rownames(data.frame(somObj$data)[which(somObj$unit.classif==nodeid),]),]
    node_price_mean <- append(node_price_mean, mean(node$price_euro))
    node_price_std <- append(node_price_std, sd(node$price_euro))
  }
  node_nums <- t(matrix(seq(1:(somObj$grid$xdim*somObj$grid$ydim)), nrow = somObj$grid$xdim, ncol = somObj$grid$ydim))
  if(calc=='mean'){
    node_matrix <- t(matrix(node_price_mean, ncol = somObj$grid$ydim, nrow = somObj$grid$xdim))
  } else if(calc=='std') {
    node_matrix <- t(matrix(node_price_std, ncol = somObj$grid$ydim, nrow = somObj$grid$xdim))
  } 
  plot_ly(z = ~node_matrix,
          colorscale = 'Portland',
          type = "contour",
          name = sprintf("Price %s", calc),
          connectgaps=T,
          text=node_nums,
          hovertemplate = 'Node: %{text}<extra></extra>' ,
          line = list(smoothing = 0.85),
          source=source) %>% colorbar(title = sprintf("Price %s", calc)) %>% config(displaylogo = FALSE)
}

plotly_som_property3d <- function(somObj, originalData, attribute_number=1, label='make'){
  scaled_var <- scale(originalData[,attribute_number])
  if (length(unique(getCodes(somObj)[,attribute_number]* attr(scaled_var, 'scaled:scale') + attr(scaled_var, 'scaled:center')))!=1){
    propmatrix <- getProperty(somObj, attribute_number, scaled_var)
    property_plot <- plot_ly(z = ~propmatrix, name = colnames(getCodes(somObj))[attribute_number], colorscale = 'Portland') %>%
      add_surface(
        contours = list(
          z = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(z=TRUE)
          )
        )
      ) %>%
      layout(
        scene = list(
          camera=list(
            eye = list(x=1.87, y=0.88, z=0.64)
          ),
          aspectmode='manual',
          aspectratio = list(x=1.5, y=1, z=0.5),
          zaxis=list(title = colnames(getCodes(somObj))[attribute_number])
        )
      ) %>% colorbar(title = colnames(getCodes(somObj))[attribute_number]) %>% config(displaylogo = FALSE)
  }
  return(property_plot)
}

plot_som_umatrix  <- function(somObj, originalData, label='make'){
  par(mfrow = c(1, 1))
  par(oma=c(1,1,1,1))
  plot(som_model, type="dist.neighbours", border = NA, main = "Umatrix", palette.name=grey.colors, shape = "straight")
  par(new=TRUE)
  par(oma=c(1,5,1,1))
  plot(som_model, type="mapping" , main="Umatrix",  shape="straight", border = NA, labels=cars[rownames(data.frame(som_model$data)),label])
}

plotly_som_umatrix <- function(somObj, source=""){
  umatrix <- getUmatrix(somObj)
  node_nums <- t(matrix(seq(1:(somObj$grid$xdim*somObj$grid$ydim)), nrow = somObj$grid$xdim, ncol = somObj$grid$ydim))
  umatrix_plot <- plot_ly(z = ~umatrix,
                          colorscale = 'Portland',
                          type = "contour",
                          name = 'UMatrix',
                          text=node_nums,
                          hovertemplate = 'Node: %{text}<extra></extra>' ,
                          line = list(smoothing = 0.85),
                          source=source) %>% colorbar(title = "Similarity") %>% config(displaylogo = FALSE)
  return(umatrix_plot)
}

plotly_som_umatrix3d <- function(somObj){
  umatrix <- getUmatrix(somObj)
  umatrix_plot <- plot_ly(z = ~umatrix, name = 'Covsom', colorscale = 'Portland') %>%
    add_surface(
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      )
    ) %>%
    layout(
      scene = list(
        camera=list(
          eye = list(x=1.87, y=0.88, z=0.64)
        ),
        aspectmode='manual',
        aspectratio = list(x=1.5, y=1, z=0.5),
        zaxis=list(title = "Similarity")
      )
    ) %>% colorbar(title = "Similarity") %>% config(displaylogo = FALSE)
  return(umatrix_plot)
}