library(kohonen)
library(stringr)

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}

select_node <- function(somObj){
  node_selected <- identify(somObj, n=1)
  selected_row_name <- rownames(data.frame(somObj$data)[which(somObj$unit.classif==node_selected),])
  return(selected_row_name)
}

select_node_number <- function(node_number, somObj){
  match_nodes <- somObj$unit.classif %in% node_number
  selected_row_name <- rownames(data.frame(somObj$data)[match_nodes,]) # TODO this can be optimized without conversion
  return(selected_row_name)
}

search_node <- function(somObj, search_word){
  number_nodes <- somObj$grid$xdim * somObj$grid$ydim
  for(i in 1:number_nodes){
    selected_row_name <- rownames(data.frame(somObj$data)[which(somObj$unit.classif==i),])
    if(search_word %in% selected_row_name){
      return(i)
    }
  }
  return(NA)
}

search_node_som <- function(somObj, search_word){
  if(search_word %in% rownames(somObj$data[[1]])){
    prediction <- predict(somObj, newdata=t(as.matrix(somObj$data[[1]][search_word,])))
    return(prediction$unit.classif)
  } else {
    return(NA)
  }
}

normalize <- function(x) {x / sqrt(sum(x^2))}

gen_random_strings <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

search_node_hash <- function(somObj, search_word){
  position <- which( rownames(somObj$data[[1]])==search_word )
  return(somObj$unit.classif[position])
}

search_node_lookup_regex <- function(somObj,
                                     metadata,
                                     title_search=""){
  
  if(title_search != ""){
    selected_cars <- metadata[grep(title_search, metadata$makemodel, ignore.case = TRUE, perl = TRUE)]
  }
  
  position <- which(rownames(somObj$data[[1]]) %in% selected_cars$sha)
  return(somObj$unit.classif[position])
}

search_node_regex <- function(somObj, regex_expression){
  position <- grep(regex_expression, rownames(somObj$data[[1]]), ignore.case = TRUE, perl = TRUE)
  return(somObj$unit.classif[position])
}

getProperty <- function(somObj, property_number, scaled_var){
  prop <- getCodes(somObj)[,property_number] * attr(scaled_var, 'scaled:scale') + attr(scaled_var, 'scaled:center')
  property <- t(matrix(prop, ncol = somObj$grid$ydim, nrow = somObj$grid$xdim))
  return(property)
}

getUmatrix <- function(somObj){
  nhbrdist <- unit.distances(somObj$grid, TRUE)
  cddist <- as.matrix(object.distances(somObj, type = "codes"))
  cddist[abs(nhbrdist - 1) > .001] <- NA
  neigh.dists <- colMeans(cddist, na.rm = TRUE)
  umatrix <- t(matrix(neigh.dists, ncol = somObj$grid$ydim, nrow = somObj$grid$xdim))
  return(umatrix)
}

onehotencode <- function(df, cname){
  temp_df <- model.matrix(~0+df[,cname])
  attr(temp_df, "dimnames")[[2]] <- paste(cname ,levels(as.factor(df[,cname])), sep="_")
  df <- cbind(df, temp_df)
  df <- df[ , !(names(df) %in% c(cname, paste(cname, "_", sep = ""), paste(cname, "_", sep = "V1")))]
  return(df)
}

coords2node <- function(somObj, coords){
  # Node 1 x= 1.47501273290032 y=0.663371002607789
  if(is.null(coords)){
    node <- 0
  } else {
    x <- floor(coords$xy[2,1])
    y <- floor(coords$xy[2,2])
    node <- x*1+y*somObj$grid$xdim
  }
  # Check if node is not out of bounds
  if(node<1 || node > somObj$grid$xdim*somObj$grid$ydim) node <- 0
  return(node)
}

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

node2coords <- function(node, somObj){
  y <- ifelse(node%%somObj$grid$xdim==0,
              node%/%somObj$grid$xdim-1,
              node%/%somObj$grid$xdim)
  x <- node%%somObj$grid$xdim - 1
  x[which(x==-1)] <- somObj$grid$xdim - 1
  return(list("x"=x, "y"=y))
}

createCarLink <- function(val) {
    sprintf('<a href="%s" target="_blank">%s</a>', val$url, val$makemodel)
}

save_model <- function(somObj, model_name){
  dir.create('models/', showWarnings = FALSE)
  saveRDS(somObj, file=sprintf('models/%s.rds', model_name))
}

save_feature_df <- function(featuredf, feature_name){
  dir.create('encoded/', showWarnings = FALSE)
  saveRDS(featuredf, file=sprintf('encoded/%s.rds', feature_name))
}

save_pca <- function(pca_obj, pca_name){
  dir.create('encoded/', showWarnings = FALSE)
  saveRDS(pca_obj, file=sprintf('encoded/%s.rds', pca_name))
}

load_pca <- function(pca_name){
  return(readRDS(sprintf('encoded/%s.rds', pca_name)))
}

load_feature_df <- function(feature_name){
  return(readRDS(sprintf('encoded/%s.rds', feature_name)))
}

load_model <- function(model_name){
  return(readRDS(sprintf('models/%s.rds', model_name)))
}
