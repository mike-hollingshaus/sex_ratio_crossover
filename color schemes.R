# Color Schemes for plotting
require(ggplot2)

capFirstLetter <- function(x){
  s <- strsplit(x, ' ')[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep='', collapse=' ')
}

cols_setup <- function()
{
  rgb_255 <- function(r,g,b){
    rgb(red=r, green=g, blue=b, max=255)
  }
  
  cols_primary  <- list(
    rgb_255(204, 0  , 0  ), # Red
    rgb_255(0  , 0  , 0  ), # Masters Black
    rgb_255(128, 128, 128), # Granite Grey
    rgb_255(171, 168, 158), # Limestone Grey
    rgb_255(200, 198, 191), # Nickel
    rgb_255(227, 226, 223)  # White
  )
  names(cols_primary) <- c('Red', 'Masters Black', 'Granite Grey', 'Limestone Gray', 'Nickel', 'White')
  
  cols_secondary <- list(
    rgb_255(239, 228, 179), # Wheat
    rgb_255(202, 182, 136), # Fawn
    rgb_255(169, 128, 82 )  # Wood
    # Canyon Silver -- no RGB or HEX available
  )
  names(cols_secondary) <- c('Wheat', 'Fawn', 'Wood')
  
  cols_tertiary <- list(
    rgb_255(114, 24 , 18 ), # Barn Door Red
    rgb_255(246, 154, 29 ), # Amber Yellow
    rgb_255(56 , 80 , 162), # Lagoon Blue
    rgb_255(94 , 172, 69 ), # Amazon Green
    rgb_255(115, 189, 233), # Azure Blue
    rgb_255(206, 232, 241)  # Diamond
  )
  names(cols_tertiary) <- c('Barn Door Red', 'Amber Yellow', 'Lagoon Blue', 'Amazon Green', 'Azure Blue', 'Diamond')

  list(primary=cols_primary, secondary=cols_secondary, tertiary=cols_tertiary)
}
sr_cols <- cols_setup()
colorTypes <- names(sr_cols)

validateColorType <- function(type){
  if (!(type %in% colorTypes)){
    stop(paste('Color type must be one of', paste(colorTypes, collapse = ', ') , sep=': '))
  }
}
cols_vector <- function(type){
  validateColorType(type)
  v <- unlist(sr_cols[type]) 
  names(v) <- unlist(strsplit(names(v), '.', fixed=TRUE))[c(FALSE, TRUE)]
  v
}
cols_vector('tertiary')

pie_palette <- function(type){
  validateColorType(type)
  cols_vector <- cols_vector(type)
  vlength <- length(cols_vector)
  df <- data.frame(color=names(cols_vector), value=rep(1, vlength), hex=cols_vector)
  bp <- ggplot(df, aes(x='', y=value, fill=color)) + geom_bar(width = 1, stat = "identity") # Barplot
  bp + coord_polar('y', start=0) + labs(title=capFirstLetter(type)) + scale_fill_manual(values=cols_vector) # Pie chart
}
# pie_palette('primary')
# pie_palette('secondary')
# pie_palette('tertiary')


