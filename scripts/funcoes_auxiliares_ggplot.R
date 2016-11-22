## funções auziliares GGPLOT

real_format <- function(prefix = "$", suffix = "", largest_with_cents = 100000,
                        ..., big.mark = ".", negative_parens = FALSE) {
  require(reshape)
  require(stringr) 
  
  function(x) {
    x <- round_any(x, 0.01)
    if (max(x, na.rm = TRUE) < largest_with_cents &
        !all(x == floor(x), na.rm = TRUE)) {
      nsmall <- 1L
    } else {
      x <- round_any(x, 1)
      nsmall <- 0L
    }
    str_c("R$ ", format(x, nsmall = nsmall, trim = TRUE, big.mark = ".", 
                      decimal.mark = ",", scientific = FALSE, digits= 1L)) # alternativa 1L
  }
}



theme_tb <- function (base_size = 12, base_family = "Helvetica",
                      legend.position="bottom", legend_size= 10) {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(legend.position = legend.position , 
          legend.margin=unit(-.2,"cm"),
          legend.text=element_text(size=legend_size),
          legend.title = element_blank(), # size=.5
          axis.text = element_text(size = rel(0.8)), axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          legend.key.size = unit(.3, "cm"),
          panel.background = element_rect(fill = "white", colour = NA), panel.border = element_rect(fill = NA,  colour = "grey50"), 
  panel.grid.major = element_line(colour = "white", size = 0.2),
  panel.grid.minor = element_line(colour = "white",  size = 0.5), 
  strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}

