
plot_grid <- function(df, numcol = 1, titles = c("title1")){
     max_col <- ncol(df)
     if (ncol(df) < 3){ print("not enough data")
     } else{
          max_rows <- ceiling((max_col - 2) / numcol)
          y_max <- max(df[,2:max_col], na.rm = TRUE) * 1.05
          p <- list()
          p[[1]] <- lapply(names(df)[3],                                         # 1st plot
                           function(nm){ ggplot(data = df) +
                                     geom_line(aes (x= .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
                                     geom_line(aes (x= .data[[names(df)[1]]], y = .data[[nm]])) +
                                     labs(title = titles[1]) +
                                     xlab("") +
                                     ylab(titles[2]) +
                                     ylim(0,y_max)
                           })
          j <- 3
          for(i in 4:max_col){
               if((i - 2) <= max_rows){                                             # 1st column
                    p[[i-2]] <- lapply(names(df)[i],
                                       function(nm){
                                            ggplot(data = df) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[nm]])) +
                                                 xlab("") +
                                                 ylab(titles[j]) +
                                                 ylim(0,y_max)
                                       })
                    j <- j + 1
               } else if((((i - 2) - 1) %% max_rows) == 0 ){                        # 1st row
                    p[[i-2]] <- lapply(names(df)[i],
                                       function(nm){
                                            ggplot(data = df) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[nm]])) +
                                                 labs(title = titles[j]) +
                                                 xlab("") +
                                                 ylab("") +
                                                 ylim(0,y_max)
                                       })
                    j <- j + 1
               } else{                                                              # Other plots
                    p[[i-2]] <- lapply(names(df)[i],
                                       function(nm){
                                            ggplot(data = df) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
                                                 geom_line(aes (x= .data[[names(df)[1]]], y = .data[[nm]])) +
                                                 xlab("") +
                                                 ylab("") +
                                                 ylim(0,y_max)
                                       })
               }
          }
          multi_string <- "multiplot("
          for(i in 1:(max_col - 2)){
               multi_string <- paste0(multi_string,"p[[",i,"]], ")
          }
          multi_string <- paste0(multi_string, "cols = ", numcol, ")")
          eval(parse(text = multi_string))
     }
}
