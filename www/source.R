Text <- "day	CT	TR
16	1	0
17	1	0
18	0	0
19	1	0
20	2	1
21	3	0
22	0	1
23	2	2
24	10	1
25	14	1
26	12	4
27	11	12
28	2	15
29	2	8
30	3	2
31	2	5
32	4	1
33		3
34		3
35		1"
ROUND <- function(x, keep=4){
    x %<>%  format(scientific = F) %>% as.character
    if(grepl("[.]", x)){
        xSplit <- unlist(strsplit(x, "[.]"))
        xSplitBef <- xSplit[1]
        minusYN <- substring(xSplitBef, 1, 1)
        #     minusYN <- minusYN == "-"
        xSplitAft <- xSplit[2]
        xSplitAft <- substring(xSplitAft, 1:nchar(xSplitAft), 1:nchar(xSplitAft))
        
        if(minusYN == "-"){
            xSplitBef <- substring(xSplitBef, 2:nchar(xSplitBef), 2:nchar(xSplitBef))
            lenBef <- length(xSplitBef)
            lenAft <- length(xSplitAft)
            
            if(lenAft > keep){
                all <- c(xSplitBef, xSplitAft[1:(keep + 1)])
                if(as.integer(tail(all,1)) < 5){
                    res1 <- c(xSplitBef, xSplitAft[1:keep])
                    res1 <- paste(c(res1[1:lenBef], ".", res1[(lenBef+1):length(res1)]), collapse="")
                    res1 <- -as.numeric(res1)
                }else{
                    res1 <- c(xSplitBef, xSplitAft[1:keep])
                    res1 <- as.integer(res1)
                    for(i in length(res1):1){
                        if(i == length(res1)){
                            res1[i] <- res1[i] + 1
                        }
                        if(res1[i] != 10){
                            break
                        }else{
                            res1[i - 1] <- res1[i - 1] + 1
                        }
                    }
                    
                    if(res1[1] == 10){
                        res1[-1] <- 0
                        res1 <- c(1,0,res1[-1])
                        res1 <- paste(c(res1[1:(lenBef+1)], ".", res1[(lenBef+2):length(res1)]), collapse="")
                        res1 <- -as.numeric(res1)
                    }else{
                        res1[which(res1 == 10)] <- 0
                        res1 <- paste(c(res1[1:lenBef], ".", res1[(lenBef+1):length(res1)]), collapse="")
                        res1 <- -as.numeric(res1)
                    }
                    
                }
            }else{
                res1 <- as.numeric(x)
            }
        }else{
            xSplitBef <- substring(xSplitBef, 1:nchar(xSplitBef), 1:nchar(xSplitBef))
            lenBef <- length(xSplitBef)
            lenAft <- length(xSplitAft)
            
            if(lenAft > keep){
                all <- c(xSplitBef, xSplitAft[1:(keep + 1)])
                if(as.integer(tail(all,1)) < 5){
                    res1 <- c(xSplitBef, xSplitAft[1:keep])
                    res1 <- paste(c(res1[1:lenBef], ".", res1[(lenBef+1):length(res1)]), collapse="")
                    res1 <- as.numeric(res1)
                }else{
                    res1 <- c(xSplitBef, xSplitAft[1:keep])
                    res1 <- as.integer(res1)
                    for(i in length(res1):1){
                        if(i == length(res1)){
                            res1[i] <- res1[i] + 1
                        }
                        if(res1[i] != 10){
                            break
                        }else{
                            res1[i - 1] <- res1[i - 1] + 1
                        }
                    }
                    
                    if(res1[1] == 10){
                        res1[-1] <- 0
                        res1 <- c(1,0,res1[-1])
                        res1 <- paste(c(res1[1:(lenBef+1)], ".", res1[(lenBef+2):length(res1)]), collapse="")
                        res1 <- as.numeric(res1)
                    }else{
                        res1[which(res1 == 10)] <- 0
                        res1 <- paste(c(res1[1:lenBef], ".", res1[(lenBef+1):length(res1)]), collapse="")
                        res1 <- as.numeric(res1)
                    }
                    
                }
            }else{
                res1 <- as.numeric(x)
            }
        }
    }else{
        res1 <- as.numeric(x)
    }
    return(res1)
}

grid.draw.ggsurvplot <- function(x){
    survminer:::print.ggsurvplot(x, newpage = FALSE)
}
SE <- function (x, na.rm = TRUE) {
    sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}