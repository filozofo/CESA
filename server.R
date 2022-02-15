library(shiny)
library(nloptr)
library(survminer)
library(survival)
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(readr)

shinyServer(function(input, output) {
    v <- reactiveValues(data = NULL)
    observeEvent(input$Run, {
        v$data <- read_delim(file=input$file5,delim = '\t') %>% 
            dplyr::mutate(across(everything(), ~replace_na(.x, 0)))
    })

    NCOL <- reactive({
        ncol(v$data)
    })
    file3 <- reactive({
        purrr::map_df(2:NCOL(),~tibble(time=rep(dplyr::pull(v$data,1),dplyr::pull(v$data,.x)),event=1,group=names(v$data)[.x]))
    })
    nms <- reactive({
        names(v$data)[-1] %>% as.vector()
    })
 
    table1 <- reactive({
        FormatPv <- function(i){
            c <- file3() %>% dplyr::filter(group==nms()[1]|group==nms()[i]) %>% 
                surv_fit(Surv(time, event)~group, data = .) %>% 
                surv_pvalue(.) %>% dplyr::pull(2) %>% ROUND(3)
            if(c<0.0001){c <- '< 0.0001'}
            return(as.character(c))
        }
        ref <- file3()%>% dplyr::filter(group==names(v$data)[2]) %>% dplyr::pull(1) %>%  mean()
        d <- file3() %>% dplyr::group_by(group) %>% mutate(group=factor(group,levels=nms())) %>% 
            summarise(meanse=paste0(ROUND(mean(time),3),'±',ROUND(SE(time),3)),
                      PLC=ROUND((mean(time)/ref-1)*100,3),N=n())%>% 
            dplyr::mutate(pv=c('',map_chr(2:(NCOL()-1),FormatPv)),.before='PLC') 
        d[[4]][1] <- ''
        d
    })
    plot1 <- reactive({
        p1 <- ggsurvplot(surv_fit(Surv(time, event)~group, data = file3()),
                         axes.offset = F,
                         xlim = c(0, ceiling(max(dplyr::pull(file3(),1))/5)*5+1),
                         xlab="Days",
                         ylab="Fraction survival",
                         break.x.by = 5,# 设置x轴刻度间距
                         break.y.by = 0.2,# 设置y轴刻度间距
                         #legend = 'none',
                         size = 1.5, #line size
                         palette = "lancet",# line colours
                         pval = ifelse(NCOL()>3,F,T))
        
        p1$plot <- p1$plot %+%
            theme(
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.x = element_text(size = 30),
                axis.title.y = element_text(size = 30),
                plot.margin = margin(0.5,0.5,0.5,0.5,unit = "cm"),
                axis.line=element_line(size=1.5),
                #axis.ticks = element_text(size = 1),
                axis.ticks.length=unit(.25, "cm"))
        p1$plot
    })
    #main output
    output$table1 <- renderTable({
        if (is.null(v$data)) return()
        table1()},align='c')
    output$plot1 <- renderPlot({
        if (is.null(v$data)) return()
        plot1()})
    # Download
    output$downspss <- downloadHandler(
        filename = function() {
            paste0(nms()[1], ".spss.txt")
        },
        content = function(file) {
            write_delim(file3(), file,delim='\t')
        }
    )
    output$downtable <- downloadHandler(
        filename = function() {
            paste0(nms()[1], ".txt")
        },
        content = function(file) {
            write_delim(table1(), file,delim='\t')
        }
    )
    output$downplot <- downloadHandler(
        filename = function() {
            paste0(nms()[1], ".pdf")
        },
        content = function(file) {
            ggsave(file, plot=plot1())
        }
    )
})