library(shinyWidgets)
library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(shinyjs)

轨道s = c("NA","上","下")
cases = c("NA",">0","<0")

variables = c("轨道上","轨道下","轨道下","D1","D2",
              "W1","W2","M1","M2","S1","S2","DD","WD",
              "MD","SD","收盘","SC","MC","WC","DC","B信号","B候补","总金额")

c3 = "-3,3"
c100 = "0,100"
c1_10000 = "1,10000"
c10000 = "-10000,10000"


ui <- dashboardPage(
    dashboardHeader(title = "坤钰数据量化"),
    dashboardSidebar(
        fileInput("file", "Excel文件",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".xlsx")),
        sidebarMenu(
            selectInput("guidao", "轨道", c("NA","上","中","下")),
            actionButton("reset_input", "重置条件",width = 200),
            
            menuItem("勾选栏", tabName = "check", icon = NULL,
                     shinyjs::useShinyjs(),
                     id = "form1",
                     checkboxInput("b_sig","B信号"),
                     checkboxInput("b_sig2","B候补"),
                     checkboxInput("w","W"),
                     checkboxInput("m","M"),
                     checkboxInput("s","S"),
                     checkboxInput("wms11","W5>W10"),
                     checkboxInput("wms12","M5>M10"),
                     checkboxInput("wms13","S5>S10"),
                     
                     checkboxInput("wms21","W1>W2"),
                     checkboxInput("wms31","W1<W2"),
                     
                     checkboxInput("wms22","M1>M2"),
                     checkboxInput("wms32","M1<M2"),
                     
                     checkboxInput("wms23","S1>S2"),
                     checkboxInput("wms33","S1<S2"),
                     
                     checkboxInput("y1_2","Y1>Y2"),
                     
                     checkboxInput("c_bigger_than_and_eqaulTo_m5","C >= M5"),
                     checkboxInput("c_less_than_m10","C < M10"),
                     
                     checkboxInput("cd_bigger","C>=W10"),
                     checkboxInput("cd_smaller","C<W10"),
                     
                     checkboxInput("rise_9pct","ETF筛选")
            ),
            menuItem("范围选择栏", tabName = "range", icon = NULL,
                     shinyjs::useShinyjs(),
                     id = "form2",
                     textInput("w60","红1",value="0,99"),
                     textInput("m60","黄1",value="0,99"),
                     textInput("w233","绿",value="0,99"),
                     textInput("yellow","黄",value="0,99"),
                     textInput("red","红线",value="0,99"),
                     textInput("blue","青线",value="0,99"),
                     
                     textInput("d1","D1",value="-3,3"),
                     #Y1 Y2 (条件设置与D1 D2 一致)  YD（与DD WD MD SD一致）  YC与sc  mc wc 条件一致
                     textInput("d2s","D2",value="-3,3"),
                     
                     textInput("w1s","W1",value="-3,3"),
                     textInput("w2s","W2",value="-3,3"),
                     
                     
                     textInput("m1s", "M1", value="-3,3"),
                     textInput("m2s", "M2", value="-3,3"),
                     textInput("s1s", "S1", value="-3,3"),
                     textInput("s2s", "S2", value="-3,3"),
                     textInput("y1s", "Y1", value="-3,3"),
                     textInput("y2s", "Y2", value="-3,3"),
                     
                     textInput("dd","DD",value = c100),
                     textInput("wd","WD",value = c100),
                     textInput("md","MD",value = c100),
                     textInput("sd","SD",value = c100),
                     textInput("yd", "YD", value = c100),
                     
                     textInput("c","C",value = c1_10000),
                     
                     textInput("yc", "YC", value = c10000),
                     textInput("sc","SC",value = c10000),
                     textInput("mc","MC",value = c10000),
                     textInput("wc","WC",value = c10000),
                     textInput("dc","DC",value = c10000)
            ),
            menuItem("组合", tabName = "combination", icon = NULL,
                     checkboxInput("s_duck","季小鸭"),
                     checkboxInput("w_duck","周小鸭"),
                     checkboxInput("m_duck","月小鸭"),
                     checkboxInput("d_sh","日守恒"),
                     checkboxInput("w_sh","周守恒"),
                     checkboxInput("cs","测试"),
                     checkboxInput("cs2","测试2")
            ),
            menuItem("横盘精选专用", tabName = "hengpan", icon = NULL,
                     checkboxInput("liushun","六顺"),
                     checkboxInput("ryb","红黄青"),
                     checkboxInput("rybg","红黄青绿"),
                     checkboxInput("yr","黄红"),
                     checkboxInput("yb","黄青"),
                     checkboxInput("rb","红青")
            )
        )
    ),
    
    
    dashboardBody(
        tabsetPanel(
            tabPanel(title = "条件筛选",
                     downloadButton("dl", "下载筛选出的股票"),
                     textOutput("text_1"),
                     DT::dataTableOutput("dat_table")
            ),
            tabPanel(title = "股票对比",
                     textInput("typed_in_stocks","个股代码（xxxxxx,xxxxxx,....）"),
                     DT::dataTableOutput("stocks_table"))
        )
    )
)

server <- function(input, output,session){
    
    data <- eventReactive(input$file, {
        read_excel(input$file$datapath,skip = 1) %>% 
            mutate(
                涨幅 = `涨幅%`
            ) %>%
            filter(
                !is.na(涨幅)
            ) 
    })
    
    range_parse <- function(x){
        x = str_split(x,",")
        min = as.double(x[[1]][[1]])
        max = as.double(x[[1]][[2]])
        return(c(min,max))
    }
    
    rail <- reactive(input$guidao)
    d1 <-  reactive(range_parse(input$d1))
    d2 <-  reactive(range_parse(input$d2s))
    w1 <-  reactive(range_parse(input$w1s))
    w2 <-  reactive(range_parse(input$w2s))
    m1 <-  reactive(range_parse(input$m1s))
    m2 <-  reactive(range_parse(input$m2s))
    s1 <-  reactive(range_parse(input$s1s))
    s2 <-  reactive(range_parse(input$s2s))
    dd <- reactive(input$dd)
    
    GD_shang <- reactive(data()$轨道上%>% na.omit() %>% sum())
    GD_zhong <- reactive(data()$轨道中%>% na.omit() %>% sum())
    GD_xia <- reactive(data()$轨道下%>% na.omit() %>% sum())
    GD_sum  <- reactive(GD_shang()+GD_zhong()+GD_xia())
    rate_GD_shang <- reactive(round(GD_shang()/GD_sum(),3))
    rate_GD_xia <- reactive(round(GD_xia()/GD_sum(),3))
    num_dd_1q <- reactive(data() %>% filter(DD > 0 & DD < 25) %>% nrow())
    num_dd_2q <- reactive(data() %>% filter(DD >= 75 & DD <= 100)%>%nrow())
    
    
    
    reset <- {
        shinyjs::reset("form1")
    }
    
    
    
    observeEvent(input$reset_input, {
        reset()
    })
    
    
    output$dl <- downloadHandler(
        filename = function(){ 
            paste("筛选出的个股",".txt",sep="") 
        }, 
        content = function(file) {
            write_tsv(filteredTable()[1], file,col_names = FALSE)
        }) 
    
    output$text_1<- renderText({
        paste("| 强势指数: ",rate_GD_shang(),
              " | --- | 弱势指数: ",rate_GD_xia(),
              " | --- | 上: ",GD_shang(),
              " | --- | 中: ",GD_zhong(),
              " | --- | 下: ",GD_xia(),
              " |")
    })
    
    
    
    
    filteredTable <- reactive({
        
        if(rail() == "上"){
            dat = data() %>% filter(轨道上 == 1)
        }else if(rail() == "下"){
            dat = data() %>% filter(轨道下 == 1)
        }else if(rail() == "中"){
            dat = data() %>% filter(轨道中 == 1)
        }else{
            dat = data()
        }
        
        if(input$s_duck){
            dat = dat %>% 
                filter( M60 ==1  ) %>%
                filter( S5 > S10 ) %>%
                filter((W5>W10) | (W5 < W10 & 收盘 > max(W5,W10))) %>%
                filter(M1>-0.004 & M2>=-0.004)%>% 
                filter(W1>-0.004 & W2>=-0.002)%>% 
                filter(D1>-0.01 & D2>=-0.01 )%>% 
                filter(收盘 < 50) 
        }
        if(input$w_duck){
            dat = dat %>% 
                filter( D233 ==1  ) %>%
                filter(M1 > M2 & M5 > M10 ) %>%
                filter(W5>W10 ) %>%
                filter(W1>-0.01 & W2>=-0.025 & (W2<0.05 | W1<0.08) )%>% 
                filter(D1>-0.0015 & D2>=-0.0015)%>% 
                filter(收盘<40 & 收盘 >= W10)
        }
        if(input$m_duck){
            dat = dat %>% 
                filter( S60 ==1  ) %>%
                filter( M5 > M10 ) %>%
                filter((W5>W10) | (W5 < W10 & 收盘 > max(W5,W10))) %>%
                filter(W1>-0.004 & W2>=-0.002)%>% 
                filter((D1>-0.01 & D2>=-0.01) | (D1>D2) )%>% 
                filter(收盘 < 50) 
        }
        if(input$d_sh){
            dat = dat %>% 
                filter(W5>W10 & W1>W2 & D1<0.01 & D2>0 & M1>M2 & M5>M10 & D1<D2 ) %>%
                filter(轨道上 == 1 | 轨道中 ==1)
            
        }
        if(input$w_sh){
            dat = dat %>% 
                filter(W5>W10 & W1<W2 & W2>-0.01 &  W1<0.01 & W1>-0.015 & M1>M2 & M5>M10)
        }
        if(input$cs){
            dat = dat %>% 
                filter(M5>M10 & M1>M2 & W1<W2 & D1>0 & D2<0.002) 
        }
        if(input$cs2){
            dat = dat %>% 
                filter(M5>M10 & M1>M2 & W1>W2 & D1>0 & D2<0.0021 & W1<0.0015 & W2<0.0015) 
        }
        if(input$cd_bigger){
            dat = dat%>%filter(收盘 >= W10)
        }
        
        if(input$cd_smaller){
            dat = dat%>%filter(收盘 < W10)
        }
        
        if(input$c_bigger_than_and_eqaulTo_m5){
            dat = dat%>%filter(收盘 >= M5)
        }
        
        if(input$c_less_than_m10){
            dat = dat%>%filter(收盘 < M10)
        }
        
        
        
        if(input$rise_9pct){
            dat = data() %>% filter( 总金额 > 10000000) %>% filter(收盘 < 20 & 收盘>0) %>%filter(轨道上 == 1)
        }
        
        
        #********************************************范围*******************************************
        #*
        #*
        dat = dat %>% filter((W60>= range_parse(input$w60)[1]& W60<=range_parse(input$w60)[2])|is.na(W60))
        dat = dat %>% filter((M60>= range_parse(input$m60)[1]& M60<=range_parse(input$m60)[2])|is.na(M60))
        dat = dat %>% filter((W233>= range_parse(input$w233)[1]& W233<=range_parse(input$w233)[2])|is.na(W233))
        dat = dat %>% filter((黄 >= range_parse(input$yellow)[1]& 黄 <= range_parse(input$yellow)[2])|is.na(黄))
        dat = dat %>% filter((红线 >= range_parse(input$red)[1]& 红线 <= range_parse(input$red)[2])|is.na(红线))
        dat = dat %>% filter((青线>= range_parse(input$blue)[1]& 青线 <= range_parse(input$blue)[2])|is.na(青线))
        
        
        dat = dat %>% filter((D1 >= d1()[1]& D1 <= d1()[2])|is.na(D1))
        dat = dat %>% filter((D2 >= d2()[1]& D2 < d2()[2])|is.na(D2))
        dat = dat %>% filter((Y1 >= range_parse(input$y1s)[1]& Y1<=range_parse(input$y1s)[2])|is.na(Y1))
        dat = dat %>% filter((Y2 >= range_parse(input$y2s)[1]& Y2<=range_parse(input$y2s)[2])|is.na(Y2))
        
        dat = dat %>% filter((W1>= w1()[1]& W1<=w1()[2])|is.na(W1))
        dat = dat %>% filter((W2>= w2()[1]& W2<=w2()[2])|is.na(W2))
        dat = dat %>% filter((M1>= m1()[1]& M1<=m1()[2])|is.na(M1))
        dat = dat %>% filter((M2>= m2()[1]& M2<=m2()[2])|is.na(M2))
        dat = dat %>% filter((S1>= s1()[1]& S1<=s1()[2])|is.na(S1))
        dat = dat %>% filter((S2>= s2()[1]& S2<=s2()[2])|is.na(M1))
        
        dat = dat %>% filter((DD>= range_parse(input$dd)[1]& DD<=range_parse(input$dd)[2])|is.na(DD))
        dat = dat %>% filter((YD>= range_parse(input$yd)[1]& YD<=range_parse(input$yd)[2])|is.na(YD))
        dat = dat %>% filter((WD>= range_parse(input$wd)[1]& WD<=range_parse(input$wd)[2])|is.na(WD))
        dat = dat %>% filter((MD>= range_parse(input$md)[1]& MD<=range_parse(input$md)[2])|is.na(MD))
        dat = dat %>% filter((SD>= range_parse(input$sd)[1]& SD<=range_parse(input$sd)[2])|is.na(SD))
        dat = dat %>% filter((收盘>= range_parse(input$c)[1]& 收盘<=range_parse(input$c)[2])|is.na(收盘))
        dat = dat %>% filter((SC>= range_parse(input$sc)[1]& SC<=range_parse(input$sc)[2])|is.na(SC))
        dat = dat %>% filter((MC>= range_parse(input$mc)[1]& MC<=range_parse(input$mc)[2])|is.na(MC))
        dat = dat %>% filter((WC>= range_parse(input$wc)[1]& WC<=range_parse(input$wc)[2])|is.na(WC))
        dat = dat %>% filter((DC>= range_parse(input$dc)[1]& DC<=range_parse(input$dc)[2])|is.na(DC))
        dat = dat %>% filter((YC>= range_parse(input$yc)[1]& YC<=range_parse(input$yc)[2])|is.na(YC))
        
        if(input$b_sig == TRUE){
            dat = dat %>% filter(B信号==1)
        }
        
        if(input$b_sig2 == TRUE){
            dat = dat %>% filter(B候补==1)
        }
        
        '
    if(input$between_rails_A == TRUE){
      dat = dat %>% filter(B信号==1)
    }
    
    
    if(input$between_rails_B == TRUE){
      dat = dat %>% filter(B信号==1)
    }
    '
        
        if(input$y1_2){
            dat = dat %>% filter(Y1 > Y2) 
        }
        #收盘价>=W10 且 收盘价<W5  且 W1>W2
        if(input$w == TRUE){
            dat = dat %>% filter(收盘>=W10) %>% filter(W1>W2 )%>% filter(W5 > W10)
        }
        
        #收盘价>=M10 且 收盘价<M5 且M1>M2
        if(input$m == TRUE){
            dat = dat %>% filter(收盘 >=M10) %>% filter(M1>M2) %>% filter(M5 > M10) 
        }
        
        #收盘价>=S10 且 收盘价<S5  且S1>S2
        if(input$s == TRUE){
            dat = dat %>% filter(收盘 >= S10)%>% filter(S1>S2) %>% filter(S5>S10)
        }
        
        if(input$wms11){
            dat = dat %>% filter(W5 > W10)
        }
        if(input$wms12){
            dat = dat %>% filter(M5 > M10) 
        }
        if(input$wms13){
            dat = dat %>% filter(S5>S10)
        }
        
        
        
        if(input$wms21){
            dat = dat %>% filter(W1 > W2)
        }
        if(input$wms22){
            dat = dat %>% filter(M1 > M2) 
        }
        if(input$wms23){
            dat = dat %>% filter(S1>S2)
        }
        
        
        if(input$wms31){
            dat = dat %>% filter(W1 < W2) 
        }
        if(input$wms32){
            dat = dat %>% filter(M1 < M2) 
        }
        if(input$wms33){
            dat = dat %>% filter(S1<S2)
        }
        
        
        if(input$ryb){
            dat = dat %>%filter(黄 >= 1 & 红线>=1 & 青线>=1)
        }
        
        if(input$rybg){
            dat = dat %>%filter(黄 >= 1 & 红线>=1 & 青线>=1 & W233>=1)
        }
        
        if(input$yr){
            dat = dat %>%filter(黄 >= 1 & 红线>=1 & 青线<1)
        }
        
        if(input$yb){
            dat = dat %>%filter(黄 >= 1 & 红线<1 & 青线>=1)
        }
        
        if(input$rb){
            dat = dat %>%filter(黄 < 1 & 红线>=1 & 青线>=1)
        }
        
        if(input$liushun){
            dat = dat %>%filter(W60>=1&M60>=1&W233>=1&黄>=1&红线>=1&青线>=1)
        }
        
        
        
        dat = subset(dat, select = -c(D233,W5,W10,W60,W233,M5,M10,M60,
                                      S5,S10,S60,轨道上,轨道中,轨道下,红线,青线,
                                      B信号,B候补,涨幅))
    })
    
    
    output$dat_table = DT::renderDataTable({
        DT::datatable(filteredTable())
    })
    
    
    
    
    #股票对比
    output$stocks_table = DT::renderDataTable({
        if(input$typed_in_stocks != ""){
            stocks <- unlist(strsplit(input$typed_in_stocks,","))
            dat <- data() %>% filter(代码 %in% stocks)
            DT::datatable(dat)
        }
    })
}

shinyApp(ui, server)