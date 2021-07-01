library(shinydashboard)
library(shiny)
library(DT)
library(wordcloud2)
library(wordcloud)

# age0 <- input$age0 # 預計的工作年齡
# age1 <- input$age1 # 退休的第一段
# age2 <- input$age2 # 退休的第二段
# age3 <- input$age3 # 退休的第三段
# needMoney <- input$needMoney # 退休後需要的金額 第一段
# needMoney2 <- input$needMoney2 # 退休後需要的金額 第二段
# needMoney3 <- input$needMoney3 # 退休後需要的金額 第三段
# mx <- input$firstMoney # 一開始儲存的金額
# changeyear <- input$changeyear # 每隔多少年加薪

### use function ####
Sn <- function(i,n){
    if(n >=  0){
        ((1+i)^n-1)/i
    }else{
        0   
    }
    
}
An <- function(i,n){
    v <- 1/(1+i)
    return((1-v^n)/i)
}

## 年繳公式
ChangeFunction_Annual <-  function(Sn,An,mx,mi,r,age0,age1,age2,age3,needMoney,needMoney2,needMoney3,changeyear){
    
    totalyear <- (as.numeric(age0[[2]])-as.numeric(age0[[1]]))
    
    v <- 1/(1+mi)
    Output <- (mx*Sn(i = mi,n = totalyear)+
                   ## 如果要加上結婚，在第一次調薪跟第二次調薪的部分，必須取消。
                   r*Sn(i = mi,n = (totalyear-changeyear*1))+ # 第一次加薪 30
                   r*Sn(i = mi,n = (totalyear-changeyear*2))+ # 第二次加薪 35
                   r*Sn(i = mi,n = (totalyear-changeyear*3))+ # 第三次加薪 40
                   r*Sn(i = mi,n = (totalyear-changeyear*4))+ # 第四次加薪 45
                   r*Sn(i = mi,n = (totalyear-changeyear*5))+ # 第五次加薪 50
                   r*Sn(i = mi,n = (totalyear-changeyear*6))+
                   r*Sn(i = mi,n = (totalyear-changeyear*7))+
                   r*Sn(i = mi,n = (totalyear-changeyear*8))
               )-( # 第六是加薪 55
                       (needMoney)*An(i = mi,n = (age3-as.numeric(age0[[2]])))+
                           (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age1))+
                           (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age2)))
    return(Output)
}
ChangeFunction_Annual_2 <-  function(Sn,An,mx,mi,r,age0,age1,age2,age3,needMoney,needMoney2,needMoney3,changeyear){
    
    totalyear <- (as.numeric(age0[[2]])-as.numeric(age0[[1]]))
    
    v <- 1/(1+mi)
    Output <- (mx*Sn(i = mi,n = totalyear)+
                   ## 如果要加上結婚，在第一次調薪跟第二次調薪的部分，必須取消。
                   #r*Sn(i = mi,n = (totalyear-changeyear*1))+ # 第一次加薪 30
                   #r*Sn(i = mi,n = (totalyear-changeyear*2))+ # 第二次加薪 35
                   r*Sn(i = mi,n = (totalyear-changeyear*3))+ # 第三次加薪 40
                   r*Sn(i = mi,n = (totalyear-changeyear*4))+ # 第四次加薪 45
                   r*Sn(i = mi,n = (totalyear-changeyear*5))+ # 第五次加薪 50
                   r*Sn(i = mi,n = (totalyear-changeyear*6))+
                   r*Sn(i = mi,n = (totalyear-changeyear*7))+
                   r*Sn(i = mi,n = (totalyear-changeyear*8))
               )-( # 第六是加薪 55
                       (needMoney)*An(i = mi,n = (age3-as.numeric(age0[[2]])))+
                           (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age1))+
                           (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age2)))
    return(Output)
}
## 月繳公式
ChangeFunction_Month <-  function(Sn,An,mx,mi,r,age0,age1,age2,age3,needMoney,needMoney2,needMoney3,changeyear){
    
    totalyear <- (as.numeric(age0[[2]])-as.numeric(age0[[1]]))
    m <- 12
    v <- 1/(1+mi)
    Output <- (mx*Sn(i = mi,n = totalyear*m)+
                   r*Sn(i = mi,n = (totalyear-changeyear*1)*m)+ # 第一次加薪 30
                   r*Sn(i = mi,n = (totalyear-changeyear*2)*m)+ # 第二次加薪 35
                   r*Sn(i = mi,n = (totalyear-changeyear*3)*m)+ # 第三次加薪 40
                   r*Sn(i = mi,n = (totalyear-changeyear*4)*m)+ # 第四次加薪 45
                   r*Sn(i = mi,n = (totalyear-changeyear*5)*m)+ # 第五次加薪 50
                   r*Sn(i = mi,n = (totalyear-changeyear*6)*m))-( # 第六是加薪 55
                       (needMoney)*An(i = mi,n = (age3-as.numeric(age0[[2]]))*m)+
                           (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age1)*m)+
                           (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age2)*m))
    return(Output)
}
ChangeFunction_Month_2 <-  function(Sn,An,mx,mi,r,age0,age1,age2,age3,needMoney,needMoney2,needMoney3,changeyear){
    
    totalyear <- (as.numeric(age0[[2]])-as.numeric(age0[[1]]))
    m <- 12
    v <- 1/(1+mi)
    Output <- (mx*Sn(i = mi,n = totalyear*m)+
                   #r*Sn(i = mi,n = (totalyear-changeyear*1)*m)+ # 第一次加薪 30
                   #r*Sn(i = mi,n = (totalyear-changeyear*2)*m)+ # 第二次加薪 35
                   r*Sn(i = mi,n = (totalyear-changeyear*3)*m)+ # 第三次加薪 40
                   r*Sn(i = mi,n = (totalyear-changeyear*4)*m)+ # 第四次加薪 45
                   r*Sn(i = mi,n = (totalyear-changeyear*5)*m)+ # 第五次加薪 50
                   r*Sn(i = mi,n = (totalyear-changeyear*6)*m))-( # 第六是加薪 55
                       (needMoney)*An(i = mi,n = (age3-as.numeric(age0[[2]]))*m)+
                           (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age1)*m)+
                           (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = mi,n = (age3-age2)*m))
    return(Output)
}
### use function ####


### ui ####
ui <- dashboardPage(
    dashboardHeader(title = "退休好生活"), # Header
    dashboardSidebar(width = 300,
                     disable = FALSE,## 可以把邊邊關起來
        
        sidebarMenu(   
            menuItem(text = "簡述", icon = icon("th"), tabName = "Table1"),
            menuItem(text = "詳細表格", icon = icon("th"), tabName = "Tables1"),
            menuItem(text = "如果想要在30歲結婚生小孩 簡述", icon = icon("th"), tabName = "Table2"),
            menuItem(text = "如果想要在30歲結婚生小孩 詳細表格", icon = icon("th"), tabName = "Tables2"),
                    #  "選三個想要比較的利率,請以百分比為單位。",
                      
            menuItem("利率", 
                     numericInput("rate1",label = "第一利率",value = 0.8),
                     numericInput("rate2",label = "第二利率",value = 1.0),
                     numericInput("rate3",label = "第三利率",value = 1.2)
            ),         
                     # sliderInput("age0",label = "工作到幾歲退休",min = 0,max = 50,value = 25),
            menuItem("情境設定", 
                     sliderInput("age0", "工作年齡",min = 1, max = 100,value = c(25,60)),
                     numericInput("firstMoney",label = "起始金額",value = 5000),
                     sliderInput("age1",label = "第一段退休年齡",min = 60,max = 100,value = 70),
                     numericInput("needMoney",label = "第一段退休年齡想要領多少",value = 20000),
                     sliderInput("age2",label = "第二段退休年齡",min = 60,max = 100,value = 70),
                     numericInput("needMoney2",label = "第一段退休年齡想要領多少",value = 30000),
                     sliderInput("age3",label = "第三段退休年齡",min = 60,max = 100,value = 70),
                     numericInput("needMoney3",label = "第一段退休年齡想要領多少",value = 40000)
            ) 
                     
        ) # sidebarMenu
        
        
        
        
    ), # dashboardSidebar
    dashboardBody(
        # h2("Widgets tab content"),
        tabItems(
            ## 分頁第一頁
            tabItem(tabName = "Table1",
                    fluidRow(
                        box(title = "表格",width = 12,
                            DTOutput("ChDT1"))
                    ),# fluidRow
                    
                    column(
                        width = 6,
                        fluidRow(
                            box(title = "年累積已繳金額",width = 12,
                                plotOutput("plot1",height = 350))
                        )
                    ),# column 
                    column(
                        width = 6,
                        fluidRow(
                            box(title = "月累積已繳金額",width = 12,
                                plotOutput("plot2",height = 350))
                        )
                    ) # column 第二個欄
                    
            ),# tabItem
            ## 分頁第二頁
            tabItem(tabName = "Tables1",
                    column(
                        width = 6,
                        box(title = "年繳表格",width = 13,
                            DTOutput("DT1"))
                    ),
                    column(
                        width = 6,
                        box(title = "月繳表格",width = 13,
                            DTOutput("DT2"))
                    )
                    
            ),# tabItem
            ## 分頁第三頁
            tabItem(tabName = "Table2",
                    fluidRow(
                        box(title = "表格",width = 12,
                            DTOutput("ChDT1_2"))
                    ),# fluidRow
                    
                    column(
                        width = 6,
                        fluidRow(
                            box(title = "年累積已繳金額",width = 12,
                                plotOutput("plot1_2",height = 350))
                        )
                    ),# column 
                    column(
                        width = 6,
                        fluidRow(
                            box(title = "月累積已繳金額",width = 12,
                                plotOutput("plot2_2",height = 350))
                        )
                    ) # column 第二個欄
        ), #tabItems
        
        ## 分頁第四頁
        tabItem(tabName = "Tables2",
                column(
                    width = 6,
                    box(title = "年繳表格",width = 13,
                        DTOutput("DT1_2"))
                ),
                column(
                    width = 6,
                    box(title = "月繳表格",width = 13,
                        DTOutput("DT2_2"))
                )
                
        )
        )
        
    )# Body
) #dashboardPage
### ui ####

### server ####
server <- function(input, output,session) { 
    
    observe({
        val <-  input$age0
        
        updateSliderInput(session, "age1",
                          min = as.numeric(val[[2]]), max = 100,value = as.numeric(val[[2]])+10)
    })
    observe({
        val <-  input$age1
        
        updateSliderInput(session, "age2",
                          min = as.numeric(val), max = 100,value = as.numeric(val)+10)
    })
    observe({
        val <-  input$age2
        
        updateSliderInput(session, "age3",
                          min = as.numeric(val), max = 100,value = as.numeric(val)+5)
    })
    ## 年繳的表格
    DT_1 <- function(){
        
        x <- input$firstMoney*12
        i1 <- input$rate1
        i2 <- input$rate2
        i3 <- input$rate3
        
        ## 變動金額的年繳費
        r1 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i1/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        r2 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i2/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        r3 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i3/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        age <- seq(25,59,1)
        DT <- data.frame("age" = age,
                         "salary" = numeric(length(age)),     
                         "PayMoney_0.08" = numeric(length(age)),
                         "PayMoney_0.10" = numeric(length(age)),
                         "PayMoney_0.12" = numeric(length(age)),
                         "TotalPayMoney_0.08"= numeric(length(age)),
                         "TotalPayMoney_0.10"= numeric(length(age)),
                         "TotalPayMoney_0.12"= numeric(length(age)))
        
        DT$salary <- rep((seq(from = 30000,to = 1000000,by = 5000)[1:7])*12,each = 5)
        
        Collect1 <- Collect2 <- Collect3 <- c()
        a1 <- a2 <- a3 <- x
        Collect1 <-  c(Collect1,rep(a1,5))
        Collect2 <-  c(Collect2,rep(a2,5))
        Collect3 <-  c(Collect3,rep(a3,5))
        
        for(i in 2:7){
            a1 <- a1+r1 # 0.08
            a2 <- a2+r2 # 0.1
            a3 <- a3+r3 # 0.12
            # rep(a/5,5)
            Collect1 <- c(Collect1,rep(a1,5))
            Collect2 <- c(Collect2,rep(a2,5))
            Collect3 <- c(Collect3,rep(a3,5))
        }
        
        DT$PayMoney_0.08 <- ceiling(Collect1)
        DT$PayMoney_0.10 <- ceiling(Collect2)
        DT$PayMoney_0.12 <- ceiling(Collect3)
        
        C1 <- C2 <- C3 <- c()
        a1 <- a2 <- a3 <- 0
        for(i in 1:dim(DT)[1]){
            a1 <- a1+DT$PayMoney_0.08[i]
            a2 <- a2+DT$PayMoney_0.10[i]
            a3 <- a3+DT$PayMoney_0.12[i]
            C1 <- c(C1,a1)
            C2 <- c(C2,a2)
            C3 <- c(C3,a3)
            
        }
        DT$TotalPayMoney_0.08 <- C1
        DT$TotalPayMoney_0.10 <- C2
        DT$TotalPayMoney_0.12 <- C3
        names(DT) <- c("年齡","年薪資",paste0("應付金額i = ",i1,"%"),paste0("應付金額i = ",i2,"%"),paste0("應付金額i = ",i3,"%"),
                       paste0("已繳總金額i = ",i1,"%"),paste0("已繳總金額i = ",i2,"%"),paste0("已繳總金額i = ",i3,"%"))
        return(DT)
    }
    ## 月繳的表格
    DT_2 <- function(){
        x <- input$firstMoney
        # x <- 5000
        R <- input$needMoney
        i1 <- input$rate1
        i2 <- input$rate2
        i3 <- input$rate3
        ## 變動金額的年繳費
        # ChangeFunction_Month
        r1 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                      mx = x,mi = i1/100/12,age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney, needMoney2 = input$needMoney2, needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r2 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                      mx = x,mi = i2/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                      needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r3 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                      mx = x,mi = i3/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                      needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        age <- rep(seq(25,59,1),each = 12)
        DT <- data.frame("age" = age,
                         "month" = rep(c(1:12),35),
                         "salary" = numeric(length(age)),     
                         "PayMoney_0.08" = numeric(length(age)),
                         "PayMoney_0.10" = numeric(length(age)),
                         "PayMoney_0.12" = numeric(length(age)),
                         "TotalPayMoney_0.08"= numeric(length(age)),
                         "TotalPayMoney_0.10"= numeric(length(age)),
                         "TotalPayMoney_0.12"= numeric(length(age)))
        DT$salary <- rep((seq(from = 30000,to = 1000000,by = 5000)[1:7]),each = 5*12)
        
        Collect1 <- Collect2 <- Collect3 <- c()
        a1 <- a2 <- a3 <- x
        Collect1 <-  c(Collect1,rep(a1,5*12))
        Collect2 <-  c(Collect2,rep(a2,5*12))
        Collect3 <-  c(Collect3,rep(a3,5*12))
        
        for(i in 2:7){
            a1 <- a1+r1 # 0.08
            a2 <- a2+r2 # 0.1
            a3 <- a3+r3 # 0.12
            # rep(a/5,5)
            Collect1 <- c(Collect1,rep(a1,5*12))
            Collect2 <- c(Collect2,rep(a2,5*12))
            Collect3 <- c(Collect3,rep(a3,5*12))
        }
        
        DT$PayMoney_0.08 <- ceiling(Collect1)
        DT$PayMoney_0.10 <- ceiling(Collect2)
        DT$PayMoney_0.12 <- ceiling(Collect3)
        
        
        C1 <- C2 <- C3 <- c()
        a1 <- a2 <- a3 <- 0
        for(i in 1:dim(DT)[1]){
            a1 <- a1+DT$PayMoney_0.08[i]
            a2 <- a2+DT$PayMoney_0.10[i]
            a3 <- a3+DT$PayMoney_0.12[i]
            C1 <- c(C1,a1)
            C2 <- c(C2,a2)
            C3 <- c(C3,a3)
            
        }
        DT$TotalPayMoney_0.08 <- C1
        DT$TotalPayMoney_0.10 <- C2
        DT$TotalPayMoney_0.12 <- C3
        names(DT) <- c("年齡","月份","月薪資",paste0("應付金額i = ",i1,"%"),paste0("應付金額i = ",i2,"%"),paste0("應付金額i = ",i3,"%"),
                       paste0("已繳總金額i = ",i1,"%"),paste0("已繳總金額i = ",i2,"%"),paste0("已繳總金額i = ",i3,"%"))
        return(DT)
    }
    ## 在要結婚生小孩的情況下 年繳的表格
    DT_1_2 <- function(){
        
        x <- input$firstMoney*12
        i1 <- input$rate1
        i2 <- input$rate2
        i3 <- input$rate3
        
        ## 變動金額的年繳費
        r1 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i1/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        r2 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i2/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        r3 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i3/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        age <- seq(25,59,1)
        DT <- data.frame("age" = age,
                         "salary" = numeric(length(age)),     
                         "PayMoney_0.08" = numeric(length(age)),
                         "PayMoney_0.10" = numeric(length(age)),
                         "PayMoney_0.12" = numeric(length(age)),
                         "TotalPayMoney_0.08"= numeric(length(age)),
                         "TotalPayMoney_0.10"= numeric(length(age)),
                         "TotalPayMoney_0.12"= numeric(length(age)))
        
        DT$salary <- rep((seq(from = 30000,to = 1000000,by = 5000)[1:7])*12,each = 5)
        
        Collect1 <- Collect2 <- Collect3 <- c()
        a1 <- a2 <- a3 <- x
        Collect1 <-  c(Collect1,rep(a1,5))
        Collect2 <-  c(Collect2,rep(a2,5))
        Collect3 <-  c(Collect3,rep(a3,5))
        
        for(i in 2:7){
            if(i %in% c(2,3)){
                a1 <- a1 # 0.08
                a2 <- a2 # 0.1
                a3 <- a3 # 0.12
                # rep(a/5,5)
                Collect1 <- c(Collect1,rep(a1,5))
                Collect2 <- c(Collect2,rep(a2,5))
                Collect3 <- c(Collect3,rep(a3,5))
            }else{
                a1 <- a1+r1 # 0.08
                a2 <- a2+r2 # 0.1
                a3 <- a3+r3 # 0.12
                # rep(a/5,5)
                Collect1 <- c(Collect1,rep(a1,5))
                Collect2 <- c(Collect2,rep(a2,5))
                Collect3 <- c(Collect3,rep(a3,5))
            }
            
        }
        
        DT$PayMoney_0.08 <- ceiling(Collect1)
        DT$PayMoney_0.10 <- ceiling(Collect2)
        DT$PayMoney_0.12 <- ceiling(Collect3)
        
        C1 <- C2 <- C3 <- c()
        a1 <- a2 <- a3 <- 0
        for(i in 1:dim(DT)[1]){
            a1 <- a1+DT$PayMoney_0.08[i]
            a2 <- a2+DT$PayMoney_0.10[i]
            a3 <- a3+DT$PayMoney_0.12[i]
            C1 <- c(C1,a1)
            C2 <- c(C2,a2)
            C3 <- c(C3,a3)
            
        }
        DT$TotalPayMoney_0.08 <- C1
        DT$TotalPayMoney_0.10 <- C2
        DT$TotalPayMoney_0.12 <- C3
        names(DT) <- c("年齡","年薪資",paste0("應付金額i = ",i1,"%"),paste0("應付金額i = ",i2,"%"),paste0("應付金額i = ",i3,"%"),
                       paste0("已繳總金額i = ",i1,"%"),paste0("已繳總金額i = ",i2,"%"),paste0("已繳總金額i = ",i3,"%"))
        return(DT)
    }
    ## 在要結婚生小孩的情況下 月繳的表格
    DT_2_2 <- function(){
        x <- input$firstMoney
        # x <- 5000
        R <- input$needMoney
        i1 <- input$rate1
        i2 <- input$rate2
        i3 <- input$rate3
        ## 變動金額的年繳費
        # ChangeFunction_Month
        r1 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                      mx = x,mi = i1/100/12,age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney, needMoney2 = input$needMoney2, needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r2 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                      mx = x,mi = i2/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                      needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r3 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                      mx = x,mi = i3/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                      needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        
        age <- rep(seq(25,59,1),each = 12)
        DT <- data.frame("age" = age,
                         "month" = rep(c(1:12),35),
                         "salary" = numeric(length(age)),     
                         "PayMoney_0.08" = numeric(length(age)),
                         "PayMoney_0.10" = numeric(length(age)),
                         "PayMoney_0.12" = numeric(length(age)),
                         "TotalPayMoney_0.08"= numeric(length(age)),
                         "TotalPayMoney_0.10"= numeric(length(age)),
                         "TotalPayMoney_0.12"= numeric(length(age)))
        DT$salary <- rep((seq(from = 30000,to = 1000000,by = 5000)[1:7]),each = 5*12)
        
        Collect1 <- Collect2 <- Collect3 <- c()
        a1 <- a2 <- a3 <- x
        Collect1 <-  c(Collect1,rep(a1,5*12))
        Collect2 <-  c(Collect2,rep(a2,5*12))
        Collect3 <-  c(Collect3,rep(a3,5*12))
        
        for(i in 2:7){
            if(i %in% c(2,3)){
                a1 <- a1 # 0.08
                a2 <- a2 # 0.1
                a3 <- a3 # 0.12
                # rep(a/5,5)
                Collect1 <- c(Collect1,rep(a1,5*12))
                Collect2 <- c(Collect2,rep(a2,5*12))
                Collect3 <- c(Collect3,rep(a3,5*12))
            }else{
                a1 <- a1+r1 # 0.08
                a2 <- a2+r2 # 0.1
                a3 <- a3+r3 # 0.12
                # rep(a/5,5)
                Collect1 <- c(Collect1,rep(a1,5*12))
                Collect2 <- c(Collect2,rep(a2,5*12))
                Collect3 <- c(Collect3,rep(a3,5*12))
            }
            
        }
        
        DT$PayMoney_0.08 <- ceiling(Collect1)
        DT$PayMoney_0.10 <- ceiling(Collect2)
        DT$PayMoney_0.12 <- ceiling(Collect3)
        
        
        C1 <- C2 <- C3 <- c()
        a1 <- a2 <- a3 <- 0
        for(i in 1:dim(DT)[1]){
            a1 <- a1+DT$PayMoney_0.08[i]
            a2 <- a2+DT$PayMoney_0.10[i]
            a3 <- a3+DT$PayMoney_0.12[i]
            C1 <- c(C1,a1)
            C2 <- c(C2,a2)
            C3 <- c(C3,a3)
            
        }
        DT$TotalPayMoney_0.08 <- C1
        DT$TotalPayMoney_0.10 <- C2
        DT$TotalPayMoney_0.12 <- C3
        names(DT) <- c("年齡","月份","月薪資",paste0("應付金額i = ",i1,"%"),paste0("應付金額i = ",i2,"%"),paste0("應付金額i = ",i3,"%"),
                       paste0("已繳總金額i = ",i1,"%"),paste0("已繳總金額i = ",i2,"%"),paste0("已繳總金額i = ",i3,"%"))
        return(DT)
    }
    
    ## 放入年繳的詳細表格
    output$DT1 <-renderDT({
        
        datatable(as.data.frame(DT_1()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 scrollY = 600,
                                 scroller = TRUE,
                                 scrollX = TRUE))
    })
    ## 放入要結婚生小孩情況下 年繳的詳細表格
    output$DT1_2 <-renderDT({
        
        datatable(as.data.frame(DT_1_2()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 scrollY = 600,
                                 scroller = TRUE,
                                 scrollX = TRUE))
    })
    ## 放入月繳的詳細表格
    output$DT2 <-renderDT({
        
        datatable(as.data.frame(DT_2()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 scrollY = 600,
                                 scroller = TRUE,
                                 scrollX = TRUE))
    })
    ## 放入要結婚生小孩情況下 月繳的詳細表格
    output$DT2_2 <-renderDT({
        
        datatable(as.data.frame(DT_2_2()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 scrollY = 600,
                                 scroller = TRUE,
                                 scrollX = TRUE))
    })
    
    output$plot1 <- renderPlot({
        DT <- DT_1()
        
        #jpeg(filename = "累積已繳金額（年計息）.jpeg",width = 1000,height = 600)
        aa <- plot(DT[,6],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20,ylim = c(0,8000000))
        for(i in seq(1000000,10000000,by = 1000000)[1:7]){
            abline(h = i)
        }
        
        axis(side = 1,at = c(1:35),labels = paste0(c(25:59)))
        axis(side = 2,at = seq(1000000,10000000,by = 1000000)[1:7],labels = c(paste0(seq(10,70,10),'w')),las = 2,lty = 0)
        legend("topleft",cex = 1.5,
               legend = paste0(c(paste0("i = ",input$rate1,"%"),paste0("i = ",input$rate2,"%"),paste0("i = ",input$rate3,"%"))),
               col = c("black","red","blue"),
               pch = 20,border = "white",lty = 0,bty = "n",
               text.col = c("black","red","blue"),horiz = T)
        mtext(side = 3,text = "累積已繳金額（年計息）",cex = 2,font = 2)
        lines(DT[,6],type = "b",col = "black",pch = 20,cex = 2)
        lines(DT[,7],type = "b",col = "red",pch = 20,cex = 2)
        lines(DT[,8],type = "b",col = "blue",pch = 20,cex = 2)
        #dev.off()
    })
    output$plot2 <- renderPlot({
        DT <- DT_2()
        # aa <- plot(DT[,6],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20)
        #jpeg(filename = "累積已繳金額（月計息）.jpeg",width = 1000,height = 600)
        aa <- plot(DT[,7][seq(5,420,12)],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20,ylim = c(0,8000000))
        for(i in seq(1000000,10000000,by = 1000000)[1:7]){
            abline(h = i)
        }
        
        
        # axis(side = 1,at = seq(1,420,12),labels = paste0(c(25:59)))
        axis(side = 1,at = c(1:35),labels = paste0(c(25:59)))
        axis(side = 2,at = seq(1000000,10000000,by = 1000000)[1:7],labels = c(paste0(seq(10,70,10),'w')),las = 2,lty = 0)
        # names(DT)
        legend("topleft",cex = 1.5,
               legend = paste0(c(paste0("i = ",input$rate1,"%"),paste0("i = ",input$rate2,"%"),paste0("i = ",input$rate3,"%"))),
               col = c("black","red","blue"),
               pch = 20,border = "white",lty = 0,bty = "n",
               text.col = c("black","red","blue"),horiz = T)
        # title(main = "累積已繳金額（月計息）",cex = 2,font = 2)
        mtext(side = 3,text = "累積已繳金額（月計息）",cex = 2,font = 2)
        lines(DT[,7][seq(1,420,12)],type = "b",col = "black",pch = 20,cex = 2)
        lines(DT[,8][seq(1,420,12)],type = "b",col = "red",pch = 20,cex = 2)
        lines(DT[,9][seq(1,420,12)],type = "b",col = "blue",pch = 20,cex = 2)
        #dev.off()
    })
    
    output$plot1_2 <- renderPlot({
        DT <- DT_1_2()
        
        #jpeg(filename = "累積已繳金額（年計息）.jpeg",width = 1000,height = 600)
        aa <- plot(DT[,6],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20,ylim = c(0,8000000))
        for(i in seq(1000000,10000000,by = 1000000)[1:7]){
            abline(h = i)
        }
        
        axis(side = 1,at = c(1:35),labels = paste0(c(25:59)))
        axis(side = 2,at = seq(1000000,10000000,by = 1000000)[1:7],labels = c(paste0(seq(10,70,10),'w')),las = 2,lty = 0)
        legend("topleft",cex = 1.5,
               legend = paste0(c(paste0("i = ",input$rate1,"%"),paste0("i = ",input$rate2,"%"),paste0("i = ",input$rate3,"%"))),
               col = c("black","red","blue"),
               pch = 20,border = "white",lty = 0,bty = "n",
               text.col = c("black","red","blue"),horiz = T)
        mtext(side = 3,text = "累積已繳金額（年計息）",cex = 2,font = 2)
        lines(DT[,6],type = "b",col = "black",pch = 20,cex = 2)
        lines(DT[,7],type = "b",col = "red",pch = 20,cex = 2)
        lines(DT[,8],type = "b",col = "blue",pch = 20,cex = 2)
        #dev.off()
    })
    output$plot2_2 <- renderPlot({
        DT <- DT_2_2()
        # aa <- plot(DT[,6],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20)
        #jpeg(filename = "累積已繳金額（月計息）.jpeg",width = 1000,height = 600)
        aa <- plot(DT[,7][seq(5,420,12)],type = "n",xlab = "",ylab = "",axes = F,lwd = 2,lty = 5,pch = 20,ylim = c(0,8000000))
        for(i in seq(1000000,10000000,by = 1000000)[1:7]){
            abline(h = i)
        }
        
        
        # axis(side = 1,at = seq(1,420,12),labels = paste0(c(25:59)))
        axis(side = 1,at = c(1:35),labels = paste0(c(25:59)))
        axis(side = 2,at = seq(1000000,10000000,by = 1000000)[1:7],labels = c(paste0(seq(10,70,10),'w')),las = 2,lty = 0)
        # names(DT)
        legend("topleft",cex = 1.5,
               legend = paste0(c(paste0("i = ",input$rate1,"%"),paste0("i = ",input$rate2,"%"),paste0("i = ",input$rate3,"%"))),
               col = c("black","red","blue"),
               pch = 20,border = "white",lty = 0,bty = "n",
               text.col = c("black","red","blue"),horiz = T)
        # title(main = "累積已繳金額（月計息）",cex = 2,font = 2)
        mtext(side = 3,text = "累積已繳金額（月計息）",cex = 2,font = 2)
        lines(DT[,7][seq(1,420,12)],type = "b",col = "black",pch = 20,cex = 2)
        lines(DT[,8][seq(1,420,12)],type = "b",col = "red",pch = 20,cex = 2)
        lines(DT[,9][seq(1,420,12)],type = "b",col = "blue",pch = 20,cex = 2)
        #dev.off()
    })
    
    ## 變動金額與確定年金價值
    ChDT_1 <- function(){
        m <- 12
        
        i1 <- as.numeric(input$rate1)
        i2 <- as.numeric(input$rate2)
        i3 <- as.numeric(input$rate3)
        ## 變動金額的年繳費
        r1 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i1/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r2 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i2/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r3 <- uniroot(ChangeFunction_Annual,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i3/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        ## 增加金額的 月計息
        mr1 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i1/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        mr2 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i2/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        mr3 <- uniroot(ChangeFunction_Month,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i3/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        ## 確定年金價值 (年計息)
        needMoney <- input$needMoney
        needMoney2 <- input$needMoney2
        needMoney3 <- input$needMoney3
        age0 <- input$age0
        age1 <- input$age1
        age2 <- input$age2
        age3 <- input$age3
        a1 <- (needMoney)*12*An(i = i1/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i1/100,n = (age3-age1))+
            (needMoney3-needMoney2)*12*v^(age2-as.numeric(age0[[2]]))*An(i = i1/100,n = (age3-age2)) # 年
        
        a2 <- (needMoney)*12*An(i = i2/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i2/100,n = (age3-age1))+
            (needMoney3-needMoney2)*12*v^(age2-as.numeric(age0[[2]]))*An(i = i2/100,n = (age3-age2)) # 年
        
        a3 <- (needMoney)*12*An(i = i3/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i3/100,n = (age3-age1))+
            (needMoney3-needMoney2)*1*v^(age2-as.numeric(age0[[2]]))*An(i = i3/100,n = (age3-age2)) # 年
        
        ## 確定年金價值 (月計息)
        b1 <- (needMoney)*An(i = i1/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i1/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i1/100/12,n = (age3-age2)*m) # 月
        
        b2 <- (needMoney)*An(i = i2/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i2/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i2/100/12,n = (age3-age2)*m) # 月
        
        b3 <- (needMoney)*An(i = i3/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i3/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i3/100/12,n = (age3-age2)*m) # 月
        
        
        DT <- data.frame("利率%" = c(i1,i2,i3),
                         "年增加金額" = ceiling(c(r1,r2,r3)) ,
                         "月增加金額" = ceiling(c(mr1,mr2,mr3)),
                         "確定年金價值_年計息" = ceiling(c(a1,a2,a3)),
                         "確定年金價值_月計息" = ceiling(c(b1,b2,b3))
        )
    }
    ## 在要結婚生小孩的情況下 變動金額與確定年金價值
    ChDT_1_2 <- function(){
        m <- 12
        
        i1 <- as.numeric(input$rate1)
        i2 <- as.numeric(input$rate2)
        i3 <- as.numeric(input$rate3)
        ## 變動金額的年繳費
        r1 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i1/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r2 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i2/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        r3 <- uniroot(ChangeFunction_Annual_2,interval = c(1,1000000),
                      mx = input$firstMoney*12,mi = i3/100,
                      age0 = input$age0, age1 = input$age1, age2 = input$age2, age3 = input$age3,
                      needMoney = input$needMoney*12, needMoney2 = input$needMoney2*12, needMoney3 = input$needMoney3*12,
                      changeyear = 5,
                      Sn = Sn,An = An)$root
        ## 增加金額的 月計息
        mr1 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i1/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        mr2 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i2/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        mr3 <- uniroot(ChangeFunction_Month_2,interval = c(1,1000000),
                       mx = input$firstMoney,mi = i3/100/12,age0 = input$age0,age1 = input$age1,age2 = input$age2,age3 = input$age3,
                       needMoney = input$needMoney,needMoney2 = input$needMoney2,needMoney3 = input$needMoney3,changeyear = 5,
                       Sn = Sn,An = An)$root
        ## 確定年金價值 (年計息)
        needMoney <- input$needMoney
        needMoney2 <- input$needMoney2
        needMoney3 <- input$needMoney3
        age0 <- input$age0
        age1 <- input$age1
        age2 <- input$age2
        age3 <- input$age3
        a1 <- (needMoney)*12*An(i = i1/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i1/100,n = (age3-age1))+
            (needMoney3-needMoney2)*12*v^(age2-as.numeric(age0[[2]]))*An(i = i1/100,n = (age3-age2)) # 年
        
        a2 <- (needMoney)*12*An(i = i2/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i2/100,n = (age3-age1))+
            (needMoney3-needMoney2)*12*v^(age2-as.numeric(age0[[2]]))*An(i = i2/100,n = (age3-age2)) # 年
        
        a3 <- (needMoney)*12*An(i = i3/100,n = (age3-as.numeric(age0[[2]])))+
            (needMoney2-needMoney)*12*v^(age1-as.numeric(age0[[2]]))*An(i = i3/100,n = (age3-age1))+
            (needMoney3-needMoney2)*1*v^(age2-as.numeric(age0[[2]]))*An(i = i3/100,n = (age3-age2)) # 年
        
        ## 確定年金價值 (月計息)
        b1 <- (needMoney)*An(i = i1/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i1/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i1/100/12,n = (age3-age2)*m) # 月
        
        b2 <- (needMoney)*An(i = i2/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i2/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i2/100/12,n = (age3-age2)*m) # 月
        
        b3 <- (needMoney)*An(i = i3/100/12,n = (age3-as.numeric(age0[[2]]))*m)+
            (needMoney2-needMoney)*v^(age1-as.numeric(age0[[2]]))*An(i = i3/100/12,n = (age3-age1)*m)+
            (needMoney3-needMoney2)*v^(age2-as.numeric(age0[[2]]))*An(i = i3/100/12,n = (age3-age2)*m) # 月
        
        
        DT <- data.frame("利率%" = c(i1,i2,i3),
                         "年增加金額" = ceiling(c(r1,r2,r3)) ,
                         "月增加金額" = ceiling(c(mr1,mr2,mr3)),
                         "確定年金價值_年計息" = ceiling(c(a1,a2,a3)),
                         "確定年金價值_月計息" = ceiling(c(b1,b2,b3))
        )
    }
    
    ## 放入變動金額與確定年金價值的表格
    output$ChDT1 <- renderDT({
        
        datatable(as.data.frame(ChDT_1()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 # deferRender = TRUE,
                                 scrollY = 120,
                                 scroller = TRUE,
                                 scrollX = TRUE), rownames = FALSE)
    })
    ## 放入 在要結婚生小孩的情況下 變動金額與確定年金價值
    output$ChDT1_2 <- renderDT({
        
        datatable(as.data.frame(ChDT_1_2()),
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10,30),
                                 dom = 'tip',
                                 # deferRender = TRUE,
                                 scrollY = 120,
                                 scroller = TRUE,
                                 scrollX = TRUE), rownames = FALSE)
    })
    
    
}
### server ####

shinyApp(ui, server)