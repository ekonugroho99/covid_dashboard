# Import Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(dashboardthemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(readr)
library(lubridate)
library(tidyr)
library(glue)
library(DT)
library(formattable)
library(leaflet)
library(reshape)
library(scales)
library(stringr)

# Load Data
load("coronavirus.rda")
df_population <-read.csv("world_population.csv",sep=";")


# Data Wrangling
coronavirus[,c("country","province" ,"type")] <- lapply(coronavirus[,c("country","province"  , "type")], as.factor) #ubah tipe data
coronavirus$date <- ymd(coronavirus$date)
coronavirus$month_case <- month(coronavirus$date,label = TRUE,abbr = FALSE)
coronavirus$month_case_abbr <- month(coronavirus$date,label = TRUE,abbr = TRUE)
coronavirus$month_case <- factor(coronavirus$month_case, levels = c("January","February","March", "April", "May", "June", "July", "August","September", "October", "November","December"))
coronavirus$month_case_abbr <- factor(coronavirus$month_case_abbr, levels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec"))

#factor(Months, levels = my_levels)
#coronavirus[is.na(coronavirus$month_case),] %>% is.na() %>% colSums()
#coronavirus %>% head() %>% str(coronavirus)

#Function Untuk Mengecek Missing Value
check_missing_value<-function(x){
  res<-NULL
  for (i in 1:ncol(x)){
    temp<-sum(is.na(x[,i]))
    temp<-as.data.frame(temp)
    temp$column_name<-colnames(x)[i]
    temp$no_kolom <- i
    res<-rbind(res,temp)
  }
  #res <- rename(res, replace=c(missing_value=temp))

  #rename kolom
  res <- res %>% filter(temp>0) #hanya menampilkan kolom yang ada NA
  res <- res %>% select(column_name,no_kolom,temp)
  return(res)
}

# Data per Region
tmp_result <- coronavirus %>%
  group_by(country,type) %>%
  summarise(total=sum(cases)) %>%
  ungroup()
corona_per_region <-  cast(tmp_result,country~type)
corona_per_region <- merge(corona_per_region, coronavirus[!duplicated(coronavirus$country),], by="country")
corona_per_region <- corona_per_region %>% select(country,confirmed,recovered,death,lat,long )
#Tambahkan kolom persentase
corona_per_region$active <- corona_per_region$confirmed - (corona_per_region$recovered+corona_per_region$death)
corona_per_region$active_percent <- round(corona_per_region$active/corona_per_region$confirmed*100,2)
corona_per_region$recovered_percent <- round(corona_per_region$recovered/corona_per_region$confirmed*100,2)
corona_per_region$death_percent <- round(corona_per_region$death/corona_per_region$confirmed*100,2)

corona_per_region$country %>% length() #191
df_population$Country %>% length()
check_missing_value(corona_per_region)

# test <- left_join(x=corona_per_region, y=df_population, by = c("country"="Country"))
# test$country %>% length()
#
# test$country <- with(test,  factor(country, levels = c("Burma","Congo (Brazzaville)"), labels = c("Bhutan","Congo")))
#
# check_missing_value(test)
# test[is.na(test$Population),]


# test%>% left_join(distinct(country,country, .keep_all = T))

#merge(corona_per_region, coronavirus[!duplicated(coronavirus$country),], by=test$country)

#corona_per_region<-merge(x=corona_per_region,y=coronavirus,by="country",all.x=F)
#corona_per_region <- select(country,confi)

# mutate(confirmed_reg = sum(confirmed),
#        recover = sum(recover),
#        death_reg = sum(death) ) %>%

#%>% summarise(confirmed=sum())

# variable global untuk dashboard
overview_dashboard <- coronavirus %>% group_by(type) %>% summarise(hasil=sum(cases))
overview_dashboard <- data.frame(overview_dashboard)

#names(overview_dashboard)[names(overview_dashboard) == "Var1"] <- "status" #rename nama kolom
total_confirmed <-  overview_dashboard[overview_dashboard$type=='confirmed',"hasil"]
total_recovered <-  overview_dashboard[overview_dashboard$type=='recovered',"hasil"]
total_death <-  overview_dashboard[overview_dashboard$type=='death',"hasil"]
total_active <- total_confirmed-(total_recovered+total_death)
total_active_ratio <- comma(total_active/total_confirmed*100, 0.02)
total_recovered_ratio <- comma(total_recovered/total_confirmed*100, 0.02)
total_death_ratio <- comma(total_death/total_confirmed*100, 0.02)





# Set Warna Global
confirmed_color <- "blue"
active_color  <- "orange"
recovered_color <- "green"
death_color <-  "red"

#buat icon di Map
ico <- makeIcon(
  iconUrl = "corona.png",
  iconWidth= 20, iconHeight=20
)


### THEME
###Custome Style ####
customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(33,37,41)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(255,255,255)"

  ### header
  ,logoBackColor = "rgb(23,103,124)"

  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"

  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"


  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "rgb(255,255,255)"

  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(44,62,80)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"

  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0

  ,sidebarTabBackColorSelected = "rgb(30,43,55)"
  ,sidebarTabTextColorSelected = "rgb(24,188,156)"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "rgb(44,62,80)"
  ,sidebarTabTextColorHover = "rgb(24,188,156)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "rgb(233, 241, 245)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(50,150,175)" #(52,152,219) (30, 43, 55)
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(24, 188, 156)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"

  ,tabBoxTabColor = "rgb(44,62,80)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255, 255, 255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10

  ### inputs
  ,buttonBackColor = "rgb(44,62,80)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(44,62,80)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(30,43,55)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(30,43,55)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(206,212,218)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"

  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(236,240,241)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1


)





# library("xlsx")
# write.xlsx(corona_per_region, file = "region.xlsx",
#            sheetName = "region", append = FALSE)


source("ui.R")
source("server.R")

shinyApp(ui = ui,server = server)
