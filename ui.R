
customLogo <- shinyDashboardLogoDIY(

    boldText = "Covid"
    ,mainText = "Dashboard"
    ,textSize = 16
    ,badgeText = "1.0"
    ,badgeTextColor = "white"
    ,badgeTextSize = 2
    ,badgeBackColor = "#1e2b37"
    ,badgeBorderRadius = 3

)



header <-   dashboardHeader( title= customLogo ) #titleWidth = 275

sidebar <-  dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Dashboard",tabName = "dashboard", icon=icon("tachometer-alt")),
        menuItem(text = "Top Spread",tabName = "top_n",icon=icon("viruses")),
        menuItem(text = "Trend",tabName = "trend",icon=icon("chart-line")),
        menuItem(text = "Comparison",tabName = "comparison",icon=icon("chart-bar")),
        menuItem(text = "Dataset",tabName = "data",icon=icon("database"),badgeLabel = "20 Dec 2020"),
        menuItem(text = "About",tabName = "about",icon=icon("comment-alt"))

    )

)

body <- dashboardBody(customTheme,

    tabItems(
        tabItem(tabName = "dashboard",
                fluidPage(#theme = shinytheme("yeti"),

                    fluidRow(
                        valueBox(glue("{comma(total_confirmed)}") ,width = 3, glue("Confirmed Case"), icon = icon("clipboard-list"),color=confirmed_color),
                        valueBox(comma(total_active),width = 3, glue("Active Case ({total_active_ratio}%)"), icon = icon("ambulance"),color=active_color),
                        valueBox(comma(total_recovered),width = 3, glue("Recovered ({total_recovered_ratio}%)"), icon = icon("heartbeat"),color=recovered_color),
                        valueBox(comma(total_death), width= 3, glue("Death ({total_death_ratio}%)"), icon = icon("skull-crossbones"),color=death_color),
                        column(width=12,
                               box(width=14, status = "danger",
                                   leafletOutput("world_map")

                                   )
                               )

                    )


                )

                ),
        tabItem(tabName = "top_n",
                fluidPage(
                    fluidRow(
                        column(width = 4,
                               box(solidHeader=F,
                                   collapsible = F,
                                   width=14,
                                   height=425,
                                   sliderInput(inputId = "bins_top",
                                               label = "Select Top:",
                                               min = 5,max = 30,
                                               step = 1,
                                               value = 10
                                   ),
                                   radioButtons(inputId="radio_type_top",
                                                label="Case Type:",
                                                choices=c("confirmed","active","recovered","death"),
                                                selected="confirmed",
                                                inline=F),
                                   radioButtons(inputId="radio_asc_top",
                                                label="Sorting:",
                                                choices=c("Ascending","Descending"),
                                                selected="Descending",
                                                inline=T),

                                   )

                               ),
                        column(width = 8,
                               box(solidHeader=F,
                                   collapsible = F,
                                   width=14,

                                   plotOutput("top_country_plot")
                                   )
                               )


                    )

                )

                ),
        tabItem(tabName = "trend",
                fluidPage(
                    fluidRow(
                        box(
                            solidHeader=T,
                            collapsible = F,
                            width=9,
                            height = 475,
                            tags$strong("Trend's Report"),
                            plotlyOutput("trend_map",height=430),

                        ),

                        box(
                            solidHeader=F,
                            collapsible = F,
                            width=3,
                            pickerInput(
                                inputId = "country",
                                label = "Select Country:",
                                choices = levels(corona_per_region$country),
                                selected = levels(corona_per_region$country),#"Indonesia",
                                options = list(
                                    'actions-box' = TRUE
                                    ),
                                choicesOpt = list( content = stringr::str_trunc(levels(corona_per_region$country), width = 20)),

                                multiple = TRUE
                                )

                        ),
                        box(solidHeader=F,
                            collapsible = F,
                            width=3,height = 360,
                            strong("Corona's Case Graph"),
                            plotOutput("col_plot_1",height=320)
                            )




                    )


                )

                ),
        tabItem(tabName = "comparison",
                fluidPage(
                    fluidRow(
                        column(width=3,
                               box(width = 14,solidHeader = F,
                                   selectInput(inputId="compare_country_1",
                                               label="Choose Country (1):",
                                               choices=levels(coronavirus$country),
                                               selected="China",
                                               selectize = T
                                                 )
                                   ),
                               box(width = 14, height = 200, background = "light-blue",
                                   #img(src=textOutput("c1"), align = "right"),
                                   htmlOutput("picture_c1",align="right",border=1),
                                   tags$table(
                                       style = "width:100%",
                                       tags$tr(
                                           tags$td(strong("Confirmed")),
                                           tags$td(textOutput("txt_confirmed_c1")),
                                           tags$td("")
                                       ),
                                       tags$tr(
                                           tags$td(strong("Active")),
                                           tags$td(textOutput("txt_active_c1")),
                                           tags$td(textOutput("txt_active_percent_c1"))
                                       ),
                                       tags$tr(
                                           tags$td(strong("Recovered")),
                                           tags$td(textOutput("txt_recovered_c1")),
                                           tags$td(textOutput("txt_recovered_percent_c1"))
                                       ),
                                       tags$tr(
                                           tags$td(strong("Death")),
                                           tags$td(textOutput("txt_death_c1")),
                                           tags$td(textOutput("txt_death_percent_c1"))
                                       )
                                   ),
                                   tags$br(),
                                   actionButton("show_map_1", "Show Map",icon = icon("search"))

                                   )
                               ),
                        column(width=6,
                               box(width = 14,height = 325,solidHeader = F,
                                   plotlyOutput("comparison_plot",height=300)
                                   )
                               ),
                        column(width=3,
                               box(width = 14,solidHeader = F,
                                   selectInput(inputId="compare_country_2",
                                               label="Choose Country (2):",
                                               choices=levels(coronavirus$country),
                                               selected="Indonesia",
                                               selectize = T
                                   )

                                   ),
                               box(width = 14, height = 200,background = "light-blue",
                                   htmlOutput("picture_c2",align="right",border=1),
                                   tags$table(
                                       style = "width:100%",
                                       tags$tr(
                                           tags$td(strong("Confirmed")),
                                           tags$td(textOutput("txt_confirmed_c2")),
                                           tags$td("")
                                       ),
                                       tags$tr(
                                           tags$td(strong("Active")),
                                           tags$td(textOutput("txt_active_c2")),
                                           tags$td(textOutput("txt_active_percent_c2"))
                                       ),
                                       tags$tr(
                                           tags$td(strong("Recovered")),
                                           tags$td(textOutput("txt_recovered_c2")),
                                           tags$td(textOutput("txt_recovered_percent_c2"))
                                       ),
                                       tags$tr(
                                           tags$td(strong("Death")),
                                           tags$td(textOutput("txt_death_c2")),
                                           tags$td(textOutput("txt_death_percent_c2"))
                                       )
                                   ),
                                   tags$br(),
                                   actionButton("show_map_2", "Show Map",icon = icon("search"))

                                   )
                               )

                    ),
                    fluidRow(
                        column(width=12,
                               box(width = 14,height = 450, strong(glue("Trend Comparison by Case's Type")),br(),
                                   column(width = 8,
                                          radioButtons(inputId="radio_type",
                                                       label="",
                                                       choices=c("confirmed","recovered","death"),
                                                       selected="confirmed",
                                                       inline=T)

                                          ),
                                   column(width = 4,
                                          dateRangeInput(
                                              inputId="date_range_daily",
                                              label="",
                                              width = 300,
                                              start = min(coronavirus$date),
                                              end = max(coronavirus$date),
                                              min = min(coronavirus$date),
                                              max = NULL,
                                              separator = " to ",
                                              format = "yyyy-mm-dd"),
                                          ),

                                   tabsetPanel(
                                       tabPanel(title="Daily",icon=icon("calendar-day"),br(),
                                                plotlyOutput("trend_comparison_daily",height = 250)

                                       ),
                                       tabPanel(title="Monthly",icon=icon("chart-line"),br(),
                                                plotOutput("trend_comparison_month",height = 250)

                                       )

                                   )






                                   )
                               )

                    )

                )

                ),

        tabItem(tabName = "data",
                fluidPage(
                    fluidRow(
                        column(width = 12,
                               h2("Dataset Corona Virus"),
                               p("22 January 2020 to 20 December 2020"),
                               box(width=14, status="primary",
                                   dataTableOutput("coronatable")
                                   )

                               )

                    )


                )

                ),
        tabItem(tabName = "about",
                widgetUserBox(
                    title = "Eko Nugroho",
                    subtitle = "Data Scientist",
                    type = 2,
                    width = 6,
                    src = "geeks-128.png",
                    color = "yellow",
                    "Hi! This is my first project, Covid Dashboard, which  will help you to keep updated the information about Corona Virus arround the world.
                    This dashboard inform you about:",br(),
                    "1. Countries status which is struggling facing corona virus, so you can be prepared when you must go abroad.",br(),
                    "2. Top country which have the most confirm case.",br(),
                    "3. The Trend of confirm, active, recovered and death cases arround the world.",br(),
                    "4. Comparison between two countries. You can compairing your country with others and see the result!",br(),
                    "The data was taken from", a("Rami Krispin's github",href="https://github.com/RamiKrispin/coronavirus/tree/master/data"),"Thanks pal!",br(),
                    "If you have any comment or advise dont hesitate to contact me at", a("99.ekonugroho@gmail.com."),"Stay safe and dont forget your mask!",


                    footer = ""
                )


                )

    )



)

ui <- dashboardPage(
    #skin="red",
    header = header,
    body = body,
    sidebar =sidebar

)
