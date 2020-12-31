
server <- function(input, output) {

######### PAGE DASHBOARD ################
    #Coba Text
    output$caption <- renderText({
        print("coronavirus")
    })

    #Map menu Dashboard
    output$world_map <- renderLeaflet({

    content_popup <- glue("<b>{corona_per_region$country}</b><br>
                              Confirmed: {comma(corona_per_region$confirmed,accuracy=1)}<br>
                              Active Case: {comma(corona_per_region$active,accuracy=1)} ({corona_per_region$active_percent}%)<br>
                              Recovered: {comma(corona_per_region$recovered,accuracy=1)} ({corona_per_region$recovered_percent}%)<br>
                              Death: {comma(corona_per_region$death,accuracy=1)} ({corona_per_region$death_percent}%)<br>")

    pal <- colorNumeric("OrRd", corona_per_region$active_percent) #pal(corona_per_region$active_percent)

       map1 <- leaflet() %>%
        addTiles()

        map1 <-addCircleMarkers(map = map1,
                   lng = corona_per_region$long,
                   lat = corona_per_region$lat,
                   #icon = ico,
                   popup = content_popup,
                   #clusterOptions = markerClusterOptions()
                   radius = 6,
                   color = ifelse(corona_per_region$active_percent >= 30, "red" , ifelse(corona_per_region$active_percent <= 10, "green", "yellow")),
                   stroke =FALSE, fillOpacity = 0.9
        ) %>%
          addProviderTiles(providers$Stamen.TonerHybrid) %>% #providers$Esri.NatGeoWorldMap #OpenStreetMap.Mapnik
          setView(lng = 30.7832, lat = 34.5085, zoom = 2) %>%
          addLegend("bottomleft", colors = c("red","yellow","green"),
                    labels= c("Critical (active case > 30%)", "Warning (>10% - 30%)","On Track (active case < 10%)"),
                    values = corona_per_region$active_percent,
                    title = "Country's Status",
                    opacity = 0.7)

      map1

    })

####### PAGE TOP-N COUNTRY ############
    output$top_country_plot <- renderPlot({


      if (input$radio_asc_top=="Descending"){
        corona_per_region_top <- corona_per_region %>%  arrange(- get(input$radio_type_top)) %>%  head(input$bins_top)
        plot_2 <- corona_per_region_top %>%
          ggplot(mapping = aes(x = reorder(country,get(input$radio_type_top)), y = get(input$radio_type_top)))
      }
      else{
        corona_per_region_top <- corona_per_region %>%  arrange(- get(input$radio_type_top)) %>%  tail(input$bins_top)
        plot_2 <- corona_per_region_top %>%
          ggplot(mapping = aes(x = reorder(country,-get(input$radio_type_top)), y = get(input$radio_type_top)))

      }


        #plot_2 <- corona_per_region_top %>%
        plot_2 <- plot_2+geom_col(aes(fill=get(input$radio_type_top))) +
        coord_flip() +
        geom_label(aes(label=comma(get(input$radio_type_top))),size=3,colour="black")+ #,nudge_y = 4 nudge_y = 0.1
        scale_fill_gradient(low = ifelse(input$radio_type_top == "confirmed", "#004b7a", ifelse(input$radio_type_top == "active", "orange", ifelse(input$radio_type_top == "recovered", "#00542d", "#590b0b"))),
                            high = ifelse(input$radio_type_top == "confirmed", "#004b7a", ifelse(input$radio_type_top == "active", "orange", ifelse(input$radio_type_top == "recovered", "#00542d", "#590b0b"))))+ #004b7a
        #scale_fill_manual("red")+
        #scale_color_brewer()+
        #scale_color_viridis_d()+
        labs(title = glue("Top {input$bins_top} Country where Corona Virus Spread"),
             caption = "22 Jan to 20 Dec 2020",
             x="", y="",
             fill="Total Case")+
        scale_y_continuous(labels = scales::comma)+
        theme_pander()+
        theme(legend.position = "none" )

      plot_2



    })



####### PAGE TREND ############
    output$trend_map <- renderPlotly({

      validate(
        need(input$country != "", "Please fill at least one country.")
      )


      plot_2 <- coronavirus %>% filter(country %in% input$country) %>% group_by(month_case_abbr,type) %>% summarise(jumlah=sum(cases)) %>%
        ggplot(aes(x=month_case_abbr, y=jumlah,group=type,text = glue("Total {str_to_title(type)} in {month_case_abbr} : <b>{comma(jumlah)}</b>")))+
        geom_line(lwd=0.4,aes(col=type))+ #colour="steelblue"+
        #theme(axis.text.x = element_text(angle=90,hjust = 1))+
        #scale_x_date(date_labels = "%b")+
        labs(x = "",
             y = "")+
        scale_color_manual(values=c("#fabc20", "#c41d00", "#1d6604"))+ #("#999999", "#E69F00", "#56B4E9")
        scale_y_continuous(labels = scales::comma)+
        theme(text = element_text(family = "Arial"),
              strip.background = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "#e9f1f5",
                                              colour = "#e9f1f5",
                                              size = 0.5, linetype = "dotted"),
              legend.title = element_blank()
              #legend.box.margin = margin(116, 6, 6, 6)
              #legend.position="None"

              #legend.position = c(1.95, 2.95)
              #legend.position="bottom", legend.box = "horizontal"
              #legend.position = "left"
              #legend.background = element_rect(colour = "red",fill="red")

              )

        #+theme_replace()   #theme_stata()

      ggplotly <- ggplotly(plot_2,tooltip="text") %>%  config(displayModeBar = T)


    })


    output$col_plot_1 <- renderPlot({
      coronavirus$type <- factor(coronavirus$type, levels = c("confirmed","recovered", "death")) #reorder label

      plot_2 <-  coronavirus %>% filter(country %in% input$country) %>% group_by(type) %>% summarise(jumlah=sum(cases)) %>%

        ggplot( mapping = aes(x = type, y = jumlah)) +
        # geom_jitter(aes(size = jumlah),
        #             col = "black",
        #             alpha = 0.2) + # transparency
        geom_col(mapping = aes(fill = type )) +
        theme_minimal() + # mengatur tema
        theme(legend.position = "none") + # mengatur tema
        labs(x="",
             y="",
             caption = "") +
        geom_label(aes(label=comma(jumlah)),size=3)+
        scale_fill_manual(values=c("#fabc20", "#1d6604","#c41d00"))+
        scale_y_continuous(labels = scales::comma)
        #scale_fill_brewer(palette = "Set1")

       plot_2



    })

##### PAGE COMPARISON 2 COUNTRIES ######
    output$label_country_1 <- renderText({input$compare_country_1})
    output$label_country_2 <- renderText({input$compare_country_2})

    output$comparison_plot <-  renderPlotly({

      tmp_country <- c(input$compare_country_1,input$compare_country_2)

      dt_comp <- coronavirus %>% filter(country %in% tmp_country) %>% group_by(country,type) %>% summarise(total=sum(cases))

      #get active case
      tmp_active_country_1 <- (dt_comp[(dt_comp$country==input$compare_country_1 & dt_comp$type=="confirmed"),3]) - ((dt_comp[(dt_comp$country==input$compare_country_1 & dt_comp$type=="recovered"),3])+(dt_comp[(dt_comp$country==input$compare_country_1 & dt_comp$type=="death"),3]))
      tmp_active_country_2 <- (dt_comp[(dt_comp$country==input$compare_country_2 & dt_comp$type=="confirmed"),3]) - ((dt_comp[(dt_comp$country==input$compare_country_2 & dt_comp$type=="recovered"),3])+(dt_comp[(dt_comp$country==input$compare_country_2 & dt_comp$type=="death"),3]))
      # insert active case in to dataframe
      dt_comp <- rbind(dt_comp, data.frame(country=input$compare_country_1, type="active",total=tmp_active_country_1))
      dt_comp <- rbind(dt_comp, data.frame(country=input$compare_country_2, type="active",total=tmp_active_country_2))

      tmp_total_confirmed_c1 <- dt_comp[(dt_comp$country==input$compare_country_1 & dt_comp$type=="confirmed"),3]
      tmp_total_confirmed_c2 <- dt_comp[(dt_comp$country==input$compare_country_2 & dt_comp$type=="confirmed"),3]
      dt_comp$type <- factor(dt_comp$type, levels = c("confirmed","active","recovered", "death")) #reorder label


      com_plot <- dt_comp %>% ggplot(aes(x = type, y = total, text=glue("<b>{country}</b><br>{str_to_title(type)} : {comma(total,accuracy=1)}")))+
        geom_col(aes(fill = dt_comp$country), position = "dodge")+
        labs(x = "",
             y = "")+
        geom_label(aes(label=comma(total)),size=3)+ #,nudge_y = 0.5
        scale_y_continuous(labels = scales::comma)+
        scale_fill_manual(name="", label=dt_comp$country, values = c("#de7878","#68c7d4"))+
        labs(
             x="",
             y="",
             fill="")+
          theme(text = element_text(family = "Arial") ) #,axis.title.y = element_text(size=11)

        ggplotly(com_plot,tooltip = "text")%>%
          layout(legend = list(orientation = "h",
                               y = 5,
                               x = 0.2)) %>%
        config(displayModeBar = F)



    })

    output$trend_comparison_daily <- renderPlotly({
      tmp_country2 <- c(input$compare_country_1,input$compare_country_2)

      dt_comp_dailty <- coronavirus %>% filter(country %in% tmp_country2 & type==input$radio_type & date >=format(input$date_range_daily[1]) & date <=format(input$date_range_daily[2])) %>%
        group_by(date,country) %>% summarise(jumlah=sum(cases)) %>% arrange(date)

      plot_3 <- dt_comp_dailty %>% ggplot(aes(x=date, y=jumlah,group=country,text=glue("<b>{country}</b><br>{comma(jumlah,accuracy=1)} {str_to_title(input$radio_type)} cases in {date}")))+
        geom_line(lwd=0.4,aes(col=country))+
        labs(x = "",
             y = "")+  theme(text = element_text(family = "Arial"))+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(name="", label=dt_comp_dailty$country, values = c("#de7878","#68c7d4"))+
        theme(text = element_text(family = "Arial"),
              strip.background = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "white",
                                              colour = "#e9f1f5",
                                              size = 0.5, linetype = "dashed"),
              legend.title = element_blank(),
              panel.grid = element_line(colour = "#f0f0f0", size = 0.4,linetype = "dashed")
              )


      ggplotly(plot_3,tooltip="text")


    })

    output$trend_comparison_month <- renderPlot({
      tmp_country2 <- c(input$compare_country_1,input$compare_country_2)

      plot_3 <- coronavirus %>% filter(country %in% tmp_country2 & type==input$radio_type & date >=format(input$date_range_daily[1]) & date <=format(input$date_range_daily[2])) %>%
        group_by(month_case_abbr,country) %>% summarise(jumlah=sum(cases)) %>%
        ggplot(aes(x=month_case_abbr, y=jumlah,group=country))+
        geom_line(lwd=0.7,aes(col=country))+
        labs(x = "",
             y = "")+
        geom_label(aes(label=comma(jumlah,accuracy = 1)),size=2.5)+
        scale_y_continuous(labels = scales::comma)+theme_pander()

      plot_3


    })

    observeEvent(input$preview, {
      # Show a modal when the button is pressed
      shinyalert("Oops!", "Something went wrong.", type = "success")


    })

    output$map_country_1 <- renderLeaflet({
      tmp_corona_per_region <- corona_per_region %>% filter(country==input$compare_country_1)
      content_popup <- glue("<b>{tmp_corona_per_region$country}</b><br>
                              Confirmed: {comma(tmp_corona_per_region$confirmed)}<br>
                              Active Case: {comma(tmp_corona_per_region$active)} ({comma(tmp_corona_per_region$active_percent,accuracy=0.01)}%)<br>
                              Recovered: {comma(tmp_corona_per_region$recovered)} ({comma(tmp_corona_per_region$recovered_percent,accuracy=0.01)}%)<br>
                              Death: {comma(tmp_corona_per_region$death)} ({comma(tmp_corona_per_region$death_percent,accuracy=0.01)}%)<br>")



      map1 <- leaflet() %>%addTiles()

      map1 <-addMarkers(map = map1,
                        lng = tmp_corona_per_region$long,
                        lat = tmp_corona_per_region$lat,
                        icon = ico,
                        popup = content_popup
                        #clusterOptions = markerClusterOptions()
                        #radius = 6,
                        #color = ifelse(corona_per_region$active_percent >= 30, "red", ifelse(corona_per_region$active_percent <= 10, "green", "yellow")),
                        #stroke =FALSE, fillOpacity = 0.7
      ) %>%
        addProviderTiles(providers$Stamen.TonerHybrid) %>%
        setView(lng = tmp_corona_per_region$long, lat = tmp_corona_per_region$lat, zoom = 5)

      map1

    })

    observeEvent(input$show_map_1, {
      showModal(modalDialog( leafletOutput("map_country_1"),
        title = input$compare_country_1,
        ""
      ))
    })

    output$map_country_2 <- renderLeaflet({
      tmp_corona_per_region <- corona_per_region %>% filter(country==input$compare_country_2)
      content_popup <- glue("<b>{tmp_corona_per_region$country}</b><br>
                              Confirmed: {comma(tmp_corona_per_region$confirmed)}<br>
                              Active Case: {comma(tmp_corona_per_region$active)} ({comma(tmp_corona_per_region$active_percent,accuracy=0.01)}%)<br>
                              Recovered: {comma(tmp_corona_per_region$recovered)} ({comma(tmp_corona_per_region$recovered_percent,accuracy=0.01)}%)<br>
                              Death: {comma(tmp_corona_per_region$death)} ({comma(tmp_corona_per_region$death_percent,accuracy=0.01)}%)<br>")



      map1 <- leaflet() %>%addTiles()

      map1 <-addMarkers(map = map1,
                        lng = tmp_corona_per_region$long,
                        lat = tmp_corona_per_region$lat,
                        icon = ico,
                        popup = content_popup
                        #clusterOptions = markerClusterOptions()
                        #radius = 6,
                        #color = ifelse(corona_per_region$active_percent >= 30, "red", ifelse(corona_per_region$active_percent <= 10, "green", "yellow")),
                        #stroke =FALSE, fillOpacity = 0.7
      ) %>%
        addProviderTiles(providers$Stamen.TonerHybrid) %>%
        setView(lng = tmp_corona_per_region$long, lat = tmp_corona_per_region$lat, zoom = 5)




      map1

    })

    observeEvent(input$show_map_2, {
      showModal(modalDialog( leafletOutput("map_country_2"),
                             title = input$compare_country_2,
                             ""
      ))
    })


    # Function untuk  mendapatkan label untuk setiap type
    get_label<-function(x,input_country,label){
      dt <- x %>% filter(country==input_country)
      a <- dt %>% select(label)
      a <- a[1,1]

      return (comma(a))

    }

## text informasi comaprison country 1
  output$c1 <-renderText({
    tc <- str_to_lower(input$compare_country_1)

    # a <- str_to_lower("indonesia")
    tc <- paste("https://www.worldometers.info/img/flag-",a,".gif",sep = "")
    tc

  })
  output$picture_c1<-renderText({
    #html <- "https://www.countries-ofthe-world.com/flags-normal/flag-of-"
    #html2 <- "https://www.worldometers.info/img/flag-indonesia.gif"
    html <- "flags/"
    tc <- str_replace(input$compare_country_1, " ", "-")
    tc <- paste(html,tc,".png",sep = "")
    c('<img src="',tc,'",align="right", border=1, width=25,height=12>')
    })

  output$picture_c2<-renderText({
    #html <- "https://www.countries-ofthe-world.com/flags-normal/flag-of-"
    html <- "flags/"
    tc <- str_replace(input$compare_country_2, " ", "-")
    tc <- paste(html,tc,".png",sep = "")
    c('<img src="',tc,'",align="right", border=1, width=25,height=12>')
  })

  output$txt_confirmed_c1 <- renderText({
       get_label(corona_per_region,input$compare_country_1,"confirmed")

  })
  output$txt_active_c1 <- renderText({
    get_label(corona_per_region,input$compare_country_1,"active")

  })
  output$txt_recovered_c1 <- renderText({
    get_label(corona_per_region,input$compare_country_1,"recovered")

  })

  output$txt_death_c1 <- renderText({
    get_label(corona_per_region,input$compare_country_1,"death")

  })

  output$txt_confirmed_percent_c1 <- renderText({
    get_label(corona_per_region,input$compare_country_1,"confirmed_percent")


  })
  output$txt_active_percent_c1 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_1,"active_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })
  output$txt_recovered_percent_c1 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_1,"recovered_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })

  output$txt_death_percent_c1 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_1,"death_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })

  ## text informasi comaprison country 2
  output$txt_confirmed_c2 <- renderText({
    get_label(corona_per_region,input$compare_country_2,"confirmed")

  })
  output$txt_active_c2 <- renderText({
    get_label(corona_per_region,input$compare_country_2,"active")

  })
  output$txt_recovered_c2 <- renderText({
    get_label(corona_per_region,input$compare_country_2,"recovered")

  })

  output$txt_death_c2 <- renderText({
    get_label(corona_per_region,input$compare_country_2,"death")

  })

  output$txt_confirmed_percent_c2 <- renderText({
    get_label(corona_per_region,input$compare_country_2,"confirmed_percent")


  })
  output$txt_active_percent_c2 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_2,"active_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })
  output$txt_recovered_percent_c2 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_2,"recovered_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })

  output$txt_death_percent_c2 <- renderText({
    t <- get_label(corona_per_region,input$compare_country_2,"death_percent")
    t <- paste("(",t,"%)",sep="")
    t

  })

##### #Render Datatable untuk Menu Data #####
    output$coronatable = renderDataTable(
      coronavirus,
      filter = 'top'

    )


}
