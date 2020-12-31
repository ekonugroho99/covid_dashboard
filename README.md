# Covid Dashboard
This is repository of my first project, Covid Dashboard, which will help you to keep updated the information about Corona Virus arround the world. This dashboard inform you about:
1. Countries status which is struggling facing corona virus, so you can be prepared when you must go abroad.
2. Top country which have the most confirm case.
3. The Trend of confirm, active, recovered and death cases arround the world.
4. Comparison between two countries. You can compairing your country with others and see the result!

## Library
This dashboard is devoloped by using `R Languange` and some libraries i.e `shiny`,`shinydashboard`,`shinydashboardPlus`,`shinythemes`,`shinyWidgets`,`dashboardthemes`,`ggplot2`,`plotly`,`leaflet ` and more. You can check all libraries in `global.r`

## Dashboard
This dashboard page show  total confirm case, active case, recover and death case in the`valueBox`. The map on this page show the countries status and case for every country in the world. This map using `leaflet` library.

![dashboard](images/dashboard.png)

## Top Spreading
In this page, you can see where the most case was happened. You can filter it by cases type like confirm, active, recovered and death.
![dashboard](images/top_spread.png)
