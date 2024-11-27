#Case study: ER injuries
library(shiny)
library(vroom)
library(tidyverse)

#create a director to store download files
dir.create("/Users/user/Documents/machine learning/neiss")

#download a file
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/tree/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet
                = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

#reading injuries dataset
injuries<-vroom::vroom("/Users/user/Documents/machine learning/neiss/injuries1.tsv.gz")
injuries

products <- vroom::vroom("/Users/user/Documents/machine learning/neiss/products1.tsv")
products

population <- vroom::vroom("/Users/user/Documents/machine learning/neiss/population1.tsv")
population

#Data exploration

#Determine the number of toilet-related injuries.
selected<-injuries%>%filter(prod_code==649)
nrow(selected)  #2993 injuries

#basic summaries based on location, body part, diagnosis of toilet-related injuries

#location
selected%>%count(location,wt=weight,sort = TRUE)

#body part
selected%>%count(body_part,wt=weight,sort=TRUE)

#diagnosis
selected%>%count(diag,wt=weight,sort = TRUE)

#summary across age and sex
summary<-selected%>%count(age,sex,wt=weight)
summary

#line graph

summary%>%ggplot(aes(age,n, color=sex))+
  geom_line()+
  labs(y="Estimated number of Injuries")


#injury rate per 10,000
summary<-selected%>%
  count(age,sex,wt=weight)%>%
  left_join(population,by=c("age","sex")) %>%
  mutate(rate=n/population*1e4)

summary

#line graph for injury rate per 10,000
summary%>%
  ggplot(aes(age,rate,colour = sex))+
  geom_line(na.rm = TRUE)
  labs(y="Injuries per 10,000 people")
  
selected%>%
  sample_n(10)%>%
  pull(narrative)


#Function to identify top five observations
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n))%>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}



#Shiny App Development

library(shiny)
prod_codes<-setNames(products$prod_code,products$title)

ui<-fluidPage(
  fluidRow(
    column(6,selectInput(inputId = "code",label = "Product",choices = prod_codes,width = "100%")),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
    
  ),
  fluidRow(column(4,tableOutput("diag")),
           column(4,tableOutput("body_part")),
           column(4,tableOutput("location"))
           ),
  fluidRow(column(12,plotOutput("age_sex"))
           ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
  
)

server<-function(input, output, session){
  
  selected<-reactive(injuries %>% filter(prod_code ==input$code))
  output$diag<-renderTable(count_top(selected(),diag),width = "100%")
  output$body_part<-renderTable(count_top(selected(),body_part),width = "100%")
  output$location<-renderTable(count_top(selected(),location),width = "100%")
  summary<-reactive({selected()%>%
      count(age,sex, wt=weight)%>%
      left_join(population,by=c("age","sex"))%>%
      mutate(rate=n/population*1e4)})
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
  
  }

shinyApp(ui,server)




