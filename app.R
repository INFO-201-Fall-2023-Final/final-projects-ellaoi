
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)

#link to our ShinyApp: https://ellaoi.shinyapps.io/final-projects-ellaoi/

#PAGE 1
page1_ui <- fluidPage(
  titlePanel("Introduction"),
  mainPanel(
    h3("Creators: Ella Ighodaro, Natalie Cohen, Eric Carlsson-Zhang"),
    h3("Project Overview"),
    HTML('<center><img src = "https://www.cdc.gov/obesity/data/images/obesity-map-731x365-1.jpg?_=60078"
         width = "400"></center>'),
  h3("What is the analysis we are doing?"),
  p("This project will analyze the average obesity prevalence in each state of the United States, 
    using the dataset 'obesity_rates_youth.csv' which is a combined dataset from 2 datasets both from the CDC. 
    Obesity rates being higher in some states rather than others is not random, as many externalities 
    drive people’s lifestyles. Being aware of the rates of each state is important to see where we could implement 
    ideas of some states with low prevalence and use that information to help the states with high prevalence rates. 
    We hope to use our project to visualize the combining data sets of different geographical locations, age, 
    and certain dietary/physical habits… to see how these factors affect obesity rates in the U.S."), 
  h3("Why is this Important"),
  p("Approximately there are 300,000 deaths every year due to obesity in the US.
  Obesity is one of the leading causes of premature death as it contributes to 
  chronic conditions such as heart disease, stroke, type-2 diabetes, and certain types of cancer. 
  This is especially concerning as the percentage of children with obesity is growing 
  every day in the US, one of the main factors of obesity is socioeconomic status. 
  People who are a part of families of lower socioeconomic status have less access to the knowledge
  of healthy and nutritious foods but also aren’t in the environment where those foods might be. 
  Obesity is an interesting topic to research for many reasons which include the relationship between genetics, biology, and the environment we live in. 
  Due to obesity’s association with other serious medical conditions, healthcare professionals and policymakers need to address these issues to improve overall health outcomes.
"),
  h3("Context"),
  p("Using the average obesity rates in each state along with geographical locations, and the certain questions they asked students between the grades of 9th - 12th. 
    These factors will help us see the deeper reality of how living in poor, underprivileged areas with less access to healthy food and good education systems affects 
    obesity rates starting in our youth that end up following them into adulthood. ")
  )
)

#SOME THINGS NEEDED 
ob <- read.csv("obesity_rates_youth.csv")
cali_data <- subset(ob, State == "California")
compare_data <- subset(ob, State == c("Michigan", "California"))
cali_data <- cali_data[cali_data$Grade %in% c("9th", "10th", "11th", "12th"), ]

#PAGE 2
page2_ui <- fluidPage(
  titlePanel("California Obesity Based on Grade"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("question", "Choose a Question:", 
                  choices = unique(cali_data$Question),
                  selected = NULL),
      br(),
      actionButton("plotButton", "Plot")
      ),
    mainPanel(
      plotOutput("barPlot"),
      h3("Life Style Habits"),
      p("Here we are specifically looking at California which in this dataset is the closest state to 
        Washington and is also on the West Coast. Click any question and hit plot, then you can switch
        between plots to see how these different lifestyles differ between students."), 
      p("Lifestyle habits play a crucial role in childhood obesity, in this datset
      they asked certain lifestyle questions to kids between 9th - 12th grade.
      You can see here in the plots that all students were enganging in unhealthy habits
      while some 9th graders were engaging in excercise where it might have been due to the
      mandatory High School curriculum of PE in the beginning years of school. But we can see the
      overall 'overweight classification' affects mostly 12th graders and can follow them into their adult years.")
    )
  )
)

#PAGE 3
page3_ui <- fluidPage(
  titlePanel("State Obesity Prevalence"),
  sidebarLayout(
    sidebarPanel(
      selectInput("State", "Select a State:", choices = unique(ob$State))
    ),
    mainPanel(
      plotOutput("barChart"),
      h3("Obesity Prevalence in the States"),
      p("You can use the dropdown to select a state, the obesity prevalance of that state will appear on the bar chart."),
      p("The reason each state has a different obesity prevalance is because of the differing socioeconomic status 
        the populations live in some states have better access to grocery stores where they can make healthy decisons with their food and also be in a safe enviroment to excercise. 
        There are also different levels of education systems in different states, so some people do not learn about adequite nutrition. Seeing this makes a really big impact
        that across the United States we have a bigger problem and we need to figure out how we can stop these differences.")
    )
  )
)

#PAGE 4
page4_ui <- fluidPage(
titlePanel("State Obesity Comparison"),
sidebarLayout(
  sidebarPanel(
    checkboxGroupInput("comparison_variable", "Choose a Variable:",
                 choices = unique(compare_data$Question), selected = NULL),
  br(),
  actionButton("compareButton", "Compare")  
  ),
mainPanel(
  plotOutput("comparisonPlot"),
  h3("Michigan vs California"),
  p("Click any question or questions and hit compare, then you can switch between plots to see 
    how these different lifestyles differ between California and Michigan students."),
  p("Here is an example of two States on across from each other that have very different 
    results when looking at the total amount of students doing these habits, in the data
    set we can see the category of obesity using the range given by the CDC is low for California
    but high for Michigan."),
  p( "You can see this impact of how these questions differ while looking at these plots to the specific questions,
     coming from states that are in two different categories.")
     )
    )
  )

#PAGE 5
page5_ui <- fluidPage(
  titlePanel("Conclusion"),
  mainPanel(
    HTML('<center><img src = "https://dynaimage.cdn.cnn.com/cnn/c_fill,g_auto,w_1200,h_675,ar_16:9/https%3A%2F%2Fcdn.cnn.com%2Fcnnnext%2Fdam%2Fassets%2F211222102206-weight-scale-doctors-office-stock.jpg"
         width = "400"></center>'),
    h3("Takeaway 1: The Different States"),
    p("Going back to Michigan vs California example, as we seen in past years
      Michigan has had some problems acessing safe and clean water not only that
      but have many food deserts in lower socioeconimic areas. Food deserts are 
      where there isn't any nearby grocery stores that can give families access to healthy food, 
      which as a result increases the consumption of fast-food or cheap meals that end up
      contribute to obesity rates. Compared to other states like California, that are considered on the 
      Low side of obesity prevalence, they are already at a disadvantage. Michigan isn't the only state 
      that has this but states that are classified for lowersocioecomic families all go through these struggles. 
      As a society we should be implementing more access in these neighborhoods to give people proper access to take
      care of themselves in a healthy way."),
  h3("Takeaway 2: How this affects the youth"),
  p("Obesity is a problem that as a country we face, but its specifically affecting our chidlren,
    after looking at this data set we can see high levels of children that are already facing obesity
    if we don't change this it can lead to their adulthood and can be responsible for so many health condition
    that are causing pre-mature deaths."),
  h3("Limitations"),
  p("One drawback our dataset had was that there wasn't as much information about race and gender
    that we could have implemented into our observation. Also since our data set didn't show recent years
    we would've loved to compare how these rates of obesity compared to recent years such as 2021 and so on to see
    whether as a community if we are still struggling or in a way have gotten better.")
  )
)


common_server <- function(input, output){

  
  #PAGE 2
  filtered_data1 <- reactive({
  filter(cali_data, Question == input$question)
  }) 
  
  output$barPlot <- renderPlot({
    req(input$plotButton)
    ggplot(filtered_data1(), aes(x = Grade, y = Data_Value, fill = Grade)) + 
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = c("9th" = "blue", "10th" = "green", "11th" = "red", "12th" = "yellow")) +
    labs(title = paste("Distribution for", input$question), 
    coord_cartesian(xlim = c(10, 50)))
    })

  #PAGE 3
  output$barChart <- renderPlot({
    selected_state <- input$State
    filtered_data2 <- ob[ob$State == selected_state, ]

  ggplot(filtered_data2, aes(x = State, y = Prevalence_in_the_States)) +
  geom_bar(stat = "identity", fill = "pink", width = 0.5) +
  labs(title = paste("Prevalence in", selected_state),
  x = "State",
  y = "Prevalence") + 
    coord_cartesian(ylim = c(0, 4100))
  })

 #PAGE 4
  output$comparisonPlot <- renderPlot({
    req(input$compareButton)
    
    if(length(input$comparison_variable) == 0) {
      return(NULL)
    }
    
    filtered_data4 <- filter(compare_data, Question == input$comparison_variable)
    
    ggplot(filtered_data4, aes(x = State, y = Data_Value, fill = State)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(title = paste("Comparison of", input$comparison_variable, "between California and Michigan"), 
           coord_cartesian(xlim = c(10, 50)))
  })
}

shinyApp(
  ui = navbarPage(
        title = "Obesity Rates on Youth in the US",
         tabPanel("Introduction", page1_ui),
         tabPanel("California Obesity Based on Grade", page2_ui),
         tabPanel("State Obesity Prevalance", page3_ui),
         tabPanel("State Obesity Comparison", page4_ui),
         tabPanel("Conclusion", page5_ui)
         ),
         server = common_server
      )
