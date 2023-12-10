library(shiny)
library(dplyr)
library(ggplot2)

source("datajoin.R")


ui <- navbarPage(inverse = TRUE, "Music and Mental Health",
  tabPanel(
    "Welcome Page",
    
    fluidPage(
      titlePanel("Music and Mental Health"),
      tags$style(HTML("
        body {
          font-family: Palatino;
        }
         h3 {
          font-size: 24px;
        }
        h4 {
          font-weight: bold;
          font-size: 18px;
        }
        p {
          font-size: 14px; 
        }
        
      ")),
      
      br(),
      
      p(h4("Hi there! Welcome to our fantastic exploration of the correlation between music and mental health.")),
      
      br(),
      p("Music is a popularly adopted form of expression that resonates with listeners,
  and it has a healing power that provides a safe outlet for people’s stress and influences their moods.
  Through our investigation into individuals' music streaming choices and psychological conditions,
  we have come to the conclusion that a clear connection exists between the two."),
      p("Understanding the therapeutic importance of music can help you better navigate yourself in
      this fast-paced society. We will also explore the effects of other factors, such as age and listening time, on the music-mental health relation.
      This exploration will equip you with potentially useful knowledge to protect your mind during hard times!"),
      br(),
      p(h5("If you would like to learn more about music and mental health, feel free to check out the link below.")),
      Music_therapy_link <- a("Music Therapy", href="https://my.clevelandclinic.org/health/treatments/8817-music-therapy"),
      br(),
      br(),
      p(h4("Now, click around to walk through our explorations!")),

    )
  ),
  
  
  tabPanel(
    "It Varies With Age",
    fluidPage(
      p(h4("First of all, let's take a look at how music impacts mental health across different age groups.")),
        
      br(),
      
      p("When you select an age range, you will be able to see the proportion of experienced impact
        reported by individuals within that age group."),
      
      
      sidebarLayout(
        sidebarPanel(style = "background: #2b2b2b",
                     wellPanel(
                       selectInput(inputId = "age_category", label = "Select an age category", choices = reordered_age)
                     ),
                     wellPanel(
                       h4("Description"),
                       textOutput("improve_perc")
                     ),
                     
        ),
        
        
        mainPanel(
          plotOutput("pie_chart")
        )
      ),
      
      p("By exploring the chart from the smallest age group to the largest, you will notice a clear U-shaped trend.
      The age group under 20 has the highest reported proportion of experiencing improved mental health conditions
      through music listening, followed by a drop in this proportion in the age group from 30 to 40, and ended with
      a rise of this proportion in the two age groups after that."),
      p("This means that, while music is generally effective in improving mental health condition, it is least effective
        among people of 30 to 40."),
      
      br(),
    ),
  
   
  ),
  tabPanel(
    "Genre Matters",
    fluidPage(
      p(h4("You might wonder: does the music genre matter? Yes, but not significantly.")),
      br(),
      p("When you select a music genre, you will be able to see the proportion of experienced impact
        reported by individuals who listen to that genre."),
      
      sidebarLayout(
        sidebarPanel(
          style = "background: #2b2b2b",
          wellPanel(
            selectInput("musicGenre", "Select a music genre", choices = genre_names, selected = genre_names[1])),
        wellPanel(
          h4("Description"),
          textOutput("proportion_description"),
        )
        ),
        
        mainPanel(
          plotOutput("bar_chart")
        )
      ),
      br(),
      p("The music genre doesn't significantly impact its effect on mental health. What does that imply?"),
      p("Bad news: no single genre stands out as particularly effective in healing your mind. Sorry if you were 
        expecting a magic genre."),
      p("Good news: no single genre stands out, but they all work very well! And this trend unvariously applies to both moderate listening and frequent listening"),
      br(),
    )
  ),
  

  tabPanel(
    "Time Length of Listening is Not Vital",
    fluidPage(
      p(h4("Many people might think that different length of listening time will 
     have significantly different effects on mental health. However, this is not the case.")),
      br(),
      
      p("Zoom in (select the shortest time length) to see details of listening time vs. negative mental health
                level, and zoom out (select the longest time length) to have a better big picture of the graph."),
      br(),
      
      sidebarLayout(
        sidebarPanel(style = "background: #2b2b2b",
          wellPanel(
            h5("The time length of listening to music in a day"),
            sliderInput(
              inputId = "listen_time",
              label = "",
              min = 0,
              max = 24,
              value = 24
            )
          ),
        
        ),
        mainPanel(
          plotOutput(outputId = "scatter"),
        )
      ),
      
      br(),
      p("Counterintuitively, the dispersed scatter points across the graph indicate that the time length of listening to music
                does not have a distinctive effect on individuals' mental health condition. Listening
                to music for 5 hours is really not that different from listening to music for 30 minutes, in terms of how well
                it improves your mental health level!"),
      p("However, through this chart, we also found out that listening to music for over
        10 hours a day might harm your mental health. You can see how the dots after 10 hours are located at higher places on the graph,
        indicating that high negative mental health level is directly associated with listening to music for too long. Be careful, don't get addicted
        although you love music!"),
      br(),
    )
  )
)



server <- function(input, output) {
  output$scatter <- renderPlot({
    filt_df <- filter(df, Hours.per.day <= input$listen_time)
    scatter <- ggplot(filt_df, aes(x = Hours.per.day, y = negative.mental.level, color = genres)) +
      geom_point() +
      labs(
        x = "The time length of listening to music in a day",
        y = "Negative mental health level",
        title = "Negative Mental Health Level vs. Time length of listening",
        color = "Music genre"
      )
    plot(scatter)
  })
  
  
  output$pie_chart <- renderPlot({
    df_new <- df[df$Music.effects != "",]
    filtered_df <- filter(df_new, Age_category == input$age_category)
    counts <- table(filtered_df$Music.effects)
    category_percentages <- prop.table(counts) * 100
    data <- data.frame(Category = names(counts), Count = as.numeric(counts))
    
    pie_chart <- ggplot(data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = sprintf("%.1f%%", category_percentages)), position = position_stack(vjust = 0.5)) +
      coord_polar("y") +  
      theme_void() +  
      labs(title = paste("Proportion of the Effects on Mental Health Reported by Age Group", input$age_category), fill = "Effects on mental health")+
      scale_fill_manual(values = c("Improve" = "#a9a6ea", "No effect" = "#afbbcb", "Worsen" = "#edabba"))
    plot(pie_chart)
  })
    
  
  
  output$bar_chart <- renderPlot({
    df_new <- df[df$Music.effects != "",]
    df_selected <- df_new[, c(input$musicGenre, "Music.effects")]
    df_selected <- subset(df_selected, !df_selected[, 1] %in% c("Never", "Rarely"))
    
    ggplot(df_selected, aes(x = df_selected[, 1], fill = Music.effects)) +
      geom_bar(position = "stack") +
      labs(title = paste("The Effect of",input$musicGenre,"Music on Mental Health"),
           x = paste("Frequency of listening to", input$musicGenre,"music"),
           y = "Report counts",
           fill = "Effect on mental health") +
      scale_fill_manual(values = c("Improve" = "#a9a6ea", "No effect" = "#afbbcb", "Worsen" = "#edabba")) +
      theme_minimal() +
    scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50))
  })
  
  output$proportion_description <- renderText({
    df_new <- df[df$Music.effects != "",]
    df_selected <- df_new[, c(input$musicGenre, "Music.effects")]
    df_selected <- subset(df_selected, !df_selected[, 1] %in% c("Never", "Rarely"))
    df_selected_improve_proportion <- sum(df_selected[, 2] == "Improve") / nrow(df_selected)
    
    proportion_text <- paste("The proportion of",input$musicGenre,
    "music listeners that reported improvement in mental health is at", 
                             sprintf("%.2f%%", df_selected_improve_proportion * 100))
    
    return(proportion_text)
  })
  
  improve_p <- reactive({
    filter_df <- filter(df_new, Age_category == input$age_category)
    improve_df <- filter(filter_df, Music.effects == "Improve")
    percen <- (nrow(improve_df))/(nrow(filter_df))*100
    return(percen)
  })
  
  output$improve_perc <- renderText({
    percen <- improve_p()
    paste("Among individuals in age group from",input$age_category,"years old",round(percen, 1),",% think that music listening has improved their mental health.")
  })
  
}



shinyApp(ui = ui, server = server)
