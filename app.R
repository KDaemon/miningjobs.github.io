
library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  h1(style = "text-align: center;","Mining For Jobs"),
  br(),
  br(),
  
  fluidRow(
            column(3, offset = 3,
                   textInput(inputId = "job", label = "Enter Job Title")),
            column(3,
                    textInput(inputId = "city", label = "Enter your ZIP Code")),
            column(1, 
                    actionButton(style = "position: relative; top: 25px",inputId = "go", 
                                 label = "Update"))
            ),
  wellPanel(
            fluidRow(
            column(4, offset = 1, dataTableOutput("table"),
                      p(style = "height: 500px; width: 300px")),
            column(6,plotOutput("barPopLang"),
                      p())
          )),
  plotOutput("barPopLangDiffZip"),
  p(),
  plotOutput("barSalary"),
  p()
)

server <- function(input,output){
  
  observeEvent(input$go,{
    jobTitle <- input$job
    cityState <- input$city
    
    jobTitle <- gsub(" ","-",jobTitle)
    webSite <-  paste("https://www.monster.com/jobs/search/?q=",jobTitle,"&where=",cityState,"&intcid=skr_navigation_nhpso_searchMain&sort=rv.dt.di", sep="")
    
    cat(webSite)
    
    Monster <- read_html(webSite)
    results <- Monster %>% html_text()
    url_pattern  <- "http[s]?://job-openings(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    
    x <- str_extract_all(results,url_pattern)
    x <- unlist(x)
    
    job <- c()
    count <- 1
    for( i in 1:length(x)){
      
      jobWebpage <- tryCatch(read_html(x[i]), error=function(e)e)
      if(inherits(jobWebpage, "error"))next
      
      jobHTML <- jobWebpage %>% html_nodes("div") %>% html_text()
      jobHTML <- unlist(jobHTML)
      jobHTML <- tryCatch(toupper(jobHTML[1]), error=function(e)e)
      if(inherits(jobHTML, "error"))next
      
      job[count] <- jobHTML
      count <- count+1
    }
    keywordsTable <- matrix(c(rep(0,15)), nrow=15, ncol = 1, byrow= TRUE)
    dimnames(keywordsTable) <- list( c("C++", "C", "C#", "Java", "JavaScript",
                                       "HTML", "PHP", "SQL", "COBOL", "FORTRAN", "Visual Basic", ".NET",
                                       "Ruby", "Perl", "Python"), c("How Many Times"))
    keywords <- c(" C++ ", " C ", " C# ", " Java ", " JavaScript ",
                  " HTML ", " PHP ", " SQL ", " COBOL ", " FORTRAN ", " Visual Basic ", " .NET ",
                  " Ruby ", " Perl ", " Python ")
    keywords <- toupper(keywords)
    
    for (r in 1:length(job)){
      for(l in 1:length(keywords)){
        if(str_detect(job[r], keywords[l])){
          cat("In Job ", r, " Keyword", keywords[l], "was detected \n")
          keywordsTable[l] <- keywordsTable[l] + 1
        }
      }
    }
    ####################################################################################
    output$table <- renderDataTable({
     keywordsTable
    })
    output$barPopLang <- renderPlot({
      key <- data.frame(keywordsTable)
      ggplot(key) + geom_bar(aes(keywords, fill=keywords), position = "fill") +
        labs(x = "Programming Languages", y = "Frequency", fill = "Programming Language", title = "Languages Employers are looking for") +
        theme_classic()
    })
  })
  
}
shinyApp(ui,server)