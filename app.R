library(shiny)
library(tidyverse)
library(plotly)

## DEFINE CLANS
AllClans_short <- data.frame(
    "Bandit" = c("Scarlet", "Twiss", "Horace", "Sylas"),
    "Bear" = c("Sana", "Brun", "Ghor", "Yordana"),
    "Wolf" = c("Thane", "River", "Magna", "Fang"),
    "Rabbit" = c("Amber", "Barnaby", "Elyssia", "Hargrave"),
    "Rat" = c("Mercurio", "Zosha", "Griotte", "Sargon"),
    "Dragon" = c("Volodar", "Nazar", "Oxana", "Agniya")
)
AllClans <- pivot_longer(AllClans_short, cols = 1:6,
                         names_to = "Clan", values_to = "Hero")

ClanNames <- unique(AllClans$Clan)

## DEFINE CLAN COLOURS
ClanColours <- set_names(c('#464646', '#33a02c', '#1f78b4', '#ffc425', '#e41a1c', '#6a3d9a'),
                         c(unique(AllClans$Clan)))

## CREATE LABELS FOR VICTORY TYPES
VictoryNames <- c("BanishKing" = "Spirt Stone Victory",
                  "DefeatKing" = "Defeat the King",
                  "Prestige" = "Prestige Victory",
                  "Rot" = "Rot Victory")

VictoryColours <- set_names(c('#1f78b4', '#e31a1c',  '#33a02c', '#6a3d9a'),
                            c(names(VictoryNames)))

## RING COLOURS
AllRings <- c("Turquoise", "Black Opal", "Sulfur", "Jade", "Amethyst", "Taaffeite", "Diamond",
              "Firestone", "Spinel", "Celestite", "Pink Topaz", "Tanzanite", "Chrysocolla",
              "Emerald", "Serendibite", "Ruby", "Rubellite", "Tremolite", "Amber", "Cat's Eye",
              "Aquamarine", "Cinnabar", "Rainbow Quartz", "Moonstone", "Obsidian", "Onyx", "Sunstone", "Quartz")  

RingColours <- set_names(c('#40E0D0', '#464646', '#edff21', '#00a86b', '#9966cc', '#ffd700', '#c0c0c0',
                           '#b22222', '#ff2349', '#adcae6', '#ff7d94', '#800080', '#6ee96e',
                           '#50c878', '#00a86b', '#ff0000', '#ff0000', '#340034', '#ff4500', '#8b4513',
                           '#7fffd4', '#8b0000', '#add8e6', '#c0c0c0', '#464646', '#464646', '#ffae49', '#c0c0c0'),
                         AllRings)


# Define UI for data upload app ----
ui <- navbarPage(title = "Your Overview of Armello Games",
                 
                 tabPanel(title = "Game Overview",
                          # Sidebar panel for first tab ----
                          sidebarPanel(
                              width = 3,
                              
                              p("The 'StratAssist' Data assistance tool needs to be downloaded first. Instructions can be find ", a("here.", 
                                                                                                                              href = "https://steamcommunity.com/app/290340/discussions/0/1696040635913061106/")),
                              p("This creates a .csv files that you can be uploaded here."),
                              
                              # Input: Select a file ----
                              fileInput("UserData", "Choose CSV File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              # X variable input
                              selectInput("Xvar", "X-axis",
                                          choices = c("Clan" = "Clan",
                                                      "Victory Type" = "Victory.Type",
                                                      "Hero" = "Hero"),
                                          selected = "Victory.Type",
                                          multiple = FALSE
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              # Group variable input
                              selectInput("groupVar", "Group by:",
                                          choices = c("Clan" = "Clan",
                                                      "Victory Type" = "Victory.Type"
                                          ),
                                          selected = "Clan",
                                          multiple = FALSE
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              p("Credits go to ",
                              em("Dolop O'Dog"), " for creating the StratAssist tool."),
                              
                             # Shiny credits
                             p("Made with", a("Shiny",
                                               href = "http://shiny.rstudio.com" ),
                                "."),
                             img(
                                  src = "ShinyImage.png",
                                  width = "50px", height = "50px"
                              )
                          ),
                          

                          # Output: Data file ----
                          mainPanel(
                              plotlyOutput("contents")
                              )
                          ),
                 tabPanel("Hero Rings and Amulets",
                          # Sidebar panel for second tab ----
                          sidebarPanel( width =  3,
                              # Select Hero
                              selectInput("selectedHero", "Select your hero",
                                          choices = sort(AllClans$Hero),
                                          multiple = FALSE,
                                          selected = "Amber"
                                       )
                                   ),
                                   # Output: Data file ----
                                   mainPanel(
                                       width = 9,
                                       fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       plotlyOutput("RingWon"), plotlyOutput("RingAll"))
                                       )
                                       
                                   )
                          )
                 )
                                  
                        
# Define server logic to read selected file ----
server <- function(input, output) {
        
        # input$UserData will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        df <- reactive({
            req(input$UserData)
            df <- read_csv(input$UserData$datapath)
            
            df %>% 
            filter(!`Game Mode` == "Prologue" & !`Local Outcome` == "DidNotFinish") %>%
            select(-grep("Steam", colnames(df))) %>%
            pivot_longer(cols = `P1 Init Type`:`P4 Rot`, names_to = "Player", values_to = "Outcome") %>%
            separate(col = "Player", into = c('Player','A'), sep = " ", extra = "merge") %>%
            pivot_wider(names_from = A, values_from = Outcome) %>%
            rename_all(list(~make.names(.))) %>%
            filter(Init.Type == "Local") %>%
            left_join(y = AllClans, by = "Hero") %>%
                mutate_at(vars(Game.Mode, Match.Mode, Victory.Type, Local.Outcome, Player, Init.Type, End.Type,
                               Hero, Hero.Skin, Dice.Skin, Amulet, Ring, Clan), factor)
        })
     
        ## FRIST PLOT   
        output$contents <- renderPlotly({
        
        ggplotly(
            ggplot(data = df() %>% filter(Local.Outcome == "Won")) +
            geom_histogram(aes_string(x = input$Xvar, fill = input$groupVar), stat = "count") + 
            theme_bw() +
            scale_fill_manual(values = c(ClanColours, VictoryColours), 
                              labels = c(ClanNames, paste(VictoryNames))) +
            ggtitle("Number of Games Won per Victory Type", 
                    subtitle = paste("Overall Win Rate =", round(nrow(df() %>% filter(Local.Outcome == "Won")) / nrow(df()) * 100, 2), "%")) + 
            ylab("Number of Games Won") + xlab(input$Xvar)
        ) %>%
                layout(title = list(text = paste0('Number of Games Won per Victory Type',
                                                  '<br>',
                                                  '<sup>',
                                                  paste("Overall Win Rate =", round(nrow(df() %>% filter(Local.Outcome == "Won")) / nrow(df()) * 100, 2), "%"),
                                                  '</sup>')))
        })
    
        Hero.df <- reactive({
            df() %>% filter(Hero == input$selectedHero)
        })
        
        output$RingWon <- renderPlotly({
                ggplotly(
                    ggplot(data = Hero.df() %>% filter(Local.Outcome == "Won")) +
                        geom_histogram(aes_string(x = 'Victory.Type', fill = 'Ring'), stat = "count") + 
                        theme_bw() + theme(legend.position = "none") +
                        scale_fill_manual(values = RingColours) +
                        ggtitle("Number of Games Won per Victory Type") +
                        ylab("Number of Games Won") + xlab("Victory Types")
                ) %>% 
                layout(title = list(text = paste0(Hero.df()$Hero[1], ': Number of Games Won per Victory Type',
                                                  '<br>',
                                                  '<sup>',
                                                  paste("Overall Win Rate =", round(nrow(Hero.df() %>% filter(Local.Outcome == "Won")) / nrow(Hero.df()) * 100, 2), "%"),
                                                  '</sup>')))   
        })
            
            output$RingAll <- renderPlotly({
                ggplotly(
                    ggplot(data = Hero.df()) +
                        geom_histogram(aes_string(x = 'Victory.Type', fill = 'Ring'), stat = "count") + 
                        theme_bw() +
                        scale_fill_manual(values = RingColours) +
                        ylab("Number of Games Won") + xlab("Victory Types")
                ) %>% 
                    layout(title = list(text = paste0(Hero.df()$Hero[1], ': All Games',
                                                      '<br>',
                                                      '<sup>',
                                                      'Including games lost',
                                                      '</sup>')))
    })
}
# Run the app ----
shinyApp(ui, server)