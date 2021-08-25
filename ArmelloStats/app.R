library(shiny)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(scales)

# DEFINE CLANS
AllClans_short <- data.frame(
    "Bandit" = c("Scarlet", "Twiss", "Horace", "Sylas"),
    "Bear" = c("Sana", "Brun", "Ghor", "Yordana"),
    "Wolf" = c("Thane", "River", "Magna", "Fang"),
    "Rabbit" = c("Amber", "Barnaby", "Elyssia", "Hargrave"),
    "Rat" = c("Mercurio", "Zosha", "Griotte", "Sargon"),
    "Dragon" = c("Volodar", "Nazar", "Oxana", "Agniya")
)
AllClans <- pivot_longer(
    AllClans_short,
    cols = 1:6,
    names_to = "Clan",
    values_to = "Hero"
)

ClanNames <- unique(AllClans$Clan)

## DEFINE CLAN COLOURS
ClanColours <-
    set_names(c(
        '#464646',
        '#33a02c',
        '#1f78b4',
        '#ffc425',
        '#e41a1c',
        '#6a3d9a'
    ),
    c(unique(AllClans$Clan)))

## CREATE LABELS FOR VICTORY TYPES
VictoryNames <- c(
    "Spirit.Stone" = "Spirt Stone",
    "DefeatKing" = "Defeat the King",
    "Prestige" = "Prestige",
    "Rot" = "Rot"
)

VictoryColours <-
    set_names(c('#1f78b4', '#e31a1c',  '#33a02c', '#6a3d9a'),
              c(names(VictoryNames)))

## RING COLOURS
AllRings <-
    c(
        "Turquoise",
        "Black Opal",
        "Sulfur",
        "Jade",
        "Amethyst",
        "Taaffeite",
        "Diamond",
        "Firestone",
        "Spinel",
        "Celestite",
        "Pink Topaz",
        "Tanzanite",
        "Chrysocolla",
        "Emerald",
        "Serendibite",
        "Ruby",
        "Rubellite",
        "Tremolite",
        "Amber",
        "Cat's Eye",
        "Aquamarine",
        "Cinnabar",
        "Rainbow Quartz",
        "Moonstone",
        "Obsidian",
        "Onyx",
        "Sunstone",
        "Quartz"
    )

RingColours <-
    set_names(
        c(
            '#40E0D0',
            '#464646',
            '#edff21',
            '#00a86b',
            '#9966cc',
            '#ffd700',
            '#c0c0c0',
            '#b22222',
            '#ff2349',
            '#adcae6',
            '#ff7d94',
            '#800080',
            '#6ee96e',
            '#50c878',
            '#00a86b',
            '#ff0000',
            '#ff0000',
            '#340034',
            '#ff4500',
            '#8b4513',
            '#7fffd4',
            '#8b0000',
            '#add8e6',
            '#c0c0c0',
            '#79577C',
            '#464646',
            '#ffae49',
            '#c0c0c0'
        ),
        AllRings
    )

### AMULET COLOURS
AllAmulets <- c("Think", "Feel", "Scratch", "Soak", "Discipline", "Sprint", "Resist", "Decay",
                "Watch", "Spoil", "Ruin", "Listener", "Intimidate", "Favour")

AmuletColours <- set_names(
    c( colorRampPalette(brewer.pal(9, "Set1"))(14)), ## 14 STANDS FOR TOTAL AMULETS
    AllAmulets
)


# Define UI for data upload app ----
ui <- navbarPage(
    title = "Your Armello Games Visualized",
    
    tabPanel(
        title = "Victories Overview",
        # Sidebar panel for first tab ----
        sidebarPanel(
            width = 3,
            
            helpText(
                "The 'StratAssist' Data assistance tool needs to be downloaded first. Instructions can be find ",
                a("here.",
                  href = "https://steamcommunity.com/app/290340/discussions/0/1696040635913061106/")
            ),
            helpText("This creates a .csv files that you can upload below."),
            
            # Input: Select a file ----
            fileInput(
                "UserData",
                "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            
            # Horizontal line ----
            tags$hr(),
    
            
            # Add credits paragraph ----
            p(
                "Credits go to ",
                em("Dolop O'Dog"),
                " for creating the StratAssist tool. To ",
                em("Toxic Toast"),
                "for fine-tuning this app."
            ),
            
            # Shiny credits
            p("Made with", a("Shiny",
                             href = "http://shiny.rstudio.com"),
              "."),
            img(
                src = "ShinyImage.png",
                width = "50px",
                height = "50px"
            )
        ),
        
        
        # Output: Data file ----
        mainPanel(
            width = 9,
            plotlyOutput("contents",
                         width = "auto",
                         height = "650px"))
    ),
    tabPanel(
        "Rings and Amulets",
        # Sidebar panel for second tab ----
        sidebarPanel(
            h2( "Hero Stats",
                align = "Left"),
            width =  2,
            # Select Hero
            selectInput(
                "selectedHero",
                "Select your hero",
                choices = sort(AllClans$Hero),
                multiple = FALSE,
                selected = "Amber"
            )
        ),
        # Output: Data file ----
        mainPanel(
            width = 10,
            
            fluidRow(
                column(6,
                       plotlyOutput("RingWon")),
                column(6, 
                       plotlyOutput("RingAll")),
                column(6,
                       plotlyOutput("AmuletWon")),
                column(6, 
                       plotlyOutput("AmuletAll"))
                      )
                  ))
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
            filter(!`Game Mode` == "Prologue" &
                       !`Local Outcome` == "DidNotFinish") %>%
            select(-grep("Steam", colnames(df))) %>%
            pivot_longer(
                cols = `P1 Init Type`:`P4 Rot`,
                names_to = "Player",
                values_to = "Outcome"
            ) %>%
            separate(
                col = "Player",
                into = c('Player', 'A'),
                sep = " ",
                extra = "merge"
            ) %>%
            pivot_wider(names_from = A, values_from = Outcome) %>%
            rename_all(list( ~ make.names(.))) %>%
            filter(Init.Type == "Local") %>%
            left_join(y = AllClans, by = "Hero") %>%
            mutate_at(
                vars(
                    Game.Mode, Match.Mode, Victory.Type, Local.Outcome, Player,
                    Init.Type, End.Type, Hero, Hero.Skin, Dice.Skin, Amulet, Ring, Clan
                ),
                factor
            ) %>%
            mutate(
                `Victory.Type` = recode(`Victory.Type`, `BanishKing` = "Spirit.Stone")
            )
    })
    
    ## FRIST PLOT
    output$contents <- renderPlotly({
        ggplotly(
            ggplot(
                data = df() %>% filter(Local.Outcome == "Won")
                ) +
                geom_histogram(
                    aes(
                    x = Hero, fill = Victory.Type
                ), stat = "count"
                ) +
                facet_wrap(facets = vars(Clan), ncol = 2, scales = "free_x") +
                theme_bw() +
                theme(axis.title.x = element_blank(),
                      legend.position = "top") +
                scale_fill_manual(
                    values = VictoryColours,
                    labels = VictoryNames
                ) +
                ggtitle(
                    "Number of Games Won per Victory Type",
                    subtitle = paste("Overall Win Rate =", round(
                        nrow(df() %>% filter(Local.Outcome == "Won")) / nrow(df()) * 100, 2
                    ), "%")
                ) +
                scale_y_continuous("Number of Games Won",
                                   expand = expansion(mult = c(0, 0.1)))

            )%>%
            
            layout(title = list(
                text = paste0(
                    'Number of Games Won per Victory Type',
                    '<br>',
                    '<sup>',
                    paste("Overall Win Rate =", round(
                        nrow(df() %>% filter(Local.Outcome == "Won")) / nrow(df()) * 100, 2
                    ), "%"),
                    '</sup>'
                )
            ),
            legend = list(orientation = "h", x = 0.49, y = 1.125),
            margin=list(t=90, b=50)
            )
    })
    
    Hero.df <- reactive({
        df() %>% filter(Hero == input$selectedHero)
    }
    )


    output$RingWon <- renderPlotly({
        
        Ring_Yaxe <- Hero.df() %>%
                group_by(Victory.Type) %>%
                count()
        Ring_Yaxe <- max(Ring_Yaxe$n)
        
        ggplotly(
            ggplot(data = Hero.df() %>% filter(Local.Outcome == "Won")) +
                geom_histogram(aes_string(x = 'Victory.Type', fill = 'Ring'), stat = "count") +
                theme_bw() + theme(legend.position = "none") +
                scale_fill_manual(values = RingColours) +
                scale_x_discrete("Victory types", drop = FALSE) +
                scale_y_continuous(name = "Number of Games Won", limits = c(0, Ring_Yaxe),
                                   breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
                ggtitle("Number of Games Won per Victory Type")
        ) %>%
            layout(title = list(
                text = paste0(
                    Hero.df()$Hero[1],
                    ': Wins per victory type and ring',
                    '<br>',
                    '<sup>',
                    paste("Overall Win Rate =", round(
                        nrow(Hero.df() %>% filter(Local.Outcome == "Won")) / nrow(Hero.df()) * 100, 2
                    ), "%"),
                    '</sup>'
                )
            ))
    })
    
    output$RingAll <- renderPlotly({
        ggplotly(
            ggplot(data = Hero.df()) +
                geom_histogram(
                    aes_string(x = 'Victory.Type', fill = 'Ring'), 
                    stat = "count") +
                theme_bw() +
                theme(legend.position = "none") + 
                scale_fill_manual(values = RingColours) +
                scale_x_discrete("Victory types", drop = FALSE) +
                scale_y_continuous(name = "", 
                                   breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
                ggtitle("All Games")
        ) %>%
            layout(title = list(
                text = paste0(
                    Hero.df()$Hero[1],
                    ': All Games, including losses',
                    '<br>',
                    '<sup>',
                    paste("Total games played =", round(nrow(Hero.df()))),
                    '</sup>'
                )
            )
            )
    })
    
    ### AMULET PLOTS
    output$AmuletWon <- renderPlotly({
        
        Ring_Yaxe <- Hero.df() %>%
            group_by(Victory.Type) %>%
            count()
        Ring_Yaxe <- max(Ring_Yaxe$n)
        
        ggplotly(
            ggplot(data = Hero.df() %>% filter(Local.Outcome == "Won")) +
                geom_histogram(aes_string(x = 'Victory.Type', fill = 'Amulet'), stat = "count") +
                theme_bw() + theme(legend.position = "none") +
                scale_fill_manual(values = AmuletColours) +
                scale_x_discrete("Victory types", drop = FALSE) +
                scale_y_continuous(name = "Number of Games Won", limits = c(0, Ring_Yaxe),
                                   breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
                ggtitle("Number of Games Won per Victory Type")
        ) %>%
            layout(title = list(
                text = paste0(
                    Hero.df()$Hero[1],
                    ': Wins per victory type and amulet'
                )
            ))
    })
    
    output$AmuletAll <- renderPlotly({
        ggplotly(
            ggplot(data = Hero.df()) +
                geom_histogram(
                    aes_string(x = 'Victory.Type', fill = 'Amulet'), 
                    stat = "count") +
                theme_bw() +
                theme(legend.position = "none") + 
                scale_fill_manual(values = AmuletColours) +
                scale_x_discrete("Victory types", drop = FALSE) +
                scale_y_continuous(name = "", 
                                   breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
                ggtitle("All Games")
        ) %>%
            layout(title = list(
                text = paste0(
                    Hero.df()$Hero[1],
                    ': All Games, including losses',
                    '<br>',
                    '<sup>',
                    paste("Total games played =", round(nrow(Hero.df()))),
                    '</sup>'
                )
            )
            )
    })
    
}
# Run the app ----
shinyApp(ui, server)