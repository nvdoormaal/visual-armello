#########################
## ARMELLO DATA SCRIPT ##
#########################

## REMOVE EXISTING VARIABLES
rm(list = ls())

## LOAD PACKAGES
pacman::p_load(tidyverse, plotly)

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
                           '#50c878', '#464646', '#ff0000', '#ff0000', '#340034', '#ff4500', '#8b4513',
                           '#7fffd4', '#8b0000', '#add8e6', '#c0c0c0', '#464646', '#464646', '#ffae49', '#c0c0c0'),
                         AllRings)


## IMPORT DATA
Original_Data <- read_csv("./Data/ArmelloData_20200417.csv")


## REMOVE PrOLOGUE GAMES, AND UNFINISHED GAMES
All_Games <- Original_Data %>% 
  filter(!`Game Mode` == "Prologue" & !`Local Outcome` == "DidNotFinish") %>%
  select(-grep("Steam", colnames(Original_Data)))

All_Games_Reshaped <- All_Games %>% 
  pivot_longer(cols = `P1 Init Type`:`P4 Rot`, names_to = "Player", values_to = "Outcome") %>%
  separate(col = "Player", into = c('Player','A'), sep = " ", extra = "merge") %>%
  pivot_wider(names_from = A, values_from = Outcome) %>%
  rename_all(list(~make.names(.)))

My_Games <- All_Games_Reshaped %>%
  filter(Init.Type == "Local") %>%
  left_join(y = AllClans, by = "Hero") %>%
  mutate_at(vars(Game.Mode, Match.Mode, Victory.Type, Local.Outcome, Player, Init.Type, End.Type,
                 Hero, Hero.Skin, Dice.Skin, Amulet, Ring, Clan), factor)

N_Games_Won <- nrow(My_Games %>% filter(Local.Outcome == "Won"))


## OVERALL VICTORIES
My_Games %>% 
  filter(Local.Outcome == "Won") %>%
  count(Victory.Type) %>% 
  group_by(Victory.Type)

test <- ggplot(  data = My_Games %>% filter(Local.Outcome == "Won")) +
  geom_histogram(aes(x = Hero, fill = Victory.Type), stat = "count") +
  theme_bw() + theme(axis.title.x = element_blank()) +
  facet_wrap(facets = vars(Clan), ncol = 2, scales = "free_x") +
  scale_fill_manual(values = VictoryColours) +
  scale_y_continuous("Number of Games Won", expand = expansion(mult = c(0, 0.1)))
  
#  ggtitle("Number of Games Won per Victory Type", 
#          subtitle = paste("Overall Win Rate =", round(N_Games_Won / nrow(My_Games) * 100, 2), "%")


p <- ggplotly(test)

test <- ggplot(data = My_Games %>% filter(Local.Outcome == "Won")) +
  geom_histogram(aes(x = Hero, fill = Victory.Type), stat = "count") + theme_bw() +
  scale_fill_manual(values = VictoryColours,
                    drop = FALSE) +
  ggtitle("Types of Victories per Clan", 
          subtitle = paste("Overall Win Rate =", round(N_Games_Won / nrow(My_Games) * 100, 2), "%")) + 
  ylab("Number of Games Won") + xlab("Victory Type") + 
  facet_wrap(facets = vars(Clan), scales = "free_x")

My_Games %>%
  group_by(Clan, Hero, Ring) %>%
  summarise(n = n()) %>%
  group_by(Clan, Hero) %>%
  slice(which.max(n))

ggplot(data = My_Games %>% filter(Local.Outcome == "Won")) + 
  geom_histogram(aes(x = Victory.Type, fill = Hero), stat = "count") + 
  facet_wrap(facets = "Clan") + theme_bw() + 
  ggtitle("Number of victories per clan-character")

ggplot(data = My_Games %>% filter(Local.Outcome == "Won")) +
  geom_histogram(aes(x = Clan, fill = Clan), stat = "count", colour = "black") +
  ggtitle("Number of victories per clan") +
  facet_wrap(facets = "Victory.Type", labeller = as_labeller(VictoryNames)) +
  theme_bw() + 
  scale_fill_manual(values = ClanColours)
  
## FIND BEST STRATEGIES
HeroRings <- My_Games %>% 
  filter(Local.Outcome == "Won") %>%
  count(Clan, Hero, Victory.Type) %>% 
  group_by(Clan, Hero) %>% 
  filter(n == max(n))

My_Games %>% filter(Local.Outcome == "Won" & Hero == "Sylas") %>%
  group_by(Victory.Type) %>%
  count(Ring) %>%
  ungroup %>%
  complete(Victory.Type, fill = list(n = 0)) %>%
  ggplot() +
  geom_histogram(aes(x = Victory.Type, fill = Ring), stat = "count") +
  theme_bw() + theme(legend.position = "none") +
  scale_fill_manual(values = RingColours) +
  ggtitle("Number of Games Won per Victory Type") +
  ylab("Number of Games Won") + xlab("Victory Types")

ggplot(data = My_Games %>% filter(Local.Outcome == "Won" & Hero == "Amber")) +
  geom_histogram(aes(x = Victory.Type, fill = Ring), stat = "count") +
  theme_bw() + theme(legend.position = "none") +
  scale_fill_manual(values = RingColours) +
  scale_x_discrete(drop = FALSE) +
  ggtitle("Number of Games Won per Victory Type") +
  ylab("Number of Games Won") + xlab("Victory Types")

             