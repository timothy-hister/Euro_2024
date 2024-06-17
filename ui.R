credentials <- data.frame(
  user = c("euro", "shinymanager"), # mandatory
  password = c("analytics", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

ui = page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  title = "Euro 2024",
  sidebar = list(
    img(src='logo.jpg'),
    virtualSelectInput("players", "Choose Your Players", choices = players$name, selected = players$name, multiple = T, width = "100%", dropboxWrapper = "body"),
    sliderTextInput("as_of_game", "Choose The Game Number", choices = 0:last_game, selected = last_game),
    #p("For the supremely dorky of you..."),
    pickerInput("teams", "Choose Your Teams", choices = all_teams, selected = all_teams, multiple = T, options = pickerOptions(container = "body"), width = "100%"),
    pickerInput("locations", "Choose Your Locations", choices = all_locations, selected = all_locations, multiple = T, options = pickerOptions(container = "body"), width = "100%")
    ),
  navset_card_underline(
    title = "",
    nav_panel("Standings", withSpinner(reactableOutput("standings_tbl"))),
    nav_panel("Fun Graph",
      card(
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            virtualSelectInput("graph_player", "Choose Your Player to See", choices = players$name, selected = filter(players, name == sample(players$name, 1))$name, multiple = F, width = "100%", dropboxWrapper = "body")
          ),
          withSpinner(girafeOutput("graph"))
        )
      )
    ),
    nav_panel("Games", withSpinner(reactableOutput("games_tbl")))
  )
)

if (params$authenticate) ui = secure_app(ui)