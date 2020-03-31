#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shinyMobile::f7Page(
      title = "My app",
      shinyMobile::f7TabLayout(
        panels = tagList(
          shinyMobile::f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover")
          # shinyMobile::f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
        ),
        navbar = shinyMobile::f7Navbar(
          title = "ND Deer Lottery Odds",
          hairline = FALSE,
          shadow = TRUE,
          left_panel = TRUE,
          right_panel = TRUE
        ),

        shinyMobile::f7Tabs(
          animated = TRUE,
          # swipeable = TRUE,  shinyMobile::f7Tabs(
          # animated = TRUE,
          # swipeable = TRUE,


          # LEAFLET -----------------------------------------------------------------



          shinyMobile::f7Tab(
            tabName = "Map",
            icon = shinyMobile::f7Icon("map"),
            active = TRUE,


            shinyMobile::f7Card(
              title = "What do you want to see?",
              shinyMobile::f7Select(
                inputId = "color_by",
                label = "Shade the map with: ",
                choices = c(
                  "total_applicants", "total_available_for_resident_drawing", "pt0", "pt1",
                  "pt2", "pt3", "pt4", "pt5", "pt6", "pt7", "pt8", "pt9"
                )
              ),

              tags$br(),

              shinyMobile::f7Select(
                inputId = "tag_type",
                label = "Which type of tag: ",
                choices = c("Antlered Mule Deer", "Antlerless Mule Deer", "Any Antlered Deer", "Any Antlerless Deer", "Antlered White-tailed Deer", "Antlerless White-tailed Deer")
              )
            ),




            shinyMobile::f7Shadow(
              intensity = 10,
              hover = TRUE,
              shinyMobile::f7Card(
                title = "Unit Map",
                print("This app lets you explore the results of the 2019 North Dakota Deer Gun Lottery. While the 
                      tag counts and odds do change every year, the prior year is the best indicator for your luck in the next year in a given unit."),



                textOutput("selected_unit"),

                # selectInput("yours", choices = c("", lottery@data$UNIT_ID), label = "Select Unit:"),
                # sliderInput("obs1", "Number of observations", 0, 1000, 500),
                leaflet::leafletOutput("map"),
                footer = tagList(
                  shinyMobile::f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                  shinyMobile::f7Badge("Badge", color = "green")
                )
              )
            ),


            shinyMobile::f7Shadow(
              intensity = 10,
              hover = TRUE,
              shinyMobile::f7Card(
                title = "Unit Stats",
                
                textOutput("n_applicants"),

                plotOutput("odds_curve")
                

                # footer = tagList(
                #   shinyMobile::f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                #   shinyMobile::f7Badge("Badge", color = "green")
                )
              )
            )
          ),
          shinyMobile::f7Tab(
            tabName = "Tab 2",
            icon = shinyMobile::f7Icon("today"),
            active = FALSE,
            shinyMobile::f7Shadow(
              intensity = 10,
              hover = TRUE,
              shinyMobile::f7Card(
                title = "Card header",
                shinyWidgets::prettyRadioButtons(
                  "obs2",
                  "Distribution type:",
                  c(
                    "Normal" = "norm",
                    "Uniform" = "unif",
                    "Log-normal" = "lnorm",
                    "Exponential" = "exp"
                  ),
                  inline = TRUE,
                  status = "warning",
                  animation = "pulse"
                ),
                plotOutput("distPlot2"),
                footer = tagList(
                  shinyMobile::f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                  shinyMobile::f7Badge("Badge", color = "green")
                )
              )
            )
          ),
          shinyMobile::f7Tab(
            tabName = "Tab 3",
            icon = shinyMobile::f7Icon("cloud_upload"),
            active = FALSE,
            shinyMobile::f7Shadow(
              intensity = 10,
              hover = TRUE,
              shinyMobile::f7Card(
                title = "Card header",
                shinyWidgets::prettyCheckboxGroup(
                  "variable",
                  "Variables to show:",
                  c(
                    "Cylinders" = "cyl",
                    "Transmission" = "am",
                    "Gears" = "gear"
                  ),
                  inline = TRUE,
                  status = "danger",
                  animation = "pulse"
                ),
                tableOutput("data"),
                footer = tagList(
                  shinyMobile::f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
                  shinyMobile::f7Badge("Badge", color = "green")
                )
              )
            )
          )
        )
      )
    )
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "deerLottery"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
