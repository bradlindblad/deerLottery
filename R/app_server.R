#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 

app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  output$map <- leaflet::renderLeaflet({
    
    
    color <- input$color_by
   
    
    lottery <- lottery %>% 
      dplyr::filter(license_type == input$tag_type)
    
    pal <- leaflet::colorNumeric("Blues",reverse = F,
                             lottery[[color]]
                             
     )
    
   
    labels <- sprintf(
      "<strong>%s</strong>",
      lottery$unit
    ) %>% lapply(htmltools::HTML)
    
    
    
  
    
    leaflet::leaflet(lottery) %>% 
      leaflet::addProviderTiles("Stamen.Toner") %>% 
      leaflet::addPolygons(weight = 2,
                           layerId = ~unit,
                           fillOpacity  = 0.9,
                           label = labels,
      highlight = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        # dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE),
      fillColor = ~pal(lottery[[color]])) %>% 
      
      leaflet::addLegend(pal = pal, values = ~lottery[[color]], opacity = 0.9, title = NULL, 
                      position = "bottomright")
      
      
  })
  
  
  output$n_applicants <- renderText({

    event <- input$map_shape_click    
   
      
    val <- lottery$unit[lottery$unit == event$id]
    
    
      
    fig <- lottery %>%
      dplyr::filter(license_type == input$tag_type) %>% 
      dplyr::filter(unit == val) %>%
      dplyr::select(total_applicants) %>%
      dplyr::pull(total_applicants)
    
    paste("Total Applicants: ", fig)

  })

  
  
  output$odds_curve <- renderPlot({
    
    event <- input$map_shape_click    
    
    
    val <- lottery$unit[lottery$unit == event$id]
    
    
    
    fig <- lottery %>%
      dplyr::filter(license_type == input$tag_type) %>% 
      dplyr::filter(unit == val) 
    
    # to_plot <- lottery %>% 
    #   dplyr::filter(license_type == "Any Antlered Deer") %>% 
    #   dplyr::filter(unit == "2B")
    # 
    to_plot <- fig %>%
      tidyr::gather(pt0, pt1, pt2, pt3, pt4, pt5, pt6, pt7, pt8, pt9, key = "pts", value = "odds") %>%
      dplyr::select(unit, pts, odds) %>%
      dplyr::filter(odds > 0)

    ggplot2::ggplot(to_plot, ggplot2::aes(pts, odds)) +
      ggplot2::geom_area() +
      ggplot2::geom_line(group = 1) +
      ggplot2::theme_minimal()
    
  })
  
  # 
  # output$distPlot2 <- renderPlot({
  #   dist <- switch(
  #     input$obs2,
  #     norm = rnorm,
  #     unif = runif,
  #     lnorm = rlnorm,
  #     exp = rexp,
  #     rnorm
  #   )
  #   
  #   hist(dist(500))
  # })
  # 
  # output$data <- renderTable({
  #   mtcars[, c("mpg", input$variable), drop = FALSE]
  # }, rownames = TRUE)
  # 
  # 
 
# PROXY -------------------------------------------------------------------

  observe({
    event <- input$map_shape_click
    output$selected_unit <- renderText(lottery$unit[lottery$unit == event$id])

  })


 
  
}

