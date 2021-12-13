




shinyServer <- function(input, output , session) {
  
  rv <- reactiveValues()
  
  ### read data file 
  
  data <- reactive({
    occ<-read.csv("code/downloaded_data.csv")
    return(occ)
  })
  
  ### Display data summary
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$structure <- renderPrint({
    str(data())
  })
  
  ### Display data table
  
  output$contents <- renderDT(
    data(), options = list(lengthChange = TRUE,scrollX = TRUE)
  )
  
  
  
  ### Display data introduction
  
  output$data_structure <-  renderDataTable({
    p1 <- data.frame(introduce(data()))
    intro_table <- data.frame(attr = names(p1), value = transpose(p1))
    return(intro_table)
  })
  
  ### page selection for distribution plot
  
  output$page_selection <- renderUI({
    p <- plot_bar(data())
    selectInput("page_number", "Select page",
                names(p),
                selected= '')
  })
  
  ### column selection for distribution plot
  
  output$bi_variate_selection <- renderUI({
    selectInput("bi_variate_column", "Select a Variable to break-out the frequencies",
                names(select_if(data(),(is.character))),
                selected= NULL)
  })
  
  ### Display distribution plot 
  
  output$data_distribution_2 <-  renderPlot({
    
    withProgress(message = 'Generating plot', value = 0, {
      
      col <- input$bi_variate_column
      p4 <- plot_bar(data(), by = col)
      p4[input$page_number]
    })
  })
  
  ### column selection for map plot
  
  output$map_col_selection <- renderUI({
    selectInput("map_column", "Select a Variable",
                c("Forests of Australia 2013","Order"),
                selected= "Forests of Australia 2013")
  })
  
  ### Display map-plot
  
  output$map_plot <-  renderPlotly({
    withProgress(message = 'Generating plot', value = 0, {
      req(input$map_column)
      act<-ozmaps::ozmap_states %>% 
        filter(NAME == "Australian Capital Territory" | NAME=="Other Territories")
      
      col <- if (input$map_column == 'Forests of Australia 2013') 'cl10902' else 'order'
      
      a1<-ggplot() + 
        geom_sf(data = act, mapping = aes(), show.legend = FALSE) +
        geom_point(data = subset(data(),col!=""),
                   mapping = aes_string(x = 'decimalLongitude',
                                        y = 'decimalLatitude', colour = col), size=0.5) +
        coord_sf(xlim=c(148.8,150.8), ylim = c(-36,-35)) +
        scale_color_discrete(name = input$map_column)
      
      a1 <- ggplotly(a1)
      
      rv$map_plot <- a1
      
      return(a1)
    })
  })
  
  
  ### column selection for distribution plot
  
  output$dist_col_selection <- renderUI({
    selectInput("dist_column", "Select a Variable",
                c("Forests of Australia 2013","Nature of records (basisOfRecord)"),
                selected= "Forests of Australia 2013")
  })
  
  ### Display distribution-plot
  
  output$dist_plot <-  renderPlot({
    withProgress(message = 'Generating plot', value = 0, {
      req(input$dist_column)
      if (input$dist_column == 'Forests of Australia 2013'){
        
        forest<-data() %>% group_by(cl10902) %>% summarise(n=n()) %>%
          filter (cl10902!="") %>% arrange(desc(n))
        
        a2<-ggplot(data= forest, aes(x=reorder(cl10902,n),y=n, fill = cl10902)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) +
          scale_y_continuous(limits = c(0,4300), expand = c(0, 0)) +
          xlab("Forests of Australia 2013") + ylab("Nº records of Reptilia")+
          theme_classic()+
          scale_fill_brewer(name = "Forest",palette = "Paired")
        return(a2)
      }else{
        typeR<-data() %>% group_by(basisOfRecord) %>% summarise(n=n())
        a2<-ggplot(data=typeR, aes(x=reorder(basisOfRecord,n),y=n,fill = basisOfRecord)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) +
          scale_y_continuous(limits = c(0,4300), expand = c(0, 0)) +coord_flip() +
          xlab("Nature of records (basisOfRecord)") +
          ylab("Nº records of Reptilia")+
          theme_classic()+
          scale_fill_brewer(name = "Forest",palette = "Paired")
        return(a2)
      }
      
      
    })
  })
  
  ### Display Facet-plot
  
  output$facet_plot <-  renderPlot({
    withProgress(message = 'Generating plot', value = 0, {
      order_family<-data() %>% group_by(order,family) %>% 
        summarise(n=n()) %>% filter(order!="" & family != "")
      #now the bar plot
      a3<-ggplot(data= order_family, aes(x=reorder(family,n),y=n,fill = family)) + 
        geom_bar(stat = "identity", width = 0.5) +
        scale_y_continuous(limits = c(0,4300), expand = c(0, 0))+ 
        coord_flip() + 
        xlab("Family taxon") +
        ylab("Nº records") + 
        geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) + 
        facet_grid(rows=vars(order), scales = "free", space = "free")+
        theme_classic()+
        scale_fill_brewer(name = "Forest",palette = "Paired")
      return(a3)
      
    })
  })
  
  ### Display Heat map
  
  output$heat_plot1 <-  renderPlot({
    withProgress(message = 'Generating plot', value = 0, {
      
      order_by_forest<-data() %>% group_by(order,cl10902) %>% 
        summarise(n=n()) %>% filter(order!="" & cl10902 !="") %>% 
        arrange(desc(n),.by_group = TRUE)
      
      a4<-ggplot(order_by_forest, aes(x=order, y=cl10902, fill= n)) + 
        geom_tile() + geom_text(aes(label = n)) + 
        xlab("Order") + 
        ylab("Forests of Australia 2013") + 
        scale_fill_gradient(low = "#e5f5f9", high = "#2ca25f", name = "Nº records") + 
        theme(axis.text=element_text(size=8), 
              axis.text.x = element_text(angle = 45, hjust = 1), 
              panel.background = element_rect(fill = 'white', color = 'lightgray'))
      return(a4)
      
    })
  })
  
  output$heat_plot2 <-  renderPlot({
    withProgress(message = 'Generating plot', value = 0, {
      
      basisR_by_rname<-data() %>% group_by(basisOfRecord,dataResourceName) %>% 
        summarise(n=n()) %>% arrange(desc(n),.by_group = TRUE)
      
      a5<-ggplot(basisR_by_rname, aes(x=basisOfRecord, y=dataResourceName, fill= n)) + 
        geom_tile() + geom_text(aes(label = n)) + 
        xlab("Nature of records (basisOfRecord)") + 
        ylab("Source of the record (dataResourceName)") + 
        scale_fill_gradient(low = "#e5f5f9", high = "#2ca25f", name = "Nº records") + 
        theme(axis.text=element_text(size=8), 
              axis.text.x = element_text(angle = 45, hjust = 1), 
              panel.background = element_rect(fill = 'white', color = 'lightgray'))
      return(a5)
      
    })
  })
  
  observeEvent(input$report,{
    #print(rv$map_plot)
    print(input$format)
    # tempReport <- file.path(tempdir(), "report.Rmd")
    # file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
  })
  
}