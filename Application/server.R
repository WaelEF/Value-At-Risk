server <- function(input, output, session){
  
  ###Dataframe Titres
  DB = reactive({
    if(is.null(input$file1))
    {
      data=read_xlsx("Historique_Cotation_2019.xlsx", sheet = 1)
    }
    else{    data=read_xlsx(input$file1$datapath, sheet = 1)}
    
    data = subset(data,select =c("SEANCE","LIB_VAL","CLOTURE") )
    titres = unique(data$LIB_VAL)
    titres = titres[!grepl("[[:digit:]]",titres)]
    
    updateSelectizeInput(session,server = TRUE,inputId = "Choix", choices=titres,selected="AMEN BANK")
    updateSelectizeInput(session,server = TRUE,inputId = "choix_normal", choices=titres,selected="AMEN BANK")
    updateSelectizeInput(session,server = TRUE,inputId = "choix_var", choices=titres,selected="AMEN BANK")
    
    pivot = Extraction(data,titres)$db
    return(pivot)
  })
  
  ### Dataframe Variations
  DB_V = reactive({
    if(is.null(DB()))
      return (NULL)
    
    
    
    DBT = DB()
    DBT$SEANCE=NULL
    N = nrow(DBT)
    Variation <- data.frame(matrix(NA, nrow = N-1))
    Variation[1] = NULL
    k=1
    for(titre in names(DBT))
    {
      Variation[[paste0("R_",titre)]] = log( DBT[[titre]][2:(N)]) - log(DBT[[titre]][1:(N-1)])
    }
    
    return(Variation)
  })
  
  ####Debug pour input
  Portefeuille = reactive({
    if(is.null(DB()))
      return(NULL)
    
    return(Contenu(input))
  })
  
  ### Debug pour input
  output$Contenu = DT::renderDataTable({
    Contenu(input)
    
  })
  
  jours<-reactive({
    return( input$Jours)
  })
  ####Affichage mode series temporelles
  output$hcontainer <- renderHighchart({
    if(is.null(
      DB()
    ))
      return(NULL)
    
    ST_Plot(DB(),input$Choix)
    
  })
  
  ###### Affichage Contenu du Portefeuille
  output$Contenu_PF<-renderTable({
    Portefeuille()
  })
  #####QQ-PLOT
  output$qqplot <- renderPlot({
    if(is.null(DB()))
      return(NULL)
    
    dat1=data.frame(x=DB_V()[[paste0("R_",input$choix_normal)]])
    ggplot(data=dat1, aes(sample=x)) + stat_qq(geom="point",color="navy",shape=1) +
      theme_bw() + theme(text=element_text(size=15)) + xlab("Théorique")+ylab("Titre")+ggtitle("Q-Q Plot")
    
  })
  ##### Test de normalité
  output$sw <- renderTable({
    if(is.null(DB()))
      return(NULL)
    
    dat1=data.frame(x=DB_V()[[paste0("R_",input$choix_normal)]])
    norm = shapiro.test(dat1$x)
    tab = matrix(c(norm$statistic,norm$p.value),nrow=1)
    colnames(tab) = c("W statistic","p-value")
    rownames(tab) = "Data"
    tab
  })
  
  ####Histogramme individuel
  output$histograph = renderPlot({
    if(is.null(DB()))
      return(NULL)
    
    dat1=data.frame(x=DB_V()[[paste0("R_",input$choix_var)]])
    
    ggplot(data=dat1) + geom_histogram(aes(x=x), fill="blue", alpha=.5) +
      xlab(paste("Variations journalières de",input$choix_var)) + ylab("Fréquence") +
      ggtitle("Histogramme des variations journalières") + theme_bw()
  })
  
  
  ##### Histogramme Variation Portefeuille
  output$histo_portefeuille = renderPlot({
    if(is.null(DB()))
      return(NULL)
    Graphique(DB_V(),Portefeuille(),input$alpha,input$Jours)
    
  })
  
  ##### Stats Desc Indiv
  output$stats<- renderTable({
    if(is.null(DB()))
      return(NULL)
    vec=DB_V()[[paste0("R_",input$choix_var)]]
    table = t(matrix(c((as.matrix(summary(vec)[1:6])),
                       round(sd(vec,na.rm=TRUE)))))
    colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
    return(table);
  },digits = 2)
  
  
  ##################################################
  ############Affichage VaR Individuelle############
  ##################################################
  output$Monte_ind<- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"MonteCarlo",width="10",icon=icon("list")
        ,color="green")
    
    valueBox(
      paste0(round(Monte_var(DB_V(),input$choix_var,input$alpha)$var,digits=2),"%"), "Monte Carlo",width="10", icon = icon("list"),
      color = "green"
    )
  })
  output$Param_ind<- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Parametrique",width="10",icon=icon("list")
        ,color="blue")
    
    valueBox(
      paste0(round(Para_var(DB_V(),input$choix_var,input$alpha)$var,digits=2),"%"), "Parametrique",width="10", icon = icon("list"),
      color = "blue"
    )
  })
  output$Histo_ind<- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Historique",width="5",icon=icon("list")
        ,color="purple")
    
    valueBox(
      paste(round(Histo_var(DB_V(),input$choix_var,input$alpha)$var,digits=2),"%"), "Historique",width="10", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$Histo_ind_BT <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Backtesting VaR Historique",width="5",icon=icon("list")
        ,color="purple")
    
    valueBox(
      paste0(round(Histo_var(DB_V(),input$choix_var,input$alpha)$exceptions,digits=2)," Jours"), "Backtesting VaR Historique",width="10", icon = icon("list"),
      color = "purple"
    )
    
  })
  
  output$Param_ind_BT <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Exceptions VaR Paramétrique",width="5",icon=icon("list")
        ,color="blue")
    
    valueBox(
      paste0(round(Para_var(DB_V(),input$choix_var,input$alpha)$exceptions,digits=2)," Jours"), "Backtesting VaR Parametrique",width="10", icon = icon("list"),
      color = "blue"
    )
    
  })
  
  output$Monte_ind_BT <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Exceptions Monte Carlo",width="5",icon=icon("list")
        ,color="green")
    
    
    valueBox(
      paste0(round(Monte_var(DB_V(),input$choix_var,input$alpha)$exceptions,digits=2)," Jours"), "Backtesting VaR Monte Carlo",width="10", icon = icon("list"),
      color = "green"
    )
    
  })
  
  ##################################################
  ############Affichage VaR Portefeuille############
  ##################################################
  output$Histo_Portefeuille <- renderValueBox({
    if(is.null(DB()) || (input$alpha >100 || input$alpha <0))
    {
      valueBox(value=0,"Historique",width="5",icon=icon("list"),color="purple")
      return();
    }
    
    
    valueBox(
      value=round(Histo_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$var,digits=2),"Historique",width="10",icon=icon("list")
      ,color="purple")
    
  })
  output$Para_Portefeuille <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Paramétrique",width="5",icon=icon("list")
        ,color="blue")
    valueBox(
      value=round(Para_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$var,digits=2),"Paramétrique",width="10",icon=icon("list")
      ,color="blue")
  })
  
  
  output$Monte_Portefeuille <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Monte Carlo",width="5",icon=icon("list")
        ,color="green")
    valueBox(
      value=round(Monte_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$var,digits=2),"Monte Carlo",width="10",icon=icon("list")
      ,color="green")
  })
  
  
  
  
  
  
  output$Back_Histo_PF <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Exceptions VaR Historique",width="5",icon=icon("list")
        ,color="purple")
    
    valueBox(
      value=paste0(round(Histo_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$exceptions,digits=2)," Jours"),"Backtesting VaR Historique",width="10",icon=icon("list")
      ,color="purple")
    
  })
  
  output$Back_Para_PF <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Exceptions VaR Paramétrique",width="5",icon=icon("list")
        ,color="purple")
    
    valueBox(
      value=paste0(round(Para_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$exceptions,digits=2)," Jours"),"Backtesting VaR Paramétrique",width="10",icon=icon("list")
      ,color="blue")
    
  })
  
  output$Back_Monte_PF <- renderValueBox({
    if(is.null(DB()))
      valueBox(
        value=0,"Exceptions Monte Carlo",width="5",icon=icon("list")
        ,color="purple")
    
    valueBox(
      value=paste0(round(Monte_portefeuille(DB_V(),Portefeuille(),input$alpha,input$Jours)$exceptions,digits=2)," Jours"),"Backtesting VaR Monte Carlo",width="10",icon=icon("list")
      ,color="green")
    
  })
  
  
  
  
  
  
  output$intervalle1 <- renderText({ paste0( "[",qbinom(0.025,size=249,( 1 -  input$alpha/100),lower.tail=TRUE),","
                                             ,qbinom( 0.025,size=249,prob=( 1 -  input$alpha/100),lower.tail=FALSE),"]")
  })
  output$intervalle2 <- renderText({ paste0( "[",qbinom( 0.025,size=249,prob=( 1 -  input$alpha/100),lower.tail=TRUE),","
                                             ,qbinom( 0.025,size=249,prob=(1 - input$alpha /100 ),lower.tail=FALSE),"]")
  })
  
  
  ##### Script Input
  
  
  output$allInputs <- renderUI({
    titres = names(DB())[-1]
    
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    inputsToShow <- input$appendInput
    # Return if button not pressed yet
    if(is.null(inputsToShow) || inputsToShow < 1) return()
    # Initialize list of inputs
    inputTagList <- tagList()
    ValueTagList <- tagList()
    # Populate the list of inputs
    lapply(1:inputsToShow,function(i){
      # Define unique input id and label
      newInputId <- paste0("input", i)
      newInputVal <- paste0("Value", i)
      newInputLabel <- paste0("Titre ", i)
      # Prevent dynamic inputs from resetting
      newInputValue <- "Option 1"
      if (newInputId %in% names(input)) {
        newInputValue <- input[[newInputId]]
      }
      newInputValz <- 1
      if (newInputVal %in% names(input)) {
        newInputValz <- input[[newInputVal]]
      }
      # Define new input
      newInput <- selectizeInput(newInputId, newInputLabel, choices=titres, selected=newInputValue)
      ValeurTitre <-  numericInput(newInputVal, label = paste("Valeur du titre ",i), value = newInputValz )
      
      # Append new input to list of existing inputs
      inputTagList <<- tagAppendChild(inputTagList, newInput)
      inputTagList <<- tagAppendChild(inputTagList, ValeurTitre)
      
    })
    # Return updated list of inputs
    inputTagList
  })
  
  
  
  
  
  
  
}
