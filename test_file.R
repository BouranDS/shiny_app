# data_secpticemie <- 
#   utils::read.csv2(
#     file = "D:/MyR/scepticemie/sceptiemie_Donnee.csv", header = TRUE, stringsAsFactors = FALSE
#   )
# Required R packages 
require("ggplot2")
require("plotly")

# data Importation 
scepticemie <- 
  utils::read.csv2(
    file = "D:/MyR/scepticemie/sceptiemie_Donnee.csv", 
    header = TRUE, stringsAsFactors = FALSE
  )
columIndex <- base::colnames(
  scepticemie
)
select_column <- c("ID",                                                        
"UniqueKey",                                                 
"AGE_ANS"   ,                                                   
                                            
"sexe"       ,                                                  
"profession"  ,                                                 
 "RESIDENCE"  ,
 "nationalité",
"Modederecrutement",
 "activitphysique",                                              
 "N1Facteur",
 "N2Facteurs",                                                   
 "N3Facteursetplus",
 "statut"           ,                                            
 "Medicaux"   ,
 "typedepathologie1",                                            
 
 "typedepathologie"   ,                                          
 
 "typedepathologie2",                                            
 
 "typedepathologie3",                                            
 
 "typedepathologie4" ,                                           
 
 "typedepathologie5" ,
 "Issuedelamaladie" ,
 "ScepticemiqueGnrale",                                          
 "Temperature"         ,                                         
 "Tension"              ,                                        
 "TENSION_MAX"           ,                                       
 "TENSION_MIN"            ,                                      
 "RAPPORT_TENSION"         ,                                     
 "Poul"                     )  

new_scepticemie <- scepticemie[,select_column]
new_scepticemie$sexe <- stringr::str_to_title(new_scepticemie$sexe)
new_scepticemie$TENSION_MAX <- as.numeric(new_scepticemie$TENSION_MAX)
new_scepticemie$TENSION_MIN <- as.numeric(new_scepticemie$TENSION_MIN)
new_scepticemie$Poul <- as.numeric(new_scepticemie$Poul)
sexe <- base::unique(new_scepticemie$sexe)
sexe <- c(sexe, "Tous")
sexe <- stats::setNames(sexe,c("Female", "Male", "All"))
mode <- base::unique(new_scepticemie$Modederecrutement)
mode <- stats::setNames(mode, c("Consultation", "Emergency"))
mes_variable <- base::colnames(new_scepticemie)
mes_variable <- mes_variable[3:base::length(mes_variable)]
variable_names <- c("Year", "Sexe", "Occupation", "Residence", "Nationality",
                    "Recruitement_mode",  "Physical_activity", "N1_Facteur",
                    "N2_Facteurs", "N3_Facteurs_and_more", "Sepsis",
                    "Medical_aspect", "TypeOfpathology_1", "TypeOfpathology",   
                    "TypeOfpathology_2", "Typeofpathology_3", "TypeOfpathology_4",
                    "TypeOfpathology_5", "OutComeOfdisease",
                    "GeneralSepsis", "BodyTemperature", "Tension",            
                    "TensionMax", "TensionMin", "RatioOftension", "Poul"
                  )
mes_variable <- stats::setNames(mes_variable,variable_names)
## Num and cat 
select_type <- base::logical(0)
lik <- 1
for(colonne in mes_variable){
  
  select_type[lik] <- is.numeric(new_scepticemie[, colonne])
  lik <- lik + 1
}

##
functionCatvariable <- function(var_name,dataframe){
  dataouput <- base::data.frame(
                          base::table(
                            dataframe[,var_name]
                          )
  )
  colnames(dataouput) <- c("Categories", "Frequency")
  dataouput <- dataouput[order(dataouput[,"Frequency"], 
                               decreasing = TRUE),]
  return(dataouput)
}
functionBivariable <- function(var_nameX,var_nameY,dataframe){
  dataframe <- base::data.frame(dataframe)
  testXY <- c(
              is.numeric(dataframe[,var_nameX]),
              is.numeric(dataframe[,var_nameY])
              )
  # print(testXY)
#
  if(sum(testXY) == 2){
    # print("2 OK")
    result <- list()
    for(cor_type in c("pearson", "kendall", "spearman")){
      result[[paste(cor_type, "correlation", sep = " ")]] <- 
        stats::cor(dataframe[,var_nameX],
                              dataframe[,var_nameY],
                              method = cor_type,
                   use = "pairwise.complete.obs")
    }
    return(unlist(result))
  }
  if(sum(testXY) == 0){
    # print("0 OK")
    return(base::table(dataframe[,var_nameX], 
                       dataframe[,var_nameY], 
                       dnn = c(var_nameX,var_nameY) 
                      )
           )
  }
  if(sum(testXY == 1)){
    # print("1 OK")
    ikl <- which(testXY == TRUE)
    if(ikl == 1){
      # print("1 est X OK")
      return(
      #with(dataframe,{
       # unlist(
        base::tapply(X = dataframe[,var_nameX], 
                     INDEX = dataframe[,var_nameY] , FUN = summary
                     )
        #)
     # })
      )
    }else if(ikl == 2){
      # print("2 est X OK")
      return(
        
     # with(dataframe,{
        #unlist(
        base::tapply(X = dataframe[,var_nameY], 
                     INDEX = dataframe[,var_nameX], FUN = summary)
        #)
      #})
      )
    }
  }
}
plotnum <- function(vartoplot, dataframe, 
                    typeofgeom="histogram"){
  plot_num <- ggplot2::ggplot()
  if(typeofgeom == "boxplot"){
    plot_num <- 
      plot_num + 
        ggplot2::geom_boxplot(
           mapping = ggplot2::aes( 
                                  x = "",
                                  y = dataframe[,vartoplot]),
                              color = "skyblue",
           fill = "skyblue",
           alpha = 0.4,
           notch = TRUE, notchwidth = .9,outlier.shape = 19,
           outlier.color = "red", outlier.fill = "red"
                              ) +
      ggplot2::labs(
        title = "",
        caption = "Source :  Mrs. doumbouya thesis",
        x = vartoplot,
        y = "Frequency Distribution"
      )
    
  }else if(typeofgeom == "density"){
    
    plot_num <- plot_num + 
        ggplot2::geom_density(data = dataframe,
          mapping = ggplot2::aes_string(x = vartoplot),
                              color = "blue",
          fill = "skyblue", alpha = 0.4,
                              size = 2.
        )+
      ggplot2::labs(
        title = "",
        caption = "Source :  Mrs. doumbouya thesis",
        x = vartoplot,
        y = "Frequency Distribution"
      )
    
  }else{
    plot_num <- 
      plot_num + 
        ggplot2::geom_histogram(data = dataframe,
          mapping = ggplot2::aes_string(
          x = vartoplot),
                              bins = 5,
                              color = "blue",
                              fill = "skyblue"
                              
        )+
      ggplot2::labs(
        title = "",
        caption = "Source :  Mrs. doumbouya thesis",
        x = vartoplot,
        y = "Frequency Distribution"
      )
    
  }
    return(plot_num)
}
#
plotcat <- function(vartoplot, dataframe,typeofgeom="barplot"){
 
  plot_cat <- ggplot2::ggplot()+
    ggplot2::geom_bar(
      mapping = ggplot2::aes( 
        x = dataframe[,vartoplot]),
      stat = "count",
      color = "skyblue",
      fill = "skyblue",
      alpha = 0.4
    ) +
    ggplot2::labs(
      title = "",
      caption = "Source :  Mrs. doumbouya thesis",
      x = vartoplot,
      y = "Frequency Distribution"
    )
  if(typeofgeom == "barplot"){
    return(plot_cat)
  }else if(typeofgeom == "barplotother"){
    return(plot_cat + ggplot2::coord_flip())
  }else{
    plot_cat <- plot_cat + 
      ggplot2::coord_polar()
    return(plot_cat)
  }
  
}  
#
#
doubleplot <- function(varx,vary, dataframe){
  testtodo <- c(
    varx %in% mes_variable[select_type],
    vary %in% mes_variable[select_type]
  )
  plotbivariate <- ggplot2::ggplot()
  if(sum(testtodo) == 2){
    plotbivariate <- plotbivariate +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          x = dataframe[,varx],
          y = dataframe[,vary]
        ),
        size = I(2.),
        alpha = .2,
        color = "blue",
        fill = "skyblue"
        
      ) +
      ggplot2::labs(
        x = varx, y = vary,
        caption = "Source : These Mrs. Doumbouya"
      )
     
  }else if(sum(testtodo) == 0){
    plotbivariate <- plotbivariate +
      ggplot2::geom_bar(
        mapping = ggplot2::aes(
          x = dataframe[,varx],
          fill = dataframe[,vary]
        )
      )+ggplot2::theme(
        legend.title = ggplot2::element_blank()
      )+
      ggplot2::labs(
        x = varx, y = "Frequency",
        caption = "Source : These Mrs. Doumbouya"
      )
    
  }else if(sum(testtodo) == 1){
    lik <- which(testtodo == TRUE)
    
      plotbivariate <- plotbivariate +
        ggplot2::geom_boxplot(
          mapping = ggplot2::aes(
            x = dataframe[,varx],
            y = dataframe[,vary]
          ),
        color = "skyblue",
        fill = "skyblue",
        alpha = 0.4,
        notch = TRUE, notchwidth = .9,outlier.shape = 19,
        outlier.color = "red", outlier.fill = "red") +
        ggplot2::labs(
          x = varx, y = vary,
          caption = "Source : These Mrs. Doumbouya"
        )
  }
  return(plotbivariate)
}
#plot_cat
# "Consommationdalimentconservlongtempsdansles48havantlamaladie1"
# "ATCDdetoxiinfectionalimentairedansles48h"                     
# "FacteursdeclenchantdeladernireTIA"                            
# "Modederecrutement"                                            
#                                                       
#                      
# "Mnarche"                                                      
# "cycle"                                                        
# "Dure"                                                         
#                                                        
# "Troubledelaconscience"                                        
# "obnubulation"                                                 
# "Coma"                                                         
# "Cachexie"                                                     
#                                                     
#                                                         
# "typedepathologie1"                                            
#                                             
# "typedepathologie"                                             
#                                        
# "typedepathologie2"                                            
#                                         
# "typedepathologie3"                                            
#                                           
# "typedepathologie4"                                            
#                                            
# "typedepathologie5"                                            
# "Tauxdhemoglobine"                                             
# "VGM"                                                          
# "CCHM"                                                         
# "LCR"                                                          
# "Goutteepaisse"                                                
#                                                   
#                                                
# "Duredhospitalisation"                                         
# "Issuedelamaladie"                                             
#                                                     
# "Paludismegraveformeanmique"                                   
# "VIH"                                                          
# "Meningiteformesceptique"                                      
# "ScepticemiqueGnrale"                                          
# "Temperature"                                                  
# "Tension"                                                      
# "TENSION_MAX"                                                  
# "TENSION_MIN"                                                  
# "RAPPORT_TENSION"                                              
# "Poul"                                                         
# "NbredeLCR"                                                    
# "NbredeECBU"                                                   
# "NbredeTROPHO"                                                 
# "NbredeCV"                                                     
# "Nbredebateries"
