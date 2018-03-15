library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

mjr <- read.csv("demo_data_update.csv", stringsAsFactors = FALSE)

mjr_res <- filter(mjr, Case.Type == "Resolved")
mjr_comp <- filter(mjr, Case.Type == "Completed")

theme_mjr <- function(){
  theme(axis.text = element_text(size = 12))+
    theme(plot.title = element_text(size = 16))+
    theme(axis.title = element_text(size = 14))+
    theme(legend.text = element_text(size = 12))+
    theme(legend.title = element_text(size = 12))
}

tc_time <- summarize(group_by(mjr, TC_name), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                     avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                     avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), avg_arraign_res = mean(arraign_to_res, na.rm = TRUE),
                     avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE))

inst_time <- summarize(group_by(mjr, Installation), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                     avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                     avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), avg_arraign_res = mean(arraign_to_res, na.rm = TRUE),
                     avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE))

time_stack <- summarize(mjr, avg_roi_pref=mean(roi_to_pref),avg_pref_hear = mean(pref_to_hear, na.rm = TRUE),
                        avg_hear_ref = mean(hear_to_ref, na.rm=TRUE), avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign),
                        avg_arraign_res = mean(arraign_to_res, na.rm = TRUE), avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE),
                        avg_total_time = mean(total_time), avg_days_court = mean(Days.Court))

time_gather <- tidyr::gather(time_stack, Stage, Days, c(avg_roi_pref:avg_days_court))
time_gather$Stage <- factor(time_gather$Stage, levels = c("avg_roi_pref", "avg_pref_hear","avg_hear_ref", "avg_pref_ref",
                                                          "avg_ref_arraign", "avg_arraign_res", "avg_arraign_adj", "avg_total_time",
                                                          "avg_days_court"))


tc_stack_all <- summarize(group_by(mjr, TC_name),
                          avg_roi_pref=mean(roi_to_pref), avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign),
                          avg_arraign_res = mean(arraign_to_res, na.rm = TRUE), avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE))

# replace the arraign-adj and arraign-res with arraign-to-end variable
tc_stack_all$arraign_to_end <- NA

for(i in 1:nrow(tc_stack_all)){
  tc_stack_all$arraign_to_end[i] <- mean(c(tc_stack_all$avg_arraign_adj[i],tc_stack_all$avg_arraign_res[i]))
}

tc_stack_all <- tidyr::gather(tc_stack_all, Stage, Days, c(avg_roi_pref:avg_ref_arraign, arraign_to_end))
tc_stack_all$Stage <- factor(tc_stack_all$Stage, levels = c("avg_roi_pref", "avg_pref_ref","avg_ref_arraign","arraign_to_end"))




shinyServer(function(input, output) {
 
  ######### Overview section #########
    output$stacked_tc <- renderPlot({
      ggplot(tc_stack_all, aes(x = TC_name, y = Days, fill = Stage))+
        geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
        theme_mjr()+
        guides(fill = guide_legend(reverse = TRUE))+
        scale_fill_brewer(palette = "Set1", labels = c("Report of Incident\n  to Preferral", "Preferrral to Referral",
                                                           "Referral to Arraignment", "Arraigment to \nResolution or Adjournment"))+
        ggtitle("All Cases:\nAverage Processing Time by Trial Counsel") +
        theme(axis.title.x = element_blank())
  })

  output$stage_days <- renderPlot({
    ggplot(data = time_gather, aes(x = Stage, y = Days))+
      geom_bar(stat = "identity", fill = "#6699CC")+
      theme_mjr()+
      scale_x_discrete(labels = c("Report of Incident\n to Preferral", "Preferral to Hearing", "Hearing to Referral", "Preferral to Referral",
                                  "Referral \nto Arraignment","Arraignment \nto Resolution", "Arraignment \nto Adjournment","Total Time",
                                  "Days in Court"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
      ylab("Days")+
      ggtitle("Average Number of Days by Courts Martial Stage")
    })
  
  output$inst_cases <- renderPlot({
    cases <- summarize(group_by(mjr, Installation, Pilot), num_cases = n())
    
    ggplot(data = cases, aes(x = reorder(Installation, desc(num_cases)), y = num_cases, fill = Pilot))+
      geom_col()+
      theme_mjr()+
      theme(axis.title.y = element_blank())+
      ggtitle("Number of Cases per Installation") +
      xlab("Installation")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
      labs(fill = "Pilot Location?")+
      scale_fill_brewer(palette = "Set1", breaks = c("Yes", "No"))
  })
  
  ##### By Crime Type Section ###########
  output$stage_days_crime <- renderPlot({
    crime_stack <- summarize(group_by(mjr, Crime.Type), avg_roi_pref=mean(roi_to_pref),avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), 
                             avg_hear_ref = mean(hear_to_ref, na.rm=TRUE), avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), 
                             avg_arraign_res = mean(arraign_to_res, na.rm = TRUE), avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE),
                             avg_total_time = mean(total_time), avg_days_court = mean(Days.Court))
    
    
    crime_gather <- tidyr::gather(crime_stack, Stage, Days, c(avg_roi_pref:avg_days_court))
    
    crime_gather$Stage <- factor(crime_gather$Stage, levels = c("avg_roi_pref", "avg_pref_hear","avg_hear_ref", "avg_pref_ref",
                                                                "avg_ref_arraign", "avg_arraign_res", "avg_arraign_adj", "avg_total_time",
                                                                "avg_days_court"))
    
    ggplot(data = crime_gather, aes(x = Stage, y = Days))+
      geom_bar(stat = "identity", fill = "#6699CC")+
      theme_mjr()+
      scale_x_discrete(labels = c("Report of Incident to Preferral", "Preferral to Hearing", "Hearing to Referral", "Preferral to Referral",
                                  "Referral to Arraignment","Arraignment to Resolution", "Arraignment to Adjournment","Total Time",
                                  "Days in Court"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
      ylab("Average Number of Days")+
      ggtitle("Average Time for Court Martial Stages")+
      facet_grid(.~Crime.Type)
  })
  
  output$inst_cases_crime <- renderPlot({
    cases_crime <- summarize(group_by(mjr, Installation, Pilot, Crime.Type), num_cases = n())
    
    ggplot(data = cases_crime, aes(x = reorder(Installation, desc(num_cases)), y = num_cases, fill = Pilot))+
      geom_col()+
      theme_mjr()+
      theme(axis.title.y = element_blank())+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
      ggtitle("Number of Cases per Installation") +
      xlab("Installation")+
      labs(fill = "Pilot Location?")+
      scale_fill_brewer(palette = "Set1",breaks = c("Yes","No"))+
      facet_grid(.~Crime.Type)
  })

    ######### By TC Section #######
  output$myplot <- renderPlot({
    yName <- switch(input$yAxis,
                    "Total Days - RoI to Resolution/Adjournment" = "avg_total",
                    "RoI to Preferral" = "avg_roi_pref",
                    "Preferral to Hearing" = "avg_pref_hear",
                    "Hearing to Referral" = "avg_hear_ref",
                    "Preferral to Referral" = "avg_pref_ref",
                    "Referral to Arraignment" = "avg_ref_arraign",
                    "Arraignment to Resolution" = "avg_arraign_res",
                    "Arraignment to Adjournment" = "avg_arraign_adj",
                    "Days in Court" = "avg_days_court")
    if(input$case_type =="Completed"){
      if(input$yAxis == "Arraignment to Resolution"){
        # No Resolution in Completed Cases
        ggplot(mjr, aes(x = Installation, y = ID))+
          ggtitle("CASE TYPE MISMATCH")+
          theme(axis.title.y = element_blank())+
          theme(plot.title = element_text(hjust = .5, size =20))
        
      }else{
        tc_time_comp <- summarize(group_by(mjr_comp, TC_name), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                                  avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                                  avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), 
                                  avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE))
        
        ggplot(data = tc_time_comp, aes_string(x = "TC_name", y = yName)) +
          geom_col(fill = "#6699CC") +
          theme_mjr()+
          xlab("Trial Counsel")+
          ylab("Average Number of Days")+
          ggtitle(paste("Completed Cases: ", input$yAxis)) 
      }
      
    }else if(input$case_type == "Resolved"){
      if(input$yAxis == "Arraignment to Adjournment"){
        # No Adjournment in Resolved Cases
        ggplot(mjr, aes(x = Installation, y = ID))+
          ggtitle("CASE TYPE MISMATCH")+
          theme(axis.title.y = element_blank())+
          theme(plot.title = element_text(hjust = .5, size =20))
      }else{
        tc_time_res <- summarize(group_by(mjr_res, TC_name), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                                 avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                                 avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), 
                                 avg_arraign_res = mean(arraign_to_res, na.rm = TRUE))
        
        ggplot(data = tc_time_res, aes_string(x = "TC_name", y = yName)) +
          geom_col(fill = "#6699CC") +
          theme_mjr()+
          xlab("Trial Counsel")+
          ylab("Average Number of Days")+
          ggtitle(paste("Resolved Cases: ", input$yAxis)) 
      }
      
    }else{
      ggplot(data = tc_time, aes_string(x = "TC_name", y = yName)) +
        geom_col(fill = "#6699CC") +
        theme_mjr()+
        xlab("Trial Counsel")+
        ylab("Average Number of Days")+
        ggtitle(paste("Completed and Resolved Cases: ", input$yAxis)) 
    }
    
    
  })
  
  
  ###### TC Table ####
  output$tc_number_table <- renderTable(align = 'c',{
    if(input$tc_number_check==TRUE){
      numCase <- summarize(group_by(mjr, TC_name, Case.Type), total = n())
      numCase <- tidyr::spread(numCase, Case.Type, total)
      numCase$Completed[is.na(numCase$Completed)] <- 0
      numCase$Resolved[is.na(numCase$Resolved)] <- 0
      numCase$Total = numCase$Completed + numCase$Resolved
      numCase
    }
  })
  ######### Installation Chart and Table ##########
  output$myplot2 <- renderPlot({
    yName2 <- switch(input$yAxis_inst,
                    "Total Days - RoI to Resolution/Adjournment" = "avg_total",
                    "RoI to Preferral" = "avg_roi_pref",
                    "Preferral to Hearing" = "avg_pref_hear",
                    "Hearing to Referral" = "avg_hear_ref",
                    "Preferral to Referral" = "avg_pref_ref",
                    "Referral to Arraignment" = "avg_ref_arraign",
                    "Arraignment to Resolution" = "avg_arraign_res",
                    "Arraignment to Adjournment" = "avg_arraign_adj",
                    "Days in Court" = "avg_days_court")
    if(input$case_type_inst =="Completed"){
      
      if(input$yAxis_inst == "Arraignment to Resolution"){
        # No Resolution in Completed Cases
        ggplot(mjr, aes(x = Installation, y = ID))+
          ggtitle("CASE TYPE MISMATCH")+
          theme(axis.title.y = element_blank())+
          theme(plot.title = element_text(hjust = .5, size =20))
        
      }else{
        inst_time_comp <- summarize(group_by(mjr_comp, Installation), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                                    avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                                    avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), 
                                    avg_arraign_adj = mean(arraign_to_adjourn, na.rm = TRUE))
        
        ggplot(data = inst_time_comp, aes_string(x = "Installation", y = yName2)) +
          geom_col(fill = "#6699CC") +
          theme_mjr()+
          xlab("Installation")+
          ylab("Days")+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
          ggtitle(paste("Completed Cases: ", input$yAxis_inst)) 
      }
       
      
    }else if(input$case_type_inst == "Resolved"){
      if(input$yAxis_inst == "Arraignment to Adjournment"){
        # No Adjournment in Resolved Cases
        ggplot(mjr, aes(x = Installation, y = ID))+
          ggtitle("CASE TYPE MISMATCH")+
          theme(axis.title.y = element_blank())+
          theme(axis.text = element_text(size = 12))+
          theme(plot.title = element_text(size = 14))
      }else{
        
        inst_time_res <- summarize(group_by(mjr_res, Installation), num_cases = n(), avg_days_court = mean(Days.Court), avg_total = mean(total_time),
                                   avg_roi_pref=mean(roi_to_pref), avg_pref_hear = mean(pref_to_hear, na.rm = TRUE), avg_hear_ref = mean(hear_to_ref, na.rm=TRUE),
                                   avg_pref_ref = mean(pref_to_ref), avg_ref_arraign = mean(ref_to_arraign), 
                                   avg_arraign_res = mean(arraign_to_res, na.rm = TRUE))
        
        ggplot(data = inst_time_res, aes_string(x = "Installation", y = yName2)) +
          geom_col(fill = "#6699CC") +
          theme_mjr()+
          xlab("Installation")+
          ylab("Days")+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
          ggtitle(paste("Resolved Cases: ", input$yAxis_inst)) 
      }
      
      
    }else{
      ggplot(data = inst_time, aes_string(x = "Installation", y = yName2)) +
        geom_col(fill = "#6699CC") +
        theme_mjr()+
        xlab("Installation")+
        ylab("Days")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+
        ggtitle(paste("All Cases: ", input$yAxis_inst)) 
    }
  })
  output$inst_number_table <- renderTable(align = 'c',{
    if(input$inst_number_check==TRUE){
      numCase <- summarize(group_by(mjr, Installation, Case.Type), total = n())
      numCase <- tidyr::spread(numCase, Case.Type, total)
      numCase$Completed[is.na(numCase$Completed)] <- 0
      numCase$Resolved[is.na(numCase$Resolved)] <- 0
      numCase$Total = numCase$Completed + numCase$Resolved
      numCase
    }
  })
  
  
  
  
})  #end of shinyServer function