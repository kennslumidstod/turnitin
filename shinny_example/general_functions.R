#By Pétur Stefánsson

library("ggplot2")
library("tidyverse")
library(reshape2)
library(plotly)

#fall semtekur dataframe(df) og nafn á dálki (dname) við 3-em dálkum Ar,Manudur og Skolaar
add_skolaar_to_dataframe <- function(df,dname) {
  df <- mutate(df, Ar = as.numeric(substring(!!as.name(dname), 0, 4)), Manudur = as.numeric(substring(!!as.name(dname), 6, 7)), .before = dname)
  df <- mutate(df, Skolaar = if_else(Manudur <= 8, paste((Ar - 1), "-", Ar, sep=""), paste(Ar, "-", (Ar + 1), sep = "")), .before = Ar)
  return (df)
}

#fall sem tekur two strengi sem eru skólaár.d "2015-2016" og "2019-20202 og birtir öll skólár á milli þeira
get_skolar_in_range <- function(minschoolyear, maxschoolyear) {
  minyear <- as.numeric(substring(minschoolyear, 0, 4))
  maxyear <- as.numeric(substring(maxschoolyear, 6,10))
  return (paste(seq(minyear,maxyear-1),"-",seq(minyear+1,maxyear), sep = ""))
}

#fall sem tekur dataframe(df) fyrsta tölfræði dálk (first_num_col) og seinni tölfræðidálk(sec_num_col), lista af dálkum og strengja vector tekur síðan saman alla tölfræðidálka milli tveggja tölfræði dálkana og er skipplagður eftir þar sem dálkalistinn hefur ekki sömu gildi dálkarnir munu síðan fá nafn sem strengja vectorinn inniheldur
month_to_schoolyear <- function(df, first_num_col, sec_num_col, col_list, col_list_names) {
  return (setNames(aggregate(df[which(first_num_col == names(df)):which(sec_num_col == names(df))], by=col_list, FUN=sum, na.rm=TRUE),append(col_list_names, names(df[which(first_num_col == names(df)):which(sec_num_col == names(df))]))))
}

#fall hugsað fyrir dataframe sem hefur lesið classaskrá, masterclassaskrá eða samaneiða af þveim tveimur og skilar dataframe sem telur hversu margir tímar eru með fleiri en tíu skil annars eru parametrarnir þeir sömu og í month_to_schoolyear fallinu
#first_name_col verður að vera dálkurinn fyrir fjölda submissiona
get_yearly_stats_for_class <- function(df,first_num_col,sec_num_col,col_list, name_list) {
  
  eval_String <- paste(deparse(substitute(df)),"$",col_list, ",", collapse=" ", sep = "")
  eval_list <- eval(parse(text = paste0("list(", substr(eval_String,0, (nchar(eval_String)-1)), ")") ))
  df_agg_bf_sub <- month_to_schoolyear(df, first_num_col, sec_num_col, append(eval_list ,list(df$Class.Name)), append(name_list, "Class.Name"))
  
  df_agg_bf_sub$Class.Name <- 1
  df_agg_bf_sub <- filter(df_agg_bf_sub, Submissions >= 10)
  
  eval_String <- paste("df_agg_bf_sub","$",col_list, ",", collapse=" ", sep = "")
  eval_list <- eval(parse(text = paste0("list(", substr(eval_String,0, (nchar(eval_String)-1)), ")") ))
  
  df_agg_af_sub <- month_to_schoolyear(df_agg_bf_sub, "Class.Name", sec_num_col, eval_list, name_list)
  colnames(df_agg_af_sub)[which("Class.Name" == names(df_agg_af_sub))] <- "Active.classes"
  
  return (df_agg_af_sub)
}

#fall sem er með sömu og get_yearly_stats_for_class og reiknar hversu margir kennarar eru í dataframe 
#colist verður að innihalda Instructor.ID
get_yearly_stats_for_instructors <- function(df,first_num_col,sec_num_col,col_list, name_list) {
  
  eval_String <- paste(deparse(substitute(df)),"$",col_list, ",", collapse=" ", sep = "")
  eval_list <- eval(parse(text = paste0("list(", substr(eval_String,0, (nchar(eval_String)-1)), ")") ))
  df_agg_bf_sub <- month_to_schoolyear(df, first_num_col, sec_num_col, append(eval_list ,list(df$Instructor.ID)), append(name_list, "Instructor.ID"))
  
  df_agg_bf_sub$Instructor.ID <- 1
  
  eval_String <- paste("df_agg_bf_sub","$",col_list, ",", collapse=" ", sep = "")
  eval_list <- eval(parse(text = paste0("list(", substr(eval_String,0, (nchar(eval_String)-1)), ")") ))
  
  df_agg_af_sub <- month_to_schoolyear(df_agg_bf_sub, "Instructor.ID", sec_num_col, eval_list, name_list)
  colnames(df_agg_af_sub)[which("Instructor.ID" == names(df_agg_af_sub))] <- "Active.Instructors"
  
  return (df_agg_af_sub)
}

#fall sem leggur saman dálk eftir öðrum dálki
get_agg_dataframe_for_dep <- function(agg_data, agg_group, df_ordered=NULL) {
  agg_df <- setNames(aggregate(agg_data, by=list(agg_group), FUN=sum, na.rm=TRUE), c("svid","data"))
  if (!is.null(df_ordered) && df_ordered == TRUE)
  {
    agg_df <- agg_df[order(agg_df$data, decreasing = TRUE),]
  }
  
  return (agg_df)
}

#fall sem leggur saman lista af dálkum eftir lista af öðrum dálkum 
get_agg_dataframe_list_from_col_for_dep <- function(col_list,name_list,group_name,df_melted=NULL )
{
  df <- setNames(aggregate(col_list, by=list(group_name), FUN=sum), gsub("\\.", " ", append("svid", name_list)))
  
  if (!is.null(df_melted) && df_melted == TRUE)
  {
    df <- melt(df, id= "svid")
  }
  
  return (df)
}

#fall sem birtir plotly súlurit þar sem df er dataframe x_val dálkar dataframe-sem eiga að vera x-megin á súluritinu, y_val, y-megin, name_val sem fer í legend ritsins og x_name hvað xás á að heita og y_name hvað y-ás á að heita
plotly_barplot_with_resize <- function(df, x_val, y_val, name_val, x_name, y_name) {
  plot_ly(df, x = x_val, y = y_val, name = name_val, type = 'bar') %>%
    layout(title = "",
           xaxis = list(title = x_name),
           yaxis = list (title = y_name))
}

#fall sem birtir plotly staflap súlurit (e. stacked bar chart) þar sem df er dataframe x_val dálkar dataframe-sem eiga að vera x-megin á súluritinu, y_val, y-megin, name_val sem fer í legend ritsins og x_name hvað xás á að heita og y_name hvað y-ás á að heita
plotly_stacked_bar_chart <- function(df, x_val, y_val, name_val, x_name,y_name) {
  plot_ly(df, x = x_val, y = y_val, type = 'bar', 
          name = name_val, color = name_val) %>%
    layout(yaxis = list(title = y_name), xaxis = list(title = x_name), barmode = 'stack')
}