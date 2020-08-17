#By Pétur Stefánsson

library("ggplot2")
library("tidyverse")
library(reshape2)
library(plotly)

#### plot milli ara ####

# rit sem birtir fjölda kennara eftir skólaárum í stærðarröð (order_by_size) eða ekki
kennarar_milli_ara_bar_plot_df_plottly  <- function(df, sk_year, order_by_size=NULL, use_years=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$`Active Instructors`, hi$Ar, TRUE)
    y_name <- "Ár"
    
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$`Active Instructors`, hi$Skolaar, TRUE)
    y_name <- "Skóla áe"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
}

# rit sem birtir fjölda tíma  eftir skólaárum í stærðarröð (order_by_size) eða ekki
namsked_kend_milli_ara_barplot_df_plottly <- function(df, sk_year, order_by_size=NULL, use_years = NULL)
{
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Active.classes, hi$Ar, TRUE)
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Active.classes, hi$Skolaar, TRUE)
    y_name <- "Skóla ár"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }

  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, "Svið", "Fjöldi")
}

# rit sem birtir heildar feedback eftir skólaárum í stærðarröð (order_by_size) eða ekki
total_feedback_milli_ara_bar_plot_df_plotty <- function(df, sk_year, order_by_size=NULL, use_years = NULL, colname_spaced=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    
    hi <- filter(df,Ar %in% sk_year)
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      barplot_df <- get_agg_dataframe_for_dep(hi$`Total Feedback Comments and Marks`, hi$Ar, TRUE)
      
    } else {
      barplot_df <- get_agg_dataframe_for_dep(hi$Total.Feedback..Comments.and.Marks., hi$Ar, TRUE)
    }
    
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      barplot_df <- get_agg_dataframe_for_dep(hi$`Total Feedback Comments and Marks`, hi$Skolaar, TRUE)
      
    } else {
      barplot_df <- get_agg_dataframe_for_dep(hi$Total.Feedback..Comments.and.Marks., hi$Skolaar, TRUE)
    }
    y_name <- "Skóla ár"
  }
  
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
   
}

# rit sem birtir fjölda verkefna skila  eftir skólaárum í stærðarröð (order_by_size) eða ekki
total_submissins_milli_skola_ara_bar_plot_df <- function(df, sk_year, order_by_size=NULL, use_years = NULL) {
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Submissions, hi$Ar, TRUE)
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Submissions, hi$Skolaar, TRUE)
    y_name <- "Skóla ár"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
 
}

# rit sem birtir fjölda simmilarity reports  eftir skólaárum í stærðarröð (order_by_size) eða ekki
total_symilarity_reports__milli_skola_ara_df <- function(df, sk_year, order_by_size=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Similarity.Reports, hi$Ar, TRUE)
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    barplot_df <- get_agg_dataframe_for_dep(hi$Similarity.Reports, hi$Skolaar, TRUE)
    y_name <- "Skóla ár"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
  
}

# rit sem birtir var farið yfir verkefni  eftir skólaárum í stærðarröð (order_by_size) eða ekki
types_of_submissions_milli__skola_ara_df_plotly <-  function(df, sk_year, order_by_size=NULL, use_years = NULL, type= NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    aggr <- setNames(aggregate(hi$Submissions.Scored.with.a.Rubric, by=list(hi$Ar), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
    aggf <- setNames(aggregate(hi$Submissions.with.Feedback, by=list(hi$Ar), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    aggr <- setNames(aggregate(hi$Submissions.Scored.with.a.Rubric, by=list(hi$Skolaar), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
    aggf <- setNames(aggregate(hi$Submissions.with.Feedback, by=list(hi$Skolaar), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
    y_name <- "skóla ár"
  }
  
  stacked_barplot_df <- melt(merge(aggr,aggf), id= "Account.Name")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    total_types_df <- setNames(data.frame(Account.Name <- aggr$Account.Name, data <- (aggr$Submissions.Scored.with.a.Rubric + aggf$Submissions.with.Feedback)), c("Account.Name", "data"))
    total_types_df <- total_types_df[order(total_types_df$data, decreasing = TRUE),]
    total_types_df$Account.Name <- factor(total_types_df$Account.Name, levels= total_types_df$Account.Name)
    stacked_barplot_df$Account.Name <- factor(stacked_barplot_df$Account.Name, levels= total_types_df$Account.Name)
  }
  
  plotly_stacked_bar_chart(stacked_barplot_df, stacked_barplot_df$Account.Name, stacked_barplot_df$value, stacked_barplot_df$variable, y_name, "Fjöldi")
}

# rit sem birtir tegundir endurgjafar  eftir skólaárum í stærðarröð (order_by_size) eða ekki
feedback_skolar_milli_skola_ara_bar_plots_df_plotly <- function(df, sk_year, order_by_size=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))], names(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))]), hi$Ar, df_melted=FALSE)
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))], names(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))]), hi$Skolaar, df_melted=FALSE)
    y_name <- "Skóla ár"
  }
  
  barplot_df <- melt(ag_total, id="svid")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    
    ag_total$total <- rowSums(ag_total[,2:10])
    ag_total[2:10] <- NULL
    ag_total <- ag_total[order(ag_total$total, decreasing = TRUE),]
    ag_total$svid <- factor(ag_total$svid, levels= ag_total$svid)
    barplot_df$svid <- factor(barplot_df$svid, levels = ag_total$svid)
  }
  plotly_stacked_bar_chart(barplot_df, barplot_df$svid, barplot_df$value, barplot_df$variable, y_name, "Fjöldi")
  
}

# rit sem birtir hversu lík % verkefni var fyrri verkefnum skilað í tutnitin  eftir skólaárum í stærðarröð (order_by_size) eða ekki
skolar_similarity_bar_plots_eftir_skola_arum_df_plotly <- function(df, sk_year, order_by_size=NULL, use_years = NULL, colname_spaced=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))], names(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))]), hi$Ar, df_melted=FALSE)
    } else {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))], names(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))]), hi$Ar, df_melted=FALSE)
    }
    y_name <- "Ár"
    
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))], names(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))]), hi$Skolaar, df_melted=FALSE)
    } else {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))], names(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))]), hi$Skolaar, df_melted=FALSE)
    }
    y_name <- "Skóla ár"
  }
  
  
  colnames(ag_total)[2] <- "0%"
  colnames(ag_total)[3] <- "1-24%"
  colnames(ag_total)[4] <- "25-49%"
  colnames(ag_total)[5] <- "50-74%"
  colnames(ag_total)[6] <- "75-100%"
  
  barplot_df <- melt(ag_total, id="svid")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    
    ag_total$total <- rowSums(ag_total[,2:6])
    ag_total[2:6] <- NULL
    ag_total <- ag_total[order(ag_total$total, decreasing = TRUE),]
    ag_total$svid <- factor(ag_total$svid, levels= ag_total$svid)
    barplot_df$svid <- factor(barplot_df$svid, levels = ag_total$svid)
  }
  plotly_stacked_bar_chart(barplot_df, barplot_df$svid, barplot_df$value, barplot_df$variable, y_name, "Fjöldi")
}

# rit sem birtir tegundir Integration  eftir skólaárum í stærðarröð (order_by_size) eða ekki
skipting_a_intergration_milli_skola_ara_bar_plot_df_plotly <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
    agg <- setNames(aggregate(hi$`Active Classes`, by=list(hi$Ar,hi$Integration), FUN=sum), c("ar","Integration","active.classes"))
    y_name <- "Ár"
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
    agg <- setNames(aggregate(hi$`Active Classes`, by=list(hi$Skolaar,hi$Integration), FUN=sum), c("ar","Integration","active.classes"))
    y_name <- "Skóla ár"
  }
 
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    aggTotal <- setNames(aggregate(agg$active.classes, by=list(agg$ar), FUN=sum), c("ar","total"))
    aggTotal <- aggTotal[order(aggTotal$total, decreasing = TRUE),]
    aggTotal$ar <- factor(aggTotal$ar, levels= aggTotal$ar)
    agg$ar <- factor(agg$ar, levels = aggTotal$ar)
  }
  plotly_stacked_bar_chart(agg, agg$ar, agg$active.classes, agg$Integration, y_name, "Fjöldi")
}
#### plot eftir skolum/svidum####
# rit sem birtir fjölda tíma  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
total_active_classes_eftir_skolum_df <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    barplot_df <- get_agg_dataframe_for_dep(hi$Active.classes, hi$Parent.Account.Name, TRUE)
    y_name <- "Skólar"
    
  } else {
    barplot_df <- get_agg_dataframe_for_dep(hi$Active.classes, hi$Account.Name, TRUE)
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }

  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
}

# rit sem birtir fjölda kennara  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
total_active_instructors_eftir_skolum_df <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    barplot_df <- get_agg_dataframe_for_dep(hi$`Active Instructors`, hi$`Parent Account Name`, TRUE)
    y_name <- "Skólar"
    
  } else {
    barplot_df <- get_agg_dataframe_for_dep(hi$`Active Instructors`, hi$`Account Name`, TRUE)
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }

  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
}

# rit sem birtir fjölda endurgjafar  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
total_feedback_eftir_skolum_bar_plot_df_plotty <- function(df, sk_year, order_by_size=NULL, use_parent= NULL, use_years = NULL, colname_spaced=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      barplot_df <- get_agg_dataframe_for_dep(hi$`Total Feedback Comments and Marks`, hi$`Parent Account Name`, TRUE)
      
    } else {
      barplot_df <- get_agg_dataframe_for_dep(hi$Total.Feedback..Comments.and.Marks., hi$Parent.Account.Name, TRUE)
    }
    y_name <- "Skólar"
    
  } else {
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      barplot_df <- get_agg_dataframe_for_dep(hi$`Total Feedback Comments and Marks`, hi$`Account Name`, TRUE)
      
    } else {
      barplot_df <- get_agg_dataframe_for_dep(hi$Total.Feedback..Comments.and.Marks., hi$Account.Name, TRUE)
    }
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
}

# rit sem birtir tegundir Intergrationa í stærðarröð (order_by_size) eða ekki
types_of_intergration_eftir_skola_ari_bar_plot_df_plotly <- function(df, sk_year, order_by_size=NULL, use_years = NULL)
{
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  agg <- setNames(aggregate(hi$`Active Classes`, by=list(hi$Integration), FUN=sum), c("Integration","active.classes"))
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    agg <- agg[order(agg$active.classes, decreasing = TRUE),]
    agg$Integration <- factor(agg$Integration, levels= agg$Integration)
  }
  
  plotly_barplot_with_resize(agg, agg$Integration, agg$active.classes, agg$Integration, "Integration", "Fjöldi")
}

# rit sem birtir tegundir Integrationa eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
skipting_a_intergration_eftir_skolum_bar_plot_df_plotly <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL)
{
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    agg <- setNames(aggregate(hi$`Active Classes`, by=list(hi$`Parent Account Name`,hi$Integration), FUN=sum), c("svid","Integration","active.classes"))
    y_name <- "Skólar"
    
  } else {
    agg <- setNames(aggregate(hi$`Active Classes`, by=list(hi$`Account Name`,hi$Integration), FUN=sum), c("svid","Integration","active.classes"))
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    aggTotal <- setNames(aggregate(agg$active.classes, by=list(agg$svid), FUN=sum), c("svid","total"))
    aggTotal <- aggTotal[order(aggTotal$total, decreasing = TRUE),]
    aggTotal$svid <- factor(aggTotal$svid, levels= aggTotal$svid)
    agg$svid <- factor(agg$svid, levels = aggTotal$svid)
  }
  
  plotly_stacked_bar_chart(agg, agg$svid, agg$active.classes, agg$Integration, y_name, "Fjöldi")
}


# rit sem birtir fjölda verkefnaskila  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
total_submissins_eftir_skolum_hi_bar_plot_df_plottly <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    barplot_df <- get_agg_dataframe_for_dep(hi$Submissions, hi$Parent.Account.Name, TRUE)
    y_name <- "Skólar"
  } else {
    barplot_df <- get_agg_dataframe_for_dep(hi$Submissions, hi$Account.Name, TRUE)
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")
}

# rit sem birtir hvernig var farið yfir verkefni eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
types_of_submissions_df_plotly <-  function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL, colname_spaced=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df))
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    if (!is.null(colname_spaced) && colname_spaced == TRUE)
    {
      aggr <- setNames(aggregate(hi$`Submissions Scored with a Rubric`, by=list(hi$`Parent Account Name`), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
      aggf <- setNames(aggregate(hi$`Submissions with Feedback`, by=list(hi$`Parent Account Name`), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
      
    } else {
      aggr <- setNames(aggregate(hi$Submissions.Scored.with.a.Rubric, by=list(hi$Parent.Account.Name), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
      aggf <- setNames(aggregate(hi$Submissions.with.Feedback, by=list(hi$Parent.Account.Name), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
    }
    
    y_name <- "Skólar"
    
  } else {
    if (!is.null(colname_spaced) && colname_spaced == TRUE)
    {
      aggr <- setNames(aggregate(hi$`Submissions Scored with a Rubric`, by=list(hi$`Account Name`), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
      aggf <- setNames(aggregate(hi$`Submissions with Feedback`, by=list(hi$`Account Name`), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
      
    } else {
      aggr <- setNames(aggregate(hi$Submissions.Scored.with.a.Rubric, by=list(hi$Account.Name), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.Scored.with.a.Rubric"))
      aggf <- setNames(aggregate(hi$Submissions.with.Feedback, by=list(hi$Account.Name), FUN=sum, na.rm=TRUE), c("Account.Name", "Submissions.with.Feedback"))
    }
    
    y_name <- "Svið"
  }
  
  stacked_barplot_df <- melt(merge(aggr,aggf), id= "Account.Name")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    total_types_df <- setNames(data.frame(Account.Name <- aggr$Account.Name, data <- (aggr$Submissions.Scored.with.a.Rubric + aggf$Submissions.with.Feedback)), c("Account.Name", "data"))
    total_types_df <- total_types_df[order(total_types_df$data, decreasing = TRUE),]
    total_types_df$Account.Name <- factor(total_types_df$Account.Name, levels= total_types_df$Account.Name)
    stacked_barplot_df$Account.Name <- factor(stacked_barplot_df$Account.Name, levels= total_types_df$Account.Name)
  }
  
  plotly_stacked_bar_chart(stacked_barplot_df, stacked_barplot_df$Account.Name, stacked_barplot_df$value, stacked_barplot_df$variable, y_name, "Fjöldi")
}

# rit sem birtir fjölda simmilarity reports  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
total_symilarity_reports_df <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    barplot_df <- get_agg_dataframe_for_dep(hi$Similarity.Reports, hi$Parent.Account.Name, TRUE)
    y_name <- "Skólar"
    
  } else {
    barplot_df <- get_agg_dataframe_for_dep(hi$Similarity.Reports, hi$Account.Name, TRUE)
    y_name <- "Svið"
  }
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    barplot_df$svid <- factor(barplot_df$svid, levels= barplot_df$svid)
  }
  
  plotly_barplot_with_resize(barplot_df, barplot_df$svid, barplot_df$data, barplot_df$svid, y_name, "Fjöldi")

}

# rit sem birtir tegundir endurgjafar  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
feedback_skolar_bar_plots_df_plotly <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  colnames(df) <- gsub(" ", ".", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(use_parent) && use_parent == TRUE)
  {
    ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))], names(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))]), hi$Parent.Account.Name, df_melted=FALSE)
    y_name <- "Skólar"
    
  } else {
    ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))], names(hi[which("Summary.Text.Comments" == names(hi)):which("Peer.Reviews.Created" == names(hi))]), hi$Account.Name, df_melted=FALSE)
    y_name <- "Svið"
    
  }
  
  barplot_df <- melt(ag_total, id="svid")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    
    ag_total$total <- rowSums(ag_total[,2:10])
    ag_total[2:10] <- NULL
    ag_total <- ag_total[order(ag_total$total, decreasing = TRUE),]
    ag_total$svid <- factor(ag_total$svid, levels= ag_total$svid)
    barplot_df$svid <- factor(barplot_df$svid, levels = ag_total$svid)
  }
  plotly_stacked_bar_chart(barplot_df, barplot_df$svid, barplot_df$value, barplot_df$variable, y_name, "Fjöldi")
  
}

# rit sem birtir hversu lík verkefni voru öðrum verkefnum  eftir skólum ef use_parent er TRUE (skoðar Parent Account Name gildi) annars eftir sviðum (Account Name) í stærðarröð (order_by_size) eða ekki
skolar_similarity_bar_plots_eftir_arum_df_plotly <- function(df, sk_year, order_by_size=NULL, use_parent=NULL, use_years = NULL, colname_spaced=NULL) {
  colnames(df) <- gsub("_", " ", colnames(df), fixed = TRUE)
  if (!is.null(use_years) && use_years == TRUE)
  {
    hi <- filter(df,Ar %in% sk_year)
  } else {
    hi <- filter(df,Skolaar %in% sk_year)
  }
  
  if (!is.null(colname_spaced) && colname_spaced == TRUE)
  {
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))], names(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))]), hi$`Parent Account Name`, df_melted=FALSE)
    } else {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))], names(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))]), hi$Parent.Account.Name, df_melted=FALSE)
    }
    
    y_name <- "Skólar"
    
  } else {
    if (!is.null(colname_spaced) && colname_spaced == TRUE) {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))], names(hi[which("0% Similarity" == names(hi)):which("75-100% Similarity" == names(hi))]), hi$`Account Name`, df_melted=FALSE)
    } else {
      ag_total <- get_agg_dataframe_list_from_col_for_dep(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))], names(hi[which("X0..Similarity" == names(hi)):which("X75.100..Similarity" == names(hi))]), hi$Account.Name, df_melted=FALSE)
    }
    
    y_name <- "Svið"
  }
  
  colnames(ag_total)[2] <- "0%"
  colnames(ag_total)[3] <- "1-24%"
  colnames(ag_total)[4] <- "25-49%"
  colnames(ag_total)[5] <- "50-74%"
  colnames(ag_total)[6] <- "75-100%"
  
  barplot_df <- melt(ag_total, id="svid")
  
  if (!is.null(order_by_size) && order_by_size == TRUE)
  {
    
    ag_total$total <- rowSums(ag_total[,2:6])
    ag_total[2:6] <- NULL
    ag_total <- ag_total[order(ag_total$total, decreasing = TRUE),]
    ag_total$svid <- factor(ag_total$svid, levels= ag_total$svid)
    barplot_df$svid <- factor(barplot_df$svid, levels = ag_total$svid)
  }
  
  plotly_stacked_bar_chart(barplot_df, barplot_df$svid, barplot_df$value, barplot_df$variable, y_name, "Fjöldi")
}