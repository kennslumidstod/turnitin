library("tidyverse")
library(reshape2)
library(plotly)

df <- read.csv("func_examples_csv/account_68652_full_report_by_class.csv")
df <- add_skolaar_to_dataframe(df, "Month")
colnames(df)

get_skolar_in_range("2012-2013", "2017-2018")

sim <- month_to_schoolyear(df, "X1.24..Similarity","X75.100..Similarity", list(df$Parent.Account.Name, df$Account.Name), c("Parent.Account.Namr", "Account.Name"))
view(sim)

skyear_class <- get_yearly_stats_for_class(df,"Submissions", "Peer.Reviews.Created", list("Skolaar", "Parent.Account.Name","Account.Name"), c("Skolaar", "Parent.Account.Name", "Account.Name"))
view(skyear_class)

inst_df <- read.csv("func_examples_csv/account_68652_full_report_by_instructor.csv")
inst_df <- add_skolaar_to_dataframe(inst_df, "Month")
school_year_stat_instructors <- get_yearly_stats_for_instructors(inst_df,"Submissions", "Peer.Reviews.Created", list("Skolaar", "Parent.Account.Name","Account.Name"), c("Skolaar", "Parent.Account.Name", "Account.Name"))
view(school_year_stat_instructors)

agg_total <- get_agg_dataframe_for_dep(skyear_class$Total.Feedback..Comments.and.Marks., skyear_class$Account.Name, TRUE)
view(agg_total)

agg_feedback <- get_agg_dataframe_list_from_col_for_dep(skyear_class[which("Summary.Text.Comments" == names(skyear_class)):which("Peer.Reviews.Created" == names(skyear_class))], names(skyear_class[which("Summary.Text.Comments" == names(skyear_class)):which("Peer.Reviews.Created" == names(skyear_class))]), skyear_class$Account.Name, df_melted=TRUE)
view(agg_feedback)

plotly_barplot_with_resize(agg_total,agg_total$svid,agg_total$data, agg_total$svid, "total", "svid")

plotly_stacked_bar_chart(agg_feedback, agg_feedback$svid, agg_feedback$value, agg_feedback$variable, "total", "svid")
