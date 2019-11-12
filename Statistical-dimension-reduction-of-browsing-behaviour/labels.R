# Old rfmp labels for comparison to new versions 

# HyperIntent Label
# data$day_count > 3*(max(data$day_count)/5) & 
# data$recent_date_day > 52 

# Active Label
# data$day_count > 2*(max(data$day_count)/5) & 
# data$recent_date_day >= 1

# Regular Label
# data$day_count > 1*(max(data$day_count)/5) & 
# (data$recent_date_day - data$date_range) < 20 &
#  data$recent_date_day > 40

Label_Old_rfmp_Active <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count > 2*(max(data$day_count)/5) & 
                       data$recent_date_day >= 1)
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
}  
rfmp_sample$Old_rfmp_Active <- Label_Old_rfmp_Active(rfmp_sample)

Label_Old_rfmp_HyperIntent <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count > 3*(max(data$day_count)/5) & 
                       data$recent_date_day > 52)
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
}  
rfmp_sample$Old_rfmp_HyperIntent <- Label_Old_rfmp_HyperIntent(rfmp_sample)

Label_Old_rfmp_Regular <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count > 1*(max(data$day_count)/5) & 
                      (data$recent_date_day - data$date_range) < 20 &
                       data$recent_date_day > 40)
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
}  
rfmp_sample$Old_rfmp_Regular <- Label_Old_rfmp_Regular(rfmp_sample)

plot_ly(
  x = c("Old_rfmp_Active", "Old_rfmp_HyperIntent",
        "Old_rfmp_Regular", "No_Label"),
  y = c(length(which(rfmp_sample$Old_rfmp_Active == 1)),
        length(which(rfmp_sample$Old_rfmp_HyperIntent == 1)),
        length(which(rfmp_sample$Old_rfmp_Regular == 1)),
        length(which(rfmp_sample$Old_rfmp_Active == 0 & 
                    rfmp_sample$Old_rfmp_HyperIntent == 0 & 
                    rfmp_sample$Old_rfmp_Regular == 0
        ))),
  name = "Number of labels",
  type = "bar"
)

plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_Active),
  colors = c("blue", "orange")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_HyperIntent),
  colors = c("blue", "red")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_Regular),
  colors = c("blue", "green")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_Active),
  colors = c("blue", "orange")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_HyperIntent),
  colors = c("blue", "red")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(Old_rfmp_Regular),
  colors = c("blue", "green")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)


# Coloured pairs plots
rec_vars <- rfmp_sample[c('recent_date_day', 'median_date_day', 'avg_date_day', 'min_date_day',
                          'Old_rfmp_Active', 'Old_rfmp_HyperIntent', 'Old_rfmp_Regular')]
pairs(rec_vars[c(1:4)], col = rec_vars$Old_rfmp_HyperIntent + 1)

var_vars <- rfmp_sample[c('avg_inter_visit', 'max_inter_visit', 'min_inter_visit',
                          'inter_visit_range', 'inter_visit_median', 'Old_rfmp_Active',
                          'Old_rfmp_HyperIntent', 'Old_rfmp_Regular')]
pairs(var_vars[c(1:5)], col = var_vars$Old_rfmp_HyperIntent + 1)


# Variables to plot: day_count, recent_date_day, min_date, 

# Colours: HyperIntent = brewer.pal(3, "Paired")[6]
#          Active = brewer.pal(3, "Paired")[4]            
#          Regular = brewer.pal(3, "Paired")[2]

layout(plot_ly(data = rfmp_sample, x = ~recent_date_day, y = ~day_count, name = "Recent Date vs Day Count"),
       title = 'Recent Date vs Day Count - Old label rules',
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[4], line = list(color = brewer.pal(6, "Paired")[4]), opacity = 0.5,
              x0 = 41, x1 = 60, xref = "x",
              y0 = 12, y1 = 56, yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[2], line = list(color = brewer.pal(6, "Paired")[2]), opacity = 0.4,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 23, y1 = 56, yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[6], line = list(color = brewer.pal(6, "Paired")[6]), opacity = 0.6,
              x0 = 53, x1 = 60, xref = "x",
              y0 = 34, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[6]), opacity = 1,
              x0 = 53, x1 = 60, xref = "x",
              y0 = 34, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[2]), opacity = 1,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 23, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[4]), opacity = 1,
              x0 = 41, x1 = 60, xref = "x",
              y0 = 12, y1 = 56, yref = "y")))

layout(plot_ly(data = rfmp_sample, x = ~recent_date_day, y = ~min_date_day, name = "Recent Date vs Min Date"),
       title = 'Recent Date vs Min Date - Old label rules',
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[2], line = list(color = brewer.pal(6, "Paired")[2]), opacity = 0.3,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 1, y1 = 60, yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[4], line = list(color = brewer.pal(6, "Paired")[4]), opacity = 0.4,
              x0 = 41, x1 = 60, xref = "x",
              y0 = 1, y1 = 19, yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[6], line = list(color = brewer.pal(6, "Paired")[6]), opacity = 0.6,
              x0 = 53, x1 = 60, xref = "x",
              y0 = 1, y1 = 60, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[6]), opacity = 1,
              x0 = 53, x1 = 60, xref = "x",
              y0 = 1, y1 = 60, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[4]), opacity = 1,
              x0 = 41, x1 = 60, xref = "x",
              y0 = 1, y1 = 19, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[2]), opacity = 1,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 1, y1 = 60, yref = "y")))

layout(plot_ly(data = rfmp_sample, x = ~min_date_day, y = ~day_count, name = "Min Date vs Day Count"),
       title = 'Min Date vs Day Count - Old label rules',
       shapes = list(
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[4], line = list(color = brewer.pal(6, "Paired")[4]), opacity = 0.5,
              x0 = 1, x1 = 19, xref = "x",
              y0 = 12, y1 = 56, yref = "y"),
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[2], line = list(color = brewer.pal(6, "Paired")[2]), opacity = 0.4,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 23, y1 = 56, yref = "y"), 
         list(type = "rect",
              fillcolor = brewer.pal(6, "Paired")[6], line = list(color = brewer.pal(6, "Paired")[6]), opacity = 0.6,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 34, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[6]), opacity = 1,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 34, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[2]), opacity = 1,
              x0 = 1, x1 = 60, xref = "x",
              y0 = 23, y1 = 56, yref = "y"),
         list(type = "rect",
              line = list(color = brewer.pal(6, "Paired")[4]), opacity = 1,
              x0 = 1, x1 = 19, xref = "x",
              y0 = 12, y1 = 56, yref = "y")))

nrow(rfmp_sample)
length(which(rfmp_sample$Old_rfmp_Active == 0 &
               rfmp_sample$Old_rfmp_HyperIntent == 0 & 
               rfmp_sample$Old_rfmp_Regular == 0))


#----------------------------------------------------------------------

# Attributes to labels 

names(rfmp_sample)

Label_new_rfmp_Active <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count_disc != 'Low' & 
                       (data$recent_date_day_disc == 'Medium' | 
                       data$recent_date_day_disc == 'Very High' |
                       data$recent_date_day_disc == 'High'))
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
} 
rfmp_sample$new_rfmp_Active <- Label_new_rfmp_Active(rfmp_sample)


Label_new_rfmp_HyperIntent <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count_disc != 'Low' & 
                       (data$recent_date_day_disc == 'Very High' |
                       data$recent_date_day_disc == 'High') &
                       data$avg_hits_disc != 'Low')
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
} 
rfmp_sample$new_rfmp_HyperIntent <- Label_new_rfmp_HyperIntent(rfmp_sample)


Label_new_rfmp_Regular <- function(data) {
  new_label <- numeric(length=nrow(data))
  label_index <- which(data$day_count_disc != 'Low' & 
                       data$recent_date_day_disc != 'Very Low' & 
                       data$variability2 != 'High')
  print(length(label_index))
  new_label[label_index] = 1
  return(new_label)
} 
rfmp_sample$new_rfmp_Regular <- Label_new_rfmp_Regular(rfmp_sample)

nrow(rfmp_sample)
length(which(rfmp_sample$new_rfmp_Active == 0 &
               rfmp_sample$new_rfmp_HyperIntent == 0 & 
               rfmp_sample$new_rfmp_Regular == 0))



plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_Active),
  colors = c("blue", "orange")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_HyperIntent),
  colors = c("blue", "red")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~day_count, 
  y = ~recent_date,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_Regular),
  colors = c("blue", "green")) %>% 
  layout(
    xaxis = list(title = "DayCount"),
    yaxis = list(title = "RecentDate"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_Active),
  colors = c("blue", "orange")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_HyperIntent),
  colors = c("blue", "red")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)

plot_ly(
  data=rfmp_sample, 
  x = ~avg_inter_visit, 
  y = ~max_inter_visit,
  type = "scatter",
  mode = "markers",
  color = ~factor(new_rfmp_Regular),
  colors = c("blue", "green")) %>% 
  layout(
    xaxis = list(title = "AverageInterVisit"),
    yaxis = list(title = "MaximumInterVisit"),
    showlegend = FALSE)

#----------------------------------------------------------------------