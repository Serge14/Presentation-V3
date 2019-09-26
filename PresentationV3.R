
library(data.table)
df = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201907/df.csv")

brands.to.show = c("Nutrilon", "Milupa", "Malysh Istr")

df = df[Form !- "Liquid" & PS0 == "IMF"]
dt = dcast.data.table(df, Brand ~ Ynb + Mnb, 
                      value.var = "VolumeC", 
                      fun.aggregate = sum)

n = dim(df)[2]

df[, P3M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 2):n]
df[, P6M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 5):n]
df[, P12M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]

df[, P3MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 14):(n - 12)]
df[, P6MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 17):(n - 12)]
df[, P12MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 23):(n - 12)]




dict.months = data.table(Mnb = as.character(1:12),
                         month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

dictColors = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V2/dictColor.csv")
dictColors = dictColors[Color != ""]
customColors = dictColors$Color
names(customColors) = dictColors$Name

Month = 7
Year = 2019
No.to.show = 6

build.line.chart = function(df, measure, linesToShow, Year, Month) {
  
  if (measure == "Volume") {measure = "VolumeC"}
  if (measure == "Value") {measure = "ValueC"}
  
  df = df[Ynb >= Year - 2, 
          .(VolumeC = sum(VolumeC),
            ValueC = sum(ValueC)), 
          by = .(Company, Ynb, Mnb)]
  
  df = dcast.data.table(df, Company ~ Ynb + Mnb, 
                        value.var = measure, 
                        fun.aggregate = sum)
  
  n = dim(df)[2]
  
  df[, paste0(Year - 1, "_MAT") := rowSums(.SD, na.rm = TRUE), 
     .SDcols = (n - 23):(n - 12)]
  df[, paste0(Year, "_MAT") := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]
  df[, . := as.numeric(NA)]
  
  df[, paste0(Year - 1, "_YTD") := rowSums(.SD, na.rm = TRUE), 
     .SDcols = (n - Month - 11):(n - 12)]
  df[, paste0(y, "_YTD") := rowSums(.SD, na.rm = TRUE), 
     .SDcols = (n - Month + 1):n]
  df[, .. := as.numeric(NA)]
  
  df = df[, names(df)[2:length(df)] := lapply(.SD, function(x) 100*x/sum(x)), 
          .SDcols = 2:length(df)] 
  
  setcolorder(df, c(1, (n + 1):(n + 6), 2:n))
  df = df[order(-df[,3])][1:linesToShow]
  
  df = melt.data.table(df, "Company")
  
  df[, c("Ynb", "Mnb") := tstrsplit(variable, "_", fixed=TRUE)][]
  
  df[dict.months, on = "Mnb", Mnb := i.month.name]                        
  
  df[, Period := paste0(toupper(Mnb), " ", stri_sub(Ynb,-2,-1))][]
  df[Period == "NA .", Period := "."]
  df[Period == "NA ..", Period := ".."]
  
  df$Ynb = as.numeric(df$Ynb)
  df = df[Ynb > Year - 2 | is.na(Ynb)]
  df[, c("Ynb", "Mnb") := NULL]
  
  df$Period <- factor(df$Period, levels = unique(df$Period))
  
  maxY = ceiling(max(df$value, na.rm = TRUE)/10)*10
  
  df.plot = ggplot(df, aes(
    x = Period,
    y = value,
    group = Company,
    col = Company
  )) +
    geom_line(size = 1) +
    geom_point() +
    geom_text_repel(
      aes(label = sprintf("%0.1f", value)),
      direction = "y",
      nudge_y = 0.5,
      show.legend = FALSE,
      size = 3,
      min.segment.length = 0.5
    ) +
    scale_color_manual(values = customColors) +
    #scale_x_discrete(breaks = NULL) +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", colour = "darkgrey"),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    # guides(color = guide_legend(nrow = LegendRowNumber)) +
    coord_cartesian(ylim = c(0, maxY))
  
  return(df.plot)
  
}



ppt = ppt %>%
  add_slide(layout = "1_Two Content", master = "Office Theme") %>%
  ph_with_text(type = "title", str = dictContent$Name[i]) %>%
  ph_with_ul(style = text_prop, 
             type = "body", 
             index = 3, 
             str_list = c(str_pad(dictContent$Region[i], 16)), 
             level_list = c(1)) %>%
  ph_with_gg(value = buildBarChart(getBarChart(dfName, fopt2), fopt2), index = 2) %>%
  ph_with_gg(value = buildBarChart(getBarChart(dfName, fopt1), fopt1), index = 1)
