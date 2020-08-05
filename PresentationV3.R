
library(data.table)
library(stringi)
library(ggplot2)
library(ggrepel)
library(officer)
library(flextable)
library(magrittr)
library(rvg) # vector graphics
library(patchwork) # combine charts

extract.brands.data = function(df, selection, measure, brands.to.show) {
  measure = paste0(measure, "C")
  df = df[eval(parse(text = selection))]
  
  df = dcast.data.table(df,
                        Brand ~ Ynb + Mnb,
                        value.var = measure,
                        fun.aggregate = sum)
  
  n = dim(df)[2]
  
  df[, P3M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 2):n]
  df[, P6M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 5):n]
  df[, P12M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]
  
  df[, P3MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 14):(n - 12)]
  df[, P6MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 17):(n - 12)]
  df[, P12MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 23):(n - 12)]
  
  # MS
  df[, names(df)[2:length(df)] := lapply(.SD, function(x)
    100 * x / sum(x)),
    .SDcols = 2:length(df)]
  
  df = df[Brand %in% brands.to.show]
  
  df[, `:=`(
    P3M.delta.bps = (P3M - P3MPY) * 100,
    P6M.delta.bps = (P6M - P6MPY) * 100,
    P12M.delta.bps = (P12M - P12MPY) * 100
  )]
  
  df[, c(2:n, (n + 4):(n + 6)) := NULL]
  
  df = df[order(-Brand)]
  names(df)[1] = "Group"
  
  return(df)
  
}

extract.category.data = function(df, selection, measure, exclude.malysh) {
  # measure = paste0(measure, "C")
  df = df[eval(parse(text = selection))]
  
  if (exclude.malysh == TRUE) {df[Brand == "Malysh Istr", Company := "No Name"]}
  
  df1 = df[PS0 == "IMF", .(Volume = sum(VolumeC), Value = sum(ValueC)), 
           by = .(PS2, Company, Ynb, Mnb)]
  names(df1)[1] = "Category"
  
  df2 = df[PS3 %in% c("Instant Cereals", "Cereal Biscuits", "Fruits", "Savoury Meal"), 
               .(Volume = sum(VolumeC), Value = sum(ValueC)), 
           by = .(PS3, Company, Ynb, Mnb)]
  
  df2[PS3 == "Fruits" | PS3 == "Savoury Meal", PS3 := "Puree"]
  names(df2)[1] = "Category"
  
  df = rbindlist(list(df1, df2))
  
  df = dcast.data.table(df,
                        Category + Company ~ Ynb + Mnb,
                        value.var = measure,
                        fun.aggregate = sum)
  
  n = dim(df)[2]
  
  df[, P3M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 2):n]
  df[, P6M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 5):n]
  df[, P12M := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 11):n]
  
  df[, P3MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 14):(n - 12)]
  df[, P6MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 17):(n - 12)]
  df[, P12MPY := rowSums(.SD, na.rm = TRUE), .SDcols = (n - 23):(n - 12)]
  
  # MS
  df[, names(df)[3:length(df)] := lapply(.SD, function(x)
    100 * x / sum(x)),
    .SDcols = 3:length(df), by = Category]
  
  df = df[Company == "Nutricia"]
  
  df[, `:=`(
    P3M.delta.bps = (P3M - P3MPY) * 100,
    P6M.delta.bps = (P6M - P6MPY) * 100,
    P12M.delta.bps = (P12M - P12MPY) * 100
  )]
  
  df[, c(2:n, (n + 4):(n + 6)) := NULL]
  
  setorder(df[, .r := order(c(4, 2, 3, 1, 5, 6))], .r)[, .r := NULL]
  
  names(df)[1] = "Group"
  
  return(df)
  
}

make.flextable = function(df, level) {
  ft = flextable(df)
  
  # Add header, perhaps need to be universal for both tables
  ft = add_header_row(ft,
    top = TRUE,
    values = c(level, "Danone Market Share, %", "Changes, bps"),
    colwidths = c(1, 3, 3)
  )
  
  # Merge header
  ft = merge_at(ft, i = 1:2, j = 1, part = "header")
  
  # Assign labels to the header
  ft <- set_header_labels(ft,
    Group = level,
    P3M = "P3M",
    P6M = "P6M",
    P12M = "P12M",
    P3M.delta.bps = "P3M",
    P6M.delta.bps = "P6M",
    P12M.delta.bps = "P12M"
  )
  
  # Theme
  ft = theme_box(ft)
  
  # Color and fonsize of the header
  ft <-  bg(ft, bg = "#D3D3D3", part = "header")
  ft = fontsize(ft, size = 11, part = "header")
  
  # Alignment
  ft =  align(ft,
    j = c(
      "P3M",
      "P6M",
      "P12M",
      "P3M.delta.bps",
      "P6M.delta.bps",
      "P12M.delta.bps"
    ),
    align = "center",
    part = "all"
  )
  
  ft = color(ft, ~ P3M.delta.bps < 0, ~ P3M.delta.bps, color = "red")
  ft = color(ft, ~ P6M.delta.bps < 0, ~ P6M.delta.bps, color = "red")
  ft = color(ft, ~ P12M.delta.bps < 0, ~ P12M.delta.bps, color = "red")
  
  ft = color(ft, ~ P3M.delta.bps >= 0, ~ P3M.delta.bps, color = "#3895D3")
  ft = color(ft, ~ P6M.delta.bps >= 0, ~ P6M.delta.bps, color = "#3895D3")
  ft = color(ft, ~ P12M.delta.bps >= 0, ~ P12M.delta.bps, color = "#3895D3")
  
  # Digits format
  ft = colformat_num(ft,
    c(
      "P3M",
      "P6M",
      "P12M"
      ),
    digits = 1
  )
  
  ft = colformat_num(ft,
                     c(
                       "P3M.delta.bps",
                       "P6M.delta.bps",
                       "P12M.delta.bps"
                     ),
                     digits = 0
  )
  
  # Font size
  ft = fontsize(ft, size = 10, part = "all")
  
  # Width
  ft <- width(ft, j = ~ Group, width = 1.17)
  
  ft <- width(ft, j = ~ P3M, width = 0.65)
  ft <- width(ft, j = ~ P6M, width = 0.65)
  ft <- width(ft, j = ~ P12M, width = 0.65)
  
  ft <- width(ft, j = ~ P3M.delta.bps, width = 0.65)
  ft <- width(ft, j = ~ P6M.delta.bps, width = 0.65)
  ft <- width(ft, j = ~ P12M.delta.bps, width = 0.65)
  
  
  # Border
  ft <- vline(ft,
    j = c("P3M", "P6M", "P3M.delta.bps", "P6M.delta.bps"),
    border = fp_border(color = "black", style = "dotted"),
    part = "body"
  )
  
  # ft <- border_outer(ft, part="all", border = fp_border(color = "grey63", style = "solid"))
  
  return(ft)

}


build.line.chart = function(df, selection, measure, linesToShow, Year, Month, exclude.malysh) {
  
  df = df[eval(parse(text = selection))]
  
  if (exclude.malysh == TRUE) {df[Brand == "Malysh Istr", Company := "No Name"]}
  
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
  df[, paste0(Year, "_YTD") := rowSums(.SD, na.rm = TRUE), 
     .SDcols = (n - Month + 1):n]
  df[, .. := as.numeric(NA)]
  
  df = df[, names(df)[2:length(df)] := lapply(.SD, function(x) 100*x/sum(x)), 
          .SDcols = 2:length(df)] 
  
  setcolorder(df, c(1, (n + 1):(n + 6), 2:n))
  df = df[order(-df[,3])][1:linesToShow]
  
  df = melt.data.table(df, "Company")
  
  df[, c("Ynb", "Mnb") := tstrsplit(variable, "_", fixed=TRUE)]
  
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

build.bar.chart = function(df, selection, measure, 
                           reporting.Ynb, reporting.Mnb,
                           exchane.rate, growth.rate){
  
  
  # Check selection
  
  if (measure == "Volume") {
    measure = "VolumeC"
    exchane.rate = 1
    y.title = "Volume, mln"
    color.palette = "#00B0F0"
    if (selection == 'Form != "Liquid" & PS0 == "IMF"') {
      growth.rate = growth.imf.volume
    } else {
      growth.rate = growth.foods.volume
    }
  } else if (measure == "Value") {
    measure = "ValueC"
    color.palette = "#0070C0"
    y.title = "EUR, mln"
    if (selection == 'Form != "Liquid" & PS0 == "IMF"') {
      growth.rate = growth.imf.value
    } else {
      growth.rate = growth.foods.value
    }
  } else {
    print("Unknown measure!")
  }
  
  # Check Ynb & Mnb
  
  if (exists("reporting.Ynb")) {
    if (is.na(reporting.Ynb) | reporting.Ynb == "") {
      reporting.Ynb = df[, max(Ynb)]
    }
    if (reporting.Ynb > format(Sys.Date(), "%Y")) {
      print("Year cannot be bigger than current year")
      print(paste("Ynb =", reporting.Ynb))
    }
    if (reporting.Ynb < as.integer(format(Sys.Date(), "%Y")) - 2) {
      print("Suspicious year")
      print(paste("Ynb =", reporting.Ynb))
    }
    
  } else {
    reporting.Ynb = df[, max(Ynb)]
  }
  
  
  if (exists("reporting.Mnb")) {
    if (is.na(reporting.Mnb) | reporting.Mnb == "") {
      reporting.Mnb = df[Ynb == max(Ynb), max(Mnb)]
    }
    if (!between(reporting.Mnb, 1, 12, incbounds = TRUE)) {
      print("Month must be in the range [1, 12]")
      print(paste("Mnb =", Mnb))
    }
    
  } else {
    reporting.Mnb = df[Ynb == max(Ynb), max(Mnb)]
  }
  
  
  df = df[eval(parse(text = selection))]
  df = df[, .(Sales = sum(get(measure))/1000000/exchane.rate), by = .(Ynb, Mnb)]
  
  # Extract right periods
  df = df[(Ynb >= (reporting.Ynb - 2) & Ynb < reporting.Ynb) | 
            (Ynb == reporting.Ynb & Mnb <= reporting.Mnb)]
  
  df[, Change := shift(Sales, type = "lag", n = 12)]
  df[, Change := 100*(Sales / Change - 1)]
  
  if (reporting.Mnb < 12){
    df.temp = data.table(Ynb = reporting.Ynb,
                         Mnb = (reporting.Mnb+1):12,
                         NA,
                         NA)
    df = rbindlist(list(df, df.temp))
  }
  
  # df[, c("Ynb", "Mnb") := tstrsplit(variable, "_", fixed=TRUE)][]
  df$Mnb = as.character(df$Mnb) # to align the tyoe with the dictionary
  df[dict.months, on = "Mnb", Period := i.month.name]                        
  df$Mnb = as.integer(df$Mnb) # return back
  
  df[, Period := paste0(toupper(Period), " ", stri_sub(Ynb,-2,-1))]
  
  
  df.YTD = df[between(Mnb, 1, reporting.Mnb), .(Sales = sum(Sales)), by = Ynb]
  df.YTD = df.YTD[, Change := shift(Sales, type = "lag", n = 1)]
  df.YTD[, Change := 100*(Sales / Change - 1)]
  
  
  df.FY = df[, .(Sales = sum(Sales)), by = Ynb]
  df.FY = df.FY[, Change := shift(Sales, type = "lag", n = 1)]
  df.FY = df.FY[Ynb == reporting.Ynb, Sales := Change * growth.rate]
  df.FY[, Change := 100*(Sales / Change - 1)]
  
  
  df.YTG = df.FY - df.YTD
  df.YTG = df.YTG[, Change := shift(Sales, type = "lag", n = 1)]
  df.YTG[, Change := 100*(Sales / Change - 1)]
  
  
  # Modify column names
  df = df[Ynb > (reporting.Ynb - 2)]
  df = df[, c("Ynb", "Mnb") := NULL]
  # setcolorder(df, c(3, 1, 2))
  
  df.YTD = df.YTD[!is.na(Change)]
  df.YTD[, Period := paste0("YTD", " ", stri_sub(Ynb,-2,-1))]
  df.YTD[, Ynb := NULL]
  
  df.FY = df.FY[!is.na(Change)]
  names(df.FY)[1] = "Period"
  df.FY$Period = as.character(df.FY$Period)
  
  df.YTG = df.YTG[!is.na(Change)]
  df.YTG$Period = df.FY$Period
  df.YTG[, Period := paste0("YTG", " ", stri_sub(Period,-2,-1))]
  df.YTG[, Ynb := NULL]
  
  
  # df = rbindlist(list(df, 
  #                     list(".", NA, NA),
  #                     df.YTD,
  #                     list("..", NA, NA),
  #                     df.YTG,
  #                     list("...", NA, NA),
  #                     df.FY
  #                     ))
  
  
  
  df$Period <- factor(df$Period, levels = unique(df$Period))
  
  
  df.plot = ggplot(df, 
                   aes(y = Sales, x = Period)) + 
    geom_col(fill = color.palette) +
    geom_text(
      aes(label = sprintf("%0.1f", Sales)),
      vjust = 2,
      show.legend = FALSE,
      size = 3,
      color = "white"
      
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", Change)),
      vjust = -1,
      show.legend = FALSE,
      size = 3
    ) +
    scale_y_continuous(limits = c(0, df[, max(Sales, na.rm = TRUE)]*1.1)) +
    theme_minimal() +
    ylab(y.title) + xlab(NULL) +
    # labs(title = y.title) +
    theme(
      # legend.position = "right",
      panel.grid.minor = element_blank(),
      # panel.grid.major.y = element_line(linetype = "dotted", colour = "darkgrey"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title.y = element_text(angle = 90, hjust = 0.85)
    )
  
  df.YTD.plot = ggplot(df.YTD, 
                       aes(y = Sales, x = Period)) + 
    geom_col(fill = color.palette) +
    geom_text(
      aes(label = sprintf("%0.1f", Sales)),
      vjust = 2,
      show.legend = FALSE,
      size = 3,
      color = "white"
      
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", Change)),
      vjust = -1,
      show.legend = FALSE,
      size = 3
    ) +
    scale_y_continuous(limits = c(0, df.YTD[, max(Sales, na.rm = TRUE)]*1.1)) +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    theme(
      # legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      # panel.grid.major.y = element_line(linetype = "dotted", colour = "darkgrey"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_blank()
    )
  
  df.YTG.plot = ggplot(df.YTG, 
                       aes(y = Sales, x = Period)) + 
    geom_col(fill = color.palette) +
    geom_text(
      aes(label = sprintf("%0.1f", Sales)),
      vjust = 2,
      show.legend = FALSE,
      size = 3,
      color = "white"
      
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", Change)),
      vjust = -1,
      show.legend = FALSE,
      size = 3
    ) +
    scale_y_continuous(limits = c(0, df.YTG[, max(Sales, na.rm = TRUE)]*1.1)) +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    theme(
      # legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      # panel.grid.major.y = element_line(linetype = "dotted", colour = "darkgrey"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_blank()
    )
  
  df.FY.plot = ggplot(df.FY, 
                      aes(y = Sales, x = Period)) + 
    geom_col(fill = color.palette) +
    geom_text(
      aes(label = sprintf("%0.1f", Sales)),
      vjust = 2,
      show.legend = FALSE,
      size = 3,
      color = "white"
      
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", Change)),
      vjust = -1,
      show.legend = FALSE,
      size = 3
    ) +
    scale_y_continuous(limits = c(0, df.FY[, max(Sales, na.rm = TRUE)]*1.1)) +
    theme_minimal() +
    ylab(NULL) + xlab(NULL) +
    theme(
      # legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      # panel.grid.major.y = element_line(linetype = "dotted", colour = "darkgrey"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_blank()
    )
  
  df.plot = df.plot + df.YTD.plot + df.YTG.plot + df.FY.plot + 
    plot_layout(ncol = 4, width = c(10, 1, 1, 1))
  
  return(df.plot)
  
}

# Dictionaries
dict.months = data.table(Mnb = as.character(1:12),
                         month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

dictColors = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V2/dictColor.csv")
# dictColors = fread("d:/Temp/3/Presentation-V2-master/dictColor.csv")

dictColors = dictColors[Color != ""]
customColors = dictColors$Color
names(customColors) = dictColors$Name

# Dataset
df = fread("/home/sergiy/Documents/Work/Nutricia/Data/202005/df.csv")
# df = fread("d:/Temp/1/1/Aug/df.corrected.csv")

# Parameters
brands.to.show = c("Nutrilon", "Milupa", "Malysh Istr")

Month = 5
Year = 2020
No.to.show = 6
linesToShow = 5
exchage.rate = 27.43

growth.imf.volume = 0.9582
growth.imf.value = 1.0156
growth.foods.volume = 0.9962
growth.foods.value = 1.0342


df = df[(Ynb < Year) | (Ynb == Year & Mnb <= Month)]

measure = "Volume"
selection = quote(Form != "Liquid" & PS0 == "IMF")
selection = 'Form != "Liquid" & PS0 == "IMF"'
selection = 'Form != "Liquid" & PS0 != "AMN"'
### Checks
extract.brands.data(df, selection, "Volume", brands.to.show)
extract.category.data(df, selection, "Volume", FALSE)

### PRESENTATION
# ppt <- read_pptx("d:/Temp/2/1/Market Research data - pattern.pptx") 
ppt <- read_pptx("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/Market Research data - pattern3.pptx")
dictContent = read.csv("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/dictContent.csv",
                       stringsAsFactors = FALSE)

# dictContent = read.csv("d:/Temp/2/1/dictContent2.csv", stringsAsFactors = FALSE)


setDT(dictContent)
# Slide 4

# ppt.content = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/dictContent2.csv", stringsAsFactors = FALSE)

for (i in dictContent[, Slide]) {
  print(i)
  
  if (dictContent[Slide == i, Type == "Bar chart"]) {
    ppt %>%
      on_slide(index = i) %>%
      ph_with_vg(
        ggobj = build.bar.chart(df,
                                 dictContent[Slide == i, Selection],
                                 "Volume",
                                 # linesToShow = linesToShow,
                                 Year, Month,
                                 exchage.rate,
                                 growth.rate),
        index = 5
      ) %>%
      ph_with_vg(
        ggobj = build.bar.chart(df,
                                 dictContent[Slide == i, Selection],
                                 "Value",
                                 # linesToShow = linesToShow,
                                Year, Month,
                                exchage.rate,
                                growth.rate),
        index = 4
      )
    
  }
  
  if (dictContent[Slide == i, Type == "Line chart"]) {
    ppt %>%
      on_slide(index = i) %>%
      ph_with_vg(
        ggobj = build.line.chart(df,
                                 dictContent[Slide == i, Selection],
                                 "Volume",
                                 linesToShow = linesToShow,
                                 Year, Month,
                                 dictContent[Slide == i, Excl.Malysh]),
        index = 3 #5
      ) %>%
      ph_with_vg(
        ggobj = build.line.chart(df,
                                 dictContent[Slide == i, Selection],
                                 "Value",
                                 linesToShow = linesToShow,
                                 Year, Month,
                                 dictContent[Slide == i, Excl.Malysh]),
        index = 5 #4
      )
    
  }
  
  if (dictContent[Slide == i, Type == "Brands"]) {
    ppt %>%
      on_slide(index = i) %>%
      ph_with_flextable(value = make.flextable(
        extract.brands.data(df,
                            dictContent[Slide == i, Selection],
                            "Volume",
                            brands.to.show),
        "Brand"
      ),
      index = 3) %>%
      ph_with_flextable(value = make.flextable(
        extract.brands.data(df,
                            dictContent[Slide == i, Selection],
                            "Value",
                            brands.to.show),
        "Brand"
      ),
      index = 1)
    
    
  }
  
  if (dictContent[Slide == i, Type == "Categories"]) {
    ppt %>%
      on_slide(index = i) %>%
      ph_with_flextable(value = make.flextable(
        extract.category.data(df,
                              dictContent[Slide == i, Selection],
                              "Volume",
                              dictContent[Slide == i, Excl.Malysh]),
        "Category"
      ),
      index = 3) %>%
      ph_with_flextable(value = make.flextable(
        extract.category.data(df,
                              dictContent[Slide == i, Selection],
                              "Value",
                              dictContent[Slide == i, Excl.Malysh]),
        "Category"
      ),
      index = 1)
    
  }
  
}



print(ppt, "/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/Market Research Data May20.pptx")
# print(ppt, "d:/Temp/2/1/p.pptx")
