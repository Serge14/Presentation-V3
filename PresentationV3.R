
library(data.table)
library(stringi)
library(ggplot2)
library(ggrepel)
library(officer)
library(flextable)
library(magrittr)
library(rvg) # vector graphics

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
    P6M = "P3M",
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

# Dictionaries
dict.months = data.table(Mnb = as.character(1:12),
                         month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

dictColors = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V2/dictColor.csv")
dictColors = fread("d:/Temp/3/Presentation-V2-master/dictColor.csv")

dictColors = dictColors[Color != ""]
customColors = dictColors$Color
names(customColors) = dictColors$Name

# Dataset
df = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201908/df.corrected.csv")
df = fread("d:/Temp/1/1/Aug/df.corrected.csv")

# Parameters
brands.to.show = c("Nutrilon", "Milupa", "Malysh Istr")

Month = 8
Year = 2019
No.to.show = 6
linesToShow = 5

df = df[(Ynb < Year) | (Ynb == Year & Mnb <= Month)]

measure = "Volume"
selection = quote(Form != "Liquid" & PS0 == "IMF")
selection = 'Form != "Liquid" & PS0 == "IMF"'
selection = 'Form != "Liquid" & PS0 != "AMN"'
### Checks
extract.brands.data(df, selection, "Volume", brands.to.show)
extract.category.data(df, selection, "Volume", FALSE)

### PRESENTATION
ppt <- read_pptx("d:/Temp/2/1/Market Research data - pattern.pptx") 
ppt <- read_pptx("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/Market Research data - pattern.pptx") 
dictContent = read.csv("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/dictContent2.csv",
                       stringsAsFactors = FALSE)

dictContent = read.csv("d:/Temp/2/1/dictContent2.csv", stringsAsFactors = FALSE)


setDT(dictContent)
# Slide 4

ppt.content = fread("/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/dictContent2.csv", stringsAsFactors = FALSE)

for (i in dictContent[, Slide]) {
  print(i)
  
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
        index = 5
      ) %>%
      ph_with_vg(
        ggobj = build.line.chart(df,
                                 dictContent[Slide == i, Selection],
                                 "Value",
                                 linesToShow = linesToShow,
                                 Year, Month,
                                 dictContent[Slide == i, Excl.Malysh]),
        index = 4
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
      index = 2) %>%
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
      index = 2) %>%
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



print(ppt, "/home/sergiy/Documents/Work/Nutricia/Scripts/Presentation-V3/p.pptx")
print(ppt, "d:/Temp/2/1/p.pptx")
