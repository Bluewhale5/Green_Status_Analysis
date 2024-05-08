
#Function to prepare data:

prepare_trait_data <- function(Data){
  Data |>
    mutate(Data$Red_List_Category <- factor(Data$Red_List_Category, levels=unique(Data$Red_List_Category))) |>
    mutate(logBM = log(Avg_Adult_Body_Mass_kg)) |>
    mutate(logHR = log(Home_Range_km2)) |>
    mutate(PARO = litter_or_clutch_size_n * litters_or_clutches_per_y)
    
}


prepare_analysis_data <- function(Data){
  Data |>
    mutate(PARO = litter_or_clutch_size_n * litters_or_clutches_per_y) |>
    mutate("SRS01" = SRS/100) |>
    mutate(best_trans = ((SRS01*(40-1))+0.5)/40)|>
    mutate(Data, "L01" = Lower/100)|>
    mutate(Data, lower_trans = ((L01*(40-1))+0.5)/40)|>
    mutate(Data, "U01" = Upper/100)|>
    mutate(Data, upper_trans = ((U01*(40-1))+0.5)/40)|>
    mutate(Data$Red_List_Category <- factor(Data$Red_List_Category, levels = c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")))|>
    mutate(Data$BMC <- factor(Data$BMC, levels = c("S", "M", "L")))|>
    mutate(Data$SRC <- factor(Data$SRC, levels = c("Fully Recovered", "Slightly Depleted", "Moderately Depleted", "Largely Depleted", "Critically Depleted", "Indeterminate")))
}




# Function for preparing estimated marginal means for plotting

SumFunction <- function(marginal){
  as.data.frame(cld(marginal, alpha   = 0.05,
                    Letters = letters,         ###  Use lowercase letters for .group
                    adjust  = "sidak",         ###  Sidak-adjusted comparisons
                    reversed = TRUE))
}
