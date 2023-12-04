#### PREPROCESS ================================================================
# ==============================================================================

# setwd to file location === #####
gcam_folder = 'gcam-core-iamcompact'
setwd(paste0('C:\\GCAM\\GCAM_7.0_Claudia\\',gcam_folder,'\\input\\gcamdata\\study7_analysis'))
.libPaths('C:\\Users\\claudia.rodes\\Documents\\R\\win-library\\4.1 - gcamdata_no_CP')

# load libraries and paths' variables, and extra functions and styles
source('load_libs_paths.R')
source('utils_data.R')
source('utils_style.R')
dir_name = 'paper_v1'
if (!dir.exists(paste0(figures_path, dir_name))) dir.create(paste0(figures_path, dir_name))

# load dataset
assign('dt_ref', get(load(paste0(outputs_path, 'REF_queries_all.RData'))))
rm(dt)

# load basic data
year_s = 2000
year_e = 2050
final_db_year <<- year_e
selected_year = 2030

load_mapping_data()
load_nutritional_data(remove_sstcols(dt_ref$pop_all_regions))

# ==============================================================================
# ================================= Fig 1 SI ===================================
# ========================== Scenarios parallel plot ===========================
# ==============================================================================

example_data = db_scen_mapping %>%
  select(scenario = scen_name) %>%
  tidyr::separate(scenario, into = c("protein_type", "final_share", "peak_year", "slope"), sep = "_", remove = FALSE) %>%
  filter(protein_type != 'REF') %>%
  mutate(region_type = ifelse(grepl('all', final_share), 'allX', '+X')) %>%
  mutate(final_share = as.numeric(gsub("[^0-9]", "", final_share)))


# map char to numbers
data = example_data %>%
  # mapping col1: scenario_protein_type (between 0.25 and 0.75)
  dplyr::mutate(col1_protein_type = match(protein_type, unique(protein_type)) / (length(unique(protein_type))+1)) %>%
  # mapping col2: scenario_region_type
  dplyr::mutate(col2_region_type = match(region_type, unique(region_type)) / (length(unique(region_type))+1)) %>%
  # mapping col3: final_share
  dplyr::mutate(col3_final_share = match(final_share, unique(final_share)) / (length(unique(final_share))+1)) %>%
  # mapping col4: peak_year
  dplyr::mutate(col4_peak_year = match(peak_year, unique(peak_year)) / (length(unique(peak_year))+1)) %>%
  # mapping col5: slope
  dplyr::mutate(col5_slope = match(slope, unique(slope)) / (length(unique(slope))+1))

# reshape data to plot
reshaped_data = data %>%
  select(tidyr::starts_with('col'), protein_type) %>%
  mutate(across(starts_with("col"), list(prev_value = ~.), .names = "prev_{.col}")) %>%
  tidyr::pivot_longer(cols = starts_with("col"), names_to = "variable", values_to = "value") %>%
  tidyr::pivot_longer(cols = starts_with("prev_col"), names_to = "prev_variable", values_to = "prev_value") %>%
  filter(variable != 'col1_protein_type', prev_variable != 'prev_col5_slope') %>%
  mutate(pairs = paste(variable, '-', prev_variable)) %>%
  filter(pairs %in% c('col2_region_type - prev_col1_protein_type',
                      'col3_final_share - prev_col2_region_type',
                      'col4_peak_year - prev_col3_final_share',
                      'col5_slope - prev_col4_peak_year')) %>%
  select(-pairs) %>%
  mutate(prev_variable = factor(stringr::str_sub(prev_variable, 6, 9))) %>%
  mutate(variable = factor(stringr::str_sub(variable, 1, 4)))

# keep the text for the plot
data = data %>%
  select(-scenario) %>%
  rename('col1_protein_type_text' = 'protein_type',
         'col2_region_type_text' = 'region_type',
         'col3_final_share_text' = 'final_share',
         'col4_peak_year_text' = 'peak_year',
         'col5_slope_text' = 'slope')
unique_letters = unique(substr(names(data[,6:10]), 1, 4))

# Combine columns by letter into pairs
result <- lapply(unique_letters, function(letter) {
  columns_to_combine <- data %>%
    dplyr::select(starts_with(letter))
  data %>%
    mutate(!!paste0(letter, "_combined") := do.call(paste, c(select(., colnames(columns_to_combine)), sep = " - "))) %>%
    select(contains("_combined"))
})

# Combine the results into a single dataframe
text_data = bind_cols(result) %>%
  tidyr::pivot_longer(cols = everything(), names_to = c(".value", "letter"), names_sep = "_") %>%
  select(-letter) %>%
  mutate(across(1:5, as.character)) %>%
  tidyr::pivot_longer(cols = 1:5, names_to = 'type', values_to = 'pairs') %>%
  distinct() %>%
  separate(pairs, into = c("text", "value"), sep = " - ") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(text = ifelse(text == '1d205', '1.205',
                       ifelse(text == '0d805', '1.805',
                              ifelse(text == '0d405','0.405',
                                     ifelse(text == '0d005', '0.005', text)))))

# Curvature factor for controlling the degree of curve
curve_factor = 0.075

# Create the plot with curved lines
desired_labels = c('Protein type','Region aggrupation','Final share','Peak year','Slope')
pl = ggplot() +
  geom_curve(data = reshaped_data %>% filter(protein_type == 'spp') %>% order_facets('protein_type'),
             aes(x = prev_variable, y = prev_value, xend = variable, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = curve_factor,
             lineend = "round", size = 0.5, alpha = 0.5) +
  geom_curve(data = reshaped_data %>% filter(protein_type == 'snr') %>% order_facets('protein_type'),
             aes(x = prev_variable, y = prev_value, xend = variable, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = -curve_factor,
             lineend = "round", size = 0.5, alpha = 0.5) +
  scale_color_manual(values = scen_palette_refVsSppVsSnr) +
  # # text
  geom_point(data = text_data, aes(x = type, y = value),
             shape = 21, size = 33, fill = "white", color = "white", alpha = 1) +
  geom_point(data = text_data, aes(x = type, y = value),
             shape = 21, size = 33, fill = "#F2F3CB", color = "black", alpha = 0.25) +
  geom_text(data = text_data,
            aes(label = text, x = type, y = value), color = 'black', size = 10) +
  # theme
  labs(title = "Overview of the considered scenarios & uncertainty runs") +
  xlab("") + ylab("") + scale_x_discrete(labels = desired_labels) +
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name,"/",'fig1_SI_overview_scen_runs.png'), width = 3401.575, height = 2267.717, res = 150)
print(pl)
dev.off()


# ==============================================================================
# ================================= Fig 2 SI ===================================
# ================================ REG - ADESA =================================
# ==============================================================================

###### compute ADESA

# Dietary energy supply
# Units: kcal/capita/day
# By region
# Get total consumption in calories
total_regional_calories <- remove_sstcols(dt_ref$food_consumption_regional) %>%
  filter(year >= year_s, year <= year_e) %>%
  group_by(scenario, region, year) %>%
  # Aggregate staple and non-staple calories
  summarize(value = sum(value)) %>%
  left_join(remove_sstcols(dt_ref$pop_all_regions), by = c("year", "scenario", "region")) %>%
  # Convert from Pcal to kcal/capita/day
  mutate(value = (value * 1e12) / (population * 365),
         units = "kcal/capita/day")
## Share of dietary energy supply from staples =================================
# Find share of staple and non-staple calories in total calories
share_diet_staples_region <- remove_sstcols(dt_ref$staples_nonstaples_regional) %>%
  mutate(staples_in_total = Staples/Total,
         nonstaples_in_total = NonStaples/Total,
         percent_staples_in_total = staples_in_total * 100)

# Calculate Average Dietary Supply Adequacy as:
# total regional calories / SUM_age,sex(calreq_a,s * pop_a,s)

# join with MDER data, calculate caloric requirements by sex and age
adesa_denominator <- weighted_pop_sex_age %>%
  left_join(mder, by = "variable") %>%
  select(-std) %>%
  group_by(scenario, scen_type, variable, year, region) %>%
  # compute a range because of differing physical activity levels
  summarize(cal_req_x_pop = mder * pop_sex_age,
            min_cal_req_x_pop = min * pop_sex_age,
            max_cal_req_x_pop = max * pop_sex_age) %>%
  # aggregate caloric requirements to get total regional values
  group_by(scenario, scen_type, region, year) %>%
  summarize(denominator_sum = sum(cal_req_x_pop),
            min_denominator_sum = sum(min_cal_req_x_pop),
            max_denominator_sum = sum(max_cal_req_x_pop)) %>%
  mutate(year = as.numeric(year))

# add in regional calorie info, calculate ADESA
adesa <- left_join(adesa_denominator, total_regional_calories) %>%
  # select(-population) %>%
  group_by(year, region, scenario, scen_type) %>%
  reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
          min_adesa = (value / min_denominator_sum) * population * 100,
          max_adesa = (value / max_denominator_sum) * population * 100)

# plot: horizontal plot ADESA vs TIME. Colors by adesa > or < 100
pl = ggplot(data = adesa %>%
              group_by(region) %>%
              mutate(region_ok = ifelse(min(adesa) < 100, FALSE, TRUE)) %>%
              ungroup() %>%
              order_facets()) +
  geom_line(aes(x = year, y = adesa, group = region,
                color = region_ok),
            linewidth = 1.5, alpha = 1) +  # Median line
  geom_text(data = adesa %>%
              group_by(region) %>%
              mutate(region_ok = ifelse(min(adesa) < 100, FALSE, TRUE)) %>%
              ungroup() %>%
              filter(year == max(year)) %>%
              order_facets(),
              aes(x = year + 0.1, y = adesa + 0.1, label = region, group = region,
                  color = region_ok),
            hjust = 0) +
  geom_hline(yintercept = 100, linetype = 'dashed', linewidth = 2, color = my_red) +
  scale_color_manual(values = c(my_gray,my_green)) +  # Custom color scale
  labs(y = 'ADESA value', x = '') +
  ggtitle('ADESA World') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
png(paste0(figures_path,dir_name,"/",'fig2_SI_adesa_vs_time_by_reg.png'), width = 3401.575, height = 2267.717, res = 150)
print(pl)
dev.off()


