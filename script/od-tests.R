# Aim: generate inter-country OD dataset

# Load country dataset:
source("script/geocomputation_cover.R")

# Install tradestatistics package if not already installed:
if(!require(tradestatistics)) install.packages("tradestatistics")

library(tradestatistics)

as_tibble(ots_tables)
head(mp_with_countries)

# Get OD data
stats = tradestatistics::ots_create_tidy_data()
dim(stats)

nrow(stats)
names(stats)
names(mp_with_countries)

summary(stats$reporter_name %in% mp_with_countries$name) # most match


# Test example in docs:

yrp <- ots_create_tidy_data(
  years = 2018:2019,
  reporters = c("chl", "per"),
  partners = "arg",
  table = "yrp"
)

eu_countries = mp_with_countries |>
  filter(continent == "Europe") 

summary(tolower(eu_countries$adm0_a3_is) %in% stats$reporter_iso) # most match
# other way around:
summary(toupper(stats$reporter_iso) %in% mp_with_countries$adm0_a3_is) # most match

eu_countries_in_stats = eu_countries |>
  filter(tolower(adm0_a3_is) %in% stats$reporter_iso) |>
  mutate(reporter_iso = tolower(adm0_a3_is)) |>
  select(reporter_iso, name)

if (!file.exists("data/eu_flows_clean.csv")) {
  # Get EU flows:
  eu_flows = ots_create_tidy_data(
    years = 2019,
    reporters = eu_countries_in_stats$reporter_iso,
    partners = eu_countries_in_stats$reporter_iso,
    table = "yrp"
  )
  
  names(eu_flows)
  head(eu_flows)
  write_csv(eu_flows, "data/eu_flows.csv")
  
  eu_flows_clean = eu_flows |>
    select(reporter_iso, partner_iso, trade_value_usd_exp)
  write_csv(eu_flows_clean, "data/eu_flows_clean.csv")
} else {
  eu_flows_clean = read_csv("data/eu_flows_clean.csv")
}
# convert to desire lines:
eu_flows_sf = od::od_to_sf(eu_flows_clean, eu_countries_in_stats)
plot(eu_flows_sf) # Example plot: it works!
nrow(eu_flows_sf) # 1600

# Get top 100 flows:
eu_flows_top = eu_flows_sf |>
  arrange(desc(trade_value_usd_exp)) |>
  slice(1:100)

mapview::mapview(eu_flows_top)

plot(eu_flows_top)
eu_flows_top
