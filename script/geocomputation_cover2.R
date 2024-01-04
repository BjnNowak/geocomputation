library(camcorder)
library(glue)
library(MoMAColors) # dev version available on github: https://github.com/BlakeRMills/MoMAColors
library(rnaturalearth)
library(sf)
library(tidyverse)
library(colorspace)

# Set size
gg_record(dir = file.path(tempdir(), "recording2"),
        device = "png",
        width = 15.6,
        # I chose the size of the 1st edition book, so the image created here may cover all of the first page
        height = 23.4,
        units = "cm",
        # may also be exported as svg
        dpi = 600)

#############################
# Creation of the first map #
#############################

# Let"s start with the 1st map: buffer area around capitals in Africa

# Loading data
##############
# Vector files of world borders loaded from {rnaturalearth}
mp_with_countries = ne_countries(scale = 50, type = "countries", returnclass = "sf") |>
        st_transform(crs = "+proj=robin")

# Populated places layer has been downloaded from Natural Earth:
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
# For script reproducibility purposes, I uploaded a subset on my github:
cp = read_sf("https://github.com/BjnNowak/geocomputation/raw/main/data/ne_10m_populated_places.gpkg") |>
        # Select only administrative capitals
        filter(ADM0CAP == 1) |>
        # Reproject to Robinson
        st_transform(crs = "+proj=robin") |>
        # Intersection to keep only capitals in Africa or (West) Asia
        st_intersection(mp_with_countries |> filter(continent %in% c("Africa", "Asia")))

# Clean data
############
# Dissolve countries for world basemap
mp = mp_with_countries |>
        mutate(entity = "world", ct = 1) |>
        group_by(entity) |>
        summarize(sm = sum(ct)) |>
        ungroup()

# Function to create successive buffers around capitals
fun_buff = function(cp, buff){
        # Create empty list
        buff_list = list()
        # Fill list with 5 successive buffers
        for (i in 1:5) {
                buff_list[[i]] = cp |>
                        st_buffer(buff * i) |>
                        st_intersection(mp)
        }
        return(buff_list)
}

# Apply function to create list with buffers around points
buff_list = fun_buff(cp = cp, buff = 100000)

##############################
# Creation of the second map #
##############################
library(tidyterra)
library(terra)
sf_use_s2(FALSE)
myurl = "/vsicurl/https://zenodo.org/records/5774954/files/clm_snow.prob_esacci.nov_sd_500m_s0..0cm_2000..2012_v2.0.tif"
snow = rast(myurl)
europe_bbox = st_as_sfc(st_bbox(c(xmin = -25, xmax = 74, ymin = 34, ymax = 71.5), crs = "EPSG:4326"))
europe = ne_countries(scale = 50, type = "countries", returnclass = "sf") |>
        subset(continent == "Europe") |>
        st_make_valid()
europe2 = st_crop(europe, europe_bbox)
europe3 = st_transform(europe, "+proj=robin")
snow_europe = crop(snow, europe2)
snow_europe = aggregate(snow_europe, 8, na.rm = TRUE)
snow_europe = project(snow_europe, "+proj=robin")
snow_europe = mask(snow_europe, europe3)

##################
# Make final map #
##################

# Color palette for buffers
pal = moma.colors("Flash" , n = 22, type = "continuous")
alp = 1 # optional parameter to add transparencies to buffer
col_lv1 = alpha(pal[1], alp)
col_lv2 = alpha(pal[6], alp)
col_lv3 = alpha(pal[9], alp)
col_lv4 = alpha(pal[12], alp)
col_lv5 = alpha(pal[15], alp)

col_world = "#073B4C"
col_borders = "grey80"
col_back = "#1D201F"

pal_dis = moma.colors("ustwo" , n = 5, type = "discrete")

col_fossil = pal_dis[1]
col_nuke = pal_dis[3]
col_renew = pal_dis[5]

# Robinson bounding box
xlims = c(-2200000, 4500000)
ylims = c(-2000000, 8000000)

grat = st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

# mp_with_countries2 = mp_with_countries |>
#         st_cast("POLYGON") |>
#         mutate(area = as.numeric(st_area(geometry))/1e6) |>
#         filter(area > 1e3, continent %in% c("Africa", "Europe", "Asia"))

gg = ggplot() +
        # Add basemap
        geom_sf(mp, mapping = aes(geometry = geometry), fill = "#151529",
                color = alpha("white", 0.15), lwd = 0.1) +
        # First map
        ###########
        # Add successive buffers
        geom_sf(buff_list[[5]], mapping = aes(geometry = geom),
                fill = col_lv5, color = alpha("white", 0)) +
        geom_sf(buff_list[[4]], mapping = aes(geometry = geom),
                fill = col_lv4, color = alpha("white", 0)) +
        geom_sf(buff_list[[3]], mapping = aes(geometry = geom),
                fill = col_lv3, color = alpha("white", 0)) +
        geom_sf(buff_list[[2]], mapping = aes(geometry = geom),
                fill = col_lv2, color = alpha("white", 0)) +
        geom_sf(buff_list[[1]], mapping = aes(geometry = geom),
                fill = col_lv1, color = alpha("white", 0)
        ) +
        # Add countries borders above buffers
        geom_sf(mp_with_countries, mapping = aes(geometry = geometry),
                fill = NA, color = alpha("white", 0.05), lwd = 0.15) +
        # Second map
        ############
        geom_spatraster(data = snow_europe, maxcell = 500000)  +
        scale_fill_continuous_sequential("Greens", na.value = NA,
                                         rev = FALSE, end = 1) +
        # scale_fill_moma_c("Flash", na.value = NA) +
        # geom_sf(data = mp_with_countries2, col = "white", fill = NA) +
        # Add graticule
        geom_sf(grat, mapping = aes(geometry = geometry),
                color = alpha("white", 0.15)) +
        # Center map
        coord_sf(xlims, ylims) +
        # Custom theme (color background can be changed here)
        guides(fill = "none") +
        theme_void() +
        theme(plot.background = element_rect(fill = "#191930", color = "#191930"))
gg
ggsave("cov2.png", gg, device = "png", width = 15.6, height = 23.4,
       units = "cm", dpi = 600)
