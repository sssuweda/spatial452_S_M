library(tidycensus)
library(tidyverse)
library(sf)
census_api_key("306e50c218617a55c043f159a06837b609930c0b", install = TRUE)
#census_api_key("FILL IN", install = TRUE) # to get your own data, you need a Census API key (https://walker-data.com/tidycensus/articles/basic-usage.html)

v20 <- load_variables(2020, "acs5")  
#View(v20)

VARS <- c(Income = "B19013_001", Pop = 'B01003_001', Age = "B01002_001", HouseValue = "B25077_001",HouseholdSize = "B25010_001",NumHouse = "B25003_001")

tc_quant <- get_acs(state = "MN", county = c("Hennepin", "Ramsey"), geography = "tract", 
                        variables = VARS, geometry = TRUE, output='wide', key = Sys.getenv("CENSUS_API_KEY"))

tc_quant <- tc_quant %>% select(-ends_with('M'))

#BirthPlace: B06001_001
#Industry: 	C24070_001
#Race: B02001_001
get_tc_data <- function(V,V1,NAMES){
  tc_cat <- get_acs(state = "MN", county = c("Hennepin", "Ramsey"), geography = "tract", 
                        variables = V, geometry = FALSE, output='wide', summary_var = V1, key = Sys.getenv("CENSUS_API_KEY"))
  
  tc_cat %>% 
    select(-ends_with('M')) %>%
    mutate(Total = select(.,-c(GEOID,NAME,summary_est,summary_moe)) %>% rowSums(na.rm=TRUE)) %>%
    mutate(across(-c(GEOID,NAME,summary_est,summary_moe), ~.x/tc_cat$summary_est)) %>%
    select(-c(summary_est,summary_moe,Total))
}


V1 <- "C24070_001"
V <- paste0("C24070_0",str_pad(c(2:14),2,"0",side='left'))
NAMES <- v20 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
industry = get_tc_data(V,V1,NAMES)
names(industry) = c('GEOID','NAME',paste0('Industry_',str_sub(NAMES,0,5)))
NMS <- c(VARS,V)

V1 <- "B06001_001"
V <- paste0("B06001_0",str_pad(c(13,25,37,49),2,"0",side='left'))
NAMES <- v20 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
birthplace = get_tc_data(V,V1,NAMES)
names(birthplace) = c('GEOID','NAME',trimws(paste0('BirthPlace_',str_sub(NAMES,0,20))))
NMS <- c(NMS,V)

V1 <- "B25003_001"
V <- paste0("B25003_0",str_pad(c(2:3),2,"0",side='left'))
NAMES <- v20 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
housetype = get_tc_data(V,V1,NAMES)
names(housetype) = c('GEOID','NAME',trimws(paste0('HouseType_',str_sub(NAMES,0,20))))
NMS <- c(NMS,V)


V1 <- "B02001_001"
V <- paste0("B02001_0",str_pad(c(2:8),2,"0",side='left'))
NAMES <- v20 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
race = get_tc_data(V,V1,NAMES)
names(race) = c('GEOID','NAME',trimws(paste0('Race_',str_sub(NAMES,0,26))))
NMS <- c(NMS,V)

tc_data <- tc_quant %>% left_join(birthplace) %>% left_join(industry) %>% left_join(housetype) %>% left_join(race)


colleges <- read_csv('hd2020.csv')
colleges <- sf::st_as_sf(colleges,coords = c('LONGITUD','LATITUDE'))
st_crs(colleges) <- tc_data %>% st_crs()

col_coord <- data.frame(st_coordinates(colleges))

colleges_sub <- colleges[col_coord$X > st_bbox(tc_data)$xmin &  col_coord$X < st_bbox(tc_data)$xmax & col_coord$Y > st_bbox(tc_data)$ymin &  col_coord$Y < st_bbox(tc_data)$ymax,]


areawater_1 <- read_sf('tl_2019_27123_areawater') 
areawater_2 <- read_sf('tl_2019_27053_areawater') 

roads <- read_sf('roads')
roads <- st_transform(roads,crs = st_crs(tc_data))
roads_sub <- roads %>% filter( (st_intersects(roads,tc_data) %>% lengths()) > 0)


elevation <- elevatr::get_elev_raster(tc_data, z = 9, clip = "bbox")
raster::crs(elevation) <- sf::st_crs(tc_data)

# Convert to data frame for plotting
elev_df <- elevation %>% terra::as.data.frame(xy = TRUE)
colnames(elev_df) <- c("x", "y", "elevation")

tc_data$avg_elevation <- raster::extract(elevation, tc_data, fun=mean, na.rm=TRUE)

#library(crsuggest)
#suggest_crs(tc_data)

colleges_sub <- st_transform(colleges_sub,crs = 6505)
tc_data <- st_transform(tc_data,crs = 6505)
tc_data$AREA = st_area(tc_data) %>% as.vector()
areawater_1 <- st_transform(areawater_1,crs=6505) %>% filter(AWATER > 435600)
areawater_2 <- st_transform(areawater_2,crs=6505) %>% filter(AWATER > 435600)
areawater <- rbind(areawater_1 ,areawater_2)


roads_sub <- st_transform(roads_sub,crs=6505) %>% filter(RTTYP %in% c('U','I'))
roads_sub <- st_crop(roads_sub,st_bbox(tc_data))


# Num of colleges within 1/2 mile radius
tc_data$NumColleges <- st_intersects(tc_data,st_buffer(colleges_sub,dist=800)) %>% lengths()

tc_data$DistToRiver = st_distance(tc_data, st_union(areawater_1 %>% filter(FULLNAME == 'Mississippi Riv'),areawater_2 %>% filter(FULLNAME == 'Mississippi Riv'))) %>% as.vector()

#Lake greater than 10 acres
tc_data$DistToLake = st_distance(tc_data, areawater %>% filter(FULLNAME != 'Mississippi Riv')) %>% apply(1,min)

distToRoads <- st_distance(tc_data,roads_sub) %>% units::drop_units() %>% as.matrix() 
tc_data$MinDistToHwy = distToRoads %>% apply(1,min)
tc_data$NumHwys = distToRoads %>% apply(1,function(v) length(unique(roads_sub$FULLNAME[v == 0])))
tc_data$AnyHwys = tc_data$NumHwys > 0

tc_data <- tc_data %>% filter(!is.na(HouseValueE))

CodeBook <- v20 %>% filter(name %in% NMS)


save(colleges_sub,tc_data,areawater,roads_sub,CodeBook,file = 'SpatialData.RData')





## Plotting
tc_data %>%
  ggplot() + 
  geom_sf(aes(fill = HouseValueE/1000),alpha=.7,color='white',size = .2) + 
  geom_sf(data=colleges_sub,color = 'black') + 
  geom_sf(data=areawater,color='lightblue',fill='lightblue')+
  geom_sf(data=roads_sub, color = 'yellow')+
  scale_fill_viridis_c(option = "magma",    trans = "log10", # Apply log10 transformation to the fill aesthetic
                breaks = scales::log_breaks(n = 5), # Automatically generate log-based breaks
  ) + 
  coord_sf(xlim = st_bbox(tc_data)[c(1,3)], # min & max of x values
           ylim = st_bbox(tc_data)[c(2,4)]) +
  labs(fill = 'Median Home \nValue ($1000s)', title='2020 ACS5 Data') +
  theme_classic()


tc_data %>%
  ggplot() + 
  geom_sf(aes(fill = `Race_White alone`),alpha=.7,color='white',size = .2) + 
  geom_sf(data=colleges_sub,color = 'black') + 
  geom_sf(data=areawater,color='lightblue',fill='lightblue')+
  geom_sf(data=roads_sub, color = 'yellow')+
  scale_fill_viridis_c(option = "magma") + 
  coord_sf(xlim = st_bbox(tc_data)[c(1,3)], # min & max of x values
           ylim = st_bbox(tc_data)[c(2,4)]) +
  labs(fill = 'Percent White Only', title='2020 ACS5 Data') +
  theme_classic()

tc_data %>%
  ggplot(aes(fill = AgeE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot() + 
  geom_sf(color = NA, aes(fill = avg_elevation)) + 
  geom_sf(data=areawater,color='lightblue',fill='lightblue')+
  geom_sf(data=roads_sub, color = 'yellow')+
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = DistToRiver)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = DistToLake)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = log(HouseValueE))) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = AREA)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = NumHouseE)) + #Number of Households 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = NumHouseE/AREA)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = PopE/AREA)) +  #population density
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = PopE/NumHouseE)) + #Population per Household
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = HouseholdSizeE)) + #Average Household Size
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = `HouseType_Owner occupied`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = `HouseType_Renter occupied`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = PopE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = log(IncomeE))) + #Median Income
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()


tc_data %>%
  ggplot(aes(fill = `BirthPlace_Born in state of res`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = `BirthPlace_Born in other state`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()


tc_data %>%
  ggplot(aes(fill = `BirthPlace_Foreign born:`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>%
  ggplot(aes(fill = `BirthPlace_Native; born outside`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()


tc_data %>%
  ggplot(aes(fill = `Industry_Publi`)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") + 
  theme_classic()

tc_data %>% ggplot()+
  geom_boxplot(aes(y = AgeE, x = factor(NumColleges)))

tc_data %>% ggplot(aes(y = HouseValueE, x = IncomeE, color = factor(NumColleges)))+
  geom_point() +
  geom_smooth(method='lm', se = FALSE) + 
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = HouseholdSizeE, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `BirthPlace_Born in state of res`, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `BirthPlace_Born in other state`, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `BirthPlace_Foreign born:`, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `Industry_Const`, color = factor(NumColleges))) +
  theme_classic()


tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `Industry_Manuf`, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot()+
  geom_point(aes(y = HouseValueE, x = `Industry_Educa`, color = factor(NumColleges))) +
  theme_classic()

tc_data %>% ggplot(aes(y = HouseValueE, x = `Industry_Profe`))+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_classic()


tc_data %>% ggplot(aes(y = HouseValueE, x = DistToRiver))+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_classic()

tc_data %>% ggplot(aes(y = HouseValueE, x = cut(DistToRiver,breaks=c(-1,0,5280,2*5280,3*5280,5*5280,130000))))+
  geom_boxplot() +
  theme_classic()

tc_data %>% ggplot(aes(y = HouseValueE, x = DistToLake))+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_classic()

tc_data %>% ggplot(aes(y = HouseValueE, x = cut(DistToLake,breaks=c(-1,0,5280,2*5280,50000))))+
  geom_boxplot() +
  theme_classic()

