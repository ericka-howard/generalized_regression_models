library(ggplot2)
library(maps)
library(dplyr)

map('county', 'washington', fill = FALSE)


AllCounty <- map_data("county")
AllCities <- data("us.cities")

WACities <- us.cities %>%
  filter(country.etc == "WA",
         name=="Seattle WA") 


# geom_point(data=us.cities, aes(x=long, y=lat, size = pop))
# geom_point(data=MainCities, aes(x=long, y=lat, size = pop/1000000), 
#            color = "gold", alpha = .5) + scale_size(name="City Population")

AllCounty %>%
  filter(region=="washington") %>%
  mutate(group = as.factor(group),
         is_king = ifelse(subregion=="king", "King County", "Other")) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill = is_king), color = "black")+
  geom_point(data = WACities, aes(x = long, y=lat), size=3, shape = 19)+
  theme_void()+
  scale_fill_manual(values = c("Other"="lightgrey",
                               "King County" = "yellow"))+
  # scale_color_manual(values = c("Seattle" = "black",
  #                              "Other" = "blue"))+
  # scale_size_manual(values = c("Seattle"=3,
  #                              "Other"=1))+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "darkgrey"))+
  annotate("text", x=-122.04, y=47.61, label="Seattle")
