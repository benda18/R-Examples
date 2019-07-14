
# #source: stop_verify.R----
# #code
# ggmap(get_stamenmap(bbox = c(left = stop.bbox2$left - padding, bottom = stop.bbox2$bottom - padding,
#                              right = stop.bbox2$right + padding, top = stop.bbox2$top + padding),
#                     zoom = 11, 
#                     maptype = "toner-lite")) +
#   geom_point(data = stops.sids[stops.sids$color != "no change" & 
#                                  stops.sids$route_short_name != "40X",], 
#              aes(x = stop_lon, y = stop_lat, color = route_short_name)) +
#   facet_grid(~color) +
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank(), 
#         axis.ticks = element_blank()) +
#   coord_quickmap()

# #source: Span_of_Service.R
# #code
# ggplot() + 
#   geom_segment(data = all.sids, 
#                size = 3,
#                aes(x = start_date, xend = end_date, 
#                    y = service_id, yend = service_id, 
#                    color = as.factor(feed))) +
#   theme(strip.text.x = element_text(angle = 0),
#         strip.text.y = element_text(angle = 0),
#         axis.text.x = element_text(angle = 90,  vjust = 0)) +
#   scale_linetype(labels = waiver()) +
#   scale_y_discrete(name = "service_id") +
#   scale_x_date(name = "Dates of Span", 
#                date_labels = "%Y", date_breaks = "1 year", 
#                limits = c(ymd(20131225),ymd(20201231))) +
#   geom_vline(data = feed.index[colnames(feed.index) != "feed"], color = "black",
#              aes(xintercept = date, linetype = factor(format(date, "%D")))) +
#   facet_grid(feed~., as.table = FALSE, scales = "free", space = "free", margins = FALSE)+
#   labs(title = "Spans of Service in GTFS feeds\nPrior to Analysis",
#        subtitle = "Too much overlap; 2017 and 2018 didn't yield\nanticipated results", 
#        color = "Feed Name", 
#        linetype = "Week of Service")

# #source: working2.R, 
# #code: 
# fun_map(bbox = fun_bbox(data = master.36$stops,#[master.36$stops$stop_code %in% change.stickers,], 
#                         padding = 0.008), zoom = 14) +
#   geom_point(data = master.36$stops,#[master.36$stops$stop_code %in% change.stickers,], 
#              color = "white", #fill = "red", 
#              size = 3, shape = 24,
#              aes(x = stop_lon, y = stop_lat, 
#                  fill = sticker.backup)) +
#   geom_label_repel(data = master.36$stops,#[master.36$stops$stop_code %in% change.stickers,], 
#                    min.segment.length = 0, point.padding = unit(0.125, units = "inches"),
#                    size = 3,
#                    aes(x = stop_lon, y = stop_lat, 
#                        #label = stop_code, 
#                        label = sticker,
#                        color = sticker.backup)) + facet_grid(~sticker_f)

# #source: 2019_stop_changes.R
# #code: 
# fun_map(bbox = fun_bbox(filtered.data$stops), 
#         padding = p, zoom = z)+
#   geom_path(data = filtered.data$shapes, 
#             color = "black", size = 2,
#             aes(x = shape_pt_lon, y = shape_pt_lat, 
#                 group = shape_id)) +
#   geom_label_repel(data = sign.stickers[!sign.stickers$stop_code %in% c(8355,8001),], 
#                    size = 3, min.segment.length = 0, fill = "light grey",
#                    label.size = 0.75, segment.size = 1.5,
#                    fontface = "bold", 
#                    point.padding = unit(0.125, units = "inches"),
#                    aes(x = stop_lon, y = stop_lat, label = sticker,
#                        color = sticker_f)) +
#   geom_point(data = sign.stickers[!sign.stickers$stop_code %in% c(8355,8001),], 
#              size = 4, shape = 21, fill = "dark grey",
#              aes(x = stop_lon, y = stop_lat, color = sticker_f)) +
#   labs(title = paste("Route", route_names, "-", dir, "sign stickers", sep = " "), 
#        subtitle = date) +
#   theme(legend.position = "right") 

# #source: newbernvote.R
# #code:
# ggplot() +
#   geom_polygon(data = p.maps[p.maps$Muni %in% "RAL",], 
#                #geom_polygon(data = p.maps, 
#                color = "white", 
#                #fill = "light blue", 
#                aes(x = x, y = y, group = ENR_DESC, 
#                    fill = `Turnout %`)) +
#   geom_path(data = brt.nb, color = "red",
#             aes(x = X, y = Y, group = group)) +
#   scale_fill_viridis_c(option = "D") +
#   coord_equal() +
#   theme_minimal() + 
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank()) +
#   labs(title = "Voting Precinct Turnout for 2016 Sales Tax Referendum", 
#        fill = "Voter Turnout")
# ggplot() +
#   geom_polygon(data = p.maps[p.maps$Muni %in% "RAL",], 
#                color = "white", 
#                #fill = "light blue", 
#                aes(x = x, y = y, group = ENR_DESC, 
#                    fill = pct_for)) +
#   geom_path(data = brt.nb, color = "red",
#             aes(x = X, y = Y, group = group)) +
#   scale_fill_viridis_c(option = "D") +
#   coord_equal() +
#   theme_minimal() + 
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank()) +
#   labs(title = "Voting Precinct Support for 2016 Sales Tax Referendum", 
#        fill = "% Voting YES")
# ggplot() +
#   geom_polygon(data = p.maps[p.maps$Muni %in% "RAL",], 
#                color = "white", 
#                #fill = "light blue", 
#                aes(x = x, y = y, group = ENR_DESC, 
#                    fill = Against / `Registered Voters` * 100)) +
#   geom_path(data = brt.nb, color = "red",
#             aes(x = X, y = Y, group = group)) +
#   scale_fill_viridis_c(option = "A") +
#   coord_equal() +
#   theme_minimal() + 
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank()) +
#   labs(title = "Voting Precinct Support for 2016 Sales Tax Referendum", 
#        fill = "% of Registered\nVoters who Turned\nout and Voted NO")

# #source: RoutePlanningGroup.R
# #code: 
# ggplot() + 
#   labs(title = "Planning Group Projects", 
#        subtitle = format(Sys.Date(), format = "%B %d, %Y"),
#        color = "Owner") +
#   geom_blank(data = group.work) + 
#   geom_vline(xintercept = Sys.Date(),color = "#aa6666", size = 1, linetype = "2232") +
#   geom_segment(data = gw.segs[gw.segs$proj.arrow != TRUE,], 
#                size = 1.75,
#                aes(x = proj.start, xend = proj.end, 
#                    y = project, yend = project, 
#                    color = owner_f)) + 
#   geom_segment(data = gw.segs[gw.segs$proj.arrow == TRUE,], 
#                #arrow.fill = "white", 
#                size = 1.75,
#                arrow = arrow(type = "closed", angle = 35, 
#                              length = unit(0.075, "inches")),
#                aes(x = proj.start, xend = proj.end, 
#                    y = project, yend = project, 
#                    color = owner_f)) + 
#   geom_point(data = gw.points, 
#              aes(x = due, y = project)) + 
#   geom_text(data = gw.points, 
#             aes(x = due, y = project, label = task)) +
#   
#   scale_x_date(name = "Date", 
#                date_labels = "%b '%y", 
#                date_breaks = "3 months",
#                date_minor_breaks = "1 month") +
#   scale_y_discrete(name = element_blank()) +
#   scale_color_brewer(palette = "Dark2")+
#   theme_bw() +
#   theme(strip.text.y = element_text(angle = 0), 
#         #axis.text.y = element_blank(),
#         #axis.ticks.y = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "right",
#         panel.background = element_rect(color = "light grey",  size = 1)) +
#   coord_cartesian(xlim = c(min(c(Sys.Date(),
#                                  group.work$due)[!is.na(c(Sys.Date(), 
#                                                           group.work$due))]), 
#                            max(c(group.work$due, 
#                                  group.work$proj.end)[!is.na(c(group.work$due, 
#                                                                group.work$proj.end))]))) +
#   facet_grid(area~owner_f, drop = TRUE, scales = "free_y", space = "free_y") 

