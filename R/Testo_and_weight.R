
dss = ds[species == 'PESA']

# model 
m <- glmmTMB(testo_log ~ sex * poly(date_doy, 2) + sex * weight + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss
)

plot(allEffects(m))
summary(m)


# extract season effect from model for plot
es = effect("sex:weight", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = dss[!is.na(weight), .(first_data = min(weight), last_data = max(weight)), by = sex]
es = merge(es, dr, by = c('sex'), all.x = TRUE)
es[, in_range := weight %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]

# factor order
es[, sex := factor(sex, levels = c('M', 'F'))]



p1 =
ggplot() +
  ggtitle('Pectoral sandpiper') + 
  geom_point(data = dss, aes(weight, testo, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = 10^fit, x = weight, color = sex), size = 0.8) +
  geom_ribbon(data = es, aes(y = 10^fit, x = weight, fill = sex, 
                             ymin = 10^lower, ymax = 10^upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "l") +  
  # scale_x_continuous(limits = c(38, 127), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Testosteron (ng/ml)') +
  xlab('Weight (g)')


dss = ds[species == 'REPH']

# model 
m <- glmmTMB(testo_log ~ sex * poly(date_doy, 2) + sex * weight + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss
)

plot(allEffects(m))
summary(m)


# extract season effect from model for plot
es = effect("sex:weight", m, xlevels = 1000) |>
  data.frame() |>
  setDT()

# subset period with data
dr = dss[!is.na(weight), .(first_data = min(weight), last_data = max(weight)), by = sex]
es = merge(es, dr, by = c('sex'), all.x = TRUE)
es[, in_range := weight %between% c(first_data, last_data), by = 1:nrow(es)]
es = es[in_range == TRUE]

# factor order
es[, sex := factor(sex, levels = c('M', 'F'))]



p2 =
  ggplot() +
  ggtitle('Red Phalarope') + 
  geom_point(data = dss, aes(weight, testo, color = sex), size = 0.5, alpha = 0.5) +
  geom_line(data = es, aes(y = 10^fit, x = weight, color = sex), size = 0.8) +
  geom_ribbon(data = es, aes(y = 10^fit, x = weight, fill = sex, 
                             ymin = 10^lower, ymax = 10^upper), alpha = 0.2) +
  scale_color_manual(values = c('#7aa048', '#E69F00')) +
  scale_fill_manual(values = c('#7aa048', '#E69F00')) +
  scale_y_log10(limits = c(0.01, 350),
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c(0.01, 0.1, 1, 10, 100)) +
  annotation_logticks(sides = "l") +  
  # scale_x_continuous(limits = c(38, 127), expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylab('Testosteron (ng/ml)') +
  xlab('Weight (g)')




# merge plots
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

ggsave('./OUTPUTS/FIGURES/testo_by_sex_and_weight.tiff', plot = last_plot(),  width = 177, height = 120,
       units = c('mm'), dpi = 'print')




##### PESA subset males
dss = ds[species == 'PESA' & sex == 'M']

# model 
m <- glmmTMB(testo_log ~ poly(date_doy, 2) + weight + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss
)

plot(allEffects(m))
summary(m)


em = effect("weight", m, xlevels = 1000) |>
  data.frame() |>
  setDT()


##### PESA subset females
dss = ds[species == 'PESA' & sex == 'F']

# model 
m <- glmmTMB(testo_log ~ poly(date_doy, 2) + weight + (1 | year_),
             family = gaussian(link = "identity"), 
             data = dss
)

plot(allEffects(m))
summary(m)


ef = effect("weight", m, xlevels = 1000) |>
  data.frame() |>
  setDT()


























# model
m <- glmmTMB(haema ~ weight + date_doy + testo_log  + (1 | year_) + (1 | ID),
             family = gaussian(link = "identity"), 
             data = dss
)

plot(allEffects(m))
summary(m)
