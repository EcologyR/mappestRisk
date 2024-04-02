

plot_uncertainties <- function(bootstrap_uncertainties_tpcs,
                               temp,
                               dev_rate,
                               species,
                               life_stage) {

  devdata <- tibble(temp,
                    dev_rate)
  central_curve <- bootstrap_uncertainties_tpcs |>
    filter(curvetype == "estimate")
  uncertainty_curves <- bootstrap_uncertainties_tpcs |>
    filter(curvetype == "uncertainty")

  my_title <- substitute(italic(paste(x)), list(x = species))
  plot_boot_tpcs <- ggplot() +
    geom_line(data = uncertainty_curves,
              aes(x = temp,
                  y = pred,
                  group = iter),
              col = "#0E4D62", #'#586A64',
              alpha = 0.08,
              linewidth = 0.32) +
    geom_line(data = central_curve,
              aes(x = temp,
                  y = pred),
              col = "#CF8143", #'#B1492E',
              linewidth = .85) +
    geom_point(data = devdata,
               aes(temp, dev_rate),
               size = 2) +
    facet_wrap(~model_name, scales = "free")+
    scale_x_continuous(limits = c(0, 50))+
    scale_y_continuous(limits = c(0, max(bootstrap_uncertainties_tpcs$pred)))+
    ggthemes::theme_few(base_size = 12) +
    labs(x = 'Temperature (ºC)',
         y = italic(R)(T)~(d^-1),
         title = my_title,
         subtitle = life_stage,
         caption = "Bootstrapping with residual resampling, see `rTPC` package vignettes"
    )
  return(plot_boot_tpcs)
}
