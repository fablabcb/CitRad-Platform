
theme_set(theme_linedraw() +
            theme(
              panel.grid.major = element_line(size=0.2, color="gray80"),
              panel.grid.minor= element_line(size=0.1, color="gray80"),
              text = element_text(size=16, family = "Open Sans"),
              plot.title = element_text(hjust = 0.5, size=16),
              plot.margin = unit(c(4,5.5,5.5,5.5), "pt"),
              strip.background = element_blank(),
              strip.text = element_text(colour = "black"),
              panel.background = element_rect(fill = "white"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA),
              legend.background = element_rect(fill="transparent", color=NA),
              legend.key = element_rect(fill="transparent", color=NA),
              legend.title = element_text(size=14),
              legend.text = element_text(size=13),
              legend.position="top",
              legend.direction="horizontal",
              # axis.title.y=element_text(angle=90, vjust=15)
            )
)
