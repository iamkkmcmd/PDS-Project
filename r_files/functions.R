# ------------------- Custom Theme -------------------------------------
custom_theme <- function(pos = "top"){
    extrafont::loadfonts(device = "win")
    theme_bw() +
        theme(text = element_text(family = 'Bookman Old Style'),
              legend.position = pos,
              plot.title = element_text(hjust = 0.5, size = 14),
              plot.subtitle = element_text(hjust = 0.5, face = 'italic', size = 12),
              plot.caption = element_text(face = "italic", size = 10),
              axis.text.x = element_text(face = 'bold', size = 10),
              axis.title.x = element_text(face = 'bold', size = 12),
              axis.text.y = element_text(face = 'bold', size = 10),
              axis.title.y = element_text(face = 'bold', size = 12),
              panel.background = element_rect(fill = "#f5f5f5"))
}
