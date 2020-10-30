# LINHAS HORIZONTAR + LEGENDA ACIMA E A DIREITA

theoffice_theme1 <- function() {
  theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(family = "StaffMeetingPlain"),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")
}

# LINHAS VERTICAIS

theoffice_theme2 <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = "StaffMeetingPlain"))
}
