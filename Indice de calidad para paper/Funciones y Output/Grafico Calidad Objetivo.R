tabla_6b <- read.csv("output\\data calidad objetivo\\6.b_distibucion_km_ciclovias_operativos_inoperativos.csv")


chart_6b <- ggplot(tabla_6b,
                   aes(x = reorder(comuna, -km_total), y = km_total, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#D6648E", "#13D657")) +
  # geom_text(aes(label = km_total), position = position_stack(vjust = 0.5)) +
  labs(fill = "Tipo de ciclovia",
       title = "Distribución de kilometros de tipo de ciclovia por comunas") +
  xlab("Comunas") +
  ylab("Kilometros Totales") +
  coord_flip() +
  # ggtitle("Distribución de tipologías de ciclovías") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "gray"))
chart_6b
ggsave("output/charts/6.b_Distribución de kilometros de tipo de ciclovia por comunas.png",
       width = 35, height = 30, units = "cm")
