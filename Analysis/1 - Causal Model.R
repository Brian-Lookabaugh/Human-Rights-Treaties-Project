############################################################################
##################--------------HR Treaty DAG-------------##################
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation
  "dagitty", # DAG Math
  "ggdag", # DAG Visualization
  install = FALSE
)

# HR Treaty DAG
node.info <- tribble(
  ~name, ~label, ~x, ~y,
  "hrt", "Human Rights Treaty", 0, 0,
  "rep", "Repression", 1, 0,
  "ei", "Interdependence", -.25, 1,
  "ingo", "INGOs", .75, -1,
  "dem", "Democracy", 0.75, 1,
  "exw", "External War", .25, 1,
  "ji", "Judicial Independence", -.75, -1,
  "ns", "Naming & Shaming", -.75, 1,
  "pop", "Population", -.25, -1,
  "vd", "Violent Dissent", -1, 0,
  "dev", "Development", -1, -1,
  "mil", "Military Size", 1, 1
)

## Creating a Node Labels Object
node.labels <- node.info$label
names(node.labels) <- node.info$name

## Creating and Tidying the DAG Object
hrt.dag <- dagify(
  hrt ~ ei + dem + mil + ns + ingo,
  rep ~ vd + ei + dem + hrt + ji + ingo + ns + mil,
  ei ~ dem,
  ingo ~ dem,
  dem ~ pop,
  exw ~ ei + dem,
  ji ~ dem + exw + ji,
  ns ~ ingo,
  vd ~ dev + pop + exw + dem + mil,
  dev ~ pop + exw + ei + dem + ingo,
  mil ~ exw + dev,
  exposure = "hrt",
  outcome = "rep",
  labels = node.labels,
  coords = node.info
) %>%
  tidy_dagitty()

## Assigning Types to the Objects
hrt.dag <- hrt.dag %>%
  mutate(
    type = case_when(
      name == "hrt" ~ 1,
      name == "rep" ~ 2,
      name %in% c("dem", "ei", "ingo", "mil", "ns") ~ 3,
      name %in% c("dev", "pop", "vd", "ji", "exw") ~ 4
    )
  )

## Creating the DAG Plot
hrt.dag.plot <-
  ggplot(hrt.dag, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
  geom_dag_edges() +
  geom_dag_point(aes(color = as.factor(type))) +
  geom_dag_label_repel(
    aes(label = label, fill = as.factor(type)),
    seed = 1234,
    color = "white",
    fontface = "bold",
    box.padding = 3,
    force = 2
  ) +
  scale_color_manual(values = c("#42be71", "#228b8d", "#471164", "grey20", "grey")) +
  scale_fill_manual(values = c("#42be71", "#228b8d", "#471164", "grey20", "grey")) +
  guides(color = "none", fill = "none") +
  theme_dag()

ggsave(
  "hrt.dag.png",
  width = 10,
  height = 6,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/Human Rights Replication/Graphics"
)
