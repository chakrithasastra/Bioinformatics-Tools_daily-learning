install.packages(c("xml2", "dplyr"))
library(xml2)
library(dplyr)
df<-read.csv(file.choose())
blast_xml <- read_xml("EJFTT3WA014-Alignment_SOD1.xml")
# Get all <Hit> nodes
hits <- xml_find_all(blast_xml, ".//Hit")

# Extract data for each hit
get_hit_info <- function(hit_node) {
  hit_id <- xml_text(xml_find_first(hit_node, "Hit_id"))
  hit_def <- xml_text(xml_find_first(hit_node, "Hit_def"))
  hit_len <- xml_text(xml_find_first(hit_node, "Hit_len"))
  # Extract organism name, often in Hit_def after "OS="
  # For example: "sp|Q9Y6K9|YBOX1_HUMAN Y-box-binding protein 1 OS=Homo sapiens OX=9606 GN=YBX1 PE=1 SV=2"
  organism <- sub(".*OS=([^ ]+).*", "\\1", hit_def)
  data.frame(hit_id, hit_def, hit_len, organism, stringsAsFactors = FALSE)
}

hits_info <- do.call(rbind, lapply(hits, get_hit_info))
library(ggplot2)
ggplot(data = hits_info, aes(x=reorder(organism, hit_len), y=hit_len)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  labs(title="Top 10 Organisms in BLAST Hits", x="Organism", y="Count")

highlight_hit <- "Drosophila melanogaster superoxide dismutase 1 (Sod1), mRNA"

# Plot
ggplot(data = hits_info, aes(x = organism, y =hit_len, 
                             fill = organism == highlight_hit)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "grey70")) +
  coord_flip() +
  labs(title = "Top 10 Organisms in BLAST Hits",
       x = "Organism", y = "Count")
#true -SOD1 and #FALSE - Remaining Organisms

