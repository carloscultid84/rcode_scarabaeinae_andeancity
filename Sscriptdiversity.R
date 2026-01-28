# ============================================================================
# DIVERSITY ANALYSIS SCRIPT
# Analysis of dung beetle communities using abundance and biomass data
# ============================================================================

# Load required libraries
library(ggplot2)
library(iNEXT)

# Set working directory
setwd("~/Ecourban")

# ============================================================================
# SECTION 1: ABUNDANCE-BASED DIVERSITY ANALYSIS
# ============================================================================

# 1.1 Load and inspect abundance data
abundance_data <- read.csv("zonesNUM.csv", header = TRUE, sep = ";")

print(abundance_data)
print(summary(abundance_data))

# 1.2 Prepare data for iNEXT analysis
# Convert to list format and remove zero values
abundance_list <- list(
  IU = abundance_data$IU[abundance_data$IU > 0],
  PU = abundance_data$PU[abundance_data$PU > 0],
  EU = abundance_data$EU[abundance_data$EU > 0]
)

# 1.3 iNEXT analysis for abundance data
out_abundance <- iNEXT(abundance_list, q = c(0, 1, 2), datatype = "abundance")
print(out_abundance)

# 1.4 Visualize abundance-based diversity
ggiNEXT(out_abundance, type = 1, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Sample-size-based Rarefaction/Extrapolation - Abundance")
ggiNEXT(out_abundance, type = 2, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Coverage-based Rarefaction/Extrapolation - Abundance")
ggiNEXT(out_abundance, type = 3, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Combined Rarefaction and Extrapolation - Abundance")

# ============================================================================
# SECTION 2: BIOMASS-BASED DIVERSITY ANALYSIS
# ============================================================================

# 2.1 Load and inspect biomass data
biomass_data <- read.csv("zonesBIO.csv", header = TRUE, sep = ";")
print(biomass_data)

# 2.2 Prepare data for iNEXT analysis
# Convert to list format and remove zero values
biomass_list <- lapply(biomass_data, function(x) x[x > 0])


# 2.3 iNEXT analysis for biomass data

out_biomass <- iNEXT(biomass_list, q = c(0, 1, 2), datatype = "abundance")
print(out_biomass)

# 2.4 Visualize biomass-based diversity
ggiNEXT(out_biomass, type = 1, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Sample-size-based Rarefaction/Extrapolation - Biomass")
ggiNEXT(out_biomass, type = 2, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Coverage-based Rarefaction/Extrapolation - Biomass")
ggiNEXT(out_biomass, type = 3, facet.var = "Assemblage", color.var = "Order.q") +
  ggtitle("Combined Rarefaction and Extrapolation - Biomass")

# ============================================================================
# SECTION 3: EFFECTIVE NUMBER OF SPECIES (ENS) PROFILES
# ============================================================================

# 3.1 Load biomass-based ENS profiles
biomass_ens <- read.csv("Perfilesbio.csv", header = TRUE, sep = ";")

print(biomass_ens)

# Set factor levels for urban zones
biomass_ens$Zone <- factor(biomass_ens$Zone, levels = c("EU", "PU", "IU"))
biomass_ens$Order <- as.factor(biomass_ens$Order)

# 3.2 Plot biomass-based ENS profiles
biomass_ens_plot <- ggplot(biomass_ens, aes(x = Zone, y = Est, 
                                            group = Order, shape = Order)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Est - R, ymax = Est + R), width = 0.2) +
  labs(title = "Effective Number of Species (ENS) - Biomass Based",
       y = "ENS (based on biomass)",
       x = "Urban Zone",
       shape = "Diversity Order (q)") +
  theme_classic() +
  theme(
    text = element_text(size = 12, family = "Times New Roman"),
    panel.background = element_rect(fill = "white", color = 'black', linewidth = 1),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("q=0 (Richness)", "q=1 (Shannon)", "q=2 (Simpson)"))

print(biomass_ens_plot)

# 3.3 Load abundance-based ENS profiles
abundance_ens <- read.csv("Perfilesnum.csv", header = TRUE, sep = ";")

print(abundance_ens)

# Set factor levels for urban zones
abundance_ens$Zone <- factor(abundance_ens$Zone, levels = c("EU", "PU", "IU"))
abundance_ens$Order <- as.factor(abundance_ens$Order)

# 3.4 Plot abundance-based ENS profiles
abundance_ens_plot <- ggplot(abundance_ens, aes(x = Zone, y = Est, 
                                                group = Order, shape = Order)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Est - R, ymax = Est + R), width = 0.2) +
  labs(title = "Effective Number of Species (ENS) - Abundance Based",
       y = "ENS (based on abundance)",
       x = "Urban Zone",
       shape = "Diversity Order (q)") +
  theme_classic() +
  theme(
    text = element_text(size = 12, family = "Times New Roman"),
    panel.background = element_rect(fill = "white", color = 'black', linewidth = 1),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("q=0 (Richness)", "q=1 (Shannon)", "q=2 (Simpson)"))

print(abundance_ens_plot)

