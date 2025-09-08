# ====== Packages ======
library(haven)
library(dplyr)
library(metafor)
library(clubSandwich)
library(ggplot2)
library(ggtext)   # for bold y-axis labels

# ====== Load data ======
campos <- read_sav("IPD_Step2_CAMPOS_SC_NEW_FOR PLOT.sav")

#############################
#### RQ1a (S-S/S-I vs. I-I) #
#############################

rq1_data <- subset(campos, RQ == 1)

meta_data <- data.frame(
  row_id    = seq_len(nrow(rq1_data)),
  Study_ID  = rq1_data$Study_ID,
  Sample_ID = rq1_data$Sample_ID,
  yi        = rq1_data$HedgesG,
  sei       = rq1_data$SE
)

# ====== Multilevel meta-analysis ======
res_rq1 <- rma.mv(yi, V = sei^2, random = ~ 1 | Sample_ID, data = meta_data)
print(res_rq1)

# ====== ID → Label mapping (EDIT ONLY the pairs below) ======
meta_data <- meta_data %>%
  mutate(
    Study_ID_chr  = trimws(as.character(Study_ID)),
    Sample_ID_chr = trimws(as.character(Sample_ID)),
    id_combo      = paste0(Study_ID_chr, Sample_ID_chr),
    id_key        = tolower(id_combo)
  )

# >>> EDIT ONLY THIS: wrap labels in **bold** if you want them bold <<<
study_lookup <- c(
  "1a" = "**Bureau, 2014**",
  "2b" = "**Carcamo, n.d.**",
  "3c" = "**Colonnessi, 2013**",
  "4d" = "**Eiden, 2002**",
  "5e" = "**George, 2010 (CAB)**",
  "6e" = "**George, 2010 (CBS)**",
  "7f" = "**Grossmann, 2002**",
  "8g" = "**Laurent, 2008**",
  "9h" = "**Loheide-Niesmann , 2020**",
  "10I" = "**Monteiro, 2008**",
  "11J" = "**Olsavsky , 2020 (Empathy)**",
  "12J" = "**Olsavsky , 2020 (Prosociality)**",
  "13K" = "**Paquette , 2024**",
  "14L" = "**Sagi-Schwartz , 1985**",
  "15M" = "**Steele, 1996**",
  "16N" = "**Suess, 1992**",
  "17O" = "**Volling, 2014**",
  "18P" = "**Witte, 2021 (Empathy)**",
  "19P" = "**Witte, 2021 (EmQue)**",
  "20P" = "**Witte, 2021 (EU)**",
  "21P" = "**Witte, 2021 (Prosocial)**"
  
  # add more like: "4d" = "**Smith et al. (2010), Sample D**"
)
# ------------------------------------------------------------

names(study_lookup) <- tolower(names(study_lookup))

lbl <- unname(study_lookup[meta_data$id_key])
lbl[is.na(lbl)] <- meta_data$id_combo[is.na(lbl)]
meta_data$study <- lbl

# ====== Prep & ordering for plot ======
df <- meta_data %>%
  mutate(
    ci_lb          = yi - 1.96 * sei,
    ci_ub          = yi + 1.96 * sei,
    Study_ID_num   = suppressWarnings(as.numeric(Study_ID_chr)),
    Sample_ID_chr  = Sample_ID_chr
  ) %>%
  arrange(Study_ID_num, Sample_ID_chr)

df$study <- factor(df$study, levels = rev(df$study))

# Weights (for square sizes and %)
wts_raw    <- as.numeric(weights(res_rq1))
df$wts_pct <- 100 * wts_raw / sum(wts_raw, na.rm = TRUE)

# Right-column text
df$est_txt <- sprintf("%.2f [%.2f, %.2f]", df$yi, df$ci_lb, df$ci_ub)
df$wts_txt <- sprintf("%.1f", df$wts_pct)

# Layout calculations - FORCE WIDER X-AXIS
rng <- range(c(df$ci_lb, df$ci_ub), na.rm = TRUE)
dr  <- diff(rng); if (!is.finite(dr) || dr == 0) dr <- 1

# FORCE the axis to be wider by setting fixed positions
axis_min  <- -20
axis_max  <-  20
axis_width <- axis_max - axis_min  # = 30

x_ci_min <- axis_min
x_ci_max <- axis_max

# Position right columns way out to the right
x_gcol <- axis_max + 8    # Effect column starts 8 units after axis ends
x_wcol <- x_gcol + 12     # Weight column starts 12 units after Effect
x_end  <- x_wcol + 6      # End 6 units after Weight
x_plot_min <- axis_min - 2  # Start plot area 2 units before axis

tick_vals <- c(-10,-8, -5, 0,3, 5, 10)
axis_y    <- 0.20

# Vertical dashed line range
y_min_seg <- 1.05
y_max_seg <- length(levels(df$study)) + 0.6

# Padding
y_top_pad       <- 1.9
y_bottom_pad    <- 1.2
bottom_title_px <- 12

# ====== Plot (Option 1: smaller squares so CIs are visible) ======
p <- ggplot(df, aes(y = study)) +
  # CI bars first
  geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub),
                 height = 0, linewidth = 0.5, colour = "grey40", alpha = 0.8, na.rm = TRUE) +
  # Weighted squares (smaller range than before)
  geom_point(aes(x = yi, size = wts_pct), shape = 15) +
  scale_size_continuous(range = c(1.2, 3.5), guide = "none") +
  
  # ONLY dashed vertical line at 0
  geom_segment(aes(x = 0, xend = 0, y = y_min_seg, yend = y_max_seg),
               linetype = 2, linewidth = 0.6, inherit.aes = FALSE) +
  
  # Right-side columns
  geom_text(aes(x = x_gcol, label = est_txt), hjust = 0, size = 3.1) +
  geom_text(aes(x = x_wcol, label = wts_txt), hjust = 0, size = 3.1) +
  
  # Headers
  annotate("text", x = x_gcol, y = length(levels(df$study)) + 0.95,
           label = "Effect [95% CI]", fontface = 2, hjust = 0, size = 3.3) +
  annotate("text", x = x_wcol, y = length(levels(df$study)) + 0.95,
           label = "Weight (%)", fontface = 2, hjust = 0, size = 3.3) +
  
  # Bottom baseline axis
  annotate("segment", x = axis_min, xend = axis_max, y = axis_y, yend = axis_y,
           linewidth = 0.9) +
  geom_segment(data = data.frame(x = tick_vals),
               aes(x = x, xend = x, y = axis_y, yend = axis_y + 0.16),
               inherit.aes = FALSE, linewidth = 0.48) +
  annotate("text", x = tick_vals, y = axis_y - 0.34,
           label = tick_vals, size = 3.2, vjust = 1) +
  
  # Limits - adjusted for better x-axis visibility
  scale_x_continuous(
    limits = c(x_plot_min, x_end),
    breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))  # reduced expansion for more space
  ) +
  scale_y_discrete(expand = expansion(add = c(y_bottom_pad, y_top_pad))) +
  
  labs(x = "Effect Size", y = NULL, title = "Forest Plot") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.y = element_markdown(hjust = 0),   # <-- left-aligned labels
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.line.x        = element_blank(),
    axis.text.x        = element_blank(),
    axis.title.x       = element_text(margin = margin(t = bottom_title_px)),
    plot.margin        = margin(14, 70, 36, 28)  # increased right margin
  ) +
  coord_cartesian(clip = "off")

print(p)
