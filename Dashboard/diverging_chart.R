# Data Prep
theme_set(theme_bw())

ggplot(mtcars, aes(x = `car name`, y = mpg_z, label = mpg_z)) +
  geom_point(stat = "identity", fill = "black", size = 6) +
  geom_segment(aes(y = 0,
                   x = `car name`,
                   yend = mpg_z,
                   xend = `car name`),
               color = "black") +
  geom_text(color = "white", size = 2) +
  labs(title = "Diverging Lollipop Chart",
       subtitle = "subtitle") +
  ylim(-2.5, 2.5) +
  coord_flip()


# test
ex_company_scores1 <- read.csv(paste("Business Services", ".csv", sep = ""))
ex_company_scores1 <- ex_company_scores1[ex_company_scores1$revenue_bins == "5-10M", ]
ex_company_scores1 <- ex_company_scores1 %>% group_by(company_name) %>% summarise(cy_avg = mean(cy))
ex_company_scores1$cy_avg_z <- round((ex_company_scores1$cy_avg - mean(ex_company_scores1$cy_avg))/ sd(ex_company_scores1$cy_avg), 2)
ex_company_scores1$cy_avg_type <- ifelse(ex_company_scores1$cy_avg_z < 0, "below (safe)", "above (risky)")
ex_company_scores1 <- ex_company_scores1[order(ex_company_scores1$cy_avg_z), ]
ex_company_scores1 <- rbind(head(ex_company_scores1, 20), tail(ex_company_scores1, 20))
ex_company_scores1$company_name <- factor(ex_company_scores1$company_name, levels = ex_company_scores1$company_name) # convert to factor to retain sorted order in plot

# Diverging Barcharts
p <- ggplot(ex_company_scores1, aes(x = company_name, y = cy_avg_z, label = cy_avg_z,
                                    # customize the tooltip
                                    text = paste("Normalized score: ", cy_avg_z))) +
  geom_bar(stat = "identity", aes(fill = cy_avg_type), width = 0.4) +
  scale_fill_manual(name = "Risk",
                    labels = c("Above Average", "Below Average"),
                    values = c("above (risky)" = "#f8766d", "below (safe)" = "#00ba38")) +
  labs(subtitle = "Normalized cyence scores",
       title = "Diverging Bars") +
  ylab("Normalized Score") + xlab("Company Name") +
  coord_flip()


ggplotly(p, tooltip = c("text"))




