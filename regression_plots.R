source("C:/Program Files/Git/softball-analysis/regression_analysis.R")

label <- tibble(
    adj.DIFF = Inf,
    adj.WPER = -Inf,
    label = paste(
        "A tie counted as 0.5 Win and 0.5 Loss"
    )
)

plot1 <- ggplot(data = kcsa, mapping = aes(x = adj.DIFF, y = adj.WPER)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(limits = c(0,1)) +
    labs(
        title = paste(
            "KCSA Linear Regression Analysis (2011-2016)"
        ),
        y = paste("Adjusted Win %"),
        x = paste("Adjusted Run Diff. per Game")
    ) +
    geom_text(
        aes(label = label),
        data = label,
        hjust = "right",
        vjust = -.5
    )

plot2 <- ggplot(data = kcsa16_diff, aes(x = reorder(TEAM, Luck), y = Luck)) +
    geom_bar(stat = "identity", aes(fill = Luck)) +
    coord_flip() +
    labs(
        title = ("KCSA 2016: Over/Underachievers"),
        subtitle = ("Ordered by 'Luckiest' to 'Unluckiest'"),
        x = "Teams",
        y = "Over/Underachieving Margin"
    ) +
    scale_fill_gradientn(colours = topo.colors(2), "Over/Under")

setwd("C:/Program Files/Git/softball-analysis/plots")

png(filename="regression_plot.png", 
    type="cairo",
    units="in", 
    width=10, 
    height=7, 
    res=500)
plot1
dev.off()


png(filename="luck_plot.png", 
    type="cairo",
    units="in", 
    width=9, 
    height=10, 
    res=500)
plot2
dev.off()

