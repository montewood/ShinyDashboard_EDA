
#chart function

shiny_barchart <- function(data, x, col){
    data %>% ggplot(aes_string(x = x, fill = col)) + geom_bar(stat = 'count')
}

shiny_densitychart <- function(data, x, col){
    data %>% ggplot(aes_string(x = x, col = col)) + geom_density(stat = 'count')
}

shiny_boxchart <- function(data, op1, op2, boxval){

    boxoption1 <- sym(op1)
    boxoption2 <- sym(op2)
    boxvalue <- sym(boxval)

    data %>% group_by(!!boxoption1, !!boxoption2) %>%
        summarise(sum_n = sum(!!boxvalue)) %>%
        ungroup() %>%
        ggplot(aes(x = as.factor(!!boxoption1), y = sum_n)) +
        geom_boxplot()
}

shiny_qqchart <- function(data, x){
    na.omit(data) %>% ggplot(aes_string(sample = x)) + geom_qq() + geom_qq_line()
}

shiny_scatterchart <- function(data, x, y, color){
    data %>% ggplot(aes_string(x = x, y = y, color = color)) + geom_point()
}

shiny_linechart <- function(data, x, y){

    lineaxis_x <- sym(x)
    lineaxis_y <- sym(y)

    na.omit(data) %>% group_by(!!lineaxis_x) %>% summarise(y_value = sum(!!lineaxis_y)) %>% ungroup() %>%
        ggplot(aes(x = !!lineaxis_x, y = y_value)) + geom_line()
}

# chart_options function

chart.labs <- function(chart_title, chart_x, chart_y){
    labs(title = chart_title ,x = chart_x , y = chart_y)
}

chart.themes <- function(title, x, y){
    theme(plot.title = element_text(size = as.numeric(title))) +
        theme(axis.title.x = element_text(size = as.numeric(x))) +
        theme(axis.title.y = element_text(size = as.numeric(y)))
}

# chart_themes function

theme_list <- c('None', 'theme_base', 'theme_calc', 'theme_economist', 'theme_excel', 'theme_few',
                'theme_fivethirtyeight', 'theme_gdocs', 'theme_hc', 'theme_par', 'theme_pander',
                'theme_solarized', 'theme_stata', 'theme_tufte', 'theme_wsj')

shiny_themes <- function(themes){
    eval(parse(text = paste0(themes, '()')))
}
