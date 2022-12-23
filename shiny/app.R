this_month_start <- lubridate::floor_date(lubridate::today("CET"), unit = "month")
last_month_start <- clock::add_months(this_month_start, -1)

meter_data <- eldata::lazy_read_meter_data()
spot_prices <- eldata::lazy_read_spot_prices()
fees <- eldata::lazy_read_fees()
consumption_and_prices <- eldata::lazy_join_all(meter_data, spot_prices, fees)
price_tbl <- eldata::lazy_read_prices(consumption_and_prices)
daily_tbl <- eldata::lazy_daily_prices(price_tbl)

metering_points_with_data <- meter_data |>
    dplyr::distinct(MeterId) |>
    dplyr::collect()

metering_points_info <- eldata::read_metering_points_info() |>
    dplyr::inner_join(metering_points_with_data, by = 'MeterId')

metering_points <- metering_points_info$MeterId
names(metering_points) <- metering_points_info$Name


ui <- fluidPage(
    shiny::selectInput("metering_point", label = "Meterpoint", choices = metering_points),

    shiny::dateRangeInput(
        "date_range",
        "Date range:",
        start = last_month_start,
        end = this_month_start
    ),

    shiny::mainPanel(
        shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Cost", plotly::plotlyOutput("accumulated_cost_plot")),
            shiny::tabPanel("Daily consumption", plotly::plotlyOutput("daily_consumption_plot"))
        )
    )
)


server <- function(input, output, session)
{
    metering_point <- shiny::reactive({input$metering_point})

    date_range <- shiny::reactive({input$date_range})

    long_price_data <- shiny::reactive({
        price_data <- daily_tbl |>
            dplyr::filter(
                MeterId == metering_point(),
                Date >= date_range()[1],
                Date <= date_range()[2]
            ) |>
            dplyr::select(Date, DailyPriceFlex, DailyPriceFixed) |>
            dplyr::collect()

        price_data |>
            tidyr::pivot_longer(
                c("DailyPriceFlex", "DailyPriceFixed"),
                names_prefix = "DailyPrice",
                names_to = "Type",
                values_to = "Price"
            ) |>
            dplyr::group_by(Type) |>
            dplyr::mutate(
                AccumulatedPrice = cumsum(Price)
            ) |>
            dplyr::ungroup()
    })

    output$accumulated_cost_plot <- plotly::renderPlotly({
        plotly::plot_ly(
            data = long_price_data(),
            x = ~ Date,
            y = ~ AccumulatedPrice,
            color = ~ Type,
            type = "scatter",
            mode = "lines"
        )
    })

    daily_consumption_data <- shiny::reactive({
        consumption_and_prices |>
            dplyr::filter(
                MeterId == metering_point(),
                Date >= date_range()[1],
                Date <= date_range()[2]
            ) |>
            dplyr::transmute(
                Date = strftime(Date, format = "%Y-%m-%d"),
                HourOfDay,
                Consumption
            ) |>
            dplyr::arrange(Date, HourOfDay) |>
            dplyr::collect()
    })

    output$daily_consumption_plot <- plotly::renderPlotly({
        plotly::plot_ly(
            data = daily_consumption_data(),
            x = ~ HourOfDay,
            y = ~ Consumption,
            color = ~ as.character(Date),
            type = "scatter",
            mode = "lines"
        )
    })
}

shiny::shinyApp(ui, server)
