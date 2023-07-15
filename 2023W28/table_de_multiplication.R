library(gt)
library(tibble)
library(markdown)

# systemfonts::system_fonts() %>% View

# f <- chromote::default_chromote_object()
# f$close()

make_table <- function(df, symbol, title, fn) {
  multi_tab <- gt(df, rowname_col = "x") |>
    tab_header(title) |>
    tab_stubhead(symbol) |>
    cols_width(everything() ~ px(60)) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = list(cells_column_labels(), cells_stub())) |>
    tab_style(style = cell_text(weight = "bold", align = "center",
                                color = "red", v_align = "middle"),
              locations = cells_stubhead()) |>
    tab_style(style = cell_text(size = px(40), align = "center"),
              locations = list(cells_body(), cells_stub(),
                               cells_stubhead(), cells_column_labels())) |>
    tab_style(style = cell_borders(weight = px(3)),
              locations = list(cells_body(), cells_stub(), cells_stubhead(),
                               cells_column_labels())) |>
    tab_style(style = cell_text(size = px(45), font = "EBGaramondSC08-Regular"),
              locations = cells_title()) |>
    tab_style(style = cell_borders(sides = "top", style = "hidden"),
              locations = cells_title()) |>
    tab_options(
      table.border.top.style = "hidden",
      table_body.border.bottom.color = "#000000",
      table_body.border.bottom.width  = px(3),
      column_labels.border.top.color = "#000000",
      column_labels.border.top.width  = px(3),
      column_labels.border.bottom.color = "#000000",
      column_labels.border.bottom.width  = px(3)
    ) |>
    opt_table_font(font = "EBGaramond08-Regular")

  purrr::walk(1:10, function(i) {
    multi_tab <<- multi_tab |>
      tab_style(style = list(cell_text(color = "blue", font = "EBGaramond08-Italic")),
                locations = cells_body(rows = i, columns = i + 1))
  })

  multi_tab |> gtsave(fn)
}

################################################################################

m <- (1:10 %o% 1:10)
colnames(m) <- 1:10
rownames(m) <- 1:10
df <- as_tibble(m, rownames = "x")

make_table(df, symbol = "✕", title = "\U2767 table de multiplication \U2619",
           fn = "2023W28/table_de_multiplication.pdf")

################################################################################

m <- outer(1:10, 1:10, FUN = `+`)
colnames(m) <- 1:10
rownames(m) <- 1:10
df <- as_tibble(m, rownames = "x")

make_table(df, symbol = "+", title = "\U261E table d'addition \U261C",
           fn = "2023W28/table_d_addition.pdf")
