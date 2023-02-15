#' Create an icon with a link in a `gt` table
#' 
#' Transform a column a column with that may contain links into
#' a column of fontawesome icons. Cells with a link will have a primary color and
#' wrapped in an <a> tag. Cells without a link will have a secondary color.
#' 
#' @param gt_object `gt` table object
#' @param column Bare name for a single table column
#' @param fa_name Character. Name of a fontawesome icon that is passed to `fontawesome::fa()`
#' @param enabled_color Character. Color of "enabled" icons for cells with URI. CSS color attribute. Color name or hex code.
#' @param disabled_color Character. Color of "disabled" icons for cells with URI. CSS color attribute. Color name or hex code.
#' 
#' @importFrom gt text_transform cells_body 
#' @importFrom glue glue
#' @importFrom htmltools div a
create_icon_w_link <- function(
    gt_object,
    column,
    fa_name,
    enabled_color,
    disabled_color
) {
    gt::text_transform(
        gt_object,
        locations = gt::cells_body(columns = {{column}}),
        fn = function(x) {
            lapply(X = x, FUN = function(xy) {
                # form fontawesome
                fa_icon <- list(
                    fontawesome::fa(name = fa_name)
                )
                # apply different styling depending on 
                # whether there's a link or not
                if (is.na(xy)) {
                    icon_style <- glue::glue("color: {disabled_color}")
                } else {
                    icon_style <- glue::glue("color: {enabled_color}")
                }
                # create the the fontawesome icon
                # base: icon + styling
                fa_icon <- htmltools::div(
                    role = "img", fa_icon, style = icon_style
                )
                # if the column contains a link,
                # wrap the icon in an <a> tag
                # that points the URI in the column
                if (!is.na(xy)) {
                    fa_icon <- htmltools::a(
                        href = xy,
                        fa_icon
                    )
                }

                # return the icon
                # with the appropriate styling
                # and outer tag, if applicable
                fa_icon
            }
            )
        }

    )
}
