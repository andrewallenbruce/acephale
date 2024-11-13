```{mermaid}
%%| label: tx-mermaid
%%| echo: false
%%| eval: false
flowchart LR
A{{Group}} -.- D(Multi-Specialty)
A{{Group}} -.- E(Single-Specialty)
B{{Individual}} --> F(Allopathic)
B{{Individual}} -.- G(Behavioral)
C{{Non-Individual}} -.- H(Hospitals)
C{{Non-Individual}} -.- I(Laboratories)
F(Allopathic) --> J([Anesthesiology])
J([Anesthesiology]) --o K[[207L00000X]]
J([Anesthesiology]) --> L>Addiction Medicine]
L>Addiction Medicine] --o M[[207LA0401X]]
```

taxonomy <- provider::taxonomies(shape = "wide") |>
  select(grouping,
         classification,
         specialization,
         code)

htmltools::browsable(
  htmltools::tagList(
    htmltools::tags$button(
      "Expand/collapse all",
      onclick = "Reactable.toggleAllRowsExpanded('taxonomy-table')"
    ),
    reactable::reactable(
      taxonomy,
      groupBy = c(
        "grouping",
        "classification",
        "specialization"
      ),
      elementId = "taxonomy-table",
      columns = list(
        grouping = reactable::colDef(
          name = "Provider\nGrouping"
        ),
        classification = reactable::colDef(
          name = "Classification"
        ),
        specialization = reactable::colDef(
          name = "Area of\nSpecialization",
          na = "None"
        ),
        code = reactable::colDef(
          name = "Taxonomy\nCode",
          style = list(fontFamily = "monospace", whiteSpace = "pre")
        )
      ),
      # compact = TRUE,
      searchable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "28px 16px",
        searchInputStyle = list(width = "100%")
      )
    )
  )
)
