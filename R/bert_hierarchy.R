munge_bert_hierarchy <- function(obj, hierarchy) {
  tbl_label_count <- obj |> bert_topic_count()
  tbl_hierarchy <-
    hierarchy |> janitor::clean_names() |>
    mutate(id = 1:n()) |>
    select(id, everything()) |>
    rename(topics_bert = topics)

  tbl_hierarchy <- tbl_hierarchy |> mutate_at(c("parent_id", "child_left_id", "child_right_id"), as.numeric)

  tbl_hierarchy <- tbl_hierarchy |>
    left_join(
      tbl_label_count |> select(label_bertopic) |> mutate(is_bertopic_label = T),
      by = c("parent_name" = "label_bertopic")
    ) |>
    mutate(is_bertopic_label = is_bertopic_label |> coalesce(F)) |>
    left_join(tbl_hierarchy |> distinct(parent_name) |> mutate(id_parent = 1:n()),
              by = "parent_name") |>
    select(id, parent_id, id_parent, everything()) |>
    arrange(id_parent)

  tbl_from_to_bert_parents <-
    tbl_hierarchy |>
    arrange(id_parent) |>
    select(from = parent_name, to = child_left_name) |>
    filter(from != to)





  tbl_unnested_hierarchy <-
    tbl_hierarchy |>
    unnest() |>
    rename(topic_bert = topics_bert) |>
    left_join(tbl_label_count, by = "topic_bert")

  tbl_last_label_all <-
    tbl_unnested_hierarchy |>
    group_by(label_bertopic) |>
    filter(id == max(id)) |>
    ungroup()



  tbl_final <-
    tbl_last_label_all |>
    select(from = parent_name, to = label_bertopic)



  tbl_from_to_all <-
    tbl_from_to_bert_parents |>
    mutate(level = 1) |>
    bind_rows(tbl_final |> mutate(level = 2)) |>
    mutate(id_row = 1:n()) |>
    group_by(from, to) |>
    filter(id_row == min(id_row)) |>
    ungroup()

  tbl_from_to_all |> distinct(from) |>
    tbl_hierarchy |> distinct(id_parent, parent_name)




}

tbl_hierarchy |> distinct(topic_bert)
tbl_hierarchy |> distinct(parent_name)
tbl_hierarchy |> distinct(child_right_name)
tbl_hierarchy |> distinct(child_left_name)
tbl_hierarchy |> map(unique)
