devtools::load_all("~/github-repos/evoland-plus/")

db <- evoland_db$new(
  path = "fullch.evolanddb",
  report_name = "valparish",
  report_name_pretty = "ValPar.CH inspired model setup"
)

if (db$row_count("neighbors_t") == 0) {
  db$commit(
    compute_neighbors(
      db$coords_t,
      max_distance = 1000,
      distance_breaks = c(0, 100, 500, 1000)
    ),
    table_name = "neighbors_t",
    mode = "overwrite"
  )
}

db$attach_table(
  "neighbors_t",
  "id_coord_origin, id_coord_neighbor, distance_class"
)
db$attach_table(c("lulc_data_t", "pred_meta_t", "lulc_meta_t"))

db$execute(
  r"{
    create temp table pred_meta_neighbors_t as
    with
      all_distance_classes as (select distinct distance_class from neighbors_t),
      max_id as (select coalesce(max(id_pred), 0) as max_pred from pred_meta_t)
    select
      row_number() over () + (select max_pred from max_id) as id_pred,
      concat('id_lulc_', l.id_lulc, '_dist_', c.distance_class) as name,
      concat('Count of ', l.pretty_name, ' coordinate points within distance ', c.distance_class) as pretty_name,
      'Number of neighbors of a given land use class that fall within a given distance interval' as description,
      'land use coordinate data' as orig_format,
      NULL as sources,
      'number of neighbors' as unit,
      NULL as factor_levels,
      c.distance_class,
      l.id_lulc
    from
      lulc_meta_t l
    cross join
      all_distance_classes c
  }"
)

db$execute(
  r"{
  copy (
    select
      id_pred, name, pretty_name, description, orig_format, sources, unit, factor_levels
    from 
      pred_meta_t
    union all
    select
      id_pred, name, pretty_name, description, orig_format, sources, unit, factor_levels
    from 
      pred_meta_neighbors_t
  )
  to 'fullch.evolanddb/pred_meta_t.parquet' (format parquet, compression zstd)
  }"
)

db$execute(
  r"{
  create table pred_neighbors_t as
  select
    p.id_pred,
    n.id_coord_origin as id_coord,
    t.id_period,
    count(n.id_coord_neighbor) as value
  from
   neighbors_t n,
   lulc_data_t t,
   pred_meta_neighbors_t p
  where
    n.id_coord_neighbor = t.id_coord
    and p.id_lulc = t.id_lulc
    and p.distance_class = n.distance_class
  group by
    n.id_coord_origin,
    n.distance_class,
    t.id_period,
    t.id_lulc,
    p.id_pred,
    p.name
  }"
)

if (db$row_count("pred_data_t_int") == 0) {
  db$execute(
    r"{
    copy pred_neighbors_t
    to 'fullch.evolanddb/pred_data_t_int.parquet' (format parquet, compression zstd);
    }"
  )
} else {
  db$execute(
    r"{
    copy (
      select
        id_pred, id_coord, id_period, value
      from
        read_parquet('fullch.evolanddb/pred_data_t_int.parquet')
      union all
      select
        id_pred, id_coord, id_period, value
      from
        pred_neighbors_t
    )
    to 'fullch.evolanddb/pred_data_t_int.parquet' (format parquet, compression zstd)
    }"
  )
}
