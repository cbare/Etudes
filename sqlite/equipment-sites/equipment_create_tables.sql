create table if not exists equipment (
    id integer primary key autoincrement,
    serial_no text not null,
    description text not null
);

create table if not exists sites (
    id integer primary key autoincrement,
    name text not null
);

create table if not exists jobs (
    equipment_id integer not null,
    site_id integer not null,
    timestamp text not null,
    description text
);

-- create index if not exists jobs_equipment_id_idx on work_items(equipment_id);
-- create index if not exists jobs_site_id_idx on work_items(site_id);
