
create table stringency_parameters (
    id serial primary key,
    stringency_id integer references stringency (id) not null,
    gas boolean
);
