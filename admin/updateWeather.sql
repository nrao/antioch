-- forecast types for 6 hour forecasts
INSERT INTO forecast_types (type) VALUES('0-5');
INSERT INTO forecast_types (type) VALUES('6-11');
INSERT INTO forecast_types (type) VALUES('12-17');
INSERT INTO forecast_types (type) VALUES('18-23');
INSERT INTO forecast_types (type) VALUES('24-29');
INSERT INTO forecast_types (type) VALUES('30-35');
INSERT INTO forecast_types (type) VALUES('36-41');
INSERT INTO forecast_types (type) VALUES('42-47');
INSERT INTO forecast_types (type) VALUES('48-53');
INSERT INTO forecast_types (type) VALUES('54-59');
INSERT INTO forecast_types (type) VALUES('60-65');
INSERT INTO forecast_types (type) VALUES('66-71');
INSERT INTO forecast_types (type) VALUES('72-77');
INSERT INTO forecast_types (type) VALUES('78-83');
INSERT INTO forecast_types (type) VALUES('84-89');
INSERT INTO forecast_types (type) VALUES('90-95');

-- new forecast table fields
alter table forecasts add wind_speed_mph double precision;
alter table forecasts add forecast_time_id integer;
alter table forecasts add import_time_id integer;

-- new tables:

CREATE SEQUENCE forecast_times_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE SEQUENCE import_times_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE forecast_times (
    id integer DEFAULT nextval('forecast_times_id_seq'::regclass) NOT NULL,
    date timestamp without time zone NOT NULL
);


CREATE TABLE import_times (
    id integer DEFAULT nextval('import_times_id_seq'::regclass) NOT NULL,
    date timestamp without time zone NOT NULL
);


