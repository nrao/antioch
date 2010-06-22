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

-- update t_sys and stringency tables for 'bigbox'

-- new Receivers info

CREATE TABLE receivers (
    id integer NOT NULL,
    name character varying(32) NOT NULL,
    abbreviation character varying(32) NOT NULL,
    freq_low double precision NOT NULL,
    freq_hi double precision NOT NULL
);

CREATE SEQUENCE receivers_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

ALTER TABLE public.receivers_id_seq OWNER TO dss;
ALTER SEQUENCE receivers_id_seq OWNED BY receivers.id;
ALTER TABLE ONLY receivers
    ADD CONSTRAINT receivers_pkey PRIMARY KEY (id);
ALTER TABLE receivers ALTER COLUMN id SET DEFAULT nextval('receivers_id_seq'::regclass);

-- init data!
--
  Carl's
INSERT INTO receivers VALUES (DEFAULT, 'NoiseSource',   'NS',   0.000,   0.000);
 --
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_RRI',      'RRI',  0.100,   1.600);
 -- R
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_342',      '342',  0.290,   0.395);
 -- 3
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_450',      '450',  0.385,   0.520);
 -- 4
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_600',      '600',  0.510,   0.690);
 -- 6
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_800',      '800',  0.680,   0.920);
 -- 8
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_1070',    '1070',  0.910,   1.230);
 -- A
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr1_2',       'L',    1.150,   1.730);
 -- L
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr2_3',       'S',    1.730,   2.600);
 -- S
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr4_6',       'C',    3.950,   6.100);
 -- C
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr8_10',      'X',    8.000,  10.000);
 -- X
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr12_18',     'Ku',  12.000,  15.400);
 -- U
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr18_26',     'K',   18.000,  26.500);
 -- K
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr26_40',     'Ka',  26.000,  39.500);
 -- B
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr40_52',     'Q',   38.200,  49.800);
 -- Q
INSERT INTO receivers VALUES (DEFAULT, 'Rcvr_PAR',      'MBA', 80.000, 100.000);
 -- M
INSERT INTO receivers VALUES (DEFAULT, 'Zpectrometer',  'Z',    0.000,   0.000);
 --
INSERT INTO receivers VALUES (DEFAULT, 'Holography',    'Hol', 11.700,  12.200);
 -- H
INSERT INTO receivers VALUES (DEFAULT, 'RcvrArray18_26','KFPA',17.000,  27.500);
 -- F

-- new observing types

CREATE TABLE observing_types (   
    id integer NOT NULL,
    type character varying(64) NOT NULL
);

ALTER TABLE public.observing_types OWNER TO dss;
CREATE SEQUENCE observing_types_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;
ALTER TABLE public.observing_types_id_seq OWNER TO dss;
ALTER SEQUENCE observing_types_id_seq OWNED BY observing_types.id;
ALTER TABLE observing_types ALTER COLUMN id SET DEFAULT nextval('observing_types_id_seq'::regclass);
ALTER TABLE ONLY observing_types
    ADD CONSTRAINT observing_types_pkey PRIMARY KEY (id);

-- init data: just two of them

INSERT INTO observing_types VALUES (DEFAULT, 'spectral line');
INSERT INTO observing_types VALUES (DEFAULT, 'continuum');


-- now create the necessary foriegn keys
-- add receiver_id and observing_type_id to t_sys table

ALTER TABLE t_sys ADD COLUMN observing_type_id integer;
CREATE INDEX t_sys_observing_type_id ON t_sys USING btree (observing_type_id);
ALTER TABLE ONLY t_sys ADD CONSTRAINT t_sys_observing_type_id_fkey FOREIGN KEY (observing_type_id) REFERENCES observing_types(id) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE t_sys ADD COLUMN receiver_id integer;
CREATE INDEX t_sys_receiver_id ON t_sys USING btree (receiver_id);
ALTER TABLE ONLY t_sys ADD CONSTRAINT t_sys_receiver_id_fkey FOREIGN KEY (receiver_id) REFERENCES receivers(id) DEFERRABLE INITIALLY DEFERRED;

-- add receiver_id and observing_type_id to stringency table

ALTER TABLE stringency ADD COLUMN observing_type_id integer;
CREATE INDEX stringency_observing_type_id ON stringency USING btree (observing_type_id);
ALTER TABLE ONLY stringency ADD CONSTRAINT stringency_observing_type_id_fkey FOREIGN KEY (observing_type_id) REFERENCES observing_types(id) DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE stringency ADD COLUMN receiver_id integer;
CREATE INDEX stringency_receiver_id ON t_sys USING btree (receiver_id);
ALTER TABLE ONLY stringency ADD CONSTRAINT stringency_receiver_id_fkey FOREIGN KEY (receiver_id) REFERENCES receivers(id) DEFERRABLE INITIALLY DEFERRED;


