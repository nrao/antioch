--
-- PostgreSQL database dump
--

SET client_encoding = 'LATIN1';
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: dss
--

COMMENT ON SCHEMA public IS 'standard public schema';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


--
-- Name: forecast_by_frequency; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecast_by_frequency (
    id integer NOT NULL,
    frequency integer NOT NULL,
    opacity double precision,
    tsys double precision,
    forecast_id integer NOT NULL
);


ALTER TABLE public.forecast_by_frequency OWNER TO dss;

--
-- Name: forecast_types; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecast_types (
    type_id integer NOT NULL,
    "type" character varying(30) NOT NULL
);


ALTER TABLE public.forecast_types OWNER TO dss;

--
-- Name: forecasts; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecasts (
    id integer NOT NULL,
    forecast_type_id integer,
    date timestamp without time zone NOT NULL,
    wind_speed double precision,
    w2_wind_speed double precision,
    tatm double precision  -- TBF:  Is this ever going to be used?
);


ALTER TABLE public.forecasts OWNER TO dss;


--
-- Name: stringency; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE stringency (
    id integer NOT NULL,
    frequency integer NOT NULL,
    elevation integer NOT NULL,
    total double precision NOT NULL
);


ALTER TABLE public.stringency OWNER TO dss;

--
-- Name: t_sys; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE t_sys (
    id integer NOT NULL,
    frequency integer NOT NULL,
    elevation integer NOT NULL,
    total double precision NOT NULL,
    prime double precision NOT NULL
);


ALTER TABLE public.t_sys OWNER TO dss;

--
-- Name: forecast_by_frequency_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecast_by_frequency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecast_by_frequency_id_seq OWNER TO dss;

--
-- Name: forecast_types_type_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecast_types_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecast_types_type_id_seq OWNER TO dss;

--
-- Name: forecasts_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecasts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecasts_id_seq OWNER TO dss;

--
-- Name: sessions_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE sessions_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.sessions_id_seq OWNER TO dss;

--
-- Name: stringency_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE stringency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.stringency_id_seq OWNER TO dss;

--
-- Name: t_sys_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE t_sys_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.t_sys_id_seq OWNER TO dss;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE forecast_by_frequency ALTER COLUMN id SET DEFAULT nextval('forecast_by_frequency_id_seq'::regclass);


--
-- Name: type_id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE forecast_types ALTER COLUMN type_id SET DEFAULT nextval('forecast_types_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE forecasts ALTER COLUMN id SET DEFAULT nextval('forecasts_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE stringency ALTER COLUMN id SET DEFAULT nextval('stringency_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE t_sys ALTER COLUMN id SET DEFAULT nextval('t_sys_id_seq'::regclass);


--
-- Name: forecast_by_frequency_forecast_id_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecast_by_frequency
    ADD CONSTRAINT forecast_by_frequency_forecast_id_key UNIQUE (forecast_id, frequency);


--
-- Name: forecast_by_frequency_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecast_by_frequency
    ADD CONSTRAINT forecast_by_frequency_pkey PRIMARY KEY (id);


--
-- Name: forecast_types_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecast_types
    ADD CONSTRAINT forecast_types_pkey PRIMARY KEY (type_id);


--
-- Name: forecast_types_type_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecast_types
    ADD CONSTRAINT forecast_types_type_key UNIQUE ("type");


--
-- Name: forecasts_forecast_type_id_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecasts
    ADD CONSTRAINT forecasts_forecast_type_id_key UNIQUE (forecast_type_id, date);


--
-- Name: forecasts_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecasts
    ADD CONSTRAINT forecasts_pkey PRIMARY KEY (id);


--
-- Name: stringency_frequency_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY stringency
    ADD CONSTRAINT stringency_frequency_key UNIQUE (frequency, elevation);


--
-- Name: stringency_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY stringency
    ADD CONSTRAINT stringency_pkey PRIMARY KEY (id);


--
-- Name: t_sys_frequency_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY t_sys
    ADD CONSTRAINT t_sys_frequency_key UNIQUE (frequency, elevation);


--
-- Name: t_sys_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY t_sys
    ADD CONSTRAINT t_sys_pkey PRIMARY KEY (id);


--
-- Name: forecast_by_frequency_frequency; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecast_by_frequency_frequency ON forecast_by_frequency USING btree (frequency);


--
-- Name: forecasts_date; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecasts_date ON forecasts USING btree (date);


--
-- Name: forecasts_date_and_type; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecasts_date_and_type ON forecasts USING btree (date, forecast_type_id);

--
-- Name: hour_angle_boundaries; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE hour_angle_boundaries (
    id integer NOT NULL,
    frequency integer NOT NULL,
    declination integer NOT NULL,
    boundary double precision
);


ALTER TABLE public.hour_angle_boundaries OWNER TO dss;

--
-- Name: hour_angle_boundaries_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE hour_angle_boundaries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

ALTER TABLE public.hour_angle_boundaries_id_seq OWNER TO dss;

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: dss
--

ALTER TABLE hour_angle_boundaries ALTER COLUMN id SET DEFAULT nextval('hour_angle_boundaries_id_seq'::regclass);


--
-- Name: forecast_by_frequency_forecast_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dss
--

ALTER TABLE ONLY forecast_by_frequency
    ADD CONSTRAINT forecast_by_frequency_forecast_id_fkey FOREIGN KEY (forecast_id) REFERENCES forecasts(id);


--
-- Name: forecasts_forecast_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dss
--

ALTER TABLE ONLY forecasts
    ADD CONSTRAINT forecasts_forecast_type_id_fkey FOREIGN KEY (forecast_type_id) REFERENCES forecast_types(type_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: dss
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM dss;
GRANT ALL ON SCHEMA public TO dss;
GRANT ALL ON SCHEMA public TO PUBLIC;


INSERT INTO forecast_types (type) VALUES('0-11');
INSERT INTO forecast_types (type) VALUES('12-23'); 
INSERT INTO forecast_types (type) VALUES('24-35');
INSERT INTO forecast_types (type) VALUES('36-47');
INSERT INTO forecast_types (type) VALUES('48-59');

--
-- PostgreSQL database dump complete
--

