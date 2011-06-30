--
-- PostgreSQL database dump
--

SET client_encoding = 'LATIN1';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

--
-- Name: forecast_by_frequency_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecast_by_frequency_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecast_by_frequency_id_seq OWNER TO dss;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: forecast_by_frequency; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecast_by_frequency (
    id integer DEFAULT nextval('forecast_by_frequency_id_seq'::regclass) NOT NULL,
    frequency integer NOT NULL,
    opacity double precision,
    tsys double precision,
    forecast_id integer NOT NULL
);


ALTER TABLE public.forecast_by_frequency OWNER TO dss;

--
-- Name: forecast_times_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecast_times_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecast_times_id_seq OWNER TO dss;

--
-- Name: forecast_times; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecast_times (
    id integer DEFAULT nextval('forecast_times_id_seq'::regclass) NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.forecast_times OWNER TO dss;

--
-- Name: forecast_types_type_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecast_types_type_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecast_types_type_id_seq OWNER TO dss;

--
-- Name: forecast_types; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecast_types (
    type_id integer DEFAULT nextval('forecast_types_type_id_seq'::regclass) NOT NULL,
    type character varying(30) NOT NULL
);


ALTER TABLE public.forecast_types OWNER TO dss;

--
-- Name: forecasts_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE forecasts_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.forecasts_id_seq OWNER TO dss;

--
-- Name: forecasts; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE forecasts (
    id integer DEFAULT nextval('forecasts_id_seq'::regclass) NOT NULL,
    forecast_type_id integer,
    weather_date_id integer,
    wind_speed double precision,
    wind_speed_mph double precision,
    irradiance double precision,
    forecast_time_id integer,
    import_time_id integer
);


ALTER TABLE public.forecasts OWNER TO dss;

--
-- Name: weather_station2_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE weather_station2_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.weather_station2_id_seq OWNER TO dss;

--
-- Name: gbt_weather; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE gbt_weather (
    id integer DEFAULT nextval('weather_station2_id_seq'::regclass) NOT NULL,
    weather_date_id integer,
    wind_speed double precision,
    irradiance double precision
);


ALTER TABLE public.gbt_weather OWNER TO dss;

--
-- Name: hour_angle_boundaries_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE hour_angle_boundaries_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.hour_angle_boundaries_id_seq OWNER TO dss;

--
-- Name: hour_angle_boundaries; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE hour_angle_boundaries (
    id integer DEFAULT nextval('hour_angle_boundaries_id_seq'::regclass) NOT NULL,
    frequency integer NOT NULL,
    declination integer NOT NULL,
    boundary double precision
);


ALTER TABLE public.hour_angle_boundaries OWNER TO dss;

--
-- Name: import_times_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE import_times_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.import_times_id_seq OWNER TO dss;

--
-- Name: import_times; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE import_times (
    id integer DEFAULT nextval('import_times_id_seq'::regclass) NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.import_times OWNER TO dss;

--
-- Name: stringency_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE stringency_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.stringency_id_seq OWNER TO dss;

--
-- Name: stringency; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE stringency (
    id integer DEFAULT nextval('stringency_id_seq'::regclass) NOT NULL,
    frequency integer NOT NULL,
    elevation integer NOT NULL,
    total double precision NOT NULL
);


ALTER TABLE public.stringency OWNER TO dss;

--
-- Name: t_sys_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE t_sys_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.t_sys_id_seq OWNER TO dss;

--
-- Name: t_sys; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE t_sys (
    id integer DEFAULT nextval('t_sys_id_seq'::regclass) NOT NULL,
    frequency integer NOT NULL,
    elevation integer NOT NULL,
    total double precision NOT NULL,
    prime double precision NOT NULL
);


ALTER TABLE public.t_sys OWNER TO dss;

--
-- Name: weather_dates_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE weather_dates_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.weather_dates_id_seq OWNER TO dss;

--
-- Name: weather_dates; Type: TABLE; Schema: public; Owner: dss; Tablespace: 
--

CREATE TABLE weather_dates (
    id integer DEFAULT nextval('weather_dates_id_seq'::regclass) NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.weather_dates OWNER TO dss;

--
-- Name: sessions_id_seq; Type: SEQUENCE; Schema: public; Owner: dss
--

CREATE SEQUENCE sessions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.sessions_id_seq OWNER TO dss;

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
    ADD CONSTRAINT forecast_types_type_key UNIQUE (type);


--
-- Name: forecasts_forecast_type_id_key; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY forecasts
    ADD CONSTRAINT forecasts_forecast_type_id_key UNIQUE (forecast_type_id, weather_date_id);


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
-- Name: weather_dates_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY weather_dates
    ADD CONSTRAINT weather_dates_pkey PRIMARY KEY (id);


--
-- Name: weather_station2_pkey; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY gbt_weather
    ADD CONSTRAINT weather_station2_pkey PRIMARY KEY (id);


--
-- Name: weather_station2_wind_speed_date; Type: CONSTRAINT; Schema: public; Owner: dss; Tablespace: 
--

ALTER TABLE ONLY gbt_weather
    ADD CONSTRAINT weather_station2_wind_speed_date UNIQUE (wind_speed, weather_date_id);


--
-- Name: forecast_by_frequency_frequency; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecast_by_frequency_frequency ON forecast_by_frequency USING btree (frequency);


--
-- Name: forecasts_date; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecasts_date ON forecasts USING btree (weather_date_id);


--
-- Name: forecasts_date_and_type; Type: INDEX; Schema: public; Owner: dss; Tablespace: 
--

CREATE INDEX forecasts_date_and_type ON forecasts USING btree (weather_date_id, forecast_type_id);


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
-- Name: forecasts_weather_date_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dss
--

ALTER TABLE ONLY forecasts
    ADD CONSTRAINT forecasts_weather_date_id_fkey FOREIGN KEY (weather_date_id) REFERENCES weather_dates(id);


--
-- Name: weather_station2_weather_date_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dss
--

ALTER TABLE ONLY gbt_weather
    ADD CONSTRAINT weather_station2_weather_date_id_fkey FOREIGN KEY (weather_date_id) REFERENCES weather_dates(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: dss
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM dss;
GRANT ALL ON SCHEMA public TO dss;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

