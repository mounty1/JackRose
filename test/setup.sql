--
-- PostgreSQL database dump
--

-- Dumped from database version 9.4.4
-- Dumped by pg_dump version 9.6.2

--
-- Phase one;  clear anything left by the previous test.
-- There is no easy way to 'reset database' in Postgres.
-- This may produce multiple error messages.
--

DROP FUNCTION IF EXISTS timeshift();
DROP TABLE IF EXISTS history;
DROP TABLE IF EXISTS learn_datum;
DROP TABLE IF EXISTS user_deck_end;
DROP TABLE IF EXISTS user_deck_node;
DROP TABLE IF EXISTS view;
DROP TABLE IF EXISTS data_row;
DROP TABLE IF EXISTS data_source;
DROP TABLE IF EXISTS "Member";
DROP TABLE IF EXISTS "User";
DROP SEQUENCE IF EXISTS "Member_id_seq";
DROP SEQUENCE IF EXISTS "User_id_seq";
DROP SEQUENCE IF EXISTS data_row_id_seq;
DROP SEQUENCE IF EXISTS data_source_id_seq;
DROP SEQUENCE IF EXISTS learn_datum_id_seq;
DROP SEQUENCE IF EXISTS user_deck_node_id_seq;
DROP SEQUENCE IF EXISTS view_id_seq;

--
-- Phase two;  populate the database.
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

CREATE FUNCTION timeshift() RETURNS interval
	LANGUAGE plpgsql
	AS $$
		declare
			timedelta interval = now() - '2017-04-01 05:31:14.0+00';
		begin
			return timedelta;
		end;
	$$;

ALTER FUNCTION public.timeshift() OWNER TO jackrose;
SET default_tablespace = '';
SET default_with_oids = false;

CREATE TABLE "Member" (
	id bigint NOT NULL,
	child bigint NOT NULL,
	parent bigint NOT NULL
);

ALTER TABLE "Member" OWNER TO jackrose;

CREATE SEQUENCE "Member_id_seq" START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE "Member_id_seq" OWNER TO jackrose;

ALTER SEQUENCE "Member_id_seq" OWNED BY "Member".id;

CREATE TABLE "User" (
	id bigint NOT NULL,
	username character varying NOT NULL,
	password bytea NOT NULL,
	"emailAddress" character varying NOT NULL,
	verified boolean NOT NULL,
	"verifyKey" character varying NOT NULL,
	"resetPasswordKey" character varying NOT NULL
);

ALTER TABLE "User" OWNER TO jackrose;

CREATE SEQUENCE "User_id_seq" START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE "User_id_seq" OWNER TO jackrose;

ALTER SEQUENCE "User_id_seq" OWNED BY "User".id;

CREATE TABLE data_row (
	id bigint NOT NULL,
	table_key character varying NOT NULL,
	data_source_row_id bigint NOT NULL,
	loaded timestamp with time zone NOT NULL
);

ALTER TABLE data_row OWNER TO jackrose;

CREATE SEQUENCE data_row_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE data_row_id_seq OWNER TO jackrose;

ALTER SEQUENCE data_row_id_seq OWNED BY data_row.id;

CREATE TABLE data_source (
	id bigint NOT NULL,
	accessor_write bigint NOT NULL,
	accessor_read bigint NOT NULL,
	name character varying NOT NULL,
	source_serial character varying NOT NULL,
	resynced timestamp with time zone NOT NULL
);

ALTER TABLE data_source OWNER TO jackrose;

CREATE SEQUENCE data_source_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE data_source_id_seq OWNER TO jackrose;

ALTER SEQUENCE data_source_id_seq OWNED BY data_source.id;

CREATE TABLE history (
	item bigint NOT NULL,
	stamp timestamp with time zone NOT NULL,
	grade integer NOT NULL
);

ALTER TABLE history OWNER TO jackrose;

CREATE TABLE learn_datum (
	id bigint NOT NULL,
	view_u_i_d bigint NOT NULL,
	item_id bigint NOT NULL,
	"user" bigint NOT NULL,
	activity integer NOT NULL,
	space_algorithm integer NOT NULL,
	factor1 double precision NOT NULL,
	factor2 double precision NOT NULL,
	next_review timestamp with time zone NOT NULL
);

ALTER TABLE learn_datum OWNER TO jackrose;

CREATE SEQUENCE learn_datum_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE learn_datum_id_seq OWNER TO jackrose;

ALTER SEQUENCE learn_datum_id_seq OWNED BY learn_datum.id;

CREATE TABLE user_deck_end (
	view_id bigint NOT NULL,
	"user" bigint NOT NULL,
	parent bigint,
	throttle bigint,
	shuffle boolean
);

ALTER TABLE user_deck_end OWNER TO jackrose;

CREATE TABLE user_deck_node (
	id bigint NOT NULL,
	parent bigint,
	"user" bigint NOT NULL,
	throttle bigint,
	shuffle boolean,
	label character varying NOT NULL
);

ALTER TABLE user_deck_node OWNER TO jackrose;

CREATE SEQUENCE user_deck_node_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE user_deck_node_id_seq OWNER TO jackrose;

ALTER SEQUENCE user_deck_node_id_seq OWNED BY user_deck_node.id;

CREATE TABLE view (
	id bigint NOT NULL,
	name character varying NOT NULL,
	data_source_id bigint NOT NULL,
	obverse character varying NOT NULL,
	reverse character varying NOT NULL,
	style_c_s_s character varying
);

ALTER TABLE view OWNER TO jackrose;

CREATE SEQUENCE view_id_seq START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;

ALTER TABLE view_id_seq OWNER TO jackrose;

ALTER SEQUENCE view_id_seq OWNED BY view.id;

ALTER TABLE ONLY "Member" ALTER COLUMN id SET DEFAULT nextval('"Member_id_seq"'::regclass);

ALTER TABLE ONLY "User" ALTER COLUMN id SET DEFAULT nextval('"User_id_seq"'::regclass);

ALTER TABLE ONLY data_row ALTER COLUMN id SET DEFAULT nextval('data_row_id_seq'::regclass);

ALTER TABLE ONLY data_source ALTER COLUMN id SET DEFAULT nextval('data_source_id_seq'::regclass);

ALTER TABLE ONLY learn_datum ALTER COLUMN id SET DEFAULT nextval('learn_datum_id_seq'::regclass);

ALTER TABLE ONLY user_deck_node ALTER COLUMN id SET DEFAULT nextval('user_deck_node_id_seq'::regclass);

ALTER TABLE ONLY view ALTER COLUMN id SET DEFAULT nextval('view_id_seq'::regclass);

COPY "Member" (id, child, parent) FROM stdin;
\.

SELECT pg_catalog.setval('"Member_id_seq"', 1, false);

COPY "User" (id, username, password, "emailAddress", verified, "verifyKey", "resetPasswordKey") FROM stdin;
1	mounty	\\x7368613235367c31327c572f77634a66345a59742b475768767a324541586b673d3d7c4e7a4d6f50494d45556d45685a336b316b70637750796b6d6c51794c596e6a4d6c4d73495a6c43735275453d	gate03@landcroft.com	t		
\.

SELECT pg_catalog.setval('"User_id_seq"', 1, true);

COPY data_row (id, table_key, data_source_row_id, loaded) FROM stdin;
4	1	1	2017-03-29 06:22:53.583945+00
5	2	1	2017-03-29 06:22:53.583945+00
6	3	1	2017-03-29 06:22:53.583945+00
7	4	1	2017-03-29 06:22:53.583945+00
8	5	1	2017-03-29 06:22:53.583945+00
9	6	1	2017-03-29 06:22:53.583945+00
10	7	1	2017-03-29 06:22:53.583945+00
11	8	1	2017-03-29 06:22:53.583945+00
12	9	1	2017-03-29 06:22:53.583945+00
13	10	1	2017-03-29 06:22:53.583945+00
\.

SELECT pg_catalog.setval('data_row_id_seq', 13, true);

COPY data_source (id, accessor_write, accessor_read, name, source_serial, resynced) FROM stdin;
1	0	0	nouns	P:services::mounty:numbers:jackrose:test	2017-03-29 06:22:53.583945+00
\.

SELECT pg_catalog.setval('data_source_id_seq', 1, false);

COPY history (item, stamp, grade) FROM stdin;
4	2017-03-31 03:46:13.562729+00	98
6	2017-03-31 03:46:17.219053+00	98
8	2017-03-31 03:46:20.908548+00	98
10	2017-03-31 03:46:24.347422+00	98
12	2017-03-31 03:46:27.802672+00	98
4	2017-04-01 05:30:32.688472+00	112
6	2017-04-01 05:30:36.225472+00	98
8	2017-04-01 05:30:43.170336+00	42
\.

COPY learn_datum (id, view_u_i_d, item_id, "user", activity, space_algorithm, factor1, factor2, next_review) FROM stdin;
5	2	4	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
7	2	5	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
9	2	6	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
11	2	7	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
13	2	8	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
14	1	9	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
15	2	9	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
16	1	10	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
17	2	10	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
18	1	11	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
19	2	11	1	0	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
20	1	12	1	1	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
21	2	12	1	1	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
22	1	13	1	1	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
23	2	13	1	1	1	0.299999999999999989	86400	2017-03-29 06:22:53.583945+00
10	1	7	1	2	1	0.299999999999999989	86400	2017-04-01 03:46:24.347422+00
12	1	8	1	2	1	0.299999999999999989	86400	2017-04-01 03:46:27.802672+00
4	1	4	1	2	1	0.299999999999999989	86400	2017-04-02 05:30:32.688472+00
6	1	5	1	2	1	0.299999999999999989	86400	2017-04-02 05:30:36.225472+00
8	1	6	1	2	1	0.299999999999999989	86400	2017-04-01 05:30:44.170336+00
\.

SELECT pg_catalog.setval('learn_datum_id_seq', 23, true);

COPY user_deck_end (view_id, "user", parent, throttle, shuffle) FROM stdin;
1	1	\N	\N	\N
2	1	\N	\N	\N
\.

COPY user_deck_node (id, parent, "user", throttle, shuffle, label) FROM stdin;
\.

SELECT pg_catalog.setval('user_deck_node_id_seq', 1, false);

COPY view (id, name, data_source_id, obverse, reverse, style_c_s_s) FROM stdin;
1	econosphere	1	What is the German for <field name="english"/>	<frontSide/><hr/><field name="german"/>	font-family: Code2000;\nfont-size:24pt;\nbackground-color: #00ff80;\ntext-align: center;
2	engtogreek	1	What is the Greek for <field name="english"/>	<frontSide/><hr/><field name="greek"/>	font-family: Code2000;\nfont-size:24pt;\ntext-align: center;\nbackground-color: #00ff80;
\.

SELECT pg_catalog.setval('view_id_seq', 1, false);

ALTER TABLE ONLY "Member" ADD CONSTRAINT "Member_pkey" PRIMARY KEY (id);

ALTER TABLE ONLY "User" ADD CONSTRAINT "UniqueUsername" UNIQUE (username);

ALTER TABLE ONLY "User" ADD CONSTRAINT "User_pkey" PRIMARY KEY (id);

ALTER TABLE ONLY data_row ADD CONSTRAINT data_row_pkey PRIMARY KEY (id);

ALTER TABLE ONLY data_source ADD CONSTRAINT data_source_pkey PRIMARY KEY (id);

ALTER TABLE ONLY history ADD CONSTRAINT history_pkey PRIMARY KEY (item, stamp);

ALTER TABLE ONLY learn_datum ADD CONSTRAINT learn_datum_pkey PRIMARY KEY (id);

ALTER TABLE ONLY learn_datum ADD CONSTRAINT learn_item UNIQUE (view_u_i_d, item_id, "user");

ALTER TABLE ONLY data_row ADD CONSTRAINT unique_row UNIQUE (table_key, data_source_row_id);

ALTER TABLE ONLY user_deck_end ADD CONSTRAINT user_deck_end_pkey PRIMARY KEY (view_id, "user");

ALTER TABLE ONLY user_deck_node ADD CONSTRAINT user_deck_node_pkey PRIMARY KEY (id);

ALTER TABLE ONLY view ADD CONSTRAINT view_pkey PRIMARY KEY (id);

ALTER TABLE ONLY "Member" ADD CONSTRAINT "Member_child_fkey" FOREIGN KEY (child) REFERENCES "User"(id);

ALTER TABLE ONLY "Member" ADD CONSTRAINT "Member_parent_fkey" FOREIGN KEY (parent) REFERENCES "User"(id);

ALTER TABLE ONLY data_row ADD CONSTRAINT data_row_data_source_row_id_fkey FOREIGN KEY (data_source_row_id) REFERENCES data_source(id);

ALTER TABLE ONLY history ADD CONSTRAINT history_item_fkey FOREIGN KEY (item) REFERENCES learn_datum(id);

ALTER TABLE ONLY learn_datum ADD CONSTRAINT learn_datum_item_id_fkey FOREIGN KEY (item_id) REFERENCES data_row(id);

ALTER TABLE ONLY learn_datum ADD CONSTRAINT learn_datum_view_u_i_d_fkey FOREIGN KEY (view_u_i_d) REFERENCES view(id);

ALTER TABLE ONLY user_deck_end ADD CONSTRAINT user_deck_end_parent_fkey FOREIGN KEY (parent) REFERENCES user_deck_node(id);

ALTER TABLE ONLY user_deck_node ADD CONSTRAINT user_deck_node_parent_fkey FOREIGN KEY (parent) REFERENCES user_deck_node(id);

ALTER TABLE ONLY view ADD CONSTRAINT view_data_source_id_fkey FOREIGN KEY (data_source_id) REFERENCES data_source(id);

--
-- Phase three:  'time-shift' historical data to the present.
--

UPDATE data_row SET loaded = loaded + timeshift();
UPDATE data_source SET resynced = resynced + timeshift();
UPDATE history SET stamp = stamp + timeshift();
UPDATE learn_datum SET next_review = next_review + timeshift();
