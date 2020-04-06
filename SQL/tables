DROP TABLE public.university
CREATE TABLE public.university
(
    id integer NOT NULL,
    university character varying,
    code character varying,
    grade integer,
    latitude numeric,
    longitude numeric,
    year integer,
    course_id integer,
    level_id integer,
    city_id integer,
    PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.university OWNER to postgres;




DROP TABLE public.research_topics
CREATE TABLE public.research_topics
(
    id integer NOT NULL,
    name character varying,
    PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.research_topics OWNER to postgres;




DROP TABLE public.graduation_level
CREATE TABLE public.graduation_level
(
    id integer NOT NULL,
    graduation_level character varying,
    PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.graduation_level OWNER to postgres;




DROP TABLE public.concentration_area
CREATE TABLE public.concentration_area
(
    id integer NOT NULL,
    concentration_area character varying,
    PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.concentration_area OWNER to postgres;




DROP TABLE public.university_concentration_area
CREATE TABLE public.university_concentration_area
(
    id_university integer NOT NULL,
    id_concentration_area integer NOT NULL
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.university_concentration_area OWNER to postgres;




DROP TABLE public.university_research_topics
CREATE TABLE public.university_research_topics
(
    id_university integer NOT NULL,
    id_research_topics integer NOT NULL
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.university_concentration_area OWNER to postgres;




DROP TABLE public.brazilian_cities
CREATE TABLE public.brazilian_cities
(
    ibge_code integer NOT NULL,
    city_name character varying,
    latitude numeric,
    longitude numeric,
    capital boolean,
    state_id integer,
    PRIMARY KEY (ibge_code)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.brazilian_cities OWNER to postgres;




DROP TABLE public.brazilian_states
CREATE TABLE public.brazilian_states
(
    state_id integer NOT NULL,
    state_code character varying(3),
    state_name character varying,
    PRIMARY KEY (state_id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.brazilian_states OWNER to postgres;



DROP TABLE public.course_name
CREATE TABLE public.course_name
(
    id integer NOT NULL,
    course_name character varying,
    PRIMARY KEY (id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public.course_name OWNER to postgres;

ALTER TABLE public.university
    ADD CONSTRAINT graduation_level_fk FOREIGN KEY (level_id)
    REFERENCES public.graduation_level (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.university
    ADD CONSTRAINT city_fk FOREIGN KEY (city_id)
    REFERENCES public.brazilian_cities (ibge_code) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.university
    ADD CONSTRAINT course_fk FOREIGN KEY (id)
    REFERENCES public.course_name (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.brazilian_cities
    ADD CONSTRAINT state_fk FOREIGN KEY (state_id)
    REFERENCES public.brazilian_states (state_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;



ALTER TABLE public.university_concentration_area
    ADD PRIMARY KEY (id_university, id_concentration_area);
ALTER TABLE public.university_concentration_area
    ADD FOREIGN KEY (id_university)
    REFERENCES public.university (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.university_concentration_area
    ADD FOREIGN KEY (id_concentration_area)
    REFERENCES public.concentration_area (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.university_research_topics
    ADD PRIMARY KEY (id_university, id_research_topics);
ALTER TABLE public.university_research_topics
    ADD FOREIGN KEY (id_university)
    REFERENCES public.university (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE public.university_research_topics
    ADD FOREIGN KEY (id_research_topics)
    REFERENCES public.research_topics (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

