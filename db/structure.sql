--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: osm_test; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA osm_test;


SET search_path = osm_test, pg_catalog;

--
-- Name: format_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE format_enum AS ENUM (
    'html',
    'markdown',
    'text'
);


--
-- Name: gpx_visibility_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE gpx_visibility_enum AS ENUM (
    'private',
    'public',
    'trackable',
    'identifiable'
);


--
-- Name: note_event_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE note_event_enum AS ENUM (
    'opened',
    'closed',
    'reopened',
    'commented',
    'hidden'
);


--
-- Name: note_status_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE note_status_enum AS ENUM (
    'open',
    'closed',
    'hidden'
);


--
-- Name: nwr_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE nwr_enum AS ENUM (
    'Node',
    'Way',
    'Relation'
);


--
-- Name: user_role_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE user_role_enum AS ENUM (
    'administrator',
    'moderator'
);


--
-- Name: user_status_enum; Type: TYPE; Schema: osm_test; Owner: -
--

CREATE TYPE user_status_enum AS ENUM (
    'pending',
    'active',
    'confirmed',
    'suspended',
    'deleted'
);


SET search_path = public, pg_catalog;

--
-- Name: box2d; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE box2d;


--
-- Name: box2d_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2d_in(cstring) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_in';


--
-- Name: box2d_out(box2d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2d_out(box2d) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_out';


--
-- Name: box2d; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE box2d (
    INTERNALLENGTH = 65,
    INPUT = box2d_in,
    OUTPUT = box2d_out,
    ALIGNMENT = int4,
    STORAGE = plain
);


--
-- Name: box2df; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE box2df;


--
-- Name: box2df_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2df_in(cstring) RETURNS box2df
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'box2df_in';


--
-- Name: box2df_out(box2df); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2df_out(box2df) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'box2df_out';


--
-- Name: box2df; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE box2df (
    INTERNALLENGTH = 16,
    INPUT = box2df_in,
    OUTPUT = box2df_out,
    ALIGNMENT = double,
    STORAGE = plain
);


--
-- Name: box3d; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE box3d;


--
-- Name: box3d_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box3d_in(cstring) RETURNS box3d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_in';


--
-- Name: box3d_out(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box3d_out(box3d) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_out';


--
-- Name: box3d; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE box3d (
    INTERNALLENGTH = 52,
    INPUT = box3d_in,
    OUTPUT = box3d_out,
    ALIGNMENT = double,
    STORAGE = plain
);


--
-- Name: geography; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE geography;


--
-- Name: geography_analyze(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_analyze(internal) RETURNS boolean
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', 'gserialized_analyze_nd';


--
-- Name: geography_in(cstring, oid, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_in(cstring, oid, integer) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_in';


--
-- Name: geography_out(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_out(geography) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_out';


--
-- Name: geography_recv(internal, oid, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_recv(internal, oid, integer) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_recv';


--
-- Name: geography_send(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_send(geography) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_send';


--
-- Name: geography_typmod_in(cstring[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_typmod_in(cstring[]) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_typmod_in';


--
-- Name: geography_typmod_out(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_typmod_out(integer) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_typmod_out';


--
-- Name: geography; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE geography (
    INTERNALLENGTH = variable,
    INPUT = geography_in,
    OUTPUT = geography_out,
    RECEIVE = geography_recv,
    SEND = geography_send,
    TYPMOD_IN = geography_typmod_in,
    TYPMOD_OUT = geography_typmod_out,
    ANALYZE = geography_analyze,
    DELIMITER = ':',
    ALIGNMENT = double,
    STORAGE = main
);


--
-- Name: geometry; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE geometry;


--
-- Name: geometry_analyze(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_analyze(internal) RETURNS boolean
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', 'gserialized_analyze_nd';


--
-- Name: geometry_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_in(cstring) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_in';


--
-- Name: geometry_out(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_out(geometry) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_out';


--
-- Name: geometry_recv(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_recv(internal) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_recv';


--
-- Name: geometry_send(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_send(geometry) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_send';


--
-- Name: geometry_typmod_in(cstring[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_typmod_in(cstring[]) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_typmod_in';


--
-- Name: geometry_typmod_out(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_typmod_out(integer) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_typmod_out';


--
-- Name: geometry; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE geometry (
    INTERNALLENGTH = variable,
    INPUT = geometry_in,
    OUTPUT = geometry_out,
    RECEIVE = geometry_recv,
    SEND = geometry_send,
    TYPMOD_IN = geometry_typmod_in,
    TYPMOD_OUT = geometry_typmod_out,
    ANALYZE = geometry_analyze,
    DELIMITER = ':',
    ALIGNMENT = double,
    STORAGE = main
);


--
-- Name: geometry_dump; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE geometry_dump AS (
	path integer[],
	geom geometry
);


--
-- Name: gidx; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE gidx;


--
-- Name: gidx_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gidx_in(cstring) RETURNS gidx
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gidx_in';


--
-- Name: gidx_out(gidx); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gidx_out(gidx) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gidx_out';


--
-- Name: gidx; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE gidx (
    INTERNALLENGTH = variable,
    INPUT = gidx_in,
    OUTPUT = gidx_out,
    ALIGNMENT = double,
    STORAGE = plain
);


--
-- Name: pgis_abs; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE pgis_abs;


--
-- Name: pgis_abs_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_abs_in(cstring) RETURNS pgis_abs
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'pgis_abs_in';


--
-- Name: pgis_abs_out(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_abs_out(pgis_abs) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'pgis_abs_out';


--
-- Name: pgis_abs; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE pgis_abs (
    INTERNALLENGTH = 8,
    INPUT = pgis_abs_in,
    OUTPUT = pgis_abs_out,
    ALIGNMENT = double,
    STORAGE = plain
);


--
-- Name: spheroid; Type: SHELL TYPE; Schema: public; Owner: -
--

CREATE TYPE spheroid;


--
-- Name: spheroid_in(cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spheroid_in(cstring) RETURNS spheroid
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ellipsoid_in';


--
-- Name: spheroid_out(spheroid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION spheroid_out(spheroid) RETURNS cstring
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ellipsoid_out';


--
-- Name: spheroid; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE spheroid (
    INTERNALLENGTH = 65,
    INPUT = spheroid_in,
    OUTPUT = spheroid_out,
    ALIGNMENT = double,
    STORAGE = plain
);


--
-- Name: valid_detail; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE valid_detail AS (
	valid boolean,
	reason character varying,
	location geometry
);


--
-- Name: _postgis_deprecate(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _postgis_deprecate(oldname text, newname text, version text) RETURNS void
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
BEGIN
    RAISE WARNING '% signature was deprecated in %. Please use %', oldname, version, newname;
END;
$$;


--
-- Name: _postgis_join_selectivity(regclass, text, regclass, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _postgis_join_selectivity(regclass, text, regclass, text, text DEFAULT '2'::text) RETURNS double precision
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', '_postgis_gserialized_joinsel';


--
-- Name: _postgis_selectivity(regclass, text, geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _postgis_selectivity(tbl regclass, att_name text, geom geometry, mode text DEFAULT '2'::text) RETURNS double precision
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', '_postgis_gserialized_sel';


--
-- Name: _postgis_stats(regclass, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _postgis_stats(tbl regclass, att_name text, text DEFAULT '2'::text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', '_postgis_gserialized_stats';


--
-- Name: _st_3ddfullywithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_3ddfullywithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_dfullywithin3d';


--
-- Name: _st_3ddwithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_3ddwithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_dwithin3d';


--
-- Name: _st_3dintersects(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_3dintersects(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'intersects3d';


--
-- Name: _st_asgeojson(integer, geography, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_asgeojson(integer, geography, integer, integer) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_as_geojson';


--
-- Name: _st_asgeojson(integer, geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_asgeojson(integer, geometry, integer, integer) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asGeoJson';


--
-- Name: _st_asgml(integer, geography, integer, integer, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_asgml(integer, geography, integer, integer, text, text) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'geography_as_gml';


--
-- Name: _st_asgml(integer, geometry, integer, integer, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_asgml(integer, geometry, integer, integer, text, text) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'LWGEOM_asGML';


--
-- Name: _st_askml(integer, geography, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_askml(integer, geography, integer, text) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'geography_as_kml';


--
-- Name: _st_askml(integer, geometry, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_askml(integer, geometry, integer, text) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'LWGEOM_asKML';


--
-- Name: _st_asx3d(integer, geometry, integer, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_asx3d(integer, geometry, integer, integer, text) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'LWGEOM_asX3D';


--
-- Name: _st_bestsrid(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_bestsrid(geography) RETURNS integer
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_BestSRID($1,$1)$_$;


--
-- Name: _st_bestsrid(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_bestsrid(geography, geography) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_bestsrid';


--
-- Name: _st_buffer(geometry, double precision, cstring); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_buffer(geometry, double precision, cstring) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'buffer';


--
-- Name: _st_concavehull(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_concavehull(param_inputgeom geometry) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
	DECLARE     
	vexhull GEOMETRY;
	var_resultgeom geometry;
	var_inputgeom geometry;
	vexring GEOMETRY;
	cavering GEOMETRY;
	cavept geometry[];
	seglength double precision;
	var_tempgeom geometry;
	scale_factor integer := 1;
	i integer;
	
	BEGIN

		-- First compute the ConvexHull of the geometry
		vexhull := ST_ConvexHull(param_inputgeom);
		var_inputgeom := param_inputgeom;
		--A point really has no concave hull
		IF ST_GeometryType(vexhull) = 'ST_Point' OR ST_GeometryType(vexHull) = 'ST_LineString' THEN
			RETURN vexhull;
		END IF;

		-- convert the hull perimeter to a linestring so we can manipulate individual points
		vexring := CASE WHEN ST_GeometryType(vexhull) = 'ST_LineString' THEN vexhull ELSE ST_ExteriorRing(vexhull) END;
		IF abs(ST_X(ST_PointN(vexring,1))) < 1 THEN --scale the geometry to prevent stupid precision errors - not sure it works so make low for now
			scale_factor := 100;
			vexring := ST_Scale(vexring, scale_factor,scale_factor);
			var_inputgeom := ST_Scale(var_inputgeom, scale_factor, scale_factor);
			--RAISE NOTICE 'Scaling';
		END IF;
		seglength := ST_Length(vexring)/least(ST_NPoints(vexring)*2,1000) ;

		vexring := ST_Segmentize(vexring, seglength);
		-- find the point on the original geom that is closest to each point of the convex hull and make a new linestring out of it.
		cavering := ST_Collect(
			ARRAY(

				SELECT 
					ST_ClosestPoint(var_inputgeom, pt ) As the_geom
					FROM (
						SELECT  ST_PointN(vexring, n ) As pt, n
							FROM 
							generate_series(1, ST_NPoints(vexring) ) As n
						) As pt
				
				)
			)
		; 
		

		var_resultgeom := ST_MakeLine(geom) 
			FROM ST_Dump(cavering) As foo;

		IF ST_IsSimple(var_resultgeom) THEN
			var_resultgeom := ST_MakePolygon(var_resultgeom);
			--RAISE NOTICE 'is Simple: %', var_resultgeom;
		ELSE 
			--RAISE NOTICE 'is not Simple: %', var_resultgeom;
			var_resultgeom := ST_ConvexHull(var_resultgeom);
		END IF;
		
		IF scale_factor > 1 THEN -- scale the result back
			var_resultgeom := ST_Scale(var_resultgeom, 1/scale_factor, 1/scale_factor);
		END IF;
		RETURN var_resultgeom;
	
	END;
$$;


--
-- Name: _st_contains(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_contains(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'contains';


--
-- Name: _st_containsproperly(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_containsproperly(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'containsproperly';


--
-- Name: _st_coveredby(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_coveredby(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'coveredby';


--
-- Name: _st_covers(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_covers(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'covers';


--
-- Name: _st_covers(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_covers(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_covers';


--
-- Name: _st_crosses(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_crosses(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'crosses';


--
-- Name: _st_dfullywithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dfullywithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dfullywithin';


--
-- Name: _st_distance(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distance(geography, geography, double precision, boolean) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_distance';


--
-- Name: _st_distancetree(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distancetree(geography, geography) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_DistanceTree($1, $2, 0.0, true)$_$;


--
-- Name: _st_distancetree(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distancetree(geography, geography, double precision, boolean) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_distance_tree';


--
-- Name: _st_distanceuncached(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distanceuncached(geography, geography) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_DistanceUnCached($1, $2, 0.0, true)$_$;


--
-- Name: _st_distanceuncached(geography, geography, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distanceuncached(geography, geography, boolean) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_DistanceUnCached($1, $2, 0.0, $3)$_$;


--
-- Name: _st_distanceuncached(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_distanceuncached(geography, geography, double precision, boolean) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_distance_uncached';


--
-- Name: _st_dumppoints(geometry, integer[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dumppoints(the_geom geometry, cur_path integer[]) RETURNS SETOF geometry_dump
    LANGUAGE plpgsql
    AS $$
DECLARE
  tmp geometry_dump;
  tmp2 geometry_dump;
  nb_points integer;
  nb_geom integer;
  i integer;
  j integer;
  g geometry;
  
BEGIN
  
  -- RAISE DEBUG '%,%', cur_path, ST_GeometryType(the_geom);

  -- Special case collections : iterate and return the DumpPoints of the geometries

  IF (ST_IsCollection(the_geom)) THEN
 
    i = 1;
    FOR tmp2 IN SELECT (ST_Dump(the_geom)).* LOOP

      FOR tmp IN SELECT * FROM _ST_DumpPoints(tmp2.geom, cur_path || tmp2.path) LOOP
	    RETURN NEXT tmp;
      END LOOP;
      i = i + 1;
      
    END LOOP;

    RETURN;
  END IF;
  

  -- Special case (POLYGON) : return the points of the rings of a polygon
  IF (ST_GeometryType(the_geom) = 'ST_Polygon') THEN

    FOR tmp IN SELECT * FROM _ST_DumpPoints(ST_ExteriorRing(the_geom), cur_path || ARRAY[1]) LOOP
      RETURN NEXT tmp;
    END LOOP;
    
    j := ST_NumInteriorRings(the_geom);
    FOR i IN 1..j LOOP
        FOR tmp IN SELECT * FROM _ST_DumpPoints(ST_InteriorRingN(the_geom, i), cur_path || ARRAY[i+1]) LOOP
          RETURN NEXT tmp;
        END LOOP;
    END LOOP;
    
    RETURN;
  END IF;

  -- Special case (TRIANGLE) : return the points of the external rings of a TRIANGLE
  IF (ST_GeometryType(the_geom) = 'ST_Triangle') THEN

    FOR tmp IN SELECT * FROM _ST_DumpPoints(ST_ExteriorRing(the_geom), cur_path || ARRAY[1]) LOOP
      RETURN NEXT tmp;
    END LOOP;
    
    RETURN;
  END IF;

    
  -- Special case (POINT) : return the point
  IF (ST_GeometryType(the_geom) = 'ST_Point') THEN

    tmp.path = cur_path || ARRAY[1];
    tmp.geom = the_geom;

    RETURN NEXT tmp;
    RETURN;

  END IF;


  -- Use ST_NumPoints rather than ST_NPoints to have a NULL value if the_geom isn't
  -- a LINESTRING, CIRCULARSTRING.
  SELECT ST_NumPoints(the_geom) INTO nb_points;

  -- This should never happen
  IF (nb_points IS NULL) THEN
    RAISE EXCEPTION 'Unexpected error while dumping geometry %', ST_AsText(the_geom);
  END IF;

  FOR i IN 1..nb_points LOOP
    tmp.path = cur_path || ARRAY[i];
    tmp.geom := ST_PointN(the_geom, i);
    RETURN NEXT tmp;
  END LOOP;
   
END
$$;


--
-- Name: _st_dwithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dwithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_dwithin';


--
-- Name: _st_dwithin(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dwithin(geography, geography, double precision, boolean) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_dwithin';


--
-- Name: _st_dwithinuncached(geography, geography, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dwithinuncached(geography, geography, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && _ST_Expand($2,$3) AND $2 && _ST_Expand($1,$3) AND _ST_DWithinUnCached($1, $2, $3, true)$_$;


--
-- Name: _st_dwithinuncached(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_dwithinuncached(geography, geography, double precision, boolean) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_dwithin_uncached';


--
-- Name: _st_equals(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_equals(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_Equals';


--
-- Name: _st_expand(geography, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_expand(geography, double precision) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_expand';


--
-- Name: _st_geomfromgml(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_geomfromgml(text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'geom_from_gml';


--
-- Name: _st_intersects(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_intersects(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'intersects';


--
-- Name: _st_linecrossingdirection(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_linecrossingdirection(geom1 geometry, geom2 geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_LineCrossingDirection';


--
-- Name: _st_longestline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_longestline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_longestline2d';


--
-- Name: _st_maxdistance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_maxdistance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_maxdistance2d_linestring';


--
-- Name: _st_orderingequals(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_orderingequals(geometrya geometry, geometryb geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_same';


--
-- Name: _st_overlaps(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_overlaps(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'overlaps';


--
-- Name: _st_pointoutside(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_pointoutside(geography) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_point_outside';


--
-- Name: _st_touches(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_touches(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'touches';


--
-- Name: _st_within(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION _st_within(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT _ST_Contains($2,$1)$_$;


--
-- Name: addauth(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION addauth(text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$ 
DECLARE
	lockid alias for $1;
	okay boolean;
	myrec record;
BEGIN
	-- check to see if table exists
	--  if not, CREATE TEMP TABLE mylock (transid xid, lockcode text)
	okay := 'f';
	FOR myrec IN SELECT * FROM pg_class WHERE relname = 'temp_lock_have_table' LOOP
		okay := 't';
	END LOOP; 
	IF (okay <> 't') THEN 
		CREATE TEMP TABLE temp_lock_have_table (transid xid, lockcode text);
			-- this will only work from pgsql7.4 up
			-- ON COMMIT DELETE ROWS;
	END IF;

	--  INSERT INTO mylock VALUES ( $1)
--	EXECUTE 'INSERT INTO temp_lock_have_table VALUES ( '||
--		quote_literal(getTransactionID()) || ',' ||
--		quote_literal(lockid) ||')';

	INSERT INTO temp_lock_have_table VALUES (getTransactionID(), lockid);

	RETURN true::boolean;
END;
$_$;


--
-- Name: addgeometrycolumn(character varying, character varying, integer, character varying, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION addgeometrycolumn(table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean DEFAULT true) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('','',$1,$2,$3,$4,$5, $6) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION addgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean DEFAULT true) RETURNS text
    LANGUAGE plpgsql STABLE STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('',$1,$2,$3,$4,$5,$6,$7) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION addgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer, new_type character varying, new_dim integer, use_typmod boolean DEFAULT true) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $$
DECLARE
	rec RECORD;
	sr varchar;
	real_schema name;
	sql text;
	new_srid integer;

BEGIN

	-- Verify geometry type
	IF (postgis_type_name(new_type,new_dim) IS NULL )
	THEN
		RAISE EXCEPTION 'Invalid type name "%(%)" - valid ones are:
	POINT, MULTIPOINT,
	LINESTRING, MULTILINESTRING,
	POLYGON, MULTIPOLYGON,
	CIRCULARSTRING, COMPOUNDCURVE, MULTICURVE,
	CURVEPOLYGON, MULTISURFACE,
	GEOMETRY, GEOMETRYCOLLECTION,
	POINTM, MULTIPOINTM,
	LINESTRINGM, MULTILINESTRINGM,
	POLYGONM, MULTIPOLYGONM,
	CIRCULARSTRINGM, COMPOUNDCURVEM, MULTICURVEM
	CURVEPOLYGONM, MULTISURFACEM, TRIANGLE, TRIANGLEM,
	POLYHEDRALSURFACE, POLYHEDRALSURFACEM, TIN, TINM
	or GEOMETRYCOLLECTIONM', new_type, new_dim;
		RETURN 'fail';
	END IF;


	-- Verify dimension
	IF ( (new_dim >4) OR (new_dim <2) ) THEN
		RAISE EXCEPTION 'invalid dimension';
		RETURN 'fail';
	END IF;

	IF ( (new_type LIKE '%M') AND (new_dim!=3) ) THEN
		RAISE EXCEPTION 'TypeM needs 3 dimensions';
		RETURN 'fail';
	END IF;


	-- Verify SRID
	IF ( new_srid_in > 0 ) THEN
		IF new_srid_in > 998999 THEN
			RAISE EXCEPTION 'AddGeometryColumn() - SRID must be <= %', 998999;
		END IF;
		new_srid := new_srid_in;
		SELECT SRID INTO sr FROM spatial_ref_sys WHERE SRID = new_srid;
		IF NOT FOUND THEN
			RAISE EXCEPTION 'AddGeometryColumn() - invalid SRID';
			RETURN 'fail';
		END IF;
	ELSE
		new_srid := ST_SRID('POINT EMPTY'::geometry);
		IF ( new_srid_in != new_srid ) THEN
			RAISE NOTICE 'SRID value % converted to the officially unknown SRID value %', new_srid_in, new_srid;
		END IF;
	END IF;


	-- Verify schema
	IF ( schema_name IS NOT NULL AND schema_name != '' ) THEN
		sql := 'SELECT nspname FROM pg_namespace ' ||
			'WHERE text(nspname) = ' || quote_literal(schema_name) ||
			'LIMIT 1';
		RAISE DEBUG '%', sql;
		EXECUTE sql INTO real_schema;

		IF ( real_schema IS NULL ) THEN
			RAISE EXCEPTION 'Schema % is not a valid schemaname', quote_literal(schema_name);
			RETURN 'fail';
		END IF;
	END IF;

	IF ( real_schema IS NULL ) THEN
		RAISE DEBUG 'Detecting schema';
		sql := 'SELECT n.nspname AS schemaname ' ||
			'FROM pg_catalog.pg_class c ' ||
			  'JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ' ||
			'WHERE c.relkind = ' || quote_literal('r') ||
			' AND n.nspname NOT IN (' || quote_literal('pg_catalog') || ', ' || quote_literal('pg_toast') || ')' ||
			' AND pg_catalog.pg_table_is_visible(c.oid)' ||
			' AND c.relname = ' || quote_literal(table_name);
		RAISE DEBUG '%', sql;
		EXECUTE sql INTO real_schema;

		IF ( real_schema IS NULL ) THEN
			RAISE EXCEPTION 'Table % does not occur in the search_path', quote_literal(table_name);
			RETURN 'fail';
		END IF;
	END IF;


	-- Add geometry column to table
	IF use_typmod THEN
	     sql := 'ALTER TABLE ' ||
            quote_ident(real_schema) || '.' || quote_ident(table_name)
            || ' ADD COLUMN ' || quote_ident(column_name) ||
            ' geometry(' || postgis_type_name(new_type, new_dim) || ', ' || new_srid::text || ')';
        RAISE DEBUG '%', sql;
	ELSE
        sql := 'ALTER TABLE ' ||
            quote_ident(real_schema) || '.' || quote_ident(table_name)
            || ' ADD COLUMN ' || quote_ident(column_name) ||
            ' geometry ';
        RAISE DEBUG '%', sql;
    END IF;
	EXECUTE sql;

	IF NOT use_typmod THEN
        -- Add table CHECKs
        sql := 'ALTER TABLE ' ||
            quote_ident(real_schema) || '.' || quote_ident(table_name)
            || ' ADD CONSTRAINT '
            || quote_ident('enforce_srid_' || column_name)
            || ' CHECK (st_srid(' || quote_ident(column_name) ||
            ') = ' || new_srid::text || ')' ;
        RAISE DEBUG '%', sql;
        EXECUTE sql;
    
        sql := 'ALTER TABLE ' ||
            quote_ident(real_schema) || '.' || quote_ident(table_name)
            || ' ADD CONSTRAINT '
            || quote_ident('enforce_dims_' || column_name)
            || ' CHECK (st_ndims(' || quote_ident(column_name) ||
            ') = ' || new_dim::text || ')' ;
        RAISE DEBUG '%', sql;
        EXECUTE sql;
    
        IF ( NOT (new_type = 'GEOMETRY')) THEN
            sql := 'ALTER TABLE ' ||
                quote_ident(real_schema) || '.' || quote_ident(table_name) || ' ADD CONSTRAINT ' ||
                quote_ident('enforce_geotype_' || column_name) ||
                ' CHECK (GeometryType(' ||
                quote_ident(column_name) || ')=' ||
                quote_literal(new_type) || ' OR (' ||
                quote_ident(column_name) || ') is null)';
            RAISE DEBUG '%', sql;
            EXECUTE sql;
        END IF;
    END IF;

	RETURN
		real_schema || '.' ||
		table_name || '.' || column_name ||
		' SRID:' || new_srid::text ||
		' TYPE:' || new_type ||
		' DIMS:' || new_dim::text || ' ';
END;
$$;


--
-- Name: box(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box(box3d) RETURNS box
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_to_BOX';


--
-- Name: box(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box(geometry) RETURNS box
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_BOX';


--
-- Name: box2d(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2d(box3d) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_to_BOX2D';


--
-- Name: box2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box2d(geometry) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_BOX2D';


--
-- Name: box3d(box2d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box3d(box2d) RETURNS box3d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_to_BOX3D';


--
-- Name: box3d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box3d(geometry) RETURNS box3d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_BOX3D';


--
-- Name: box3dtobox(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION box3dtobox(box3d) RETURNS box
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT box($1)$_$;


--
-- Name: bytea(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION bytea(geography) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_bytea';


--
-- Name: bytea(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION bytea(geometry) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_bytea';


--
-- Name: checkauth(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION checkauth(text, text) RETURNS integer
    LANGUAGE sql
    AS $_$ SELECT CheckAuth('', $1, $2) $_$;


--
-- Name: checkauth(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION checkauth(text, text, text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$ 
DECLARE
	schema text;
BEGIN
	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION 'Long transaction support disabled, use EnableLongTransaction() to enable.';
	END IF;

	if ( $1 != '' ) THEN
		schema = $1;
	ELSE
		SELECT current_schema() into schema;
	END IF;

	-- TODO: check for an already existing trigger ?

	EXECUTE 'CREATE TRIGGER check_auth BEFORE UPDATE OR DELETE ON ' 
		|| quote_ident(schema) || '.' || quote_ident($2)
		||' FOR EACH ROW EXECUTE PROCEDURE CheckAuthTrigger('
		|| quote_literal($3) || ')';

	RETURN 0;
END;
$_$;


--
-- Name: checkauthtrigger(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION checkauthtrigger() RETURNS trigger
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'check_authorization';


--
-- Name: disablelongtransactions(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION disablelongtransactions() RETURNS text
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	rec RECORD;

BEGIN

	--
	-- Drop all triggers applied by CheckAuth()
	--
	FOR rec IN
		SELECT c.relname, t.tgname, t.tgargs FROM pg_trigger t, pg_class c, pg_proc p
		WHERE p.proname = 'checkauthtrigger' and t.tgfoid = p.oid and t.tgrelid = c.oid
	LOOP
		EXECUTE 'DROP TRIGGER ' || quote_ident(rec.tgname) ||
			' ON ' || quote_ident(rec.relname);
	END LOOP;

	--
	-- Drop the authorization_table table
	--
	FOR rec IN SELECT * FROM pg_class WHERE relname = 'authorization_table' LOOP
		DROP TABLE authorization_table;
	END LOOP;

	--
	-- Drop the authorized_tables view
	--
	FOR rec IN SELECT * FROM pg_class WHERE relname = 'authorized_tables' LOOP
		DROP VIEW authorized_tables;
	END LOOP;

	RETURN 'Long transactions support disabled';
END;
$$;


--
-- Name: dropgeometrycolumn(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrycolumn(table_name character varying, column_name character varying) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret text;
BEGIN
	SELECT DropGeometryColumn('','',$1,$2) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: dropgeometrycolumn(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret text;
BEGIN
	SELECT DropGeometryColumn('',$1,$2,$3) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: dropgeometrycolumn(character varying, character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $$
DECLARE
	myrec RECORD;
	okay boolean;
	real_schema name;

BEGIN


	-- Find, check or fix schema_name
	IF ( schema_name != '' ) THEN
		okay = false;

		FOR myrec IN SELECT nspname FROM pg_namespace WHERE text(nspname) = schema_name LOOP
			okay := true;
		END LOOP;

		IF ( okay <>  true ) THEN
			RAISE NOTICE 'Invalid schema name - using current_schema()';
			SELECT current_schema() into real_schema;
		ELSE
			real_schema = schema_name;
		END IF;
	ELSE
		SELECT current_schema() into real_schema;
	END IF;

	-- Find out if the column is in the geometry_columns table
	okay = false;
	FOR myrec IN SELECT * from geometry_columns where f_table_schema = text(real_schema) and f_table_name = table_name and f_geometry_column = column_name LOOP
		okay := true;
	END LOOP;
	IF (okay <> true) THEN
		RAISE EXCEPTION 'column not found in geometry_columns table';
		RETURN false;
	END IF;

	-- Remove table column
	EXECUTE 'ALTER TABLE ' || quote_ident(real_schema) || '.' ||
		quote_ident(table_name) || ' DROP COLUMN ' ||
		quote_ident(column_name);

	RETURN real_schema || '.' || table_name || '.' || column_name ||' effectively removed.';

END;
$$;


--
-- Name: dropgeometrytable(character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrytable(table_name character varying) RETURNS text
    LANGUAGE sql STRICT
    AS $_$ SELECT DropGeometryTable('','',$1) $_$;


--
-- Name: dropgeometrytable(character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrytable(schema_name character varying, table_name character varying) RETURNS text
    LANGUAGE sql STRICT
    AS $_$ SELECT DropGeometryTable('',$1,$2) $_$;


--
-- Name: dropgeometrytable(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION dropgeometrytable(catalog_name character varying, schema_name character varying, table_name character varying) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $$
DECLARE
	real_schema name;

BEGIN

	IF ( schema_name = '' ) THEN
		SELECT current_schema() into real_schema;
	ELSE
		real_schema = schema_name;
	END IF;

	-- TODO: Should we warn if table doesn't exist probably instead just saying dropped
	-- Remove table
	EXECUTE 'DROP TABLE IF EXISTS '
		|| quote_ident(real_schema) || '.' ||
		quote_ident(table_name) || ' RESTRICT';

	RETURN
		real_schema || '.' ||
		table_name ||' dropped.';

END;
$$;


--
-- Name: enablelongtransactions(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION enablelongtransactions() RETURNS text
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	"query" text;
	exists bool;
	rec RECORD;

BEGIN

	exists = 'f';
	FOR rec IN SELECT * FROM pg_class WHERE relname = 'authorization_table'
	LOOP
		exists = 't';
	END LOOP;

	IF NOT exists
	THEN
		"query" = 'CREATE TABLE authorization_table (
			toid oid, -- table oid
			rid text, -- row id
			expires timestamp,
			authid text
		)';
		EXECUTE "query";
	END IF;

	exists = 'f';
	FOR rec IN SELECT * FROM pg_class WHERE relname = 'authorized_tables'
	LOOP
		exists = 't';
	END LOOP;

	IF NOT exists THEN
		"query" = 'CREATE VIEW authorized_tables AS ' ||
			'SELECT ' ||
			'n.nspname as schema, ' ||
			'c.relname as table, trim(' ||
			quote_literal(chr(92) || '000') ||
			' from t.tgargs) as id_column ' ||
			'FROM pg_trigger t, pg_class c, pg_proc p ' ||
			', pg_namespace n ' ||
			'WHERE p.proname = ' || quote_literal('checkauthtrigger') ||
			' AND c.relnamespace = n.oid' ||
			' AND t.tgfoid = p.oid and t.tgrelid = c.oid';
		EXECUTE "query";
	END IF;

	RETURN 'Long transactions support enabled';
END;
$$;


--
-- Name: equals(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION equals(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_Equals';


--
-- Name: find_srid(character varying, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION find_srid(character varying, character varying, character varying) RETURNS integer
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	schem text;
	tabl text;
	sr int4;
BEGIN
	IF $1 IS NULL THEN
	  RAISE EXCEPTION 'find_srid() - schema is NULL!';
	END IF;
	IF $2 IS NULL THEN
	  RAISE EXCEPTION 'find_srid() - table name is NULL!';
	END IF;
	IF $3 IS NULL THEN
	  RAISE EXCEPTION 'find_srid() - column name is NULL!';
	END IF;
	schem = $1;
	tabl = $2;
-- if the table contains a . and the schema is empty
-- split the table into a schema and a table
-- otherwise drop through to default behavior
	IF ( schem = '' and tabl LIKE '%.%' ) THEN
	 schem = substr(tabl,1,strpos(tabl,'.')-1);
	 tabl = substr(tabl,length(schem)+2);
	ELSE
	 schem = schem || '%';
	END IF;

	select SRID into sr from geometry_columns where f_table_schema like schem and f_table_name = tabl and f_geometry_column = $3;
	IF NOT FOUND THEN
	   RAISE EXCEPTION 'find_srid() - couldnt find the corresponding SRID - is the geometry registered in the GEOMETRY_COLUMNS table?  Is there an uppercase/lowercase missmatch?';
	END IF;
	return sr;
END;
$_$;


--
-- Name: geography(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography(bytea) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_bytea';


--
-- Name: geography(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography(geometry) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_from_geometry';


--
-- Name: geography(geography, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography(geography, integer, boolean) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_enforce_typmod';


--
-- Name: geography_cmp(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_cmp(geography, geography) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_cmp';


--
-- Name: geography_eq(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_eq(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_eq';


--
-- Name: geography_ge(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_ge(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_ge';


--
-- Name: geography_gist_compress(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_compress(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_compress';


--
-- Name: geography_gist_consistent(internal, geography, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_consistent(internal, geography, integer) RETURNS boolean
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_consistent';


--
-- Name: geography_gist_decompress(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_decompress(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_decompress';


--
-- Name: geography_gist_penalty(internal, internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_penalty(internal, internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_penalty';


--
-- Name: geography_gist_picksplit(internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_picksplit(internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_picksplit';


--
-- Name: geography_gist_same(box2d, box2d, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_same(box2d, box2d, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_same';


--
-- Name: geography_gist_union(bytea, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gist_union(bytea, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_union';


--
-- Name: geography_gt(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_gt(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_gt';


--
-- Name: geography_le(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_le(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_le';


--
-- Name: geography_lt(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_lt(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_lt';


--
-- Name: geography_overlaps(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geography_overlaps(geography, geography) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overlaps';


--
-- Name: geometry(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(bytea) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_bytea';


--
-- Name: geometry(path); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(path) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'path_to_geometry';


--
-- Name: geometry(point); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(point) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'point_to_geometry';


--
-- Name: geometry(polygon); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(polygon) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'polygon_to_geometry';


--
-- Name: geometry(box2d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(box2d) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_to_LWGEOM';


--
-- Name: geometry(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(box3d) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_to_LWGEOM';


--
-- Name: geometry(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(geography) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_from_geography';


--
-- Name: geometry(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'parse_WKT_lwgeom';


--
-- Name: geometry(geometry, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry(geometry, integer, boolean) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_enforce_typmod';


--
-- Name: geometry_above(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_above(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_above_2d';


--
-- Name: geometry_below(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_below(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_below_2d';


--
-- Name: geometry_cmp(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_cmp(geom1 geometry, geom2 geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_cmp';


--
-- Name: geometry_contains(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_contains(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_contains_2d';


--
-- Name: geometry_distance_box(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_distance_box(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_distance_box_2d';


--
-- Name: geometry_distance_centroid(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_distance_centroid(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_distance_centroid_2d';


--
-- Name: geometry_eq(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_eq(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_eq';


--
-- Name: geometry_ge(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_ge(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_ge';


--
-- Name: geometry_gist_compress_2d(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_compress_2d(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_compress_2d';


--
-- Name: geometry_gist_compress_nd(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_compress_nd(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_compress';


--
-- Name: geometry_gist_consistent_2d(internal, geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_consistent_2d(internal, geometry, integer) RETURNS boolean
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_consistent_2d';


--
-- Name: geometry_gist_consistent_nd(internal, geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_consistent_nd(internal, geometry, integer) RETURNS boolean
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_consistent';


--
-- Name: geometry_gist_decompress_2d(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_decompress_2d(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_decompress_2d';


--
-- Name: geometry_gist_decompress_nd(internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_decompress_nd(internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_decompress';


--
-- Name: geometry_gist_distance_2d(internal, geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_distance_2d(internal, geometry, integer) RETURNS double precision
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_distance_2d';


--
-- Name: geometry_gist_penalty_2d(internal, internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_penalty_2d(internal, internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_penalty_2d';


--
-- Name: geometry_gist_penalty_nd(internal, internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_penalty_nd(internal, internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_penalty';


--
-- Name: geometry_gist_picksplit_2d(internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_picksplit_2d(internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_picksplit_2d';


--
-- Name: geometry_gist_picksplit_nd(internal, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_picksplit_nd(internal, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_picksplit';


--
-- Name: geometry_gist_same_2d(geometry, geometry, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_same_2d(geom1 geometry, geom2 geometry, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_same_2d';


--
-- Name: geometry_gist_same_nd(geometry, geometry, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_same_nd(geometry, geometry, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_same';


--
-- Name: geometry_gist_union_2d(bytea, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_union_2d(bytea, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_union_2d';


--
-- Name: geometry_gist_union_nd(bytea, internal); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gist_union_nd(bytea, internal) RETURNS internal
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_union';


--
-- Name: geometry_gt(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_gt(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_gt';


--
-- Name: geometry_le(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_le(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_le';


--
-- Name: geometry_left(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_left(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_left_2d';


--
-- Name: geometry_lt(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_lt(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'lwgeom_lt';


--
-- Name: geometry_overabove(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overabove(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overabove_2d';


--
-- Name: geometry_overbelow(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overbelow(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overbelow_2d';


--
-- Name: geometry_overlaps(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overlaps(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overlaps_2d';


--
-- Name: geometry_overlaps_nd(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overlaps_nd(geometry, geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overlaps';


--
-- Name: geometry_overleft(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overleft(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overleft_2d';


--
-- Name: geometry_overright(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_overright(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_overright_2d';


--
-- Name: geometry_right(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_right(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_right_2d';


--
-- Name: geometry_same(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_same(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_same_2d';


--
-- Name: geometry_within(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometry_within(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'gserialized_within_2d';


--
-- Name: geometrytype(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometrytype(geography) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_getTYPE';


--
-- Name: geometrytype(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geometrytype(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_getTYPE';


--
-- Name: geomfromewkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geomfromewkb(bytea) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOMFromWKB';


--
-- Name: geomfromewkt(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION geomfromewkt(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'parse_WKT_lwgeom';


--
-- Name: get_proj4_from_srid(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION get_proj4_from_srid(integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
BEGIN
	RETURN proj4text::text FROM spatial_ref_sys WHERE srid= $1;
END;
$_$;


--
-- Name: gettransactionid(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gettransactionid() RETURNS xid
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'getTransactionID';


--
-- Name: gserialized_gist_joinsel_2d(internal, oid, internal, smallint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gserialized_gist_joinsel_2d(internal, oid, internal, smallint) RETURNS double precision
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_joinsel_2d';


--
-- Name: gserialized_gist_joinsel_nd(internal, oid, internal, smallint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gserialized_gist_joinsel_nd(internal, oid, internal, smallint) RETURNS double precision
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_joinsel_nd';


--
-- Name: gserialized_gist_sel_2d(internal, oid, internal, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gserialized_gist_sel_2d(internal, oid, internal, integer) RETURNS double precision
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_sel_2d';


--
-- Name: gserialized_gist_sel_nd(internal, oid, internal, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION gserialized_gist_sel_nd(internal, oid, internal, integer) RETURNS double precision
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'gserialized_gist_sel_nd';


--
-- Name: lockrow(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION lockrow(text, text, text) RETURNS integer
    LANGUAGE sql STRICT
    AS $_$ SELECT LockRow(current_schema(), $1, $2, $3, now()::timestamp+'1:00'); $_$;


--
-- Name: lockrow(text, text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION lockrow(text, text, text, text) RETURNS integer
    LANGUAGE sql STRICT
    AS $_$ SELECT LockRow($1, $2, $3, $4, now()::timestamp+'1:00'); $_$;


--
-- Name: lockrow(text, text, text, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION lockrow(text, text, text, timestamp without time zone) RETURNS integer
    LANGUAGE sql STRICT
    AS $_$ SELECT LockRow(current_schema(), $1, $2, $3, $4); $_$;


--
-- Name: lockrow(text, text, text, text, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION lockrow(text, text, text, text, timestamp without time zone) RETURNS integer
    LANGUAGE plpgsql STRICT
    AS $_$ 
DECLARE
	myschema alias for $1;
	mytable alias for $2;
	myrid   alias for $3;
	authid alias for $4;
	expires alias for $5;
	ret int;
	mytoid oid;
	myrec RECORD;
	
BEGIN

	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION 'Long transaction support disabled, use EnableLongTransaction() to enable.';
	END IF;

	EXECUTE 'DELETE FROM authorization_table WHERE expires < now()'; 

	SELECT c.oid INTO mytoid FROM pg_class c, pg_namespace n
		WHERE c.relname = mytable
		AND c.relnamespace = n.oid
		AND n.nspname = myschema;

	-- RAISE NOTICE 'toid: %', mytoid;

	FOR myrec IN SELECT * FROM authorization_table WHERE 
		toid = mytoid AND rid = myrid
	LOOP
		IF myrec.authid != authid THEN
			RETURN 0;
		ELSE
			RETURN 1;
		END IF;
	END LOOP;

	EXECUTE 'INSERT INTO authorization_table VALUES ('||
		quote_literal(mytoid::text)||','||quote_literal(myrid)||
		','||quote_literal(expires::text)||
		','||quote_literal(authid) ||')';

	GET DIAGNOSTICS ret = ROW_COUNT;

	RETURN ret;
END;
$_$;


--
-- Name: longtransactionsenabled(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION longtransactionsenabled() RETURNS boolean
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	rec RECORD;
BEGIN
	FOR rec IN SELECT oid FROM pg_class WHERE relname = 'authorized_tables'
	LOOP
		return 't';
	END LOOP;
	return 'f';
END;
$$;


--
-- Name: maptile_for_point(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION maptile_for_point(bigint, bigint, integer) RETURNS integer
    LANGUAGE c STRICT
    AS '/ws/openstreetmap-website/db/functions/libpgosm', 'maptile_for_point';


--
-- Name: path(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION path(geometry) RETURNS path
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_to_path';


--
-- Name: pgis_geometry_accum_finalfn(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_accum_finalfn(pgis_abs) RETURNS geometry[]
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_accum_finalfn';


--
-- Name: pgis_geometry_accum_transfn(pgis_abs, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_accum_transfn(pgis_abs, geometry) RETURNS pgis_abs
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_accum_transfn';


--
-- Name: pgis_geometry_collect_finalfn(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_collect_finalfn(pgis_abs) RETURNS geometry
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_collect_finalfn';


--
-- Name: pgis_geometry_makeline_finalfn(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_makeline_finalfn(pgis_abs) RETURNS geometry
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_makeline_finalfn';


--
-- Name: pgis_geometry_polygonize_finalfn(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_polygonize_finalfn(pgis_abs) RETURNS geometry
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_polygonize_finalfn';


--
-- Name: pgis_geometry_union_finalfn(pgis_abs); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION pgis_geometry_union_finalfn(pgis_abs) RETURNS geometry
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'pgis_geometry_union_finalfn';


--
-- Name: point(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION point(geometry) RETURNS point
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_to_point';


--
-- Name: polygon(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION polygon(geometry) RETURNS polygon
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_to_polygon';


--
-- Name: populate_geometry_columns(boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION populate_geometry_columns(use_typmod boolean DEFAULT true) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	inserted    integer;
	oldcount    integer;
	probed      integer;
	stale       integer;
	gcs         RECORD;
	gc          RECORD;
	gsrid       integer;
	gndims      integer;
	gtype       text;
	query       text;
	gc_is_valid boolean;

BEGIN
	SELECT count(*) INTO oldcount FROM geometry_columns;
	inserted := 0;

	-- Count the number of geometry columns in all tables and views
	SELECT count(DISTINCT c.oid) INTO probed
	FROM pg_class c,
		 pg_attribute a,
		 pg_type t,
		 pg_namespace n
	WHERE (c.relkind = 'r' OR c.relkind = 'v')
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%' AND c.relname != 'raster_columns' ;

	-- Iterate through all non-dropped geometry columns
	RAISE DEBUG 'Processing Tables.....';

	FOR gcs IN
	SELECT DISTINCT ON (c.oid) c.oid, n.nspname, c.relname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'r'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%' AND c.relname != 'raster_columns' 
	LOOP

		inserted := inserted + populate_geometry_columns(gcs.oid, use_typmod);
	END LOOP;

	IF oldcount > inserted THEN
	    stale = oldcount-inserted;
	ELSE
	    stale = 0;
	END IF;

	RETURN 'probed:' ||probed|| ' inserted:'||inserted;
END

$$;


--
-- Name: populate_geometry_columns(oid, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION populate_geometry_columns(tbl_oid oid, use_typmod boolean DEFAULT true) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	gcs         RECORD;
	gc          RECORD;
	gc_old      RECORD;
	gsrid       integer;
	gndims      integer;
	gtype       text;
	query       text;
	gc_is_valid boolean;
	inserted    integer;
	constraint_successful boolean := false;

BEGIN
	inserted := 0;

	-- Iterate through all geometry columns in this table
	FOR gcs IN
	SELECT n.nspname, c.relname, a.attname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'r'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%'
		AND c.oid = tbl_oid
	LOOP

        RAISE DEBUG 'Processing column %.%.%', gcs.nspname, gcs.relname, gcs.attname;
    
        gc_is_valid := true;
        -- Find the srid, coord_dimension, and type of current geometry
        -- in geometry_columns -- which is now a view
        
        SELECT type, srid, coord_dimension INTO gc_old 
            FROM geometry_columns 
            WHERE f_table_schema = gcs.nspname AND f_table_name = gcs.relname AND f_geometry_column = gcs.attname; 
            
        IF upper(gc_old.type) = 'GEOMETRY' THEN
        -- This is an unconstrained geometry we need to do something
        -- We need to figure out what to set the type by inspecting the data
            EXECUTE 'SELECT st_srid(' || quote_ident(gcs.attname) || ') As srid, GeometryType(' || quote_ident(gcs.attname) || ') As type, ST_NDims(' || quote_ident(gcs.attname) || ') As dims ' ||
                     ' FROM ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || 
                     ' WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1;'
                INTO gc;
            IF gc IS NULL THEN -- there is no data so we can not determine geometry type
            	RAISE WARNING 'No data in table %.%, so no information to determine geometry type and srid', gcs.nspname, gcs.relname;
            	RETURN 0;
            END IF;
            gsrid := gc.srid; gtype := gc.type; gndims := gc.dims;
            	
            IF use_typmod THEN
                BEGIN
                    EXECUTE 'ALTER TABLE ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || ' ALTER COLUMN ' || quote_ident(gcs.attname) || 
                        ' TYPE geometry(' || postgis_type_name(gtype, gndims, true) || ', ' || gsrid::text  || ') ';
                    inserted := inserted + 1;
                EXCEPTION
                        WHEN invalid_parameter_value OR feature_not_supported THEN
                        RAISE WARNING 'Could not convert ''%'' in ''%.%'' to use typmod with srid %, type %: %', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname), gsrid, postgis_type_name(gtype, gndims, true), SQLERRM;
                            gc_is_valid := false;
                END;
                
            ELSE
                -- Try to apply srid check to column
            	constraint_successful = false;
                IF (gsrid > 0 AND postgis_constraint_srid(gcs.nspname, gcs.relname,gcs.attname) IS NULL ) THEN
                    BEGIN
                        EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || 
                                 ' ADD CONSTRAINT ' || quote_ident('enforce_srid_' || gcs.attname) || 
                                 ' CHECK (st_srid(' || quote_ident(gcs.attname) || ') = ' || gsrid || ')';
                        constraint_successful := true;
                    EXCEPTION
                        WHEN check_violation THEN
                            RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not apply constraint CHECK (st_srid(%) = %)', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname), quote_ident(gcs.attname), gsrid;
                            gc_is_valid := false;
                    END;
                END IF;
                
                -- Try to apply ndims check to column
                IF (gndims IS NOT NULL AND postgis_constraint_dims(gcs.nspname, gcs.relname,gcs.attname) IS NULL ) THEN
                    BEGIN
                        EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
                                 ADD CONSTRAINT ' || quote_ident('enforce_dims_' || gcs.attname) || '
                                 CHECK (st_ndims(' || quote_ident(gcs.attname) || ') = '||gndims||')';
                        constraint_successful := true;
                    EXCEPTION
                        WHEN check_violation THEN
                            RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not apply constraint CHECK (st_ndims(%) = %)', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname), quote_ident(gcs.attname), gndims;
                            gc_is_valid := false;
                    END;
                END IF;
    
                -- Try to apply geometrytype check to column
                IF (gtype IS NOT NULL AND postgis_constraint_type(gcs.nspname, gcs.relname,gcs.attname) IS NULL ) THEN
                    BEGIN
                        EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
                        ADD CONSTRAINT ' || quote_ident('enforce_geotype_' || gcs.attname) || '
                        CHECK ((geometrytype(' || quote_ident(gcs.attname) || ') = ' || quote_literal(gtype) || ') OR (' || quote_ident(gcs.attname) || ' IS NULL))';
                        constraint_successful := true;
                    EXCEPTION
                        WHEN check_violation THEN
                            -- No geometry check can be applied. This column contains a number of geometry types.
                            RAISE WARNING 'Could not add geometry type check (%) to table column: %.%.%', gtype, quote_ident(gcs.nspname),quote_ident(gcs.relname),quote_ident(gcs.attname);
                    END;
                END IF;
                 --only count if we were successful in applying at least one constraint
                IF constraint_successful THEN
                	inserted := inserted + 1;
                END IF;
            END IF;	        
	    END IF;

	END LOOP;

	RETURN inserted;
END

$$;


--
-- Name: postgis_addbbox(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_addbbox(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_addBBOX';


--
-- Name: postgis_cache_bbox(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_cache_bbox() RETURNS trigger
    LANGUAGE c
    AS '$libdir/postgis-2.1', 'cache_bbox';


--
-- Name: postgis_constraint_dims(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_constraint_dims(geomschema text, geomtable text, geomcolumn text) RETURNS integer
    LANGUAGE sql STABLE STRICT
    AS $_$
SELECT  replace(split_part(s.consrc, ' = ', 2), ')', '')::integer
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = $1
		 AND c.relname = $2
		 AND a.attname = $3
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%ndims(% = %';
$_$;


--
-- Name: postgis_constraint_srid(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_constraint_srid(geomschema text, geomtable text, geomcolumn text) RETURNS integer
    LANGUAGE sql STABLE STRICT
    AS $_$
SELECT replace(replace(split_part(s.consrc, ' = ', 2), ')', ''), '(', '')::integer
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = $1
		 AND c.relname = $2
		 AND a.attname = $3
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%srid(% = %';
$_$;


--
-- Name: postgis_constraint_type(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_constraint_type(geomschema text, geomtable text, geomcolumn text) RETURNS character varying
    LANGUAGE sql STABLE STRICT
    AS $_$
SELECT  replace(split_part(s.consrc, '''', 2), ')', '')::varchar		
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = $1
		 AND c.relname = $2
		 AND a.attname = $3
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%geometrytype(% = %';
$_$;


--
-- Name: postgis_dropbbox(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_dropbbox(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dropBBOX';


--
-- Name: postgis_full_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_full_version() RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $$
DECLARE
	libver text;
	svnver text;
	projver text;
	geosver text;
	sfcgalver text;
	cgalver text;
	gdalver text;
	libxmlver text;
	dbproc text;
	relproc text;
	fullver text;
	rast_lib_ver text;
	rast_scr_ver text;
	topo_scr_ver text;
	json_lib_ver text;
BEGIN
	SELECT postgis_lib_version() INTO libver;
	SELECT postgis_proj_version() INTO projver;
	SELECT postgis_geos_version() INTO geosver;
	SELECT postgis_libjson_version() INTO json_lib_ver;
	BEGIN
		SELECT postgis_gdal_version() INTO gdalver;
	EXCEPTION
		WHEN undefined_function THEN
			gdalver := NULL;
			RAISE NOTICE 'Function postgis_gdal_version() not found.  Is raster support enabled and rtpostgis.sql installed?';
	END;
	BEGIN
		SELECT postgis_sfcgal_version() INTO sfcgalver;
	EXCEPTION
		WHEN undefined_function THEN
			sfcgalver := NULL;
	END;
	SELECT postgis_libxml_version() INTO libxmlver;
	SELECT postgis_scripts_installed() INTO dbproc;
	SELECT postgis_scripts_released() INTO relproc;
	select postgis_svn_version() INTO svnver;
	BEGIN
		SELECT postgis_topology_scripts_installed() INTO topo_scr_ver;
	EXCEPTION
		WHEN undefined_function THEN
			topo_scr_ver := NULL;
			RAISE NOTICE 'Function postgis_topology_scripts_installed() not found. Is topology support enabled and topology.sql installed?';
	END;

	BEGIN
		SELECT postgis_raster_scripts_installed() INTO rast_scr_ver;
	EXCEPTION
		WHEN undefined_function THEN
			rast_scr_ver := NULL;
			RAISE NOTICE 'Function postgis_raster_scripts_installed() not found. Is raster support enabled and rtpostgis.sql installed?';
	END;

	BEGIN
		SELECT postgis_raster_lib_version() INTO rast_lib_ver;
	EXCEPTION
		WHEN undefined_function THEN
			rast_lib_ver := NULL;
			RAISE NOTICE 'Function postgis_raster_lib_version() not found. Is raster support enabled and rtpostgis.sql installed?';
	END;

	fullver = 'POSTGIS="' || libver;

	IF  svnver IS NOT NULL THEN
		fullver = fullver || ' r' || svnver;
	END IF;

	fullver = fullver || '"';

	IF  geosver IS NOT NULL THEN
		fullver = fullver || ' GEOS="' || geosver || '"';
	END IF;

	IF  sfcgalver IS NOT NULL THEN
		fullver = fullver || ' SFCGAL="' || sfcgalver || '"';
	END IF;

	IF  projver IS NOT NULL THEN
		fullver = fullver || ' PROJ="' || projver || '"';
	END IF;

	IF  gdalver IS NOT NULL THEN
		fullver = fullver || ' GDAL="' || gdalver || '"';
	END IF;

	IF  libxmlver IS NOT NULL THEN
		fullver = fullver || ' LIBXML="' || libxmlver || '"';
	END IF;

	IF json_lib_ver IS NOT NULL THEN
		fullver = fullver || ' LIBJSON="' || json_lib_ver || '"';
	END IF;

	-- fullver = fullver || ' DBPROC="' || dbproc || '"';
	-- fullver = fullver || ' RELPROC="' || relproc || '"';

	IF dbproc != relproc THEN
		fullver = fullver || ' (core procs from "' || dbproc || '" need upgrade)';
	END IF;

	IF topo_scr_ver IS NOT NULL THEN
		fullver = fullver || ' TOPOLOGY';
		IF topo_scr_ver != relproc THEN
			fullver = fullver || ' (topology procs from "' || topo_scr_ver || '" need upgrade)';
		END IF;
	END IF;

	IF rast_lib_ver IS NOT NULL THEN
		fullver = fullver || ' RASTER';
		IF rast_lib_ver != relproc THEN
			fullver = fullver || ' (raster lib from "' || rast_lib_ver || '" need upgrade)';
		END IF;
	END IF;

	IF rast_scr_ver IS NOT NULL AND rast_scr_ver != relproc THEN
		fullver = fullver || ' (raster procs from "' || rast_scr_ver || '" need upgrade)';
	END IF;

	RETURN fullver;
END
$$;


--
-- Name: postgis_geos_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_geos_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_geos_version';


--
-- Name: postgis_getbbox(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_getbbox(geometry) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_BOX2D';


--
-- Name: postgis_hasbbox(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_hasbbox(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_hasBBOX';


--
-- Name: postgis_lib_build_date(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_lib_build_date() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_lib_build_date';


--
-- Name: postgis_lib_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_lib_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_lib_version';


--
-- Name: postgis_libjson_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_libjson_version() RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_libjson_version';


--
-- Name: postgis_libxml_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_libxml_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_libxml_version';


--
-- Name: postgis_noop(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_noop(geometry) RETURNS geometry
    LANGUAGE c STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_noop';


--
-- Name: postgis_proj_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_proj_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_proj_version';


--
-- Name: postgis_scripts_build_date(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_scripts_build_date() RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $$SELECT '2013-09-10 06:33:47'::text AS version$$;


--
-- Name: postgis_scripts_installed(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_scripts_installed() RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $$ SELECT '2.1.0'::text || ' r' || 11822::text AS version $$;


--
-- Name: postgis_scripts_released(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_scripts_released() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_scripts_released';


--
-- Name: postgis_svn_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_svn_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_svn_version';


--
-- Name: postgis_transform_geometry(geometry, text, text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_transform_geometry(geometry, text, text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'transform_geom';


--
-- Name: postgis_type_name(character varying, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_type_name(geomname character varying, coord_dimension integer, use_new_name boolean DEFAULT true) RETURNS character varying
    LANGUAGE sql IMMUTABLE STRICT COST 200
    AS $_$
 SELECT CASE WHEN $3 THEN new_name ELSE old_name END As geomname
 	FROM 
 	( VALUES
 		 ('GEOMETRY', 'Geometry', 2) ,
 		 	('GEOMETRY', 'GeometryZ', 3) ,
 		 	('GEOMETRY', 'GeometryZM', 4) ,
			('GEOMETRYCOLLECTION', 'GeometryCollection', 2) ,
			('GEOMETRYCOLLECTION', 'GeometryCollectionZ', 3) ,
			('GEOMETRYCOLLECTIONM', 'GeometryCollectionM', 3) ,
			('GEOMETRYCOLLECTION', 'GeometryCollectionZM', 4) ,
			
			('POINT', 'Point',2) ,
			('POINTM','PointM',3) ,
			('POINT', 'PointZ',3) ,
			('POINT', 'PointZM',4) ,
			
			('MULTIPOINT','MultiPoint',2) ,
			('MULTIPOINT','MultiPointZ',3) ,
			('MULTIPOINTM','MultiPointM',3) ,
			('MULTIPOINT','MultiPointZM',4) ,
			
			('POLYGON', 'Polygon',2) ,
			('POLYGON', 'PolygonZ',3) ,
			('POLYGONM', 'PolygonM',3) ,
			('POLYGON', 'PolygonZM',4) ,
			
			('MULTIPOLYGON', 'MultiPolygon',2) ,
			('MULTIPOLYGON', 'MultiPolygonZ',3) ,
			('MULTIPOLYGONM', 'MultiPolygonM',3) ,
			('MULTIPOLYGON', 'MultiPolygonZM',4) ,
			
			('MULTILINESTRING', 'MultiLineString',2) ,
			('MULTILINESTRING', 'MultiLineStringZ',3) ,
			('MULTILINESTRINGM', 'MultiLineStringM',3) ,
			('MULTILINESTRING', 'MultiLineStringZM',4) ,
			
			('LINESTRING', 'LineString',2) ,
			('LINESTRING', 'LineStringZ',3) ,
			('LINESTRINGM', 'LineStringM',3) ,
			('LINESTRING', 'LineStringZM',4) ,
			
			('CIRCULARSTRING', 'CircularString',2) ,
			('CIRCULARSTRING', 'CircularStringZ',3) ,
			('CIRCULARSTRINGM', 'CircularStringM',3) ,
			('CIRCULARSTRING', 'CircularStringZM',4) ,
			
			('COMPOUNDCURVE', 'CompoundCurve',2) ,
			('COMPOUNDCURVE', 'CompoundCurveZ',3) ,
			('COMPOUNDCURVEM', 'CompoundCurveM',3) ,
			('COMPOUNDCURVE', 'CompoundCurveZM',4) ,
			
			('CURVEPOLYGON', 'CurvePolygon',2) ,
			('CURVEPOLYGON', 'CurvePolygonZ',3) ,
			('CURVEPOLYGONM', 'CurvePolygonM',3) ,
			('CURVEPOLYGON', 'CurvePolygonZM',4) ,
			
			('MULTICURVE', 'MultiCurve',2 ) ,
			('MULTICURVE', 'MultiCurveZ',3 ) ,
			('MULTICURVEM', 'MultiCurveM',3 ) ,
			('MULTICURVE', 'MultiCurveZM',4 ) ,
			
			('MULTISURFACE', 'MultiSurface', 2) ,
			('MULTISURFACE', 'MultiSurfaceZ', 3) ,
			('MULTISURFACEM', 'MultiSurfaceM', 3) ,
			('MULTISURFACE', 'MultiSurfaceZM', 4) ,
			
			('POLYHEDRALSURFACE', 'PolyhedralSurface',2) ,
			('POLYHEDRALSURFACE', 'PolyhedralSurfaceZ',3) ,
			('POLYHEDRALSURFACEM', 'PolyhedralSurfaceM',3) ,
			('POLYHEDRALSURFACE', 'PolyhedralSurfaceZM',4) ,
			
			('TRIANGLE', 'Triangle',2) ,
			('TRIANGLE', 'TriangleZ',3) ,
			('TRIANGLEM', 'TriangleM',3) ,
			('TRIANGLE', 'TriangleZM',4) ,

			('TIN', 'Tin', 2),
			('TIN', 'TinZ', 3),
			('TIN', 'TinM', 3),
			('TIN', 'TinZM', 4) )
			 As g(old_name, new_name, coord_dimension)
		WHERE (upper(old_name) = upper($1) OR upper(new_name) = upper($1))
			AND coord_dimension = $2;
$_$;


--
-- Name: postgis_typmod_dims(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_typmod_dims(integer) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_typmod_dims';


--
-- Name: postgis_typmod_srid(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_typmod_srid(integer) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_typmod_srid';


--
-- Name: postgis_typmod_type(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_typmod_type(integer) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'postgis_typmod_type';


--
-- Name: postgis_version(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION postgis_version() RETURNS text
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'postgis_version';


--
-- Name: st_3dclosestpoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dclosestpoint(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_closestpoint3d';


--
-- Name: st_3ddfullywithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3ddfullywithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && ST_Expand($2,$3) AND $2 && ST_Expand($1,$3) AND _ST_3DDFullyWithin($1, $2, $3)$_$;


--
-- Name: st_3ddistance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3ddistance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'distance3d';


--
-- Name: st_3ddwithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3ddwithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && ST_Expand($2,$3) AND $2 && ST_Expand($1,$3) AND _ST_3DDWithin($1, $2, $3)$_$;


--
-- Name: st_3dintersects(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dintersects(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_3DIntersects($1, $2)$_$;


--
-- Name: st_3dlength(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dlength(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_length_linestring';


--
-- Name: st_3dlength_spheroid(geometry, spheroid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dlength_spheroid(geometry, spheroid) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_length_ellipsoid_linestring';


--
-- Name: st_3dlongestline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dlongestline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_longestline3d';


--
-- Name: st_3dmakebox(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dmakebox(geom1 geometry, geom2 geometry) RETURNS box3d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_construct';


--
-- Name: st_3dmaxdistance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dmaxdistance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_maxdistance3d';


--
-- Name: st_3dperimeter(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dperimeter(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_perimeter_poly';


--
-- Name: st_3dshortestline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_3dshortestline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_shortestline3d';


--
-- Name: st_addmeasure(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_addmeasure(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_AddMeasure';


--
-- Name: st_addpoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_addpoint(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_addpoint';


--
-- Name: st_addpoint(geometry, geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_addpoint(geom1 geometry, geom2 geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_addpoint';


--
-- Name: st_affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  $2, $3, 0,  $4, $5, 0,  0, 0, 1,  $6, $7, 0)$_$;


--
-- Name: st_affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_affine';


--
-- Name: st_area(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_area(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'area';


--
-- Name: st_area(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_area(text) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_Area($1::geometry);  $_$;


--
-- Name: st_area(geography, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_area(geog geography, use_spheroid boolean DEFAULT true) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_area';


--
-- Name: st_area2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_area2d(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_area_polygon';


--
-- Name: st_asbinary(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asbinary(geography) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asBinary';


--
-- Name: st_asbinary(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asbinary(geometry) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asBinary';


--
-- Name: st_asbinary(geography, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asbinary(geography, text) RETURNS bytea
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsBinary($1::geometry, $2);  $_$;


--
-- Name: st_asbinary(geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asbinary(geometry, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asBinary';


--
-- Name: st_asewkb(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asewkb(geometry) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'WKBFromLWGEOM';


--
-- Name: st_asewkb(geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asewkb(geometry, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'WKBFromLWGEOM';


--
-- Name: st_asewkt(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asewkt(geography) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asEWKT';


--
-- Name: st_asewkt(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asewkt(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asEWKT';


--
-- Name: st_asewkt(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asewkt(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsEWKT($1::geometry);  $_$;


--
-- Name: st_asgeojson(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgeojson(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGeoJson(1, $1::geometry,15,0);  $_$;


--
-- Name: st_asgeojson(geography, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgeojson(geog geography, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGeoJson(1, $1, $2, $3); $_$;


--
-- Name: st_asgeojson(geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgeojson(geom geometry, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGeoJson(1, $1, $2, $3); $_$;


--
-- Name: st_asgeojson(integer, geography, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgeojson(gj_version integer, geog geography, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGeoJson($1, $2, $3, $4); $_$;


--
-- Name: st_asgeojson(integer, geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgeojson(gj_version integer, geom geometry, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGeoJson($1, $2, $3, $4); $_$;


--
-- Name: st_asgml(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgml(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGML(2,$1::geometry,15,0, NULL, NULL);  $_$;


--
-- Name: st_asgml(geography, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgml(geog geography, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, $2, $3, null, null)$_$;


--
-- Name: st_asgml(geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgml(geom geometry, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsGML(2, $1, $2, $3, null, null); $_$;


--
-- Name: st_asgml(integer, geography, integer, integer, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgml(version integer, geog geography, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0, nprefix text DEFAULT NULL::text, id text DEFAULT NULL::text) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT _ST_AsGML($1, $2, $3, $4, $5, $6);$_$;


--
-- Name: st_asgml(integer, geometry, integer, integer, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asgml(version integer, geom geometry, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0, nprefix text DEFAULT NULL::text, id text DEFAULT NULL::text) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT _ST_AsGML($1, $2, $3, $4, $5, $6); $_$;


--
-- Name: st_ashexewkb(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_ashexewkb(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asHEXEWKB';


--
-- Name: st_ashexewkb(geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_ashexewkb(geometry, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asHEXEWKB';


--
-- Name: st_askml(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_askml(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsKML(2, $1::geometry, 15, null);  $_$;


--
-- Name: st_askml(geography, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_askml(geog geography, maxdecimaldigits integer DEFAULT 15) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML(2, $1, $2, null)$_$;


--
-- Name: st_askml(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_askml(geom geometry, maxdecimaldigits integer DEFAULT 15) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_AsKML(2, ST_Transform($1,4326), $2, null); $_$;


--
-- Name: st_askml(integer, geography, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_askml(version integer, geog geography, maxdecimaldigits integer DEFAULT 15, nprefix text DEFAULT NULL::text) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT _ST_AsKML($1, $2, $3, $4)$_$;


--
-- Name: st_askml(integer, geometry, integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_askml(version integer, geom geometry, maxdecimaldigits integer DEFAULT 15, nprefix text DEFAULT NULL::text) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT _ST_AsKML($1, ST_Transform($2,4326), $3, $4); $_$;


--
-- Name: st_aslatlontext(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_aslatlontext(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsLatLonText($1, '') $_$;


--
-- Name: st_aslatlontext(geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_aslatlontext(geometry, text) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_latlon';


--
-- Name: st_assvg(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_assvg(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsSVG($1::geometry,0,15);  $_$;


--
-- Name: st_assvg(geography, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_assvg(geog geography, rel integer DEFAULT 0, maxdecimaldigits integer DEFAULT 15) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_as_svg';


--
-- Name: st_assvg(geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_assvg(geom geometry, rel integer DEFAULT 0, maxdecimaldigits integer DEFAULT 15) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asSVG';


--
-- Name: st_astext(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_astext(geography) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asText';


--
-- Name: st_astext(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_astext(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_asText';


--
-- Name: st_astext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_astext(text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsText($1::geometry);  $_$;


--
-- Name: st_asx3d(geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_asx3d(geom geometry, maxdecimaldigits integer DEFAULT 15, options integer DEFAULT 0) RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT _ST_AsX3D(3,$1,$2,$3,'');$_$;


--
-- Name: st_azimuth(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_azimuth(geog1 geography, geog2 geography) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_azimuth';


--
-- Name: st_azimuth(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_azimuth(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_azimuth';


--
-- Name: st_bdmpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_bdmpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := ST_MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION 'Input is not a MultiLinestring';
	END IF;

	geom := ST_Multi(ST_BuildArea(mline));

	RETURN geom;
END;
$_$;


--
-- Name: st_bdpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_bdpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := ST_MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION 'Input is not a MultiLinestring';
	END IF;

	geom := ST_BuildArea(mline);

	IF GeometryType(geom) != 'POLYGON'
	THEN
		RAISE EXCEPTION 'Input returns more then a single polygon, try using BdMPolyFromText instead';
	END IF;

	RETURN geom;
END;
$_$;


--
-- Name: st_boundary(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_boundary(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'boundary';


--
-- Name: st_box2dfromgeohash(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_box2dfromgeohash(text, integer DEFAULT NULL::integer) RETURNS box2d
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'box2d_from_geohash';


--
-- Name: st_buffer(geography, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buffer(geography, double precision) RETURNS geography
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT geography(ST_Transform(ST_Buffer(ST_Transform(geometry($1), _ST_BestSRID($1)), $2), 4326))$_$;


--
-- Name: st_buffer(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buffer(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'buffer';


--
-- Name: st_buffer(text, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buffer(text, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_Buffer($1::geometry, $2);  $_$;


--
-- Name: st_buffer(geometry, double precision, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buffer(geometry, double precision, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_Buffer($1, $2,
		CAST('quad_segs='||CAST($3 AS text) as cstring))
	   $_$;


--
-- Name: st_buffer(geometry, double precision, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buffer(geometry, double precision, text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _ST_Buffer($1, $2,
		CAST( regexp_replace($3, '^[0123456789]+$',
			'quad_segs='||$3) AS cstring)
		)
	   $_$;


--
-- Name: st_buildarea(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_buildarea(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_BuildArea';


--
-- Name: st_centroid(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_centroid(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'centroid';


--
-- Name: st_cleangeometry(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_cleangeometry(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_CleanGeometry';


--
-- Name: st_closestpoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_closestpoint(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_closestpoint';


--
-- Name: st_collect(geometry[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_collect(geometry[]) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_collect_garray';


--
-- Name: st_collect(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_collect(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'LWGEOM_collect';


--
-- Name: st_collectionextract(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_collectionextract(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_CollectionExtract';


--
-- Name: st_collectionhomogenize(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_collectionhomogenize(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_CollectionHomogenize';


--
-- Name: st_combine_bbox(box2d, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_combine_bbox(box2d, geometry) RETURNS box2d
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'BOX2D_combine';


--
-- Name: st_combine_bbox(box3d, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_combine_bbox(box3d, geometry) RETURNS box3d
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'BOX3D_combine';


--
-- Name: st_concavehull(geometry, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_concavehull(param_geom geometry, param_pctconvex double precision, param_allow_holes boolean DEFAULT false) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
	DECLARE
		var_convhull geometry := ST_ConvexHull(param_geom);
		var_param_geom geometry := param_geom;
		var_initarea float := ST_Area(var_convhull);
		var_newarea float := var_initarea;
		var_div integer := 6; 
		var_tempgeom geometry;
		var_tempgeom2 geometry;
		var_cent geometry;
		var_geoms geometry[4]; 
		var_enline geometry;
		var_resultgeom geometry;
		var_atempgeoms geometry[];
		var_buf float := 1; 
	BEGIN
		-- We start with convex hull as our base
		var_resultgeom := var_convhull;
		
		IF param_pctconvex = 1 THEN
			return var_resultgeom;
		ELSIF ST_GeometryType(var_param_geom) = 'ST_Polygon' THEN -- it is as concave as it is going to get
			IF param_allow_holes THEN -- leave the holes
				RETURN var_param_geom;
			ELSE -- remove the holes
				var_resultgeom := ST_MakePolygon(ST_ExteriorRing(var_param_geom));
				RETURN var_resultgeom;
			END IF;
		END IF;
		IF ST_Dimension(var_resultgeom) > 1 AND param_pctconvex BETWEEN 0 and 0.98 THEN
		-- get linestring that forms envelope of geometry
			var_enline := ST_Boundary(ST_Envelope(var_param_geom));
			var_buf := ST_Length(var_enline)/1000.0;
			IF ST_GeometryType(var_param_geom) = 'ST_MultiPoint' AND ST_NumGeometries(var_param_geom) BETWEEN 4 and 200 THEN
			-- we make polygons out of points since they are easier to cave in. 
			-- Note we limit to between 4 and 200 points because this process is slow and gets quadratically slow
				var_buf := sqrt(ST_Area(var_convhull)*0.8/(ST_NumGeometries(var_param_geom)*ST_NumGeometries(var_param_geom)));
				var_atempgeoms := ARRAY(SELECT geom FROM ST_DumpPoints(var_param_geom));
				-- 5 and 10 and just fudge factors
				var_tempgeom := ST_Union(ARRAY(SELECT geom
						FROM (
						-- fuse near neighbors together
						SELECT DISTINCT ON (i) i,  ST_Distance(var_atempgeoms[i],var_atempgeoms[j]), ST_Buffer(ST_MakeLine(var_atempgeoms[i], var_atempgeoms[j]) , var_buf*5, 'quad_segs=3') As geom
								FROM generate_series(1,array_upper(var_atempgeoms, 1)) As i
									INNER JOIN generate_series(1,array_upper(var_atempgeoms, 1)) As j 
										ON (
								 NOT ST_Intersects(var_atempgeoms[i],var_atempgeoms[j])
									AND ST_DWithin(var_atempgeoms[i],var_atempgeoms[j], var_buf*10)
									)
								UNION ALL
						-- catch the ones with no near neighbors
								SELECT i, 0, ST_Buffer(var_atempgeoms[i] , var_buf*10, 'quad_segs=3') As geom
								FROM generate_series(1,array_upper(var_atempgeoms, 1)) As i
									LEFT JOIN generate_series(ceiling(array_upper(var_atempgeoms,1)/2)::integer,array_upper(var_atempgeoms, 1)) As j 
										ON (
								 NOT ST_Intersects(var_atempgeoms[i],var_atempgeoms[j])
									AND ST_DWithin(var_atempgeoms[i],var_atempgeoms[j], var_buf*10) 
									)
									WHERE j IS NULL
								ORDER BY 1, 2
							) As foo	) );
				IF ST_IsValid(var_tempgeom) AND ST_GeometryType(var_tempgeom) = 'ST_Polygon' THEN
					var_tempgeom := ST_ForceSFS(ST_Intersection(var_tempgeom, var_convhull));
					IF param_allow_holes THEN
						var_param_geom := var_tempgeom;
					ELSE
						var_param_geom := ST_MakePolygon(ST_ExteriorRing(var_tempgeom));
					END IF;
					return var_param_geom;
				ELSIF ST_IsValid(var_tempgeom) THEN
					var_param_geom := ST_ForceSFS(ST_Intersection(var_tempgeom, var_convhull));	
				END IF;
			END IF;

			IF ST_GeometryType(var_param_geom) = 'ST_Polygon' THEN
				IF NOT param_allow_holes THEN
					var_param_geom := ST_MakePolygon(ST_ExteriorRing(var_param_geom));
				END IF;
				return var_param_geom;
			END IF;
            var_cent := ST_Centroid(var_param_geom);
            IF (ST_XMax(var_enline) - ST_XMin(var_enline) ) > var_buf AND (ST_YMax(var_enline) - ST_YMin(var_enline) ) > var_buf THEN
                    IF ST_Dwithin(ST_Centroid(var_convhull) , ST_Centroid(ST_Envelope(var_param_geom)), var_buf/2) THEN
                -- If the geometric dimension is > 1 and the object is symettric (cutting at centroid will not work -- offset a bit)
                        var_cent := ST_Translate(var_cent, (ST_XMax(var_enline) - ST_XMin(var_enline))/1000,  (ST_YMAX(var_enline) - ST_YMin(var_enline))/1000);
                    ELSE
                        -- uses closest point on geometry to centroid. I can't explain why we are doing this
                        var_cent := ST_ClosestPoint(var_param_geom,var_cent);
                    END IF;
                    IF ST_DWithin(var_cent, var_enline,var_buf) THEN
                        var_cent := ST_centroid(ST_Envelope(var_param_geom));
                    END IF;
                    -- break envelope into 4 triangles about the centroid of the geometry and returned the clipped geometry in each quadrant
                    FOR i in 1 .. 4 LOOP
                       var_geoms[i] := ST_MakePolygon(ST_MakeLine(ARRAY[ST_PointN(var_enline,i), ST_PointN(var_enline,i+1), var_cent, ST_PointN(var_enline,i)]));
                       var_geoms[i] := ST_ForceSFS(ST_Intersection(var_param_geom, ST_Buffer(var_geoms[i],var_buf)));
                       IF ST_IsValid(var_geoms[i]) THEN 
                            
                       ELSE
                            var_geoms[i] := ST_BuildArea(ST_MakeLine(ARRAY[ST_PointN(var_enline,i), ST_PointN(var_enline,i+1), var_cent, ST_PointN(var_enline,i)]));
                       END IF; 
                    END LOOP;
                    var_tempgeom := ST_Union(ARRAY[ST_ConvexHull(var_geoms[1]), ST_ConvexHull(var_geoms[2]) , ST_ConvexHull(var_geoms[3]), ST_ConvexHull(var_geoms[4])]); 
                    --RAISE NOTICE 'Curr vex % ', ST_AsText(var_tempgeom);
                    IF ST_Area(var_tempgeom) <= var_newarea AND ST_IsValid(var_tempgeom)  THEN --AND ST_GeometryType(var_tempgeom) ILIKE '%Polygon'
                        
                        var_tempgeom := ST_Buffer(ST_ConcaveHull(var_geoms[1],least(param_pctconvex + param_pctconvex/var_div),true),var_buf, 'quad_segs=2');
                        FOR i IN 1 .. 4 LOOP
                            var_geoms[i] := ST_Buffer(ST_ConcaveHull(var_geoms[i],least(param_pctconvex + param_pctconvex/var_div),true), var_buf, 'quad_segs=2');
                            IF ST_IsValid(var_geoms[i]) Then
                                var_tempgeom := ST_Union(var_tempgeom, var_geoms[i]);
                            ELSE
                                RAISE NOTICE 'Not valid % %', i, ST_AsText(var_tempgeom);
                                var_tempgeom := ST_Union(var_tempgeom, ST_ConvexHull(var_geoms[i]));
                            END IF; 
                        END LOOP;

                        --RAISE NOTICE 'Curr concave % ', ST_AsText(var_tempgeom);
                        IF ST_IsValid(var_tempgeom) THEN
                            var_resultgeom := var_tempgeom;
                        END IF;
                        var_newarea := ST_Area(var_resultgeom);
                    ELSIF ST_IsValid(var_tempgeom) THEN
                        var_resultgeom := var_tempgeom;
                    END IF;

                    IF ST_NumGeometries(var_resultgeom) > 1  THEN
                        var_tempgeom := _ST_ConcaveHull(var_resultgeom);
                        IF ST_IsValid(var_tempgeom) AND ST_GeometryType(var_tempgeom) ILIKE 'ST_Polygon' THEN
                            var_resultgeom := var_tempgeom;
                        ELSE
                            var_resultgeom := ST_Buffer(var_tempgeom,var_buf, 'quad_segs=2');
                        END IF;
                    END IF;
                    IF param_allow_holes = false THEN 
                    -- only keep exterior ring since we do not want holes
                        var_resultgeom := ST_MakePolygon(ST_ExteriorRing(var_resultgeom));
                    END IF;
                ELSE
                    var_resultgeom := ST_Buffer(var_resultgeom,var_buf);
                END IF;
                var_resultgeom := ST_ForceSFS(ST_Intersection(var_resultgeom, ST_ConvexHull(var_param_geom)));
            ELSE
                -- dimensions are too small to cut
                var_resultgeom := _ST_ConcaveHull(var_param_geom);
            END IF;
            RETURN var_resultgeom;
	END;
$$;


--
-- Name: st_contains(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_contains(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Contains($1,$2)$_$;


--
-- Name: st_containsproperly(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_containsproperly(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_ContainsProperly($1,$2)$_$;


--
-- Name: st_convexhull(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_convexhull(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'convexhull';


--
-- Name: st_coorddim(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_coorddim(geometry geometry) RETURNS smallint
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_ndims';


--
-- Name: st_coveredby(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_coveredby(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_CoveredBy($1,$2)$_$;


--
-- Name: st_coveredby(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_coveredby(geography, geography) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Covers($2, $1)$_$;


--
-- Name: st_coveredby(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_coveredby(text, text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ST_CoveredBy($1::geometry, $2::geometry);  $_$;


--
-- Name: st_covers(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_covers(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Covers($1,$2)$_$;


--
-- Name: st_covers(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_covers(geography, geography) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Covers($1, $2)$_$;


--
-- Name: st_covers(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_covers(text, text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ST_Covers($1::geometry, $2::geometry);  $_$;


--
-- Name: st_crosses(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_crosses(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Crosses($1,$2)$_$;


--
-- Name: st_curvetoline(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_curvetoline(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_CurveToLine($1, 32)$_$;


--
-- Name: st_curvetoline(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_curvetoline(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_curve_segmentize';


--
-- Name: st_delaunaytriangles(geometry, double precision, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_delaunaytriangles(g1 geometry, tolerance double precision DEFAULT 0.0, flags integer DEFAULT 0) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_DelaunayTriangles';


--
-- Name: st_dfullywithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dfullywithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && ST_Expand($2,$3) AND $2 && ST_Expand($1,$3) AND _ST_DFullyWithin(ST_ConvexHull($1), ST_ConvexHull($2), $3)$_$;


--
-- Name: st_difference(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_difference(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'difference';


--
-- Name: st_dimension(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dimension(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dimension';


--
-- Name: st_disjoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_disjoint(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'disjoint';


--
-- Name: st_distance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'distance';


--
-- Name: st_distance(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance(geography, geography) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_Distance($1, $2, 0.0, true)$_$;


--
-- Name: st_distance(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance(text, text) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_Distance($1::geometry, $2::geometry);  $_$;


--
-- Name: st_distance(geography, geography, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance(geography, geography, boolean) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_Distance($1, $2, 0.0, $3)$_$;


--
-- Name: st_distance_sphere(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance_sphere(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT COST 300
    AS $_$
	select st_distance(geography($1),geography($2),false)
	$_$;


--
-- Name: st_distance_spheroid(geometry, geometry, spheroid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_distance_spheroid(geom1 geometry, geom2 geometry, spheroid) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_distance_ellipsoid';


--
-- Name: st_dump(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dump(geometry) RETURNS SETOF geometry_dump
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dump';


--
-- Name: st_dumppoints(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dumppoints(geometry) RETURNS SETOF geometry_dump
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dumppoints';


--
-- Name: st_dumprings(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dumprings(geometry) RETURNS SETOF geometry_dump
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_dump_rings';


--
-- Name: st_dwithin(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dwithin(geom1 geometry, geom2 geometry, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && ST_Expand($2,$3) AND $2 && ST_Expand($1,$3) AND _ST_DWithin($1, $2, $3)$_$;


--
-- Name: st_dwithin(geography, geography, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dwithin(geography, geography, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && _ST_Expand($2,$3) AND $2 && _ST_Expand($1,$3) AND _ST_DWithin($1, $2, $3, true)$_$;


--
-- Name: st_dwithin(text, text, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dwithin(text, text, double precision) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ST_DWithin($1::geometry, $2::geometry, $3);  $_$;


--
-- Name: st_dwithin(geography, geography, double precision, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_dwithin(geography, geography, double precision, boolean) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && _ST_Expand($2,$3) AND $2 && _ST_Expand($1,$3) AND _ST_DWithin($1, $2, $3, $4)$_$;


--
-- Name: st_endpoint(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_endpoint(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_endpoint_linestring';


--
-- Name: st_envelope(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_envelope(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_envelope';


--
-- Name: st_equals(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_equals(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 ~= $2 AND _ST_Equals($1,$2)$_$;


--
-- Name: st_estimated_extent(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_estimated_extent(text, text) RETURNS box2d
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Estimated_Extent', 'ST_EstimatedExtent', '2.1.0');
    -- We use security invoker instead of security definer 
    -- to prevent malicious injection of a same named different function
    -- that would be run under elevated permissions
    SELECT ST_EstimatedExtent($1, $2);
  $_$;


--
-- Name: st_estimated_extent(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_estimated_extent(text, text, text) RETURNS box2d
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Estimated_Extent', 'ST_EstimatedExtent', '2.1.0');
    -- We use security invoker instead of security definer 
    -- to prevent malicious injection of a different same named function
    SELECT ST_EstimatedExtent($1, $2, $3);
  $_$;


--
-- Name: st_estimatedextent(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_estimatedextent(text, text) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT SECURITY DEFINER
    AS '$libdir/postgis-2.1', 'gserialized_estimated_extent';


--
-- Name: st_estimatedextent(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_estimatedextent(text, text, text) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT SECURITY DEFINER
    AS '$libdir/postgis-2.1', 'gserialized_estimated_extent';


--
-- Name: st_expand(box2d, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_expand(box2d, double precision) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_expand';


--
-- Name: st_expand(box3d, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_expand(box3d, double precision) RETURNS box3d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_expand';


--
-- Name: st_expand(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_expand(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_expand';


--
-- Name: st_exteriorring(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_exteriorring(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_exteriorring_polygon';


--
-- Name: st_find_extent(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_find_extent(text, text) RETURNS box2d
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	tablename alias for $1;
	columnname alias for $2;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE 'SELECT ST_Extent("' || columnname || '") As extent FROM "' || tablename || '"' LOOP
		return myrec.extent;
	END LOOP;
END;
$_$;


--
-- Name: st_find_extent(text, text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_find_extent(text, text, text) RETURNS box2d
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	schemaname alias for $1;
	tablename alias for $2;
	columnname alias for $3;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE 'SELECT ST_Extent("' || columnname || '") As extent FROM "' || schemaname || '"."' || tablename || '"' LOOP
		return myrec.extent;
	END LOOP;
END;
$_$;


--
-- Name: st_flipcoordinates(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_flipcoordinates(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_FlipCoordinates';


--
-- Name: st_force2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force2d(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_2d';


--
-- Name: st_force3d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force3d(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_3dz';


--
-- Name: st_force3dm(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force3dm(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_3dm';


--
-- Name: st_force3dz(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force3dz(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_3dz';


--
-- Name: st_force4d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force4d(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_4d';


--
-- Name: st_force_2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_2d(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_2d', 'ST_Force2D', '2.1.0');
    SELECT ST_Force2D($1);
  $_$;


--
-- Name: st_force_3d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_3d(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_3d', 'ST_Force3D', '2.1.0');
    SELECT ST_Force3D($1);
  $_$;


--
-- Name: st_force_3dm(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_3dm(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_3dm', 'ST_Force3DM', '2.1.0');
    SELECT ST_Force3DM($1);
  $_$;


--
-- Name: st_force_3dz(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_3dz(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_3dz', 'ST_Force3DZ', '2.1.0');
    SELECT ST_Force3DZ($1);
  $_$;


--
-- Name: st_force_4d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_4d(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_4d', 'ST_Force4D', '2.1.0');
    SELECT ST_Force4D($1);
  $_$;


--
-- Name: st_force_collection(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_force_collection(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Force_Collection', 'ST_ForceCollection', '2.1.0');
    SELECT ST_ForceCollection($1);
  $_$;


--
-- Name: st_forcecollection(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_forcecollection(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_collection';


--
-- Name: st_forcerhr(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_forcerhr(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_clockwise_poly';


--
-- Name: st_forcesfs(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_forcesfs(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_sfs';


--
-- Name: st_forcesfs(geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_forcesfs(geometry, version text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_sfs';


--
-- Name: st_geogfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geogfromtext(text) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_from_text';


--
-- Name: st_geogfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geogfromwkb(bytea) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_from_binary';


--
-- Name: st_geographyfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geographyfromtext(text) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geography_from_text';


--
-- Name: st_geohash(geography, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geohash(geog geography, maxchars integer DEFAULT 0) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_GeoHash';


--
-- Name: st_geohash(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geohash(geom geometry, maxchars integer DEFAULT 0) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_GeoHash';


--
-- Name: st_geomcollfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomcollfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(ST_GeomFromText($1)) = 'GEOMETRYCOLLECTION'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_geomcollfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomcollfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(ST_GeomFromText($1, $2)) = 'GEOMETRYCOLLECTION'
	THEN ST_GeomFromText($1,$2)
	ELSE NULL END
	$_$;


--
-- Name: st_geomcollfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomcollfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(ST_GeomFromWKB($1)) = 'GEOMETRYCOLLECTION'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_geomcollfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomcollfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'GEOMETRYCOLLECTION'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_geometryfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geometryfromtext(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_text';


--
-- Name: st_geometryfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geometryfromtext(text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_text';


--
-- Name: st_geometryn(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geometryn(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_geometryn_collection';


--
-- Name: st_geometrytype(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geometrytype(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geometry_geometrytype';


--
-- Name: st_geomfromewkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromewkb(bytea) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOMFromWKB';


--
-- Name: st_geomfromewkt(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromewkt(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'parse_WKT_lwgeom';


--
-- Name: st_geomfromgeohash(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromgeohash(text, integer DEFAULT NULL::integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT CAST(ST_Box2dFromGeoHash($1, $2) AS geometry); $_$;


--
-- Name: st_geomfromgeojson(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromgeojson(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geom_from_geojson';


--
-- Name: st_geomfromgml(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromgml(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_GeomFromGML($1, 0)$_$;


--
-- Name: st_geomfromgml(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromgml(text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geom_from_gml';


--
-- Name: st_geomfromkml(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromkml(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geom_from_kml';


--
-- Name: st_geomfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromtext(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_text';


--
-- Name: st_geomfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromtext(text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_text';


--
-- Name: st_geomfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromwkb(bytea) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_WKB';


--
-- Name: st_geomfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_geomfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_SetSRID(ST_GeomFromWKB($1), $2)$_$;


--
-- Name: st_gmltosql(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_gmltosql(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_GeomFromGML($1, 0)$_$;


--
-- Name: st_gmltosql(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_gmltosql(text, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geom_from_gml';


--
-- Name: st_hasarc(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_hasarc(geometry geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_has_arc';


--
-- Name: st_hausdorffdistance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_hausdorffdistance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'hausdorffdistance';


--
-- Name: st_hausdorffdistance(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_hausdorffdistance(geom1 geometry, geom2 geometry, double precision) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'hausdorffdistancedensify';


--
-- Name: st_interiorringn(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_interiorringn(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_interiorringn_polygon';


--
-- Name: st_interpolatepoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_interpolatepoint(line geometry, point geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_InterpolatePoint';


--
-- Name: st_intersection(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersection(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'intersection';


--
-- Name: st_intersection(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersection(geography, geography) RETURNS geography
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT geography(ST_Transform(ST_Intersection(ST_Transform(geometry($1), _ST_BestSRID($1, $2)), ST_Transform(geometry($2), _ST_BestSRID($1, $2))), 4326))$_$;


--
-- Name: st_intersection(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersection(text, text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_Intersection($1::geometry, $2::geometry);  $_$;


--
-- Name: st_intersects(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersects(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Intersects($1,$2)$_$;


--
-- Name: st_intersects(geography, geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersects(geography, geography) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Distance($1, $2, 0.0, false) < 0.00001$_$;


--
-- Name: st_intersects(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_intersects(text, text) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ST_Intersects($1::geometry, $2::geometry);  $_$;


--
-- Name: st_isclosed(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isclosed(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_isclosed';


--
-- Name: st_iscollection(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_iscollection(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_IsCollection';


--
-- Name: st_isempty(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isempty(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_isempty';


--
-- Name: st_isring(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isring(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'isring';


--
-- Name: st_issimple(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_issimple(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'issimple';


--
-- Name: st_isvalid(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvalid(geometry) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'isvalid';


--
-- Name: st_isvalid(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvalid(geometry, integer) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT (ST_isValidDetail($1, $2)).valid$_$;


--
-- Name: st_isvaliddetail(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvaliddetail(geometry) RETURNS valid_detail
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'isvaliddetail';


--
-- Name: st_isvaliddetail(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvaliddetail(geometry, integer) RETURNS valid_detail
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'isvaliddetail';


--
-- Name: st_isvalidreason(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvalidreason(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'isvalidreason';


--
-- Name: st_isvalidreason(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_isvalidreason(geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
SELECT CASE WHEN valid THEN 'Valid Geometry' ELSE reason END FROM (
	SELECT (ST_isValidDetail($1, $2)).*
) foo
	$_$;


--
-- Name: st_length(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_length2d_linestring';


--
-- Name: st_length(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length(text) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_Length($1::geometry);  $_$;


--
-- Name: st_length(geography, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length(geog geography, use_spheroid boolean DEFAULT true) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_length';


--
-- Name: st_length2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length2d(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_length2d_linestring';


--
-- Name: st_length2d_spheroid(geometry, spheroid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length2d_spheroid(geometry, spheroid) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_length2d_ellipsoid';


--
-- Name: st_length_spheroid(geometry, spheroid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_length_spheroid(geometry, spheroid) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'LWGEOM_length_ellipsoid_linestring';


--
-- Name: st_line_interpolate_point(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_line_interpolate_point(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Line_Interpolate_Point', 'ST_LineInterpolatePoint', '2.1.0');
    SELECT ST_LineInterpolatePoint($1, $2);
  $_$;


--
-- Name: st_line_locate_point(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_line_locate_point(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Line_Locate_Point', 'ST_LineLocatePoint', '2.1.0');
     SELECT ST_LineLocatePoint($1, $2);
  $_$;


--
-- Name: st_line_substring(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_line_substring(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT _postgis_deprecate('ST_Line_Substring', 'ST_LineSubstring', '2.1.0');
     SELECT ST_LineSubstring($1, $2, $3);
  $_$;


--
-- Name: st_linecrossingdirection(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linecrossingdirection(geom1 geometry, geom2 geometry) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT CASE WHEN NOT $1 && $2 THEN 0 ELSE _ST_LineCrossingDirection($1,$2) END $_$;


--
-- Name: st_linefrommultipoint(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linefrommultipoint(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_line_from_mpoint';


--
-- Name: st_linefromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linefromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'LINESTRING'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_linefromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linefromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1, $2)) = 'LINESTRING'
	THEN ST_GeomFromText($1,$2)
	ELSE NULL END
	$_$;


--
-- Name: st_linefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'LINESTRING'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_linefromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linefromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'LINESTRING'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_lineinterpolatepoint(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_lineinterpolatepoint(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_line_interpolate_point';


--
-- Name: st_linelocatepoint(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linelocatepoint(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_line_locate_point';


--
-- Name: st_linemerge(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linemerge(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'linemerge';


--
-- Name: st_linestringfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linestringfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'LINESTRING'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_linestringfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linestringfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'LINESTRING'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_linesubstring(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linesubstring(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_line_substring';


--
-- Name: st_linetocurve(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_linetocurve(geometry geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_line_desegmentize';


--
-- Name: st_locate_along_measure(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_locate_along_measure(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_locate_between_measures($1, $2, $2) $_$;


--
-- Name: st_locate_between_measures(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_locate_between_measures(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_locate_between_m';


--
-- Name: st_locatealong(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_locatealong(geometry geometry, measure double precision, leftrightoffset double precision DEFAULT 0.0) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_LocateAlong';


--
-- Name: st_locatebetween(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_locatebetween(geometry geometry, frommeasure double precision, tomeasure double precision, leftrightoffset double precision DEFAULT 0.0) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_LocateBetween';


--
-- Name: st_locatebetweenelevations(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_locatebetweenelevations(geometry geometry, fromelevation double precision, toelevation double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_LocateBetweenElevations';


--
-- Name: st_longestline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_longestline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_LongestLine(ST_ConvexHull($1), ST_ConvexHull($2))$_$;


--
-- Name: st_m(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_m(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_m_point';


--
-- Name: st_makebox2d(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makebox2d(geom1 geometry, geom2 geometry) RETURNS box2d
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX2D_construct';


--
-- Name: st_makeenvelope(double precision, double precision, double precision, double precision, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makeenvelope(double precision, double precision, double precision, double precision, integer DEFAULT 0) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_MakeEnvelope';


--
-- Name: st_makeline(geometry[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makeline(geometry[]) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makeline_garray';


--
-- Name: st_makeline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makeline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makeline';


--
-- Name: st_makepoint(double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepoint(double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoint';


--
-- Name: st_makepoint(double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepoint(double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoint';


--
-- Name: st_makepoint(double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepoint(double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoint';


--
-- Name: st_makepointm(double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepointm(double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoint3dm';


--
-- Name: st_makepolygon(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepolygon(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoly';


--
-- Name: st_makepolygon(geometry, geometry[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makepolygon(geometry, geometry[]) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoly';


--
-- Name: st_makevalid(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_makevalid(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_MakeValid';


--
-- Name: st_maxdistance(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_maxdistance(geom1 geometry, geom2 geometry) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_MaxDistance(ST_ConvexHull($1), ST_ConvexHull($2))$_$;


--
-- Name: st_mem_size(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mem_size(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_mem_size';


--
-- Name: st_minimumboundingcircle(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_minimumboundingcircle(inputgeom geometry, segs_per_quarter integer DEFAULT 48) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $$
	DECLARE
	hull GEOMETRY;
	ring GEOMETRY;
	center GEOMETRY;
	radius DOUBLE PRECISION;
	dist DOUBLE PRECISION;
	d DOUBLE PRECISION;
	idx1 integer;
	idx2 integer;
	l1 GEOMETRY;
	l2 GEOMETRY;
	p1 GEOMETRY;
	p2 GEOMETRY;
	a1 DOUBLE PRECISION;
	a2 DOUBLE PRECISION;


	BEGIN

	-- First compute the ConvexHull of the geometry
	hull = ST_ConvexHull(inputgeom);
	--A point really has no MBC
	IF ST_GeometryType(hull) = 'ST_Point' THEN
		RETURN hull;
	END IF;
	-- convert the hull perimeter to a linestring so we can manipulate individual points
	--If its already a linestring force it to a closed linestring
	ring = CASE WHEN ST_GeometryType(hull) = 'ST_LineString' THEN ST_AddPoint(hull, ST_StartPoint(hull)) ELSE ST_ExteriorRing(hull) END;

	dist = 0;
	-- Brute Force - check every pair
	FOR i in 1 .. (ST_NumPoints(ring)-2)
		LOOP
			FOR j in i .. (ST_NumPoints(ring)-1)
				LOOP
				d = ST_Distance(ST_PointN(ring,i),ST_PointN(ring,j));
				-- Check the distance and update if larger
				IF (d > dist) THEN
					dist = d;
					idx1 = i;
					idx2 = j;
				END IF;
			END LOOP;
		END LOOP;

	-- We now have the diameter of the convex hull.  The following line returns it if desired.
	-- RETURN ST_MakeLine(ST_PointN(ring,idx1),ST_PointN(ring,idx2));

	-- Now for the Minimum Bounding Circle.  Since we know the two points furthest from each
	-- other, the MBC must go through those two points. Start with those points as a diameter of a circle.

	-- The radius is half the distance between them and the center is midway between them
	radius = ST_Distance(ST_PointN(ring,idx1),ST_PointN(ring,idx2)) / 2.0;
	center = ST_LineInterpolatePoint(ST_MakeLine(ST_PointN(ring,idx1),ST_PointN(ring,idx2)),0.5);

	-- Loop through each vertex and check if the distance from the center to the point
	-- is greater than the current radius.
	FOR k in 1 .. (ST_NumPoints(ring)-1)
		LOOP
		IF(k <> idx1 and k <> idx2) THEN
			dist = ST_Distance(center,ST_PointN(ring,k));
			IF (dist > radius) THEN
				-- We have to expand the circle.  The new circle must pass trhough
				-- three points - the two original diameters and this point.

				-- Draw a line from the first diameter to this point
				l1 = ST_Makeline(ST_PointN(ring,idx1),ST_PointN(ring,k));
				-- Compute the midpoint
				p1 = ST_LineInterpolatePoint(l1,0.5);
				-- Rotate the line 90 degrees around the midpoint (perpendicular bisector)
				l1 = ST_Rotate(l1,pi()/2,p1);
				--  Compute the azimuth of the bisector
				a1 = ST_Azimuth(ST_PointN(l1,1),ST_PointN(l1,2));
				--  Extend the line in each direction the new computed distance to insure they will intersect
				l1 = ST_AddPoint(l1,ST_Makepoint(ST_X(ST_PointN(l1,2))+sin(a1)*dist,ST_Y(ST_PointN(l1,2))+cos(a1)*dist),-1);
				l1 = ST_AddPoint(l1,ST_Makepoint(ST_X(ST_PointN(l1,1))-sin(a1)*dist,ST_Y(ST_PointN(l1,1))-cos(a1)*dist),0);

				-- Repeat for the line from the point to the other diameter point
				l2 = ST_Makeline(ST_PointN(ring,idx2),ST_PointN(ring,k));
				p2 = ST_LineInterpolatePoint(l2,0.5);
				l2 = ST_Rotate(l2,pi()/2,p2);
				a2 = ST_Azimuth(ST_PointN(l2,1),ST_PointN(l2,2));
				l2 = ST_AddPoint(l2,ST_Makepoint(ST_X(ST_PointN(l2,2))+sin(a2)*dist,ST_Y(ST_PointN(l2,2))+cos(a2)*dist),-1);
				l2 = ST_AddPoint(l2,ST_Makepoint(ST_X(ST_PointN(l2,1))-sin(a2)*dist,ST_Y(ST_PointN(l2,1))-cos(a2)*dist),0);

				-- The new center is the intersection of the two bisectors
				center = ST_Intersection(l1,l2);
				-- The new radius is the distance to any of the three points
				radius = ST_Distance(center,ST_PointN(ring,idx1));
			END IF;
		END IF;
		END LOOP;
	--DONE!!  Return the MBC via the buffer command
	RETURN ST_Buffer(center,radius,segs_per_quarter);

	END;
$$;


--
-- Name: st_mlinefromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mlinefromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'MULTILINESTRING'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mlinefromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mlinefromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(ST_GeomFromText($1, $2)) = 'MULTILINESTRING'
	THEN ST_GeomFromText($1,$2)
	ELSE NULL END
	$_$;


--
-- Name: st_mlinefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mlinefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTILINESTRING'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mlinefromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mlinefromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'MULTILINESTRING'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_mpointfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'MULTIPOINT'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mpointfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpointfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1, $2)) = 'MULTIPOINT'
	THEN ST_GeomFromText($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_mpointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTIPOINT'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mpointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'MULTIPOINT'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_mpolyfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpolyfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'MULTIPOLYGON'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1, $2)) = 'MULTIPOLYGON'
	THEN ST_GeomFromText($1,$2)
	ELSE NULL END
	$_$;


--
-- Name: st_mpolyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpolyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTIPOLYGON'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_mpolyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_mpolyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'MULTIPOLYGON'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_multi(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multi(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_force_multi';


--
-- Name: st_multilinefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multilinefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTILINESTRING'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_multilinestringfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multilinestringfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MLineFromText($1)$_$;


--
-- Name: st_multilinestringfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multilinestringfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MLineFromText($1, $2)$_$;


--
-- Name: st_multipointfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MPointFromText($1)$_$;


--
-- Name: st_multipointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTIPOINT'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_multipointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1,$2)) = 'MULTIPOINT'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_multipolyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipolyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'MULTIPOLYGON'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_multipolyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipolyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'MULTIPOLYGON'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_multipolygonfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipolygonfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MPolyFromText($1)$_$;


--
-- Name: st_multipolygonfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_multipolygonfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MPolyFromText($1, $2)$_$;


--
-- Name: st_ndims(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_ndims(geometry) RETURNS smallint
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_ndims';


--
-- Name: st_node(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_node(g geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_Node';


--
-- Name: st_npoints(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_npoints(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_npoints';


--
-- Name: st_nrings(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_nrings(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_nrings';


--
-- Name: st_numgeometries(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_numgeometries(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_numgeometries_collection';


--
-- Name: st_numinteriorring(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_numinteriorring(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_numinteriorrings_polygon';


--
-- Name: st_numinteriorrings(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_numinteriorrings(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_numinteriorrings_polygon';


--
-- Name: st_numpatches(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_numpatches(geometry) RETURNS integer
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN ST_GeometryType($1) = 'ST_PolyhedralSurface'
	THEN ST_NumGeometries($1)
	ELSE NULL END
	$_$;


--
-- Name: st_numpoints(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_numpoints(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_numpoints_linestring';


--
-- Name: st_offsetcurve(geometry, double precision, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_offsetcurve(line geometry, distance double precision, params text DEFAULT ''::text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_OffsetCurve';


--
-- Name: st_orderingequals(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_orderingequals(geometrya geometry, geometryb geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ 
	SELECT $1 ~= $2 AND _ST_OrderingEquals($1, $2)
	$_$;


--
-- Name: st_overlaps(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_overlaps(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Overlaps($1,$2)$_$;


--
-- Name: st_patchn(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_patchn(geometry, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN ST_GeometryType($1) = 'ST_PolyhedralSurface'
	THEN ST_GeometryN($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_perimeter(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_perimeter(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_perimeter2d_poly';


--
-- Name: st_perimeter(geography, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_perimeter(geog geography, use_spheroid boolean DEFAULT true) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_perimeter';


--
-- Name: st_perimeter2d(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_perimeter2d(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_perimeter2d_poly';


--
-- Name: st_point(double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_point(double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_makepoint';


--
-- Name: st_point_inside_circle(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_point_inside_circle(geometry, double precision, double precision, double precision) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_inside_circle_point';


--
-- Name: st_pointfromgeohash(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointfromgeohash(text, integer DEFAULT NULL::integer) RETURNS geometry
    LANGUAGE c IMMUTABLE
    AS '$libdir/postgis-2.1', 'point_from_geohash';


--
-- Name: st_pointfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'POINT'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_pointfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1, $2)) = 'POINT'
	THEN ST_GeomFromText($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_pointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'POINT'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_pointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'POINT'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_pointn(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointn(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_pointn_linestring';


--
-- Name: st_pointonsurface(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_pointonsurface(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'pointonsurface';


--
-- Name: st_polyfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polyfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1)) = 'POLYGON'
	THEN ST_GeomFromText($1)
	ELSE NULL END
	$_$;


--
-- Name: st_polyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polyfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromText($1, $2)) = 'POLYGON'
	THEN ST_GeomFromText($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_polyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'POLYGON'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_polyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1, $2)) = 'POLYGON'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_polygon(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygon(geometry, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ 
	SELECT ST_SetSRID(ST_MakePolygon($1), $2)
	$_$;


--
-- Name: st_polygonfromtext(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygonfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_PolyFromText($1)$_$;


--
-- Name: st_polygonfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygonfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_PolyFromText($1, $2)$_$;


--
-- Name: st_polygonfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygonfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1)) = 'POLYGON'
	THEN ST_GeomFromWKB($1)
	ELSE NULL END
	$_$;


--
-- Name: st_polygonfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygonfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(ST_GeomFromWKB($1,$2)) = 'POLYGON'
	THEN ST_GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


--
-- Name: st_polygonize(geometry[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_polygonize(geometry[]) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'polygonize_garray';


--
-- Name: st_project(geography, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_project(geog geography, distance double precision, azimuth double precision) RETURNS geography
    LANGUAGE c IMMUTABLE COST 100
    AS '$libdir/postgis-2.1', 'geography_project';


--
-- Name: st_relate(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_relate(geom1 geometry, geom2 geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'relate_full';


--
-- Name: st_relate(geometry, geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_relate(geom1 geometry, geom2 geometry, integer) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'relate_full';


--
-- Name: st_relate(geometry, geometry, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_relate(geom1 geometry, geom2 geometry, text) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'relate_pattern';


--
-- Name: st_relatematch(text, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_relatematch(text, text) RETURNS boolean
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_RelateMatch';


--
-- Name: st_removepoint(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_removepoint(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_removepoint';


--
-- Name: st_removerepeatedpoints(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_removerepeatedpoints(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_RemoveRepeatedPoints';


--
-- Name: st_reverse(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_reverse(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_reverse';


--
-- Name: st_rotate(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotate(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  cos($2), -sin($2), 0,  sin($2), cos($2), 0,  0, 0, 1,  0, 0, 0)$_$;


--
-- Name: st_rotate(geometry, double precision, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotate(geometry, double precision, geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  cos($2), -sin($2), 0,  sin($2),  cos($2), 0, 0, 0, 1, ST_X($3) - cos($2) * ST_X($3) + sin($2) * ST_Y($3), ST_Y($3) - sin($2) * ST_X($3) - cos($2) * ST_Y($3), 0)$_$;


--
-- Name: st_rotate(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotate(geometry, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  cos($2), -sin($2), 0,  sin($2),  cos($2), 0, 0, 0, 1,	$3 - cos($2) * $3 + sin($2) * $4, $4 - sin($2) * $3 - cos($2) * $4, 0)$_$;


--
-- Name: st_rotatex(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotatex(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1, 1, 0, 0, 0, cos($2), -sin($2), 0, sin($2), cos($2), 0, 0, 0)$_$;


--
-- Name: st_rotatey(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotatey(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  cos($2), 0, sin($2),  0, 1, 0,  -sin($2), 0, cos($2), 0,  0, 0)$_$;


--
-- Name: st_rotatez(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_rotatez(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Rotate($1, $2)$_$;


--
-- Name: st_scale(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_scale(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Scale($1, $2, $3, 1)$_$;


--
-- Name: st_scale(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_scale(geometry, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  $2, 0, 0,  0, $3, 0,  0, 0, $4,  0, 0, 0)$_$;


--
-- Name: st_segmentize(geography, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_segmentize(geog geography, max_segment_length double precision) RETURNS geography
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'geography_segmentize';


--
-- Name: st_segmentize(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_segmentize(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_segmentize2d';


--
-- Name: st_setpoint(geometry, integer, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_setpoint(geometry, integer, geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_setpoint_linestring';


--
-- Name: st_setsrid(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_setsrid(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_set_srid';


--
-- Name: st_sharedpaths(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_sharedpaths(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_SharedPaths';


--
-- Name: st_shift_longitude(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_shift_longitude(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_longitude_shift';


--
-- Name: st_shortestline(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_shortestline(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_shortestline2d';


--
-- Name: st_simplify(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_simplify(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_simplify2d';


--
-- Name: st_simplifypreservetopology(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_simplifypreservetopology(geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'topologypreservesimplify';


--
-- Name: st_snap(geometry, geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_snap(geom1 geometry, geom2 geometry, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_Snap';


--
-- Name: st_snaptogrid(geometry, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_snaptogrid(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_SnapToGrid($1, 0, 0, $2, $2)$_$;


--
-- Name: st_snaptogrid(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_snaptogrid(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_SnapToGrid($1, 0, 0, $2, $3)$_$;


--
-- Name: st_snaptogrid(geometry, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_snaptogrid(geometry, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_snaptogrid';


--
-- Name: st_snaptogrid(geometry, geometry, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_snaptogrid(geom1 geometry, geom2 geometry, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_snaptogrid_pointoff';


--
-- Name: st_split(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_split(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT COST 100
    AS '$libdir/postgis-2.1', 'ST_Split';


--
-- Name: st_srid(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_srid(geometry) RETURNS integer
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_get_srid';


--
-- Name: st_startpoint(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_startpoint(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_startpoint_linestring';


--
-- Name: st_summary(geography); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_summary(geography) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_summary';


--
-- Name: st_summary(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_summary(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_summary';


--
-- Name: st_symdifference(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_symdifference(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'symdifference';


--
-- Name: st_symmetricdifference(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_symmetricdifference(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'symdifference';


--
-- Name: st_touches(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_touches(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Touches($1,$2)$_$;


--
-- Name: st_transform(geometry, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_transform(geometry, integer) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'transform';


--
-- Name: st_translate(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_translate(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Translate($1, $2, $3, 0)$_$;


--
-- Name: st_translate(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_translate(geometry, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1, 1, 0, 0, 0, 1, 0, 0, 0, 1, $2, $3, $4)$_$;


--
-- Name: st_transscale(geometry, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_transscale(geometry, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Affine($1,  $4, 0, 0,  0, $5, 0,
		0, 0, 1,  $2 * $4, $3 * $5, 0)$_$;


--
-- Name: st_unaryunion(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_unaryunion(geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'ST_UnaryUnion';


--
-- Name: st_union(geometry[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_union(geometry[]) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'pgis_union_geometry_array';


--
-- Name: st_union(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_union(geom1 geometry, geom2 geometry) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'geomunion';


--
-- Name: st_within(geometry, geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_within(geom1 geometry, geom2 geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT $1 && $2 AND _ST_Contains($2,$1)$_$;


--
-- Name: st_wkbtosql(bytea); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_wkbtosql(wkb bytea) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_WKB';


--
-- Name: st_wkttosql(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_wkttosql(text) RETURNS geometry
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_from_text';


--
-- Name: st_x(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_x(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_x_point';


--
-- Name: st_xmax(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_xmax(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_xmax';


--
-- Name: st_xmin(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_xmin(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_xmin';


--
-- Name: st_y(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_y(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_y_point';


--
-- Name: st_ymax(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_ymax(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_ymax';


--
-- Name: st_ymin(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_ymin(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_ymin';


--
-- Name: st_z(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_z(geometry) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_z_point';


--
-- Name: st_zmax(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_zmax(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_zmax';


--
-- Name: st_zmflag(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_zmflag(geometry) RETURNS smallint
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_zmflag';


--
-- Name: st_zmin(box3d); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION st_zmin(box3d) RETURNS double precision
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'BOX3D_zmin';


--
-- Name: text(geometry); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION text(geometry) RETURNS text
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/postgis-2.1', 'LWGEOM_to_text';


--
-- Name: tile_for_point(integer, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION tile_for_point(integer, integer) RETURNS bigint
    LANGUAGE c STRICT
    AS '/ws/openstreetmap-website/db/functions/libpgosm', 'tile_for_point';


--
-- Name: unlockrows(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION unlockrows(text) RETURNS integer
    LANGUAGE plpgsql STRICT
    AS $_$ 
DECLARE
	ret int;
BEGIN

	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION 'Long transaction support disabled, use EnableLongTransaction() to enable.';
	END IF;

	EXECUTE 'DELETE FROM authorization_table where authid = ' ||
		quote_literal($1);

	GET DIAGNOSTICS ret = ROW_COUNT;

	RETURN ret;
END;
$_$;


--
-- Name: updategeometrysrid(character varying, character varying, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION updategeometrysrid(character varying, character varying, integer) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT UpdateGeometrySRID('','',$1,$2,$3) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: updategeometrysrid(character varying, character varying, character varying, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION updategeometrysrid(character varying, character varying, character varying, integer) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT UpdateGeometrySRID('',$1,$2,$3,$4) into ret;
	RETURN ret;
END;
$_$;


--
-- Name: updategeometrysrid(character varying, character varying, character varying, character varying, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION updategeometrysrid(catalogn_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $$
DECLARE
	myrec RECORD;
	okay boolean;
	cname varchar;
	real_schema name;
	unknown_srid integer;
	new_srid integer := new_srid_in;

BEGIN


	-- Find, check or fix schema_name
	IF ( schema_name != '' ) THEN
		okay = false;

		FOR myrec IN SELECT nspname FROM pg_namespace WHERE text(nspname) = schema_name LOOP
			okay := true;
		END LOOP;

		IF ( okay <> true ) THEN
			RAISE EXCEPTION 'Invalid schema name';
		ELSE
			real_schema = schema_name;
		END IF;
	ELSE
		SELECT INTO real_schema current_schema()::text;
	END IF;

	-- Ensure that column_name is in geometry_columns
	okay = false;
	FOR myrec IN SELECT type, coord_dimension FROM geometry_columns WHERE f_table_schema = text(real_schema) and f_table_name = table_name and f_geometry_column = column_name LOOP
		okay := true;
	END LOOP;
	IF (NOT okay) THEN
		RAISE EXCEPTION 'column not found in geometry_columns table';
		RETURN false;
	END IF;

	-- Ensure that new_srid is valid
	IF ( new_srid > 0 ) THEN
		IF ( SELECT count(*) = 0 from spatial_ref_sys where srid = new_srid ) THEN
			RAISE EXCEPTION 'invalid SRID: % not found in spatial_ref_sys', new_srid;
			RETURN false;
		END IF;
	ELSE
		unknown_srid := ST_SRID('POINT EMPTY'::geometry);
		IF ( new_srid != unknown_srid ) THEN
			new_srid := unknown_srid;
			RAISE NOTICE 'SRID value % converted to the officially unknown SRID value %', new_srid_in, new_srid;
		END IF;
	END IF;

	IF postgis_constraint_srid(schema_name, table_name, column_name) IS NOT NULL THEN 
	-- srid was enforced with constraints before, keep it that way.
        -- Make up constraint name
        cname = 'enforce_srid_'  || column_name;
    
        -- Drop enforce_srid constraint
        EXECUTE 'ALTER TABLE ' || quote_ident(real_schema) ||
            '.' || quote_ident(table_name) ||
            ' DROP constraint ' || quote_ident(cname);
    
        -- Update geometries SRID
        EXECUTE 'UPDATE ' || quote_ident(real_schema) ||
            '.' || quote_ident(table_name) ||
            ' SET ' || quote_ident(column_name) ||
            ' = ST_SetSRID(' || quote_ident(column_name) ||
            ', ' || new_srid::text || ')';
            
        -- Reset enforce_srid constraint
        EXECUTE 'ALTER TABLE ' || quote_ident(real_schema) ||
            '.' || quote_ident(table_name) ||
            ' ADD constraint ' || quote_ident(cname) ||
            ' CHECK (st_srid(' || quote_ident(column_name) ||
            ') = ' || new_srid::text || ')';
    ELSE 
        -- We will use typmod to enforce if no srid constraints
        -- We are using postgis_type_name to lookup the new name 
        -- (in case Paul changes his mind and flips geometry_columns to return old upper case name) 
        EXECUTE 'ALTER TABLE ' || quote_ident(real_schema) || '.' || quote_ident(table_name) || 
        ' ALTER COLUMN ' || quote_ident(column_name) || ' TYPE  geometry(' || postgis_type_name(myrec.type, myrec.coord_dimension, true) || ', ' || new_srid::text || ') USING ST_SetSRID(' || quote_ident(column_name) || ',' || new_srid::text || ');' ;
    END IF;

	RETURN real_schema || '.' || table_name || '.' || column_name ||' SRID changed to ' || new_srid::text;

END;
$$;


--
-- Name: xid_to_int4(xid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION xid_to_int4(xid) RETURNS integer
    LANGUAGE c STRICT
    AS '/ws/openstreetmap-website/db/functions/libpgosm', 'xid_to_int4';


--
-- Name: st_3dextent(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_3dextent(geometry) (
    SFUNC = public.st_combine_bbox,
    STYPE = box3d
);


--
-- Name: st_accum(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_accum(geometry) (
    SFUNC = pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_accum_finalfn
);


--
-- Name: st_collect(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_collect(geometry) (
    SFUNC = pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_collect_finalfn
);


--
-- Name: st_extent(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_extent(geometry) (
    SFUNC = public.st_combine_bbox,
    STYPE = box3d,
    FINALFUNC = public.box2d
);


--
-- Name: st_makeline(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_makeline(geometry) (
    SFUNC = pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_makeline_finalfn
);


--
-- Name: st_memcollect(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_memcollect(geometry) (
    SFUNC = public.st_collect,
    STYPE = geometry
);


--
-- Name: st_memunion(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_memunion(geometry) (
    SFUNC = public.st_union,
    STYPE = geometry
);


--
-- Name: st_polygonize(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_polygonize(geometry) (
    SFUNC = pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_polygonize_finalfn
);


--
-- Name: st_union(geometry); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE st_union(geometry) (
    SFUNC = pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_union_finalfn
);


--
-- Name: &&; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR && (
    PROCEDURE = geometry_overlaps,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = &&,
    RESTRICT = gserialized_gist_sel_2d,
    JOIN = gserialized_gist_joinsel_2d
);


--
-- Name: &&; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR && (
    PROCEDURE = geography_overlaps,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = &&,
    RESTRICT = gserialized_gist_sel_nd,
    JOIN = gserialized_gist_joinsel_nd
);


--
-- Name: &&&; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR &&& (
    PROCEDURE = geometry_overlaps_nd,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = &&&,
    RESTRICT = gserialized_gist_sel_nd,
    JOIN = gserialized_gist_joinsel_nd
);


--
-- Name: &<; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR &< (
    PROCEDURE = geometry_overleft,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = &>,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: &<|; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR &<| (
    PROCEDURE = geometry_overbelow,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = |&>,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: &>; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR &> (
    PROCEDURE = geometry_overright,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = &<,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: <; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR < (
    PROCEDURE = geometry_lt,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = >,
    NEGATOR = >=,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: <; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR < (
    PROCEDURE = geography_lt,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = >,
    NEGATOR = >=,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: <#>; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR <#> (
    PROCEDURE = geometry_distance_box,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <#>
);


--
-- Name: <->; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR <-> (
    PROCEDURE = geometry_distance_centroid,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <->
);


--
-- Name: <<; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR << (
    PROCEDURE = geometry_left,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = >>,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: <<|; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR <<| (
    PROCEDURE = geometry_below,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = |>>,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: <=; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR <= (
    PROCEDURE = geometry_le,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = >=,
    NEGATOR = >,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: <=; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR <= (
    PROCEDURE = geography_le,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = >=,
    NEGATOR = >,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: =; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR = (
    PROCEDURE = geometry_eq,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = =,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: =; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR = (
    PROCEDURE = geography_eq,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = =,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: >; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR > (
    PROCEDURE = geometry_gt,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <,
    NEGATOR = <=,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: >; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR > (
    PROCEDURE = geography_gt,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = <,
    NEGATOR = <=,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: >=; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR >= (
    PROCEDURE = geometry_ge,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <=,
    NEGATOR = <,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: >=; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR >= (
    PROCEDURE = geography_ge,
    LEFTARG = geography,
    RIGHTARG = geography,
    COMMUTATOR = <=,
    NEGATOR = <,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: >>; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR >> (
    PROCEDURE = geometry_right,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <<,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: @; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR @ (
    PROCEDURE = geometry_within,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = ~,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: |&>; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR |&> (
    PROCEDURE = geometry_overabove,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = &<|,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: |>>; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR |>> (
    PROCEDURE = geometry_above,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = <<|,
    RESTRICT = positionsel,
    JOIN = positionjoinsel
);


--
-- Name: ~; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR ~ (
    PROCEDURE = geometry_contains,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    COMMUTATOR = @,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: ~=; Type: OPERATOR; Schema: public; Owner: -
--

CREATE OPERATOR ~= (
    PROCEDURE = geometry_same,
    LEFTARG = geometry,
    RIGHTARG = geometry,
    RESTRICT = contsel,
    JOIN = contjoinsel
);


--
-- Name: btree_geography_ops; Type: OPERATOR CLASS; Schema: public; Owner: -
--

CREATE OPERATOR CLASS btree_geography_ops
    DEFAULT FOR TYPE geography USING btree AS
    OPERATOR 1 <(geography,geography) ,
    OPERATOR 2 <=(geography,geography) ,
    OPERATOR 3 =(geography,geography) ,
    OPERATOR 4 >=(geography,geography) ,
    OPERATOR 5 >(geography,geography) ,
    FUNCTION 1 (geography, geography) geography_cmp(geography,geography);


--
-- Name: btree_geometry_ops; Type: OPERATOR CLASS; Schema: public; Owner: -
--

CREATE OPERATOR CLASS btree_geometry_ops
    DEFAULT FOR TYPE geometry USING btree AS
    OPERATOR 1 <(geometry,geometry) ,
    OPERATOR 2 <=(geometry,geometry) ,
    OPERATOR 3 =(geometry,geometry) ,
    OPERATOR 4 >=(geometry,geometry) ,
    OPERATOR 5 >(geometry,geometry) ,
    FUNCTION 1 (geometry, geometry) geometry_cmp(geometry,geometry);


--
-- Name: gist_geography_ops; Type: OPERATOR CLASS; Schema: public; Owner: -
--

CREATE OPERATOR CLASS gist_geography_ops
    DEFAULT FOR TYPE geography USING gist AS
    STORAGE gidx ,
    OPERATOR 3 &&(geography,geography) ,
    FUNCTION 1 (geography, geography) geography_gist_consistent(internal,geography,integer) ,
    FUNCTION 2 (geography, geography) geography_gist_union(bytea,internal) ,
    FUNCTION 3 (geography, geography) geography_gist_compress(internal) ,
    FUNCTION 4 (geography, geography) geography_gist_decompress(internal) ,
    FUNCTION 5 (geography, geography) geography_gist_penalty(internal,internal,internal) ,
    FUNCTION 6 (geography, geography) geography_gist_picksplit(internal,internal) ,
    FUNCTION 7 (geography, geography) geography_gist_same(box2d,box2d,internal);


--
-- Name: gist_geometry_ops_2d; Type: OPERATOR CLASS; Schema: public; Owner: -
--

CREATE OPERATOR CLASS gist_geometry_ops_2d
    DEFAULT FOR TYPE geometry USING gist AS
    STORAGE box2df ,
    OPERATOR 1 <<(geometry,geometry) ,
    OPERATOR 2 &<(geometry,geometry) ,
    OPERATOR 3 &&(geometry,geometry) ,
    OPERATOR 4 &>(geometry,geometry) ,
    OPERATOR 5 >>(geometry,geometry) ,
    OPERATOR 6 ~=(geometry,geometry) ,
    OPERATOR 7 ~(geometry,geometry) ,
    OPERATOR 8 @(geometry,geometry) ,
    OPERATOR 9 &<|(geometry,geometry) ,
    OPERATOR 10 <<|(geometry,geometry) ,
    OPERATOR 11 |>>(geometry,geometry) ,
    OPERATOR 12 |&>(geometry,geometry) ,
    OPERATOR 13 <->(geometry,geometry) FOR ORDER BY pg_catalog.float_ops ,
    OPERATOR 14 <#>(geometry,geometry) FOR ORDER BY pg_catalog.float_ops ,
    FUNCTION 1 (geometry, geometry) geometry_gist_consistent_2d(internal,geometry,integer) ,
    FUNCTION 2 (geometry, geometry) geometry_gist_union_2d(bytea,internal) ,
    FUNCTION 3 (geometry, geometry) geometry_gist_compress_2d(internal) ,
    FUNCTION 4 (geometry, geometry) geometry_gist_decompress_2d(internal) ,
    FUNCTION 5 (geometry, geometry) geometry_gist_penalty_2d(internal,internal,internal) ,
    FUNCTION 6 (geometry, geometry) geometry_gist_picksplit_2d(internal,internal) ,
    FUNCTION 7 (geometry, geometry) geometry_gist_same_2d(geometry,geometry,internal) ,
    FUNCTION 8 (geometry, geometry) geometry_gist_distance_2d(internal,geometry,integer);


--
-- Name: gist_geometry_ops_nd; Type: OPERATOR CLASS; Schema: public; Owner: -
--

CREATE OPERATOR CLASS gist_geometry_ops_nd
    FOR TYPE geometry USING gist AS
    STORAGE gidx ,
    OPERATOR 3 &&&(geometry,geometry) ,
    FUNCTION 1 (geometry, geometry) geometry_gist_consistent_nd(internal,geometry,integer) ,
    FUNCTION 2 (geometry, geometry) geometry_gist_union_nd(bytea,internal) ,
    FUNCTION 3 (geometry, geometry) geometry_gist_compress_nd(internal) ,
    FUNCTION 4 (geometry, geometry) geometry_gist_decompress_nd(internal) ,
    FUNCTION 5 (geometry, geometry) geometry_gist_penalty_nd(internal,internal,internal) ,
    FUNCTION 6 (geometry, geometry) geometry_gist_picksplit_nd(internal,internal) ,
    FUNCTION 7 (geometry, geometry) geometry_gist_same_nd(geometry,geometry,internal);


SET search_path = pg_catalog;

--
-- Name: CAST (public.box2d AS public.box3d); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.box2d AS public.box3d) WITH FUNCTION public.box3d(public.box2d) AS IMPLICIT;


--
-- Name: CAST (public.box2d AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.box2d AS public.geometry) WITH FUNCTION public.geometry(public.box2d) AS IMPLICIT;


--
-- Name: CAST (public.box3d AS box); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.box3d AS box) WITH FUNCTION public.box(public.box3d) AS IMPLICIT;


--
-- Name: CAST (public.box3d AS public.box2d); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.box3d AS public.box2d) WITH FUNCTION public.box2d(public.box3d) AS IMPLICIT;


--
-- Name: CAST (public.box3d AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.box3d AS public.geometry) WITH FUNCTION public.geometry(public.box3d) AS IMPLICIT;


--
-- Name: CAST (bytea AS public.geography); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (bytea AS public.geography) WITH FUNCTION public.geography(bytea) AS IMPLICIT;


--
-- Name: CAST (bytea AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (bytea AS public.geometry) WITH FUNCTION public.geometry(bytea) AS IMPLICIT;


--
-- Name: CAST (public.geography AS bytea); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geography AS bytea) WITH FUNCTION public.bytea(public.geography) AS IMPLICIT;


--
-- Name: CAST (public.geography AS public.geography); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geography AS public.geography) WITH FUNCTION public.geography(public.geography, integer, boolean) AS IMPLICIT;


--
-- Name: CAST (public.geography AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geography AS public.geometry) WITH FUNCTION public.geometry(public.geography);


--
-- Name: CAST (public.geometry AS box); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS box) WITH FUNCTION public.box(public.geometry) AS ASSIGNMENT;


--
-- Name: CAST (public.geometry AS public.box2d); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS public.box2d) WITH FUNCTION public.box2d(public.geometry) AS IMPLICIT;


--
-- Name: CAST (public.geometry AS public.box3d); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS public.box3d) WITH FUNCTION public.box3d(public.geometry) AS IMPLICIT;


--
-- Name: CAST (public.geometry AS bytea); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS bytea) WITH FUNCTION public.bytea(public.geometry) AS IMPLICIT;


--
-- Name: CAST (public.geometry AS public.geography); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS public.geography) WITH FUNCTION public.geography(public.geometry) AS IMPLICIT;


--
-- Name: CAST (public.geometry AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS public.geometry) WITH FUNCTION public.geometry(public.geometry, integer, boolean) AS IMPLICIT;


--
-- Name: CAST (public.geometry AS path); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS path) WITH FUNCTION public.path(public.geometry);


--
-- Name: CAST (public.geometry AS point); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS point) WITH FUNCTION public.point(public.geometry);


--
-- Name: CAST (public.geometry AS polygon); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS polygon) WITH FUNCTION public.polygon(public.geometry);


--
-- Name: CAST (public.geometry AS text); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (public.geometry AS text) WITH FUNCTION public.text(public.geometry) AS IMPLICIT;


--
-- Name: CAST (path AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (path AS public.geometry) WITH FUNCTION public.geometry(path);


--
-- Name: CAST (point AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (point AS public.geometry) WITH FUNCTION public.geometry(point);


--
-- Name: CAST (polygon AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (polygon AS public.geometry) WITH FUNCTION public.geometry(polygon);


--
-- Name: CAST (text AS public.geometry); Type: CAST; Schema: pg_catalog; Owner: -
--

CREATE CAST (text AS public.geometry) WITH FUNCTION public.geometry(text) AS IMPLICIT;


SET search_path = osm_test, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: acls; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE acls (
    id integer NOT NULL,
    address inet,
    k character varying(255) NOT NULL,
    v character varying(255),
    domain character varying(255)
);


--
-- Name: acls_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE acls_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: acls_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE acls_id_seq OWNED BY acls.id;


--
-- Name: changeset_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE changeset_tags (
    changeset_id bigint NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL
);


--
-- Name: changesets; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE changesets (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp without time zone NOT NULL,
    min_lat integer,
    max_lat integer,
    min_lon integer,
    max_lon integer,
    closed_at timestamp without time zone NOT NULL,
    num_changes integer DEFAULT 0 NOT NULL
);


--
-- Name: changesets_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE changesets_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: changesets_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE changesets_id_seq OWNED BY changesets.id;


--
-- Name: client_applications; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE client_applications (
    id integer NOT NULL,
    name character varying(255),
    url character varying(255),
    support_url character varying(255),
    callback_url character varying(255),
    key character varying(50),
    secret character varying(50),
    user_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    allow_read_prefs boolean DEFAULT false NOT NULL,
    allow_write_prefs boolean DEFAULT false NOT NULL,
    allow_write_diary boolean DEFAULT false NOT NULL,
    allow_write_api boolean DEFAULT false NOT NULL,
    allow_read_gpx boolean DEFAULT false NOT NULL,
    allow_write_gpx boolean DEFAULT false NOT NULL,
    allow_write_notes boolean DEFAULT false NOT NULL
);


--
-- Name: client_applications_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE client_applications_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: client_applications_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE client_applications_id_seq OWNED BY client_applications.id;


--
-- Name: current_node_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_node_tags (
    node_id bigint NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL
);


--
-- Name: current_nodes; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_nodes (
    id bigint NOT NULL,
    latitude integer NOT NULL,
    longitude integer NOT NULL,
    changeset_id bigint NOT NULL,
    visible boolean NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    tile bigint NOT NULL,
    version bigint NOT NULL
);


--
-- Name: current_nodes_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE current_nodes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: current_nodes_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE current_nodes_id_seq OWNED BY current_nodes.id;


--
-- Name: current_relation_members; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_relation_members (
    relation_id bigint NOT NULL,
    member_type nwr_enum NOT NULL,
    member_id bigint NOT NULL,
    member_role character varying(255) NOT NULL,
    sequence_id integer DEFAULT 0 NOT NULL
);


--
-- Name: current_relation_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_relation_tags (
    relation_id bigint NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL
);


--
-- Name: current_relations; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_relations (
    id bigint NOT NULL,
    changeset_id bigint NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    visible boolean NOT NULL,
    version bigint NOT NULL
);


--
-- Name: current_relations_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE current_relations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: current_relations_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE current_relations_id_seq OWNED BY current_relations.id;


--
-- Name: current_way_nodes; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_way_nodes (
    way_id bigint NOT NULL,
    node_id bigint NOT NULL,
    sequence_id bigint NOT NULL
);


--
-- Name: current_way_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_way_tags (
    way_id bigint NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL
);


--
-- Name: current_ways; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE current_ways (
    id bigint NOT NULL,
    changeset_id bigint NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    visible boolean NOT NULL,
    version bigint NOT NULL
);


--
-- Name: current_ways_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE current_ways_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: current_ways_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE current_ways_id_seq OWNED BY current_ways.id;


--
-- Name: diary_comments; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE diary_comments (
    id bigint NOT NULL,
    diary_entry_id bigint NOT NULL,
    user_id bigint NOT NULL,
    body text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    visible boolean DEFAULT true NOT NULL,
    body_format format_enum DEFAULT 'html'::format_enum NOT NULL
);


--
-- Name: diary_comments_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE diary_comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: diary_comments_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE diary_comments_id_seq OWNED BY diary_comments.id;


--
-- Name: diary_entries; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE diary_entries (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    title character varying(255) NOT NULL,
    body text NOT NULL,
    created_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    latitude double precision,
    longitude double precision,
    language_code character varying(255) DEFAULT 'en'::character varying NOT NULL,
    visible boolean DEFAULT true NOT NULL,
    body_format format_enum DEFAULT 'html'::format_enum NOT NULL
);


--
-- Name: diary_entries_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE diary_entries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: diary_entries_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE diary_entries_id_seq OWNED BY diary_entries.id;


--
-- Name: friends; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE friends (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    friend_user_id bigint NOT NULL
);


--
-- Name: friends_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE friends_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: friends_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE friends_id_seq OWNED BY friends.id;


--
-- Name: gps_points; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE gps_points (
    altitude double precision,
    trackid integer NOT NULL,
    latitude integer NOT NULL,
    longitude integer NOT NULL,
    gpx_id bigint NOT NULL,
    "timestamp" timestamp without time zone,
    tile bigint
);


--
-- Name: gpx_file_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE gpx_file_tags (
    gpx_id bigint DEFAULT 0 NOT NULL,
    tag character varying(255) NOT NULL,
    id bigint NOT NULL
);


--
-- Name: gpx_file_tags_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE gpx_file_tags_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: gpx_file_tags_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE gpx_file_tags_id_seq OWNED BY gpx_file_tags.id;


--
-- Name: gpx_files; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE gpx_files (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    visible boolean DEFAULT true NOT NULL,
    name character varying(255) DEFAULT ''::character varying NOT NULL,
    size bigint,
    latitude double precision,
    longitude double precision,
    "timestamp" timestamp without time zone NOT NULL,
    description character varying(255) DEFAULT ''::character varying NOT NULL,
    inserted boolean NOT NULL,
    visibility gpx_visibility_enum DEFAULT 'public'::gpx_visibility_enum NOT NULL
);


--
-- Name: gpx_files_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE gpx_files_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: gpx_files_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE gpx_files_id_seq OWNED BY gpx_files.id;


--
-- Name: languages; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE languages (
    code character varying(255) NOT NULL,
    english_name character varying(255) NOT NULL,
    native_name character varying(255)
);


--
-- Name: messages; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE messages (
    id bigint NOT NULL,
    from_user_id bigint NOT NULL,
    title character varying(255) NOT NULL,
    body text NOT NULL,
    sent_on timestamp without time zone NOT NULL,
    message_read boolean DEFAULT false NOT NULL,
    to_user_id bigint NOT NULL,
    to_user_visible boolean DEFAULT true NOT NULL,
    from_user_visible boolean DEFAULT true NOT NULL,
    body_format format_enum DEFAULT 'html'::format_enum NOT NULL
);


--
-- Name: messages_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: messages_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE messages_id_seq OWNED BY messages.id;


--
-- Name: node_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE node_tags (
    node_id bigint NOT NULL,
    version bigint NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL
);


--
-- Name: nodes; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE nodes (
    node_id bigint NOT NULL,
    latitude integer NOT NULL,
    longitude integer NOT NULL,
    changeset_id bigint NOT NULL,
    visible boolean NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    tile bigint NOT NULL,
    version bigint NOT NULL,
    redaction_id integer
);


--
-- Name: note_comments; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE note_comments (
    id bigint NOT NULL,
    note_id bigint NOT NULL,
    visible boolean NOT NULL,
    created_at timestamp without time zone NOT NULL,
    author_ip inet,
    author_id bigint,
    body text,
    event note_event_enum
);


--
-- Name: note_comments_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE note_comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: note_comments_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE note_comments_id_seq OWNED BY note_comments.id;


--
-- Name: notes; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE notes (
    id bigint NOT NULL,
    latitude integer NOT NULL,
    longitude integer NOT NULL,
    tile bigint NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    created_at timestamp without time zone NOT NULL,
    status note_status_enum NOT NULL,
    closed_at timestamp without time zone
);


--
-- Name: notes_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE notes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: notes_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE notes_id_seq OWNED BY notes.id;


--
-- Name: oauth_nonces; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE oauth_nonces (
    id integer NOT NULL,
    nonce character varying(255),
    "timestamp" integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: oauth_nonces_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE oauth_nonces_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: oauth_nonces_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE oauth_nonces_id_seq OWNED BY oauth_nonces.id;


--
-- Name: oauth_tokens; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE oauth_tokens (
    id integer NOT NULL,
    user_id integer,
    type character varying(20),
    client_application_id integer,
    token character varying(50),
    secret character varying(50),
    authorized_at timestamp without time zone,
    invalidated_at timestamp without time zone,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    allow_read_prefs boolean DEFAULT false NOT NULL,
    allow_write_prefs boolean DEFAULT false NOT NULL,
    allow_write_diary boolean DEFAULT false NOT NULL,
    allow_write_api boolean DEFAULT false NOT NULL,
    allow_read_gpx boolean DEFAULT false NOT NULL,
    allow_write_gpx boolean DEFAULT false NOT NULL,
    callback_url character varying(255),
    verifier character varying(20),
    scope character varying(255),
    valid_to timestamp without time zone,
    allow_write_notes boolean DEFAULT false NOT NULL
);


--
-- Name: oauth_tokens_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE oauth_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: oauth_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE oauth_tokens_id_seq OWNED BY oauth_tokens.id;


--
-- Name: redactions; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE redactions (
    id integer NOT NULL,
    title character varying(255),
    description text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    user_id bigint NOT NULL,
    description_format format_enum DEFAULT 'markdown'::format_enum NOT NULL
);


--
-- Name: redactions_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE redactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: redactions_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE redactions_id_seq OWNED BY redactions.id;


--
-- Name: relation_members; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE relation_members (
    relation_id bigint DEFAULT 0 NOT NULL,
    member_type nwr_enum NOT NULL,
    member_id bigint NOT NULL,
    member_role character varying(255) NOT NULL,
    version bigint DEFAULT 0 NOT NULL,
    sequence_id integer DEFAULT 0 NOT NULL
);


--
-- Name: relation_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE relation_tags (
    relation_id bigint DEFAULT 0 NOT NULL,
    k character varying(255) DEFAULT ''::character varying NOT NULL,
    v character varying(255) DEFAULT ''::character varying NOT NULL,
    version bigint NOT NULL
);


--
-- Name: relations; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE relations (
    relation_id bigint DEFAULT 0 NOT NULL,
    changeset_id bigint NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    version bigint NOT NULL,
    visible boolean DEFAULT true NOT NULL,
    redaction_id integer
);


--
-- Name: schema_migrations; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: user_blocks; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE user_blocks (
    id integer NOT NULL,
    user_id bigint NOT NULL,
    creator_id bigint NOT NULL,
    reason text NOT NULL,
    ends_at timestamp without time zone NOT NULL,
    needs_view boolean DEFAULT false NOT NULL,
    revoker_id bigint,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    reason_format format_enum DEFAULT 'html'::format_enum NOT NULL
);


--
-- Name: user_blocks_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE user_blocks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_blocks_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE user_blocks_id_seq OWNED BY user_blocks.id;


--
-- Name: user_preferences; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE user_preferences (
    user_id bigint NOT NULL,
    k character varying(255) NOT NULL,
    v character varying(255) NOT NULL
);


--
-- Name: user_roles; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE user_roles (
    id integer NOT NULL,
    user_id bigint NOT NULL,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    role user_role_enum NOT NULL,
    granter_id bigint NOT NULL
);


--
-- Name: user_roles_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE user_roles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_roles_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE user_roles_id_seq OWNED BY user_roles.id;


--
-- Name: user_tokens; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE user_tokens (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    token character varying(255) NOT NULL,
    expiry timestamp without time zone NOT NULL,
    referer text
);


--
-- Name: user_tokens_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE user_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE user_tokens_id_seq OWNED BY user_tokens.id;


--
-- Name: users; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE users (
    email character varying(255) NOT NULL,
    id bigint NOT NULL,
    pass_crypt character varying(255) NOT NULL,
    creation_time timestamp without time zone NOT NULL,
    display_name character varying(255) DEFAULT ''::character varying NOT NULL,
    data_public boolean DEFAULT false NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    home_lat double precision,
    home_lon double precision,
    home_zoom smallint DEFAULT 3,
    nearby integer DEFAULT 50,
    pass_salt character varying(255),
    image_file_name text,
    email_valid boolean DEFAULT false NOT NULL,
    new_email character varying(255),
    creation_ip character varying(255),
    languages character varying(255),
    status user_status_enum DEFAULT 'pending'::user_status_enum NOT NULL,
    terms_agreed timestamp without time zone,
    consider_pd boolean DEFAULT false NOT NULL,
    openid_url character varying(255),
    preferred_editor character varying(255),
    terms_seen boolean DEFAULT false NOT NULL,
    description_format format_enum DEFAULT 'html'::format_enum NOT NULL,
    image_fingerprint character varying(255),
    changesets_count integer DEFAULT 0 NOT NULL,
    traces_count integer DEFAULT 0 NOT NULL,
    diary_entries_count integer DEFAULT 0 NOT NULL,
    image_use_gravatar boolean DEFAULT true NOT NULL,
    image_content_type character varying(255)
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: osm_test; Owner: -
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: osm_test; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: way_nodes; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE way_nodes (
    way_id bigint NOT NULL,
    node_id bigint NOT NULL,
    version bigint NOT NULL,
    sequence_id bigint NOT NULL
);


--
-- Name: way_tags; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE way_tags (
    way_id bigint DEFAULT 0 NOT NULL,
    k character varying(255) NOT NULL,
    v character varying(255) NOT NULL,
    version bigint NOT NULL
);


--
-- Name: ways; Type: TABLE; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE TABLE ways (
    way_id bigint DEFAULT 0 NOT NULL,
    changeset_id bigint NOT NULL,
    "timestamp" timestamp without time zone NOT NULL,
    version bigint NOT NULL,
    visible boolean DEFAULT true NOT NULL,
    redaction_id integer
);


SET search_path = public, pg_catalog;

--
-- Name: geography_columns; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW geography_columns AS
 SELECT current_database() AS f_table_catalog, 
    n.nspname AS f_table_schema, 
    c.relname AS f_table_name, 
    a.attname AS f_geography_column, 
    postgis_typmod_dims(a.atttypmod) AS coord_dimension, 
    postgis_typmod_srid(a.atttypmod) AS srid, 
    postgis_typmod_type(a.atttypmod) AS type
   FROM pg_class c, 
    pg_attribute a, 
    pg_type t, 
    pg_namespace n
  WHERE (((((((t.typname = 'geography'::name) AND (a.attisdropped = false)) AND (a.atttypid = t.oid)) AND (a.attrelid = c.oid)) AND (c.relnamespace = n.oid)) AND (NOT pg_is_other_temp_schema(c.relnamespace))) AND has_table_privilege(c.oid, 'SELECT'::text));


--
-- Name: geometry_columns; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW geometry_columns AS
 SELECT (current_database())::character varying(256) AS f_table_catalog, 
    (n.nspname)::character varying(256) AS f_table_schema, 
    (c.relname)::character varying(256) AS f_table_name, 
    (a.attname)::character varying(256) AS f_geometry_column, 
    COALESCE(NULLIF(postgis_typmod_dims(a.atttypmod), 2), postgis_constraint_dims((n.nspname)::text, (c.relname)::text, (a.attname)::text), 2) AS coord_dimension, 
    COALESCE(NULLIF(postgis_typmod_srid(a.atttypmod), 0), postgis_constraint_srid((n.nspname)::text, (c.relname)::text, (a.attname)::text), 0) AS srid, 
    (replace(replace(COALESCE(NULLIF(upper(postgis_typmod_type(a.atttypmod)), 'GEOMETRY'::text), (postgis_constraint_type((n.nspname)::text, (c.relname)::text, (a.attname)::text))::text, 'GEOMETRY'::text), 'ZM'::text, ''::text), 'Z'::text, ''::text))::character varying(30) AS type
   FROM pg_class c, 
    pg_attribute a, 
    pg_type t, 
    pg_namespace n
  WHERE (((((((((t.typname = 'geometry'::name) AND (a.attisdropped = false)) AND (a.atttypid = t.oid)) AND (a.attrelid = c.oid)) AND (c.relnamespace = n.oid)) AND ((c.relkind = 'r'::"char") OR (c.relkind = 'v'::"char"))) AND (NOT pg_is_other_temp_schema(c.relnamespace))) AND (NOT ((n.nspname = 'public'::name) AND (c.relname = 'raster_columns'::name)))) AND has_table_privilege(c.oid, 'SELECT'::text));


--
-- Name: spatial_ref_sys; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE spatial_ref_sys (
    srid integer NOT NULL,
    auth_name character varying(256),
    auth_srid integer,
    srtext character varying(2048),
    proj4text character varying(2048),
    CONSTRAINT spatial_ref_sys_srid_check CHECK (((srid > 0) AND (srid <= 998999)))
);


SET search_path = osm_test, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY acls ALTER COLUMN id SET DEFAULT nextval('acls_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY changesets ALTER COLUMN id SET DEFAULT nextval('changesets_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY client_applications ALTER COLUMN id SET DEFAULT nextval('client_applications_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_nodes ALTER COLUMN id SET DEFAULT nextval('current_nodes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_relations ALTER COLUMN id SET DEFAULT nextval('current_relations_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_ways ALTER COLUMN id SET DEFAULT nextval('current_ways_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_comments ALTER COLUMN id SET DEFAULT nextval('diary_comments_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_entries ALTER COLUMN id SET DEFAULT nextval('diary_entries_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY friends ALTER COLUMN id SET DEFAULT nextval('friends_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY gpx_file_tags ALTER COLUMN id SET DEFAULT nextval('gpx_file_tags_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY gpx_files ALTER COLUMN id SET DEFAULT nextval('gpx_files_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY messages ALTER COLUMN id SET DEFAULT nextval('messages_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY note_comments ALTER COLUMN id SET DEFAULT nextval('note_comments_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY notes ALTER COLUMN id SET DEFAULT nextval('notes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY oauth_nonces ALTER COLUMN id SET DEFAULT nextval('oauth_nonces_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY oauth_tokens ALTER COLUMN id SET DEFAULT nextval('oauth_tokens_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY redactions ALTER COLUMN id SET DEFAULT nextval('redactions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_blocks ALTER COLUMN id SET DEFAULT nextval('user_blocks_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_roles ALTER COLUMN id SET DEFAULT nextval('user_roles_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_tokens ALTER COLUMN id SET DEFAULT nextval('user_tokens_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Name: acls_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY acls
    ADD CONSTRAINT acls_pkey PRIMARY KEY (id);


--
-- Name: changesets_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY changesets
    ADD CONSTRAINT changesets_pkey PRIMARY KEY (id);


--
-- Name: client_applications_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY client_applications
    ADD CONSTRAINT client_applications_pkey PRIMARY KEY (id);


--
-- Name: current_node_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_node_tags
    ADD CONSTRAINT current_node_tags_pkey PRIMARY KEY (node_id, k);


--
-- Name: current_nodes_pkey1; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_nodes
    ADD CONSTRAINT current_nodes_pkey1 PRIMARY KEY (id);


--
-- Name: current_relation_members_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_relation_members
    ADD CONSTRAINT current_relation_members_pkey PRIMARY KEY (relation_id, member_type, member_id, member_role, sequence_id);


--
-- Name: current_relation_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_relation_tags
    ADD CONSTRAINT current_relation_tags_pkey PRIMARY KEY (relation_id, k);


--
-- Name: current_relations_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_relations
    ADD CONSTRAINT current_relations_pkey PRIMARY KEY (id);


--
-- Name: current_way_nodes_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_way_nodes
    ADD CONSTRAINT current_way_nodes_pkey PRIMARY KEY (way_id, sequence_id);


--
-- Name: current_way_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_way_tags
    ADD CONSTRAINT current_way_tags_pkey PRIMARY KEY (way_id, k);


--
-- Name: current_ways_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY current_ways
    ADD CONSTRAINT current_ways_pkey PRIMARY KEY (id);


--
-- Name: diary_comments_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY diary_comments
    ADD CONSTRAINT diary_comments_pkey PRIMARY KEY (id);


--
-- Name: diary_entries_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY diary_entries
    ADD CONSTRAINT diary_entries_pkey PRIMARY KEY (id);


--
-- Name: friends_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY friends
    ADD CONSTRAINT friends_pkey PRIMARY KEY (id);


--
-- Name: gpx_file_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY gpx_file_tags
    ADD CONSTRAINT gpx_file_tags_pkey PRIMARY KEY (id);


--
-- Name: gpx_files_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY gpx_files
    ADD CONSTRAINT gpx_files_pkey PRIMARY KEY (id);


--
-- Name: languages_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY languages
    ADD CONSTRAINT languages_pkey PRIMARY KEY (code);


--
-- Name: messages_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


--
-- Name: node_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY node_tags
    ADD CONSTRAINT node_tags_pkey PRIMARY KEY (node_id, version, k);


--
-- Name: nodes_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (node_id, version);


--
-- Name: note_comments_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY note_comments
    ADD CONSTRAINT note_comments_pkey PRIMARY KEY (id);


--
-- Name: notes_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY notes
    ADD CONSTRAINT notes_pkey PRIMARY KEY (id);


--
-- Name: oauth_nonces_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY oauth_nonces
    ADD CONSTRAINT oauth_nonces_pkey PRIMARY KEY (id);


--
-- Name: oauth_tokens_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY oauth_tokens
    ADD CONSTRAINT oauth_tokens_pkey PRIMARY KEY (id);


--
-- Name: redactions_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY redactions
    ADD CONSTRAINT redactions_pkey PRIMARY KEY (id);


--
-- Name: relation_members_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY relation_members
    ADD CONSTRAINT relation_members_pkey PRIMARY KEY (relation_id, version, member_type, member_id, member_role, sequence_id);


--
-- Name: relation_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY relation_tags
    ADD CONSTRAINT relation_tags_pkey PRIMARY KEY (relation_id, version, k);


--
-- Name: relations_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_pkey PRIMARY KEY (relation_id, version);


--
-- Name: user_blocks_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_blocks
    ADD CONSTRAINT user_blocks_pkey PRIMARY KEY (id);


--
-- Name: user_preferences_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_preferences
    ADD CONSTRAINT user_preferences_pkey PRIMARY KEY (user_id, k);


--
-- Name: user_roles_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_roles
    ADD CONSTRAINT user_roles_pkey PRIMARY KEY (id);


--
-- Name: user_tokens_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_tokens
    ADD CONSTRAINT user_tokens_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: way_nodes_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY way_nodes
    ADD CONSTRAINT way_nodes_pkey PRIMARY KEY (way_id, version, sequence_id);


--
-- Name: way_tags_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY way_tags
    ADD CONSTRAINT way_tags_pkey PRIMARY KEY (way_id, version, k);


--
-- Name: ways_pkey; Type: CONSTRAINT; Schema: osm_test; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ways
    ADD CONSTRAINT ways_pkey PRIMARY KEY (way_id, version);


SET search_path = public, pg_catalog;

--
-- Name: spatial_ref_sys_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY spatial_ref_sys
    ADD CONSTRAINT spatial_ref_sys_pkey PRIMARY KEY (srid);


SET search_path = osm_test, pg_catalog;

--
-- Name: acls_k_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX acls_k_idx ON acls USING btree (k);


--
-- Name: changeset_tags_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changeset_tags_id_idx ON changeset_tags USING btree (changeset_id);


--
-- Name: changesets_bbox_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changesets_bbox_idx ON changesets USING gist (min_lat, max_lat, min_lon, max_lon);


--
-- Name: changesets_closed_at_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changesets_closed_at_idx ON changesets USING btree (closed_at);


--
-- Name: changesets_created_at_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changesets_created_at_idx ON changesets USING btree (created_at);


--
-- Name: changesets_user_id_created_at_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changesets_user_id_created_at_idx ON changesets USING btree (user_id, created_at);


--
-- Name: changesets_user_id_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX changesets_user_id_id_idx ON changesets USING btree (user_id, id);


--
-- Name: current_nodes_tile_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_nodes_tile_idx ON current_nodes USING btree (tile);


--
-- Name: current_nodes_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_nodes_timestamp_idx ON current_nodes USING btree ("timestamp");


--
-- Name: current_relation_members_member_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_relation_members_member_idx ON current_relation_members USING btree (member_type, member_id);


--
-- Name: current_relations_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_relations_timestamp_idx ON current_relations USING btree ("timestamp");


--
-- Name: current_way_nodes_node_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_way_nodes_node_idx ON current_way_nodes USING btree (node_id);


--
-- Name: current_ways_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX current_ways_timestamp_idx ON current_ways USING btree ("timestamp");


--
-- Name: diary_comment_user_id_created_at_index; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX diary_comment_user_id_created_at_index ON diary_comments USING btree (user_id, created_at);


--
-- Name: diary_comments_entry_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX diary_comments_entry_id_idx ON diary_comments USING btree (diary_entry_id, id);


--
-- Name: diary_entry_created_at_index; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX diary_entry_created_at_index ON diary_entries USING btree (created_at);


--
-- Name: diary_entry_language_code_created_at_index; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX diary_entry_language_code_created_at_index ON diary_entries USING btree (language_code, created_at);


--
-- Name: diary_entry_user_id_created_at_index; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX diary_entry_user_id_created_at_index ON diary_entries USING btree (user_id, created_at);


--
-- Name: friends_user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX friends_user_id_idx ON friends USING btree (user_id);


--
-- Name: gpx_file_tags_gpxid_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX gpx_file_tags_gpxid_idx ON gpx_file_tags USING btree (gpx_id);


--
-- Name: gpx_file_tags_tag_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX gpx_file_tags_tag_idx ON gpx_file_tags USING btree (tag);


--
-- Name: gpx_files_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX gpx_files_timestamp_idx ON gpx_files USING btree ("timestamp");


--
-- Name: gpx_files_user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX gpx_files_user_id_idx ON gpx_files USING btree (user_id);


--
-- Name: gpx_files_visible_visibility_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX gpx_files_visible_visibility_idx ON gpx_files USING btree (visible, visibility);


--
-- Name: index_client_applications_on_key; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_client_applications_on_key ON client_applications USING btree (key);


--
-- Name: index_note_comments_on_body; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX index_note_comments_on_body ON note_comments USING gin (to_tsvector('english'::regconfig, body));


--
-- Name: index_note_comments_on_created_at; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX index_note_comments_on_created_at ON note_comments USING btree (created_at);


--
-- Name: index_oauth_nonces_on_nonce_and_timestamp; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_oauth_nonces_on_nonce_and_timestamp ON oauth_nonces USING btree (nonce, "timestamp");


--
-- Name: index_oauth_tokens_on_token; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_oauth_tokens_on_token ON oauth_tokens USING btree (token);


--
-- Name: index_user_blocks_on_user_id; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX index_user_blocks_on_user_id ON user_blocks USING btree (user_id);


--
-- Name: messages_from_user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX messages_from_user_id_idx ON messages USING btree (from_user_id);


--
-- Name: messages_to_user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX messages_to_user_id_idx ON messages USING btree (to_user_id);


--
-- Name: nodes_changeset_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX nodes_changeset_id_idx ON nodes USING btree (changeset_id);


--
-- Name: nodes_tile_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX nodes_tile_idx ON nodes USING btree (tile);


--
-- Name: nodes_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX nodes_timestamp_idx ON nodes USING btree ("timestamp");


--
-- Name: note_comments_note_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX note_comments_note_id_idx ON note_comments USING btree (note_id);


--
-- Name: notes_created_at_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX notes_created_at_idx ON notes USING btree (created_at);


--
-- Name: notes_tile_status_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX notes_tile_status_idx ON notes USING btree (tile, status);


--
-- Name: notes_updated_at_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX notes_updated_at_idx ON notes USING btree (updated_at);


--
-- Name: points_gpxid_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX points_gpxid_idx ON gps_points USING btree (gpx_id);


--
-- Name: points_tile_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX points_tile_idx ON gps_points USING btree (tile);


--
-- Name: relation_members_member_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX relation_members_member_idx ON relation_members USING btree (member_type, member_id);


--
-- Name: relations_changeset_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX relations_changeset_id_idx ON relations USING btree (changeset_id);


--
-- Name: relations_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX relations_timestamp_idx ON relations USING btree ("timestamp");


--
-- Name: unique_schema_migrations; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX unique_schema_migrations ON schema_migrations USING btree (version);


--
-- Name: user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX user_id_idx ON friends USING btree (friend_user_id);


--
-- Name: user_openid_url_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX user_openid_url_idx ON users USING btree (openid_url);


--
-- Name: user_roles_id_role_unique; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX user_roles_id_role_unique ON user_roles USING btree (user_id, role);


--
-- Name: user_tokens_token_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX user_tokens_token_idx ON user_tokens USING btree (token);


--
-- Name: user_tokens_user_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX user_tokens_user_id_idx ON user_tokens USING btree (user_id);


--
-- Name: users_display_name_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX users_display_name_idx ON users USING btree (display_name);


--
-- Name: users_display_name_lower_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX users_display_name_lower_idx ON users USING btree (lower((display_name)::text));


--
-- Name: users_email_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX users_email_idx ON users USING btree (email);


--
-- Name: users_email_lower_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX users_email_lower_idx ON users USING btree (lower((email)::text));


--
-- Name: way_nodes_node_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX way_nodes_node_idx ON way_nodes USING btree (node_id);


--
-- Name: ways_changeset_id_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX ways_changeset_id_idx ON ways USING btree (changeset_id);


--
-- Name: ways_timestamp_idx; Type: INDEX; Schema: osm_test; Owner: -; Tablespace: 
--

CREATE INDEX ways_timestamp_idx ON ways USING btree ("timestamp");


SET search_path = public, pg_catalog;

--
-- Name: geometry_columns_delete; Type: RULE; Schema: public; Owner: -
--

CREATE RULE geometry_columns_delete AS
    ON DELETE TO geometry_columns DO INSTEAD NOTHING;


--
-- Name: geometry_columns_insert; Type: RULE; Schema: public; Owner: -
--

CREATE RULE geometry_columns_insert AS
    ON INSERT TO geometry_columns DO INSTEAD NOTHING;


--
-- Name: geometry_columns_update; Type: RULE; Schema: public; Owner: -
--

CREATE RULE geometry_columns_update AS
    ON UPDATE TO geometry_columns DO INSTEAD NOTHING;


SET search_path = osm_test, pg_catalog;

--
-- Name: changeset_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY changeset_tags
    ADD CONSTRAINT changeset_tags_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: changesets_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY changesets
    ADD CONSTRAINT changesets_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: client_applications_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY client_applications
    ADD CONSTRAINT client_applications_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: current_node_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_node_tags
    ADD CONSTRAINT current_node_tags_id_fkey FOREIGN KEY (node_id) REFERENCES current_nodes(id);


--
-- Name: current_nodes_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_nodes
    ADD CONSTRAINT current_nodes_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: current_relation_members_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_relation_members
    ADD CONSTRAINT current_relation_members_id_fkey FOREIGN KEY (relation_id) REFERENCES current_relations(id);


--
-- Name: current_relation_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_relation_tags
    ADD CONSTRAINT current_relation_tags_id_fkey FOREIGN KEY (relation_id) REFERENCES current_relations(id);


--
-- Name: current_relations_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_relations
    ADD CONSTRAINT current_relations_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: current_way_nodes_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_way_nodes
    ADD CONSTRAINT current_way_nodes_id_fkey FOREIGN KEY (way_id) REFERENCES current_ways(id);


--
-- Name: current_way_nodes_node_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_way_nodes
    ADD CONSTRAINT current_way_nodes_node_id_fkey FOREIGN KEY (node_id) REFERENCES current_nodes(id);


--
-- Name: current_way_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_way_tags
    ADD CONSTRAINT current_way_tags_id_fkey FOREIGN KEY (way_id) REFERENCES current_ways(id);


--
-- Name: current_ways_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY current_ways
    ADD CONSTRAINT current_ways_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: diary_comments_diary_entry_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_comments
    ADD CONSTRAINT diary_comments_diary_entry_id_fkey FOREIGN KEY (diary_entry_id) REFERENCES diary_entries(id);


--
-- Name: diary_comments_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_comments
    ADD CONSTRAINT diary_comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: diary_entries_language_code_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_entries
    ADD CONSTRAINT diary_entries_language_code_fkey FOREIGN KEY (language_code) REFERENCES languages(code);


--
-- Name: diary_entries_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY diary_entries
    ADD CONSTRAINT diary_entries_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: friends_friend_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY friends
    ADD CONSTRAINT friends_friend_user_id_fkey FOREIGN KEY (friend_user_id) REFERENCES users(id);


--
-- Name: friends_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY friends
    ADD CONSTRAINT friends_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: gps_points_gpx_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY gps_points
    ADD CONSTRAINT gps_points_gpx_id_fkey FOREIGN KEY (gpx_id) REFERENCES gpx_files(id);


--
-- Name: gpx_file_tags_gpx_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY gpx_file_tags
    ADD CONSTRAINT gpx_file_tags_gpx_id_fkey FOREIGN KEY (gpx_id) REFERENCES gpx_files(id);


--
-- Name: gpx_files_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY gpx_files
    ADD CONSTRAINT gpx_files_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: messages_from_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_from_user_id_fkey FOREIGN KEY (from_user_id) REFERENCES users(id);


--
-- Name: messages_to_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY messages
    ADD CONSTRAINT messages_to_user_id_fkey FOREIGN KEY (to_user_id) REFERENCES users(id);


--
-- Name: node_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY node_tags
    ADD CONSTRAINT node_tags_id_fkey FOREIGN KEY (node_id, version) REFERENCES nodes(node_id, version);


--
-- Name: nodes_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: nodes_redaction_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_redaction_id_fkey FOREIGN KEY (redaction_id) REFERENCES redactions(id);


--
-- Name: note_comments_author_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY note_comments
    ADD CONSTRAINT note_comments_author_id_fkey FOREIGN KEY (author_id) REFERENCES users(id);


--
-- Name: note_comments_note_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY note_comments
    ADD CONSTRAINT note_comments_note_id_fkey FOREIGN KEY (note_id) REFERENCES notes(id);


--
-- Name: oauth_tokens_client_application_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY oauth_tokens
    ADD CONSTRAINT oauth_tokens_client_application_id_fkey FOREIGN KEY (client_application_id) REFERENCES client_applications(id);


--
-- Name: oauth_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY oauth_tokens
    ADD CONSTRAINT oauth_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: redactions_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY redactions
    ADD CONSTRAINT redactions_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: relation_members_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY relation_members
    ADD CONSTRAINT relation_members_id_fkey FOREIGN KEY (relation_id, version) REFERENCES relations(relation_id, version);


--
-- Name: relation_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY relation_tags
    ADD CONSTRAINT relation_tags_id_fkey FOREIGN KEY (relation_id, version) REFERENCES relations(relation_id, version);


--
-- Name: relations_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: relations_redaction_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_redaction_id_fkey FOREIGN KEY (redaction_id) REFERENCES redactions(id);


--
-- Name: user_blocks_moderator_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_blocks
    ADD CONSTRAINT user_blocks_moderator_id_fkey FOREIGN KEY (creator_id) REFERENCES users(id);


--
-- Name: user_blocks_revoker_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_blocks
    ADD CONSTRAINT user_blocks_revoker_id_fkey FOREIGN KEY (revoker_id) REFERENCES users(id);


--
-- Name: user_blocks_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_blocks
    ADD CONSTRAINT user_blocks_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: user_preferences_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_preferences
    ADD CONSTRAINT user_preferences_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: user_roles_granter_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_roles
    ADD CONSTRAINT user_roles_granter_id_fkey FOREIGN KEY (granter_id) REFERENCES users(id);


--
-- Name: user_roles_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_roles
    ADD CONSTRAINT user_roles_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: user_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY user_tokens
    ADD CONSTRAINT user_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: way_nodes_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY way_nodes
    ADD CONSTRAINT way_nodes_id_fkey FOREIGN KEY (way_id, version) REFERENCES ways(way_id, version);


--
-- Name: way_tags_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY way_tags
    ADD CONSTRAINT way_tags_id_fkey FOREIGN KEY (way_id, version) REFERENCES ways(way_id, version);


--
-- Name: ways_changeset_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY ways
    ADD CONSTRAINT ways_changeset_id_fkey FOREIGN KEY (changeset_id) REFERENCES changesets(id);


--
-- Name: ways_redaction_id_fkey; Type: FK CONSTRAINT; Schema: osm_test; Owner: -
--

ALTER TABLE ONLY ways
    ADD CONSTRAINT ways_redaction_id_fkey FOREIGN KEY (redaction_id) REFERENCES redactions(id);


--
-- PostgreSQL database dump complete
--

SET search_path TO osm_test,public;

INSERT INTO schema_migrations (version) VALUES ('1');

INSERT INTO schema_migrations (version) VALUES ('10');

INSERT INTO schema_migrations (version) VALUES ('11');

INSERT INTO schema_migrations (version) VALUES ('12');

INSERT INTO schema_migrations (version) VALUES ('13');

INSERT INTO schema_migrations (version) VALUES ('14');

INSERT INTO schema_migrations (version) VALUES ('15');

INSERT INTO schema_migrations (version) VALUES ('16');

INSERT INTO schema_migrations (version) VALUES ('17');

INSERT INTO schema_migrations (version) VALUES ('18');

INSERT INTO schema_migrations (version) VALUES ('19');

INSERT INTO schema_migrations (version) VALUES ('2');

INSERT INTO schema_migrations (version) VALUES ('20');

INSERT INTO schema_migrations (version) VALUES ('20100513171259');

INSERT INTO schema_migrations (version) VALUES ('20100516124737');

INSERT INTO schema_migrations (version) VALUES ('20100910084426');

INSERT INTO schema_migrations (version) VALUES ('20101114011429');

INSERT INTO schema_migrations (version) VALUES ('20110322001319');

INSERT INTO schema_migrations (version) VALUES ('20110508145337');

INSERT INTO schema_migrations (version) VALUES ('20110521142405');

INSERT INTO schema_migrations (version) VALUES ('20110925112722');

INSERT INTO schema_migrations (version) VALUES ('20111116184519');

INSERT INTO schema_migrations (version) VALUES ('20111212183945');

INSERT INTO schema_migrations (version) VALUES ('20120123184321');

INSERT INTO schema_migrations (version) VALUES ('20120208122334');

INSERT INTO schema_migrations (version) VALUES ('20120208194454');

INSERT INTO schema_migrations (version) VALUES ('20120214210114');

INSERT INTO schema_migrations (version) VALUES ('20120219161649');

INSERT INTO schema_migrations (version) VALUES ('20120318201948');

INSERT INTO schema_migrations (version) VALUES ('20120328090602');

INSERT INTO schema_migrations (version) VALUES ('20120404205604');

INSERT INTO schema_migrations (version) VALUES ('20120808231205');

INSERT INTO schema_migrations (version) VALUES ('20121005195010');

INSERT INTO schema_migrations (version) VALUES ('20121012044047');

INSERT INTO schema_migrations (version) VALUES ('20121119165817');

INSERT INTO schema_migrations (version) VALUES ('20121202155309');

INSERT INTO schema_migrations (version) VALUES ('20121203124841');

INSERT INTO schema_migrations (version) VALUES ('20130328184137');

INSERT INTO schema_migrations (version) VALUES ('20131212124700');

INSERT INTO schema_migrations (version) VALUES ('20140115192822');

INSERT INTO schema_migrations (version) VALUES ('20140117185510');

INSERT INTO schema_migrations (version) VALUES ('20140210003018');

INSERT INTO schema_migrations (version) VALUES ('21');

INSERT INTO schema_migrations (version) VALUES ('22');

INSERT INTO schema_migrations (version) VALUES ('23');

INSERT INTO schema_migrations (version) VALUES ('24');

INSERT INTO schema_migrations (version) VALUES ('25');

INSERT INTO schema_migrations (version) VALUES ('26');

INSERT INTO schema_migrations (version) VALUES ('27');

INSERT INTO schema_migrations (version) VALUES ('28');

INSERT INTO schema_migrations (version) VALUES ('29');

INSERT INTO schema_migrations (version) VALUES ('3');

INSERT INTO schema_migrations (version) VALUES ('30');

INSERT INTO schema_migrations (version) VALUES ('31');

INSERT INTO schema_migrations (version) VALUES ('32');

INSERT INTO schema_migrations (version) VALUES ('33');

INSERT INTO schema_migrations (version) VALUES ('34');

INSERT INTO schema_migrations (version) VALUES ('35');

INSERT INTO schema_migrations (version) VALUES ('36');

INSERT INTO schema_migrations (version) VALUES ('37');

INSERT INTO schema_migrations (version) VALUES ('38');

INSERT INTO schema_migrations (version) VALUES ('39');

INSERT INTO schema_migrations (version) VALUES ('4');

INSERT INTO schema_migrations (version) VALUES ('40');

INSERT INTO schema_migrations (version) VALUES ('41');

INSERT INTO schema_migrations (version) VALUES ('42');

INSERT INTO schema_migrations (version) VALUES ('43');

INSERT INTO schema_migrations (version) VALUES ('44');

INSERT INTO schema_migrations (version) VALUES ('45');

INSERT INTO schema_migrations (version) VALUES ('46');

INSERT INTO schema_migrations (version) VALUES ('47');

INSERT INTO schema_migrations (version) VALUES ('48');

INSERT INTO schema_migrations (version) VALUES ('49');

INSERT INTO schema_migrations (version) VALUES ('5');

INSERT INTO schema_migrations (version) VALUES ('50');

INSERT INTO schema_migrations (version) VALUES ('51');

INSERT INTO schema_migrations (version) VALUES ('52');

INSERT INTO schema_migrations (version) VALUES ('53');

INSERT INTO schema_migrations (version) VALUES ('54');

INSERT INTO schema_migrations (version) VALUES ('55');

INSERT INTO schema_migrations (version) VALUES ('56');

INSERT INTO schema_migrations (version) VALUES ('57');

INSERT INTO schema_migrations (version) VALUES ('6');

INSERT INTO schema_migrations (version) VALUES ('7');

INSERT INTO schema_migrations (version) VALUES ('8');

INSERT INTO schema_migrations (version) VALUES ('9');

