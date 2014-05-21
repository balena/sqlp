-- MODULE  DML160  

-- SQL Test Suite, V6.0, Interactive SQL, dml160.sql
-- 59-byte ID
-- TEd Version #

-- AUTHORIZATION FLATER            

   SELECT USER FROM HU.ECCO;
-- RERUN if USER value does not match preceding AUTHORIZATION comment
   ROLLBACK WORK;

-- date_time print

-- TEST:0859 <joined table> contained in <select list>!

   SELECT EMPNUM, (SELECT COUNT(*) FROM HU.WORKS JOIN HU.PROJ
     ON HU.WORKS.PNUM = HU.PROJ.PNUM
     AND BUDGET > AVG (OSTAFF.GRADE) * 1000
     WHERE HU.WORKS.EMPNUM = OSTAFF.EMPNUM) FROM HU.STAFF AS OSTAFF
     ORDER BY 2, 1;
-- PASS:0859 If 5 rows are returned in the following order?
--               empnum   count
--               ======   =====
-- PASS:0859 If    E5       0  ?
-- PASS:0859 If    E2       1  ?
-- PASS:0859 If    E3       1  ?
-- PASS:0859 If    E4       2  ?
-- PASS:0859 If    E1       4  ?  

   COMMIT WORK;

-- END TEST >>> 0859 <<< END TEST
-- *********************************************

-- TEST:0860 Domains over various data types!

   CREATE DOMAIN EPOCH_NOT_NORM AS DECIMAL (5, 2);
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE DOMAIN RAD_EPOCH_TYPE FLOAT (20)
     CHECK (VALUE BETWEEN 0E0 AND 2E0 * 3.1416E0);
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE DOMAIN RAD_EPOCH_NOT_NORM REAL;
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE DOMAIN TIDEDATE AS DATE
    CHECK (VALUE BETWEEN DATE '1994-01-01' AND DATE '2025-12-31');
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE DOMAIN TIDETIMESTAMP AS TIMESTAMP WITH TIME ZONE
    CHECK (VALUE BETWEEN TIMESTAMP '1994-01-01 00:00:00+00:00'
    AND TIMESTAMP '2025-12-31 23:59:59+00:00');
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE DOMAIN DINNERTIME AS TIME
    CHECK (VALUE BETWEEN TIME '17:30:00' AND TIME '19:00:00');
-- PASS:0860 If domain created successfully?

   COMMIT WORK;

   CREATE TABLE CONST_NOT_NORM (
     LOC_ID DEC (7) NOT NULL,
     CONST_ID TIDES.CONST_ID_TYPE NOT NULL,
     UNIQUE (LOC_ID, CONST_ID),
     AMPLITUDE TIDES.AMPLITUDE_TYPE,
     EPOCH EPOCH_NOT_NORM);
-- PASS:0860 If table created successfully?

   COMMIT WORK;

   CREATE VIEW CONST_RAD (LOC_ID, CONST_ID,
     AMPLITUDE, EPOCH) AS
     SELECT LOC_ID, CONST_ID, AMPLITUDE,
     CAST (EPOCH * 3.14159265358979E0 / 180E0 AS RAD_EPOCH_TYPE)
     FROM TIDES.CONSTITUENTS;
-- PASS:0860 If view created successfully?

   COMMIT WORK;

   CREATE VIEW CONST_RAD_NOT_NORM (LOC_ID, CONST_ID,
     AMPLITUDE, EPOCH) AS
     SELECT LOC_ID, CONST_ID, AMPLITUDE,
     CAST (EPOCH * 3.14159265358979E0 / 180E0 AS RAD_EPOCH_NOT_NORM)
     FROM CONST_NOT_NORM;
-- PASS:0860 If view created successfully?

   COMMIT WORK;

   CREATE TABLE PENDING (
     LOC_ID DEC (7) NOT NULL,
     FROMTIME TIDETIMESTAMP NOT NULL,
     TOTIME TIDETIMESTAMP NOT NULL,
     CHECK (FROMTIME <= TOTIME),
     JOB_ID INT PRIMARY KEY);
-- PASS:0860 If table created successfully?

   COMMIT WORK;

   CREATE VIEW CHECK_PTS (CHECK_DATES, JOB_ID, FLAG) AS
     SELECT CAST (FROMTIME AS TIDEDATE), JOB_ID,
     CAST (0 AS INT) FROM PENDING
       UNION
     SELECT CAST (TOTIME AS TIDEDATE), JOB_ID,
     CAST (1 AS INT) FROM PENDING;
-- PASS:0860 If view created successfully?

   COMMIT WORK;

   CREATE TABLE DINNER_CLUB (
     LOC_ID DEC (7) NOT NULL,
     DINNER DINNERTIME);
-- PASS:0860 If table created successfully?

   COMMIT WORK;

   SELECT EXTRACT (HOUR FROM MERIDIAN), EXTRACT
     (MINUTE FROM MERIDIAN) 
     FROM TIDES.LOCATIONS WHERE LOC_NAME LIKE '%Newfound%';
-- PASS:0860 If xhour = -3?
-- PASS:0860 If xminute = -30?

   INSERT INTO TIDES.LOCATIONS VALUES (
     300, 'Atlantis', 160.0000, 3.0000, 0, 1.2E0,
     INTERVAL -'13:00' HOUR TO MINUTE, 'GMT-13');
-- PASS:0860 If ERROR - integrity constraint violation?

   UPDATE TIDES.CONSTITUENTS
     SET AMPLITUDE = - AMPLITUDE
     WHERE LOC_ID = 100
     AND CONST_ID = 0;
-- PASS:0860 If ERROR - integrity constraint violation?

   INSERT INTO TIDES.LOCATIONS VALUES (300,
     'Bath, Maine', -69.8133, 43.9183,
     1, 3.422E0, INTERVAL '-05:00' HOUR TO MINUTE, ':US/Eastern');
-- PASS:0860 If 1 row inserted successfully?

   INSERT INTO TIDES.CONSTITUENTS VALUES (300, 2, 0.134E0, 385.0);
-- PASS:0860 If ERROR - integrity constraint violation?

   INSERT INTO CONST_NOT_NORM VALUES (300, 0, 0.021E0, 151.6);
   INSERT INTO CONST_NOT_NORM VALUES (300, 1, 0.324E0, 144.5);
   INSERT INTO CONST_NOT_NORM VALUES (300, 2, 0.134E0, 385.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 3, 0.181E0, 40.9);
   INSERT INTO CONST_NOT_NORM VALUES (300, 4, 0.037E0, 150.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 5, 3.143E0, 352.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 6, 0.000E0, 50.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 7, 0.104E0, 242.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 8, 0.031E0, 158.6);
   INSERT INTO CONST_NOT_NORM VALUES (300, 9, 0.000E0, 133.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 10, 0.744E0, 322.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 11, 0.087E0, 307.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 12, 0.260E0, 130.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 13, 0.011E0, 158.7);
   INSERT INTO CONST_NOT_NORM VALUES (300, 14, 0.107E0, 140.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 15, 0.043E0, 114.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 16, 0.007E0, 116.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 17, 0.004E0, 383.2);
   INSERT INTO CONST_NOT_NORM VALUES (300, 18, 0.000E0, 17.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 19, 0.488E0, 383.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 20, 0.000E0, 69.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 21, 0.000E0, 103.5);
   INSERT INTO CONST_NOT_NORM VALUES (300, 22, 0.053E0, 365.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 23, 0.053E0, 37.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 24, 0.023E0, 297.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 25, 0.138E0, 328.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 26, 0.010E0, 124.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 27, 0.000E0, 50.6);
   INSERT INTO CONST_NOT_NORM VALUES (300, 28, 0.000E0, 49.4);
   INSERT INTO CONST_NOT_NORM VALUES (300, 29, 0.000E0, 66.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 30, 0.000E0, 67.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 31, 0.000E0, 35.7);
   INSERT INTO CONST_NOT_NORM VALUES (300, 32, 0.073E0, 285.0);
   INSERT INTO CONST_NOT_NORM VALUES (300, 33, 0.033E0, 257.3);
   INSERT INTO CONST_NOT_NORM VALUES (300, 34, 0.000E0, 0.6);
   INSERT INTO CONST_NOT_NORM VALUES (300, 35, 0.056E0, 128.8);
   INSERT INTO CONST_NOT_NORM VALUES (300, 36, 0.038E0, 97.7);
-- PASS:0860 If 37 rows inserted from previous 37 inserts?

   SELECT EPOCH FROM CONST_RAD
     WHERE LOC_ID = 100
     AND CONST_ID = 0;
-- PASS:0860 If EPOCH = 2.11 (+ or - 0.01)?

   SELECT COUNT(*) 
     FROM CONST_RAD_NOT_NORM
     WHERE EPOCH > 6.2831853E0;
-- PASS:0860 If COUNT = 4?

   INSERT INTO PENDING VALUES (
     300, TIMESTAMP '1995-12-15 00:00:00-05:00',
          TIMESTAMP '1995-12-17 00:00:00-05:00', 0);
-- PASS:0860 If 1 row inserted successfully?

   INSERT INTO PENDING VALUES (
     101, TIMESTAMP '2025-12-30 19:00:00-05:00',
          TIMESTAMP '2025-12-31 19:00:00-05:00', 1);
-- PASS:0860 If ERROR - integrity constraint violation?

   INSERT INTO PENDING VALUES (
     101, TIMESTAMP '2025-12-30 19:00:00-05:00',
          TIMESTAMP '2025-12-31 18:59:59-05:00', 1);
-- PASS:0860 If 1 row inserted successfully?

   INSERT INTO PENDING VALUES (
     102, TIMESTAMP '1993-12-31 19:00:00-05:00',
       TIMESTAMP '1994-01-02 00:00:00-05:00', 2);
-- PASS:0860 If 1 row inserted successfully?

   SELECT EXTRACT (YEAR FROM CHECK_DATES)
     FROM CHECK_PTS WHERE JOB_ID = 2 AND FLAG = 0;
-- PASS:0860 If ERROR - integrity constraint violation?

   SELECT EXTRACT (YEAR FROM CHECK_DATES)
     FROM CHECK_PTS WHERE JOB_ID = 2 AND FLAG = 1;
-- PASS:0860 If xyear = 1994?

   INSERT INTO DINNER_CLUB VALUES
     (0, TIME '17:30:00');
-- PASS:0860 If 1 row inserted successfully?

   INSERT INTO DINNER_CLUB VALUES
     (1, CAST (TIME '18:00:00' AS DINNERTIME));
-- PASS:0860 If 1 row inserted successfully?

   INSERT INTO DINNER_CLUB VALUES
     (2, TIME '19:30:00');
-- PASS:0860 If ERROR - integrity constraint violation?

   COMMIT WORK;

   DROP DOMAIN EPOCH_NOT_NORM CASCADE;
   COMMIT WORK;
   DROP DOMAIN RAD_EPOCH_TYPE CASCADE;
   COMMIT WORK;
   DROP DOMAIN RAD_EPOCH_NOT_NORM CASCADE;
   COMMIT WORK;
   DROP DOMAIN TIDEDATE CASCADE;
   COMMIT WORK;
   DROP DOMAIN TIDETIMESTAMP CASCADE;
   COMMIT WORK;
   DROP DOMAIN DINNERTIME CASCADE;
   COMMIT WORK;
-- PASS:0860 If domains dropped successfully in 6 previous drops?

   DROP TABLE CONST_NOT_NORM CASCADE;
   COMMIT WORK;
   DROP VIEW CONST_RAD CASCADE;
   COMMIT WORK;
   DROP TABLE PENDING CASCADE;
   COMMIT WORK;
   DROP TABLE DINNER_CLUB CASCADE;
   COMMIT WORK;
-- PASS:0860 If tables and view dropped in 4 previous drops?

   DELETE FROM TIDES.LOCATIONS
     WHERE LOC_ID = 300;
-- PASS:0860 If delete completed successfully?

   COMMIT WORK;

-- END TEST >>> 0860 <<< END TEST
-- *********************************************
-- *************************************************////END-OF-MODULE
