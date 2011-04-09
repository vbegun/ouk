.
REM
REM [Time] Unit conversion script
REM
REM All utility scripts, code snippets, ideas on algorithms,
REM data access and processing imply NO WARRANTY and can be
REM used "AS IS" with appropriate credits. :-)
REM
REM Usage: units [n] <from time unit> [[to] <to time unit>]
REM
REM Author: Vladimir Begun (http://vbegun.blogspot.com/)
REM Version 0.1
REM

SET SERVEROUTPUT ON SIZE 1000000 FORMAT TRUNCATED VERIFY OFF
SET DEFINE "^" APPINFO ON LINESIZE 120 ECHO OFF TERMOUT ON
SET FEEDBACK OFF PAGESIZE 0 TRIMSPOOL ON

SET TERMOUT OFF
COLUMN 1 NEW_VALUE 1
COLUMN 2 NEW_VALUE 2
COLUMN 3 NEW_VALUE 3
COLUMN 4 NEW_VALUE 4
SELECT '' AS "1", '' AS "2", '' AS "3", '' AS "4"
  FROM dual
 WHERE ROWNUM = 0
/
COLUMN u_____script NEW_VALUE u_____script
SELECT COALESCE(
         REGEXP_SUBSTR(
           TRIM(SYS_CONTEXT('USERENV', 'MODULE'))
         , '^[[:digit:]]{2}@[< ](.*(/|\\))*(.*)$'
         , 1, 1, 'i', 3
         )
       , 'units.sql'
       ) u_____script
  FROM dual
/
VAR u_____f VARCHAR2(256)
VAR u_____t VARCHAR2(256)
VAR u_____n NUMBER
DEFINE u_____dd = "days day dd d"
DEFINE u_____hh = "hours hour hh h"
DEFINE u_____mi = "minutes minute min mi"
DEFINE u_____ss = "seconds second secs sec s"
DEFINE u_____cs = "centiseconds centisecond centi cen cs"
DEFINE u_____ms = "milliseconds millisecond milli mill mil ms"
DEFINE u_____us = "microseconds microsecond micro mic us"
DEFINE u_____ua = 'FROM dual UNION ALL'

SET TERMOUT ON
DECLARE
  PROCEDURE p (
    l_message                      IN VARCHAR2
  , l_crlf                         IN BOOLEAN DEFAULT TRUE
  )
  IS
  BEGIN
    IF (l_crlf)
    THEN
      dbms_output.put_line(l_message);
    ELSE
      dbms_output.put(l_message);
    END IF;
  END p;

  PROCEDURE el
  IS
  BEGIN
    dbms_output.put_line('');
  END el;

  PROCEDURE usage
  IS
  BEGIN
    p('Unit conversion script [pathetically :-) mimicking units (1)].');
    el;
    p('Usage: ^^u_____script [n] <from time unit> [[to] <to time unit>]');
    p('Example:');
    p('  @^^u_____script sec to us');
    p('  1 second(s) = 1000000 microsecond(s)');
    el();
    p('  @^^u_____script 15 milliseconds second');
    p('  15 millisecond(s) = .015 second(s)');
    el();
    p('  @^^u_____script 30 seconds to day');
    p('  30 second(s) = .000347222 day(s) [*.00001157407]');
    el();
    p('Supported units: us, ms, cs, ss, mi, hh, dd');
    el;
  END usage;

BEGIN
  BEGIN
    :u_____n := TO_NUMBER('^1');
  EXCEPTION
    WHEN OTHERS
    THEN NULL;
  END;
  IF (:u_____n IS NULL)
  THEN
    :u_____f := LOWER('^1');
    IF (LOWER('^2') = 'to')
    THEN
      :u_____t := LOWER('^3');
    ELSE
      :u_____t := LOWER('^2');
    END IF;
  ELSE
    :u_____f := LOWER('^2');
    IF (LOWER('^3') = 'to')
    THEN
      :u_____t := LOWER('^4');
    ELSE
      :u_____t := LOWER('^3');
    END IF;
  END IF;
  IF (:u_____f IS NULL)
  THEN
    usage();
    :u_____f := 'N/A';
  END IF;
  :u_____n := COALESCE(:u_____n, 1);
  :u_____t := COALESCE(:u_____t, 's');
END;
/

WITH units (t, f, factor) AS (
  -- each day is unique, spend it wisely and happily
  SELECT '^^u_____dd', '^^u_____dd', 1 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____hh', 1/24 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____mi', 1/1440 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____ss', 1/86400 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____cs', 0.01/86400 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____ms', 0.001/86400 ^^u_____ua
  SELECT '^^u_____dd', '^^u_____us', 0.000001/86400 ^^u_____ua
  -- because happy people never count hours as they pass
  SELECT '^^u_____hh', '^^u_____dd', 24 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____hh', 1 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____mi', 1/60 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____ss', 1/3600 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____cs', 0.01/3600 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____ms', 0.001/3600 ^^u_____ua
  SELECT '^^u_____hh', '^^u_____us', 0.000001/3600 ^^u_____ua
  -- in one second 6,242,000,000,000,000,000 electrons pass any given
  -- point in an electrical current. how many is per minute? prove
  -- you are a happy human being -- count `em! :-)
  SELECT '^^u_____mi', '^^u_____dd', 1440 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____hh', 60 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____mi', 1 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____ss', 1/60 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____cs', 0.01/60 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____ms', 0.001/60 ^^u_____ua
  SELECT '^^u_____mi', '^^u_____us', 0.000001/60 ^^u_____ua
  -- at least 100,000 different chemical reactions occur in the normal
  -- human brain every second. looks like you are counting... and proving
  -- that you have a normal human brain :-)
  SELECT '^^u_____ss', '^^u_____dd', 86400 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____hh', 3600 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____mi', 60 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____ss', 1 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____cs', 0.01 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____ms', 0.001 ^^u_____ua
  SELECT '^^u_____ss', '^^u_____us', 0.000001 ^^u_____ua
  -- 20cs the time it takes the human brain to recognize emotion in
  -- facial expressions and 30-40cs the blink of a human eye.
  -- wink-wink ;)
  SELECT '^^u_____cs', '^^u_____dd', 8640000 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____hh', 360000 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____mi', 6000 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____ss', 100 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____cs', 1 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____ms', 0.1 ^^u_____ua
  SELECT '^^u_____cs', '^^u_____us', 0.0001 ^^u_____ua
  -- according to some robots: 4ms typical average seek time
  -- for a 10,000 rpm hard disk. how many electrons have you
  -- counted?
  SELECT '^^u_____ms', '^^u_____dd', 86400000 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____hh', 3600000 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____mi', 60000 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____ss', 1000 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____cs', 10 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____ms', 1 ^^u_____ua
  SELECT '^^u_____ms', '^^u_____us', 0.001 ^^u_____ua
  -- 3.33564095us the time taken by light to travel one kilometer
  -- in a vacuum... however, there are robots traveling ten times
  -- faster
  SELECT '^^u_____us', '^^u_____dd', 86400000000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____hh', 3600000000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____mi', 60000000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____ss', 1000000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____cs', 10000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____ms', 1000 ^^u_____ua
  SELECT '^^u_____us', '^^u_____us', 1 FROM dual
)
SELECT :u_____n
    || ' '
    || SUBSTR(f, 1, INSTR(f, ' ') - 2)
    || '(s) = '
    || TO_CHAR(:u_____n * factor, 'TM9')
    || ' '
    || SUBSTR(t, 1, INSTR(t, ' ') - 2)
    || '(s)'
    || ' [*' || TO_CHAR(factor, 'TM9') || ']'
  FROM units
 WHERE ' ' || f || ' ' LIKE '% ' || :u_____f || ' %'
   AND ' ' || t || ' ' LIKE '% ' || :u_____t || ' %'
/
UNDEFINE 1 2 3 4 u_____script u_____dd u_____hh u_____mi
UNDEFINE u_____ss u_____cs u_____ms u_____us u_____ua
COLUMN u_____script CLEAR
COLUMN 1 CLEAR
COLUMN 2 CLEAR
COLUMN 3 CLEAR
COLUMN 4 CLEAR
