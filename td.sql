.
REM
REM The script simulates treedump event behaviour rendering
REM the leaf block -> table structure.
REM
REM It is provided for illustrative purpose. The script is not
REM meant to be used to dump structures of huge indexes or be
REM part of any production infrastructure.
REM
REM All utility scripts, code snippets, ideas on algorithms,
REM data access and processing imply NO WARRANTY and can be
REM used "AS IS" with appropriate credits. :-)
REM
REM Usage: td index_name [owner_name]
REM
REM Due to the bug#9036013 - result of DECODE() may be incorrectly
REM interpreted as NULL; the script may not work correctly on
REM un-patched 11.2.0.1.0 environments. Consider applying the fix
REM cause the bug may break some business logic of your application.
REM
REM Author: Vladimir Begun (http://vbegun.blogspot.com/)
REM Version 0.3
REM

SET TERMOUT OFF
REM Saving the original SQL*Plus environment settings
STORE SET .td REPLACE

REM Settings
SET DEFINE "^" PAGES 0 TRIMSPOOL ON LINESIZE 32767 TAB OFF APPINFO ON
SET HEADING OFF FEEDBACK OFF ARRAYSIZE 1 LONG 1024000 ECHO OFF
SET VERIFY OFF SERVEROUTPUT ON SIZE 100000 FORMAT TRUNCATED TERMOUT ON

REM Columns and substitution variables
COLUMN td_____i NEW_VALUE td_____i NOPRINT
COLUMN td_____o FORMAT A30 NEW_VALUE td_____o NOPRINT
COLUMN td_____t FORMAT A30 NEW_VALUE td_____t NOPRINT
COLUMN td_____x FORMAT A30 NEW_VALUE td_____x NOPRINT
COLUMN td_____f FORMAT A1 NEW_VALUE td_____f NOPRINT
COLUMN td_____c FORMAT A1000 NEW_VALUE td_____c NOPRINT
COLUMN td_____v FORMAT A1000 NEW_VALUE td_____v NOPRINT
COLUMN td_____fn FORMAT A10 NEW_VALUE td_____fn NOPRINT
COLUMN td_____y FORMAT A30 NEW_VALUE td_____y NOPRINT
COLUMN td_____y1 FORMAT A30 NEW_VALUE td_____y1 NOPRINT
COLUMN td_____banner FORMAT A60 HEADING OFF
COLUMN td_____irid NOPRINT
BREAK ON td_____irid NODUP SKIP 1

DEFINE td_____c = NULL
DEFINE td_____v = NULL
DEFINE td_____i = NULL
DEFINE td_____fn = SYS_OP_LBID

REM Command line handling
SET TERMOUT OFF
COLUMN 1 NEW_VALUE 1
COLUMN 2 NEW_VALUE 2
SELECT '' AS "1", '' AS "2"
  FROM dual
 WHERE ROWNUM = 0
/
SELECT UPPER('^^1') td_____x
     , UPPER(NVL('^^2', SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA'))) td_____o
  FROM dual
/
COLUMN td_____script NEW_VALUE td_____script
SELECT COALESCE(
         REGEXP_SUBSTR(
           TRIM(SYS_CONTEXT('USERENV', 'MODULE'))
         , '^[[:digit:]]{2}@[< ](.*(/|\\))*(.*)$'
         , 1, 1, 'i', 3
         )
       , 'units.sql'
       ) td_____script
  FROM dual
/
SET TERMOUT ON
SELECT 'Usage: ^^td_____script index_name [owner_name]' td_____banner
  FROM dual
 WHERE '^^1' IS NULL
/
REM Gathering of index/table information from the data dictionary.
REM It limits the number of indexes which are subject of dumping.
SELECT TO_CHAR(o.object_id, 'FM9999999') td_____i
     , dbms_assert.schema_name(i.table_owner) td_____o
     , dbms_assert.enquote_name(i.table_name) td_____t
     , RTRIM(
         LTRIM(
           REPLACE(
             dbms_xmlgen.convert(
               XMLTYPE.GetStringVal(
                 XMLAGG(
                   XMLELEMENT(
                     "c"
                   , dbms_assert.enquote_name(column_name)
                  || ' '
                  || descend
                   )
                 ORDER BY column_position
                 )
               )
             , 1
             )
           , '</c><c>'
           , ','
           )
         , '</c>'
         )
       , '</c>'
       ) td_____c
     , RTRIM(
         LTRIM(
           REPLACE(
             dbms_xmlgen.convert(
               XMLTYPE.GetStringVal(
                 XMLAGG(
                   XMLELEMENT(
                     "c"
                   , dbms_assert.enquote_name(column_name)
                   )
                 ORDER BY column_position
                 )
               )
             , 1
             )
           , '</c><c>'
           , ','
           )
         , '</c>'
         )
       , '</c>'
       ) td_____v
     , CASE WHEN index_type LIKE '%NORMAL%' AND t.iot_type IS NULL THEN index_type END td_____y
     , index_type td_____y1
  FROM all_objects o
     , all_indexes i
     , all_ind_columns aic
     , all_tables t
 WHERE o.object_name = UPPER('^^td_____x')
   AND o.owner = UPPER('^^td_____o')
   AND o.object_name = i.index_name
   AND o.owner = i.owner
   AND o.object_type = 'INDEX'
   AND aic.index_owner = i.owner
   AND aic.index_name = i.index_name
   AND aic.table_owner = i.table_owner
   AND aic.table_name = i.table_name
   AND t.owner = i.table_owner
   AND t.table_name = i.table_name
 GROUP BY
       o.object_id
     , i.table_owner
     , i.table_name
     , i.index_type
     , t.iot_type
/
REM Handling of non-existing indexes
SELECT 'td: index "^^td_____o"."^^td_____x" does not exist' td_____banner
  FROM dual
 WHERE '^^td_____i' = 'NULL' AND '^^1' IS NOT NULL
/
SELECT 'DECODE' td_____fn, 'SYS' td_____o, 'DUAL' td_____t
  FROM dual
 WHERE '^^td_____i' = 'NULL'
/
SET PAGES 10000 HEADING ON
COLUMN td_____treedump FORMAT A71 -
HEADING 'Treedump of ^^td_____x [^^td_____i] (^^td_____y1) on|^^td_____o..^^td_____t||  index                                        table                  |file block              dba        -> file block     row  (keys/block)' -
JUSTIFY CENTER

REM Actual dumping
WITH i AS (
  SELECT /*+
           MATERIALIZE
           CURSOR_SHARING_EXACT
           NO_MONITORING
           DYNAMIC_SAMPLING(0)
           INDEX_FFS(t ^^td_____x)
           NOPARALLEL_INDEX(t ^^td_____x)
         */
         ROWID rid
       , ^^td_____fn(^^td_____i, 'L', ROWID) irid
       , dbms_rowid.rowid_relative_fno(^^td_____fn(^^td_____i, 'L', ROWID)) irid_f
       , dbms_rowid.rowid_block_number(^^td_____fn(^^td_____i, 'L', ROWID)) irid_b
    FROM ^^td_____o..^^td_____t t
   WHERE '^^td_____i' <> 'NULL'
)
SELECT i.irid td_____irid
     , TO_CHAR(irid_f, 'FM0009')
    || '.'
    || TO_CHAR(irid_b, 'FM00000009')
    || ' ('
    ||  '0x'
    || TO_CHAR(dbms_utility.make_data_block_address(irid_f, irid_b), 'FM0000000x')
    || '/'
    ||  dbms_utility.make_data_block_address(irid_f, irid_b)
    || ')'
    || ' -> '
    || TO_CHAR(dbms_rowid.rowid_relative_fno(NVL2('^^td_____y', rid, NULL)), 'FM0009')
    || '.'
    || TO_CHAR(dbms_rowid.rowid_block_number(NVL2('^^td_____y', rid, NULL)), 'FM00000009')
    || '.'
    || TO_CHAR(dbms_rowid.rowid_row_number(NVL2('^^td_____y', rid, NULL)), 'FM00009')
/*
    || TO_CHAR(dbms_rowid.rowid_relative_fno(DECODE('^^td_____y', NULL, NULL, rid)), 'FM0009')
    || '.'
    || TO_CHAR(dbms_rowid.rowid_block_number(DECODE('^^td_____y', NULL, NULL, rid)), 'FM00000009')
    || '.'
    || TO_CHAR(dbms_rowid.rowid_row_number(DECODE('^^td_____y', NULL, NULL, rid)), 'FM00009')
*/
    || ' ('
    || TO_CHAR(ROW_NUMBER() OVER (PARTITION BY i.irid ORDER BY ^^td_____c, rid), 'FM00009')
    || '/'
    || TO_CHAR(COUNT(*) OVER (PARTITION BY i.irid), 'FM0009')
    || ')' td_____treedump
     , ^^td_____v
  FROM i
     , ^^td_____o..^^td_____t t
 WHERE i.rid = t.ROWID
 ORDER BY
       ^^td_____c
     , rid
     , i.irid
/
REM Cleanup and restoration of the original SQL*Plus environment
REPHEADER OFF
@.td
UNDEFINE 1 2 td_____i td_____o td_____t td_____x td_____f
UNDEFINE td_____c td_____v td_____fn td_____y td_____y1
COLUMN 1 CLEAR
COLUMN 2 CLEAR
COLUMN td_____i CLEAR
COLUMN td_____o CLEAR
COLUMN td_____t CLEAR
COLUMN td_____x CLEAR
COLUMN td_____f CLEAR
COLUMN td_____c CLEAR
COLUMN td_____v CLEAR
COLUMN td_____y CLEAR
COLUMN td_____y1 CLEAR
COLUMN td_____banner CLEAR
COLUMN td_____fn CLEAR
COLUMN td_____treedump CLEAR
COLUMN td_____irid CLEAR
COLUMN td_____script CLEAR
SET TERMOUT ON
