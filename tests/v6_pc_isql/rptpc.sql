-- ***************************************************************
-- ******           This file should be run under           ******
-- ******    AUTHORIZATION ID HU after running program REPORTA ***
-- ***************************************************************

-- This is a file of interactive SQL commands used for exception reporting
--    for Embedded C

-- List failures for Embedded C:

   SELECT DISTINCT * FROM TESTREPORT
     WHERE TESTTYPE = 'PC' AND RESULT = 'fail'
   ORDER BY TESTNO;

-- List missing test numbers for Embedded C:

   SELECT PROG,TESTNO FROM T_REQ
     WHERE BINDING1 = 'PC' 
     AND REQOPTNA IN ('REQ','OPT')
     AND TESTNO NOT IN
         (SELECT TESTNO FROM TESTREPORT WHERE TESTTYPE = 'PC')
   ORDER BY TESTNO;

-- List mixed pass/fail results in TESTREPORT for Embedded C:

   SELECT DISTINCT * FROM TESTREPORT X, TESTREPORT Y
     WHERE X.TESTTYPE = 'PC' AND Y.TESTTYPE = 'PC'
     AND   X.TESTNO = Y.TESTNO
     AND   X.RESULT < Y.RESULT
   ORDER BY 1;
