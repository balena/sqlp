-- MODULE  CTS5TAB  

-- NIST SQL Test Suite, V6.0, Interactive SQL, cts5tab.sql
-- 59-byte ID
-- TEd Version #

-- AUTHORIZATION CTS1              

   SELECT USER FROM HU.ECCO;
-- RERUN if USER value does not match preceding AUTHORIZATION comment

-- date_time print

   ROLLBACK WORK;

   DELETE FROM CTS1.ECCO;

   INSERT INTO CTS1.ECCO VALUES ('NL');

   DELETE FROM STAFF;

   INSERT INTO STAFF VALUES ('E1','Alice',12,'Deale');

   INSERT INTO STAFF VALUES ('E2','Betty',10,'Vienna');

   INSERT INTO STAFF VALUES ('E3','Carmen',13,'Vienna');

   INSERT INTO STAFF VALUES ('E4','Don',12,'Deale');

   INSERT INTO STAFF VALUES ('E5','Ed',13,'Akron');

   DELETE FROM WORKS;

   INSERT INTO WORKS VALUES  ('E1','P1',40);

   INSERT INTO WORKS VALUES  ('E1','P2',20);

   INSERT INTO WORKS VALUES  ('E1','P3',80);

   INSERT INTO WORKS VALUES  ('E1','P4',20);

   INSERT INTO WORKS VALUES  ('E1','P5',12);

   INSERT INTO WORKS VALUES  ('E1','P6',12);

   INSERT INTO WORKS VALUES  ('E2','P1',40);

   INSERT INTO WORKS VALUES  ('E2','P2',80);

   INSERT INTO WORKS VALUES  ('E3','P2',20);

   INSERT INTO WORKS VALUES  ('E4','P2',20);

   INSERT INTO WORKS VALUES  ('E4','P4',40);

   INSERT INTO WORKS VALUES  ('E4','P5',80);

   DELETE FROM VTABLE;

   INSERT INTO VTABLE VALUES(10,+20,30,40,10.50);

   INSERT INTO VTABLE VALUES(0,1,2,3,4.25);

   INSERT INTO VTABLE VALUES(100,200,300,400,500.01);

   INSERT INTO VTABLE VALUES(1000,-2000,3000,NULL,4000.00);

   DELETE FROM CTS1.STAFFb;

   INSERT INTO CTS1.STAFFb VALUES (10000,'Fred',40,'P2','Vienna','M');

   INSERT INTO CTS1.STAFFb VALUES (10000,'Fred',40,'P2','Vienna','M');

   INSERT INTO CTS1.STAFFb VALUES (10000,'Fred',40,'P2','Vienna','M');

   INSERT INTO CTS1.STAFFb VALUES (15000,'Carmen',35,'P2','Vienna','F');

   INSERT INTO CTS1.STAFFb VALUES (15000,'Carmen',35,'P2','Vienna','F');

   INSERT INTO CTS1.STAFFb VALUES (10000,'Alice',40,'P3','Prague','F');

   INSERT INTO CTS1.STAFFb VALUES (20000,'Betty',30,'P1','Deale','F');

   INSERT INTO CTS1.STAFFb VALUES (20000,'Betty',30,'P1','Deale','F');

   INSERT INTO CTS1.STAFFb VALUES (40000,'Don',70,'P3','Prague','M');

   INSERT INTO CTS1.STAFFb VALUES (40000,'Don',70,'P3','Prague','M');

   INSERT INTO CTS1.STAFFb VALUES (40000,'Don',70,'P3','Prague','M');

   INSERT INTO CTS1.STAFFb VALUES (10000,'Ed',40,'P1','Deale','M');

   DELETE FROM CTS1.STAFFa;

   INSERT INTO CTS1.STAFFa VALUES (40,10000,'E6',2,'Fred');

   INSERT INTO CTS1.STAFFa VALUES (40,10000,'E6',2,'Fred');

   INSERT INTO CTS1.STAFFa VALUES (40,10000,'E1',3,'Alice');

   INSERT INTO CTS1.STAFFa VALUES (40,10000,'E1',3,'Alice');

   INSERT INTO CTS1.STAFFa VALUES (70,40000,'E4',3,'Don');

   INSERT INTO CTS1.STAFFa VALUES (70,40000,'E4',3,'Don');

   INSERT INTO CTS1.STAFFa VALUES (70,40000,'E4',3,'Don');

   INSERT INTO CTS1.STAFFa VALUES (30,20000,'E2',1,'Betty');

   INSERT INTO CTS1.STAFFa VALUES (60,45000,'E7',4,'Grace');

   INSERT INTO CTS1.STAFFa VALUES (60,45000,'E7',4,'Grace');

   INSERT INTO CTS1.STAFFa VALUES (30,8000,'E8',2,'Henry');

   INSERT INTO CTS1.STAFFa VALUES (15,7000,'E9',1,'Imogen');

   DELETE FROM STAFFc;

   INSERT INTO STAFFc VALUES ('E1','Alice',12,'Deale',NULL);

   INSERT INTO STAFFc VALUES ('E2','Betty',10,'Vienna','E1');

   INSERT INTO STAFFc VALUES ('E3','Carmen',13,'Vienna','E2');

   INSERT INTO STAFFc VALUES ('E4','Don',12,'Deale','E2');

   INSERT INTO STAFFc VALUES ('E5','Don',12,'Deale','E1');

   INSERT INTO STAFFc VALUES ('E6','Tom',14,'Gettysburg','E5');

   INSERT INTO STAFFc VALUES ('E7','Kingdom',18,'Gettysburg','E7');

   DELETE FROM STAFF_CTS2;

   INSERT INTO STAFF_CTS2 VALUES ('E1','Alice',12,'Deale');

   INSERT INTO STAFF_CTS2 VALUES ('E1','Alice',12,'Deale');

   INSERT INTO STAFF_CTS2 VALUES ('E2','Betty',10,'Vienna');

   INSERT INTO STAFF_CTS2 VALUES ('E3','Carmen',13,'Vienna');

   INSERT INTO STAFF_CTS2 VALUES ('E3','Carmen',13,'Vienna');

   INSERT INTO STAFF_CTS2 VALUES ('E3','Carmen',13,'Vienna');

   INSERT INTO STAFF_CTS2 VALUES ('E4','Don',12,'Deale');

   INSERT INTO STAFF_CTS2 VALUES ('E6','Don',12,'Deale');

   DELETE FROM STAFF_CTS;

   INSERT INTO STAFF_CTS VALUES ('P1','Deale',12,'Don');

   INSERT INTO STAFF_CTS VALUES ('P3','Vienna',10,'Betty');

   INSERT INTO STAFF_CTS VALUES ('P3','Vienna',10,'Betty');

   INSERT INTO STAFF_CTS VALUES ('P4','Vienna',13,'Carmen');

   INSERT INTO STAFF_CTS VALUES ('P5','Prague',15,'Ed');

   DELETE FROM EMPLOYEES2;

   INSERT INTO EMPLOYEES2 VALUES ('Atherton',1);

   INSERT INTO EMPLOYEES2 VALUES ('Botham',2);

   INSERT INTO EMPLOYEES2 VALUES ('Cowdrey',3);

   INSERT INTO EMPLOYEES2 VALUES ('Dev',4);

   INSERT INTO EMPLOYEES2 VALUES ('Edmunds',5);

   INSERT INTO EMPLOYEES2 VALUES ('Fields',6);

   INSERT INTO EMPLOYEES2 VALUES ('Gower',7);

   DELETE FROM A;

   INSERT INTO A VALUES (1, 15);

   INSERT INTO A VALUES (3, 17);

   INSERT INTO A VALUES (7, 11);

   INSERT INTO A VALUES (5, 9);

   INSERT INTO A VALUES (13, 13);

   INSERT INTO A VALUES (9, 5);

   INSERT INTO A VALUES (11, 7);

   INSERT INTO A VALUES (17, 3);

   INSERT INTO A VALUES (15, 1);

   INSERT INTO A VALUES (13, 5);

   INSERT INTO A VALUES (13, 15);

   INSERT INTO A VALUES (11, 1);

   INSERT INTO A VALUES (5, 5);

   INSERT INTO A VALUES (5, 3);

   INSERT INTO A VALUES (1, 1);

   DELETE FROM TT;

   INSERT INTO TT (TTA, TTC) VALUES (1, 99);

   INSERT INTO TT (TTA, TTB) VALUES (2, 98);

   INSERT INTO TT VALUES (3, 97, 96);

   INSERT INTO TT (TTA) VALUES (4);

   INSERT INTO TT VALUES (5, 42, 26);

   DELETE FROM TU;

   INSERT INTO TU VALUES ('ab', 3);

   INSERT INTO TU (TUE) VALUES (5);

   INSERT INTO TU VALUES ('cd', 4);

   INSERT INTO TU (TUE) VALUES (11);

   INSERT INTO TU VALUES ('ef', 12);

   INSERT INTO TU VALUES ('gh', 11);

   DELETE FROM TT2;

   INSERT INTO TT2 VALUES (1,INTERVAL '17-3' YEAR TO MONTH,13);

   INSERT INTO TT2 (TTA,TTB) VALUES (2,INTERVAL '5-6' YEAR TO MONTH);

   INSERT INTO TT2 (TTA) VALUES (3);

   INSERT INTO TT2 (TTA,TTC) VALUES (4,20);

   INSERT INTO TT2 VALUES (5,INTERVAL '60-2' YEAR TO MONTH,19);

   DELETE FROM TV;

   INSERT INTO TV VALUES (1,'a');

   INSERT INTO TV VALUES (2,'b');

   INSERT INTO TV VALUES (3,'c');

   INSERT INTO TV VALUES (4,'d');

   INSERT INTO TV VALUES (5,'e');

   DELETE FROM TW;

   INSERT INTO TW VALUES ('b',2);

   INSERT INTO TW VALUES ('g',1);

   INSERT INTO TW VALUES ('f',2);

   INSERT INTO TW VALUES ('h',4);

   INSERT INTO TW VALUES ('i',5);

   DELETE FROM TX;

   INSERT INTO TX (TX1, TX3) VALUES (1, 'Susan');

   INSERT INTO TX (TX1, TX2) VALUES (2, 'lemon');

   INSERT INTO TX VALUES (3, 'apple', '');

   INSERT INTO TX VALUES (4, 'melon', 'Peter');

   INSERT INTO TX VALUES (5, 'peach', 'Matthew');

   DELETE FROM TEST12849B;

   INSERT INTO TEST12849B VALUES (1);

   INSERT INTO TEST12849B VALUES (2);

   INSERT INTO TEST12849B VALUES (3);

   INSERT INTO TEST12849B VALUES (4);

   DELETE FROM TABX760;

   INSERT INTO CTS1.TABX760 VALUES (10,'SPYROS',25000);

   INSERT INTO CTS1.TABX760 VALUES (11,'ALEXIS',18000);

   INSERT INTO CTS1.TABX760 VALUES (12,'LAMBIS',9000);

   INSERT INTO CTS1.TABX760 VALUES (13,'ELENI',4000);

   INSERT INTO CTS1.TABX760 VALUES (14,'MARIOS',47000);

   INSERT INTO CTS1.TABX760 VALUES (15,'NICKOLAS',78000);

   DELETE FROM TABCS;

   INSERT INTO CTS1.TABCS VALUES (1, _CS 'NICKOS', _CS 'MARIA');

   INSERT INTO CTS1.TABCS VALUES (2, _CS 'BILL', _CS 'TAKIS');

   INSERT INTO CTS1.TABCS VALUES (3, _CS 'NICKOS', _CS 'TAKIS');

   INSERT INTO CTS1.TABCS VALUES (4, _CS 'MARIA', _CS 'TAKIS');

   DELETE FROM TTIME3;

   INSERT INTO CTS1.TTIME3 VALUES (
     1,TIME '23:05:00',TIME '12:15:00-11:45',
     TIMESTAMP '1995-07-07 08:15:00+03:05');

   INSERT INTO CTS1.TTIME3 VALUES(
     2,TIME '05:10:00',TIME '00:07:00+08:39', 
     TIMESTAMP '2000-10-09 03:03:00+04:05');

   INSERT INTO CTS1.TTIME3 VALUES (
     3,TIME '12:11:00',TIME '23:19:00+10:32', 
     TIMESTAMP '1997-01-16 12:17:00-12:16');

   INSERT INTO CTS1.TTIME3 VALUES (
     4,TIME '05:10:00',TIME '00:07:00+08:39', 
     TIMESTAMP '2000-10-09 03:03:00+04:05');

   INSERT INTO CTS1.TTIME3 VALUES (
     5,TIME '17:39:00',TIME '08:28:00-11:45', 
     TIMESTAMP '1994-12-31 20:00:00+04:05');

   INSERT INTO CTS1.TTIME3 VALUES (
     6,TIME '17:39:00',TIME '08:28:00-11:45', 
     TIMESTAMP '1994-12-31 20:00:00+04:05');

   DELETE FROM CL_STANDARD;

   INSERT INTO CTS1.CL_STANDARD VALUES (1000,'NICKOS',4000,'ATHENS');

   INSERT INTO CTS1.CL_STANDARD VALUES (1001,'MARIA',4001,'RHODES');

   INSERT INTO CTS1.CL_STANDARD VALUES (1002,'MAKIS',4002,'HANIA');

   INSERT INTO CTS1.CL_STANDARD VALUES (1003,'GEORGE',4003,'ARTA');

   INSERT INTO CTS1.CL_STANDARD VALUES (1004,'MORRIS',4004,'PARGA');

   DELETE FROM TABLE728a;

   INSERT INTO CTS1.TABLE728a VALUES ('NICKOS','GEORGE');

   INSERT INTO CTS1.TABLE728a VALUES ('HARRY','TANIA');

   INSERT INTO CTS1.TABLE728a VALUES ('KILLER',NULL);

   INSERT INTO CTS1.TABLE728a VALUES (NULL,NULL);

   DELETE FROM TAB734;

   INSERT INTO CTS1.TAB734 VALUES (N'   !',N'*  *');

   INSERT INTO CTS1.TAB734 VALUES (N' * ',N'+ +');

   INSERT INTO CTS1.TAB734 VALUES (N'+ +',N'+ +');

   INSERT INTO CTS1.TAB734 VALUES (NULL,N' + ');

   DELETE FROM CL_DATA_TYPE;

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('GEORGE',1,10,100);

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('NICK',2,20,200);

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('PAUL',3,30,300);

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('PETER',4,40,400);

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('KEVIN',5,50,500);

   INSERT INTO CTS1.CL_DATA_TYPE VALUES ('JASON',6,60,600);

   COMMIT WORK;

-- *************************************************////END-OF-MODULE
