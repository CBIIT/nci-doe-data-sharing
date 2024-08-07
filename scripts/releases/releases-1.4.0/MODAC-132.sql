/* run collection_access_group.sql and collection_update_permissions.sql*/

update COLLECTION_UPDATE_PERMISSIONS_T mt set
mt.USER_ID = (select id from USER_T where EMAIL_ADDR = mt.USER_GROUP_ID) where mt.IS_OWNER='Y';

update COLLECTION_UPDATE_PERMISSIONS_T mt set mt.GROUP_ID = 
(select id from GROUP_T where GROUP_NAME = mt.USER_GROUP_ID) where mt.IS_GROUP='Y';

/* drop the tables*/


INSERT INTO NCI_DOE_DB_T.GROUP_T (ID, GROUP_NAME) VALUES (1, 'JDACS4C');
INSERT INTO NCI_DOE_DB_T.GROUP_T (ID, GROUP_NAME) VALUES (2, 'JDACS4C_Pilot_1');
INSERT INTO NCI_DOE_DB_T.GROUP_T (ID, GROUP_NAME) VALUES (3, 'JDACS4C_Pilot_2');
INSERT INTO NCI_DOE_DB_T.GROUP_T (ID, GROUP_NAME) VALUES (4, 'ATOM');
INSERT INTO NCI_DOE_DB_T.GROUP_T (ID, GROUP_NAME) VALUES (5, 'NEUROCRINE');


/* insert into user group mapping */
DECLARE cursor  cur IS
SELECT id,program_name from USER_T where program_name IS NOT NULL;
id NUMBER;
userId NUMBER;
groupId NUMBER;
program_name VARCHAR2(500);
BEGIN
FOR REC in cur loop
userId :=REC.id;
program_name :=REC.program_name;
id := 1;
BEGIN
      FOR i IN
      (SELECT trim(regexp_substr(program_name, '[^,]+', 1,LEVEL)) l
      FROM dual
       CONNECT BY LEVEL <= regexp_count(program_name, ',')+1
     ) loop
          IF i.l is not null THEN
          SELECT id INTO groupId from GROUP_T where GROUP_NAME =i.l;
          INSERT INTO USER_GROUP_T (USER_ID, GROUP_ID, ID) VALUES (userId,groupId,id);
          COMMIT;
          END IF;
id := id+1;
END LOOP;
END;
END LOOP;
END;
/

/* drop doe_user_t */

/* populate collection path for existing rows */
select distinct(COLLECTION_ID) from COLLECTION_UPDATE_PERMISSIONS_T where  COLLECTION_PATH IS NULL;

/* get the collection id from above query */
select coll_id, SUBSTR(coll_name, 19, LENGTH(coll_name)) from r_coll_main where COLL_ID in (
7330732
);
/* update COLLECTION_UPDATE_PERMISSIONS_T from the above table */
update NCI_DOE_DB_T.COLLECTION_UPDATE_PERMISSIONS_T set COLLECTION_UPDATE_PERMISSIONS_T.COLLECTION_PATH='DOE_TEST_Archive/Program_JDACS4C_Pilot_2'
where COLLECTION_ID=930851;




