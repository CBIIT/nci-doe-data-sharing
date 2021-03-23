/* get list of collection ids */
select distinct(COLLECTION_ID) from COLLECTION_UPDATE_PERMISSIONS_T where collection_ID not in 
(select collection_id from COLLECTION_ACCESS_GROUP_T);


select object_id, SUBSTR(OBJECT_PATH, 15, LENGTH(OBJECT_PATH)) ,meta_attr_value from 
IRODS.R_COLL_HIERARCHY_META_MAIN where META_ATTR_NAME='access_group' and OBJECT_ID IN (
1346055,
1351868,
1346014,
1346001,
1346020,
1351872,
1351861,
1346007,
1346081) and OBJECT_ID=COLL_ID and META_ATTR_VALUE != 'public';


/*insert the above data into collection_access_group_T */

INSERT INTO NCI_DOE_DB_T.COLLECTION_ACCESS_GROUP_T (ID, COLLECTION_ID, COLLECTION_PATH, GROUP_ID, 
CREATED_DATE, LAST_CHANGED_DATE) 
VALUES (ACCESS_GROUPS_SEQ.nextval, 939942, '/DOE_TEST_Archive/RSF-12-16-program-identifier/RSF-1216-study-identifier/RSF-1216-dataset-identifier', 
(select id from group_t where group_name ='PILOT1'), TO_DATE('2021-03-05 18:26:55', 'YYYY-MM-DD HH24:MI:SS'), null);