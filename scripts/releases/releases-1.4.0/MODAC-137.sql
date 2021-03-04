select object_id, SUBSTR(OBJECT_PATH, 15, LENGTH(OBJECT_PATH)) ,meta_attr_value from IRODS.R_COLL_HIERARCHY_META_MAIN where META_ATTR_NAME='access_group' and
                                                                                         OBJECT_ID IN (
                                                                                             1346055,
1351868,
1346014,
1346001,
1346020,
1351872,
1351861,
1346007,
1346081) and OBJECT_ID=COLL_ID and META_ATTR_VALUE != 'public';


/*on DEV:select OBJECT_ID,OBJECT_PATH,META_ATTR_VALUE from IRODS.R_COLL_HIERARCHY_META_MAIN where META_ATTR_NAME='access_group';*/

select distinct(COLLECTION_ID) from COLLECTION_UPDATE_PERMISSIONS_T where collection_ID not in (select collection_id from COLLECTION_ACCESS_GROUP_T);