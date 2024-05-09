alter table COLLECTION_UPDATE_PERMISSIONS_T
Modify COLLECTION_ID null
/
alter table TASK_MANAGER_T rename column DOWNLOAD_TYPE to TYPE
/

alter table TASK_MANAGER_T
    add PATH VARCHAR2(500)
/

alter table AUDITING_T
    modify ERROR_MSG VARCHAR2(2000)
/