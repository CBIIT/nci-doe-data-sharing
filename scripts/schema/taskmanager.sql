-- auto-generated definition
create table TASK_MANAGER_T
(
    ID            NUMBER not null
        constraint TASK_MANAGER_T_PK
            primary key,
    TASK_ID       VARCHAR2(500),
    TASK_NAME     VARCHAR2(500),
    TASK_TYPE     VARCHAR2(255),
    TASK_DATE     DATE,
    USER_ID       VARCHAR2(255),
    DOWNLOAD_TYPE VARCHAR2(255)
)
/

GRANT INSERT,UPDATE ON TASK_MANAGER_T TO MODAC_APP_USER;