-- auto-generated definition
create table INFERENCING_TASK_T
(
    USER_ID           VARCHAR2(500) not null,
    TASK_ID           VARCHAR2(500) not null,
    STATUS            VARCHAR2(500),
    START_DATE        DATE,
    COMPLETED_DATE    DATE,
    RESULT_PATH       VARCHAR2(1000),
    MODEL_IDENTIFIER  VARCHAR2(1000),
    ID                NUMBER generated as identity,
    DME_TASK_ID       VARCHAR2(1000),
    MODEL_H5_PATH     VARCHAR2(1000),
    TEST_DATASET_PATH VARCHAR2(1000),
    ASSET_PATH        VARCHAR2(500),
    BATCH_ID          VARCHAR2(1000)
)
/

create unique index TASK_INFERENCING_ID_UINDEX
    on INFERENCING_TASK_T (ID)
/

alter table INFERENCING_TASK_T
    add constraint TASK_INFERENCING_PK
        primary key (ID)
/

GRANT INSERT,UPDATE ON USER_T TO MODAC_APP_USER;