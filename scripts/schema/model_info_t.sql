create table MODEL_INFO_T
(
    ID                            NUMBER generated as identity,
    ASSET_PATH                    VARCHAR2(1000),
    IS_EXTERNAL_DATASET_SUPPORTED CHAR,
    ASSET_IDENTIFIER              VARCHAR2(1000)
)
/

comment on table MODEL_INFO_T is 'This table is used for maintaining the models avaiable for inferencing.'
/

create unique index MODEL_INFO_T_ID_UINDEX
    on MODEL_INFO_T (ID)
/

alter table MODEL_INFO_T
    add constraint MODEL_INFO_T_PK
        primary key (ID)
/

GRANT INSERT,UPDATE,DELETE,SELECT ON MODEL_INFO_T TO MODAC_APP_USER;