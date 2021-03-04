-- auto-generated definition
create table COLLECTION_UPDATE_PERMISSIONS_T
(
    ID              NUMBER not null
        constraint METADATA_PERMISSIONS_T_PK
            primary key,
    COLLECTION_ID   NUMBER not null,
    CREATED_DATE    DATE,
    COLLECTION_PATH VARCHAR2(500),
    USER_ID         NUMBER
        constraint METADATA_PERMISSIONS_T_USER_T_ID_FK
            references USER_T,
    GROUP_ID        NUMBER
        constraint METADATA_PERMISSIONS_T_GROUP_T_ID_FK
            references GROUP_T
)
/

