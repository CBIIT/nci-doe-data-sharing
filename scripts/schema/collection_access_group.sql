-- auto-generated definition
create table COLLECTION_ACCESS_GROUP_T
(
    ID                NUMBER not null,
    COLLECTION_ID     NUMBER not null,
    COLLECTION_PATH   VARCHAR2(500),
    GROUP_ID          NUMBER
        constraint ACCESS_GROUP_T_GROUP_T_ID_FK
            references GROUP_T,
    CREATED_DATE      DATE,
    LAST_CHANGED_DATE DATE
)
/

create unique index ACCESS_GROUP_T_ID_UINDEX
    on COLLECTION_ACCESS_GROUP_T (ID)
/

alter table COLLECTION_ACCESS_GROUP_T
    add constraint ACCESS_GROUP_T_PK
        primary key (ID)
/

GRANT INSERT,UPDATE,DELETE ON collection_access_group_t TO MODAC_APP_USER;