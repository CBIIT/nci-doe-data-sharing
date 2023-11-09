-- auto-generated definition
create table PREDICTION_ACCESS_T
(
    ID              NUMBER generated as identity,
    COLLECTION_ID   NUMBER         not null,
    COLLECTION_PATH VARCHAR2(1000) not null,
    USER_ID         NUMBER
        constraint PREDICTION_ACCESS_T_USER_T_ID_FK
            references USER_T,
    GROUP_ID        NUMBER
        constraint PREDICTION_ACCESS_T_GROUP_T_ID_FK
            references GROUP_T,
    CREATED_DATE    DATE,
    IS_PUBLIC       CHAR
)
/

create unique index PREDICTION_ACCESS_T_ID_UINDEX
    on PREDICTION_ACCESS_T (ID)
/

alter table PREDICTION_ACCESS_T
    add constraint PREDICTION_ACCESS_T_PK
        primary key (ID)
/

GRANT INSERT,UPDATE,DELETE ON PREDICTION_ACCESS_T TO MODAC_APP_USER;
