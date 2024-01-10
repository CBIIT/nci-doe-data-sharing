-- auto-generated definition
create table USER_GROUP_T
(
    USER_ID  NUMBER not null
        constraint USER_GROUP_T_USER_T_ID_FK
            references USER_T,
    GROUP_ID NUMBER not null
        constraint USER_GROUP_T_GROUP_T_ID_FK
            references GROUP_T,
    ID       NUMBER not null
)
/

create unique index USER_GROUP_T_ID_UINDEX
    on USER_GROUP_T (ID)
/

alter table USER_GROUP_T
    add constraint USER_GROUP_T_PK
        primary key (ID)
/

GRANT INSERT,UPDATE,SELECT ON USER_GROUP_T TO MODAC_APP_USER;