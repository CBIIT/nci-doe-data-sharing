alter table USER_T
    add DEFAULT_GROUP_ID INTEGER
/

alter table USER_T
    add constraint "USER_T_GROUP_T_ID_fk"
        foreign key (DEFAULT_GROUP_ID) references GROUP_T
/


