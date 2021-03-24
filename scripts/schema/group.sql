-- auto-generated definition
create table GROUP_T
(
    ID         NUMBER        not null
        constraint GROUP_T_PK
            primary key,
    GROUP_NAME VARCHAR2(255) not null
)
/

GRANT INSERT,UPDATE ON GROUP_T TO MODAC_APP_USER;