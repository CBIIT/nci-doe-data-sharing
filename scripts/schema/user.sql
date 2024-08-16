create table USER_T
(
    ID                         NUMBER not null
        constraint DOE_USER_PKEY
            primary key,
    EMAIL_ADDR                 VARCHAR2(255),
    FIRST_NAME                 VARCHAR2(255),
    INSTITUTION                VARCHAR2(255),
    LAST_NAME                  VARCHAR2(255),
    PASSWORD                   VARCHAR2(255),
    LOCKOUT_COUNTER            NUMBER,
    LOCKOUT_DATE               DATE,
    IS_WRITE                   CHAR,
    IS_ACTIVATED               CHAR,
    UUID                       VARCHAR2(255),
    CREATED_DATE               DATE,
    LAST_UPDATED_DATE          DATE,
    IS_ADMIN                   CHAR,
    IS_DELETE                  CHAR,
    IS_REVIEW_COMMITTEE_MEMBER CHAR,
    DEFAULT_GROUP_ID NUMBER
)
/

GRANT INSERT,UPDATE,SELECT ON USER_T TO MODAC_APP_USER;

alter table USER_T
    add constraint "USER_T_GROUP_T_ID_fk"
        foreign key (DEFAULT_GROUP_ID) references GROUP_T
