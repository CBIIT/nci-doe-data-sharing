-- auto-generated definition
create table USER_T
(
    ID                NUMBER not null
        constraint DOE_USER_PKEY
            primary key,
    EMAIL_ADDR        VARCHAR2(255),
    FIRST_NAME        VARCHAR2(255),
    INSTITUTION       VARCHAR2(255),
    LAST_NAME         VARCHAR2(255),
    PASSWORD          VARCHAR2(255),
    LOCKOUT_COUNTER   NUMBER,
    LOCKOUT_DATE      DATE,
    IS_WRITE          CHAR,
    CREATED_DATE      DATE,
    LAST_UPDATED_DATE DATE,
    UUID              VARCHAR2(255),
    IS_ACTIVATED      CHAR
)
/

