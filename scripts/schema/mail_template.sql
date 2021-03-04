-- auto-generated definition
create table MAIL_TEMPLATE_T
(
    ID               NUMBER not null
        constraint MAIL_TEMPLATE_PKEY
            primary key,
    SHORT_IDENTIFIER VARCHAR2(255),
    EMAIL_TITLE      VARCHAR2(255),
    EMAIL_SUBJECT    VARCHAR2(500),
    EMAIL_BODY       VARCHAR2(2000),
    CREATED_BY       VARCHAR2(255),
    CREATED_DATE     DATE
)
/

