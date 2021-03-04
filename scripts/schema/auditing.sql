-- auto-generated definition
create table AUDITING_T
(
    ID              NUMBER not null
        constraint AUDITING_T_PK
            primary key,
    NAME            VARCHAR2(500),
    PATH            VARCHAR2(500),
    OPERATION       VARCHAR2(500),
    STATUS          VARCHAR2(500),
    ERROR_MSG       VARCHAR2(500),
    TASK_ID         VARCHAR2(500),
    TRANSFER_TYPE   VARCHAR2(500),
    START_TIME      DATE,
    COMPLETION_TIME DATE
)
/

