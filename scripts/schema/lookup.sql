-- auto-generated definition
create table LOOKUP_T
(
    ID                      NUMBER not null
        constraint LOOKUP_PKEY
            primary key,
    LEVEL_NAME              VARCHAR2(255),
    ATTR_NAME               VARCHAR2(255),
    DISPLAY_NAME            VARCHAR2(500),
    CREATED_BY              VARCHAR2(255),
    CREATED_DATE            DATE,
    DISPLAY_ORDER           NUMBER,
    SEARCH_CRITERIA_DISPLAY CHAR,
    SEARCH_RESULTS_DISPLAY  CHAR,
    IS_EDITABLE             CHAR
)
/

