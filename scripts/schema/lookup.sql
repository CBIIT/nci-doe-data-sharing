-- auto-generated definition
create table NCI_DOE_DB.LOOKUP_T
(
    ID                        NUMBER not null
        constraint LOOKUP_PKEY
            primary key,
    LEVEL_NAME                VARCHAR2(255),
    ATTR_NAME                 VARCHAR2(255),
    DISPLAY_NAME              VARCHAR2(500),
    CREATED_BY                VARCHAR2(255),
    CREATED_DATE              DATE,
    DISPLAY_ORDER             NUMBER,
    SEARCH_CRITERIA_DISPLAY   CHAR,
    IS_EDITABLE               CHAR,
    IS_VISIBLE                CHAR,
    IS_VISIBLE_ON_UPLOAD_PAGE CHAR,
    IS_VISIBLE_FOR_REVIEW_COMMITEE_MEMBER CHAR
)
/

comment on column NCI_DOE_DB.LOOKUP_T.IS_VISIBLE is 'This column is only for display purpose on asset details page. If value is ''N'', do not display on asset details page. '
/

GRANT INSERT,UPDATE,SELECT ON LOOKUP_T TO MODAC_APP_USER;
