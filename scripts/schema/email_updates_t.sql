create table EMAIL_UPDATES_T
(
    ID            NUMBER generated as identity,
    EMAIL_ADDRESS VARCHAR2(100) not null,
    CREATED_DATE  DATE          not null
)
/

GRANT INSERT,UPDATE,DELETE,SELECT ON NCI_DOE_DB_P.EMAIL_UPDATES_T TO MODAC_APP_USER;