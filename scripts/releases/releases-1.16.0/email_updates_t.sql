create table NCI_DOE_DB.EMAIL_UPDATES_T
(
    ID            NUMBER generated as identity,
    EMAIL_ADDRESS VARCHAR2(100) not null,
    CREATED_DATE  DATE          not null
)
/

