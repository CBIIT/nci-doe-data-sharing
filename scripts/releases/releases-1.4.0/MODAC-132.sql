update METADATA_PERMISSIONS_T mt set mt.USER_ID = (select id from USER_T where EMAIL_ADDR = mt.USER_GROUP_ID) where mt.IS_OWNER='Y';

update METADATA_PERMISSIONS_T mt set mt.GROUP_ID = (select id from GROUP_T where GROUP_NAME = mt.USER_GROUP_ID) where mt.IS_GROUP='Y';