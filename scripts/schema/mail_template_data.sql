INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (1, 'REGISTRATION_EMAIL', 'Registration Confirmation', 'Registration Confirmation', '<p>This confirms your registration to the Predictive Oncology Model and Data Clearinghouse (MoDaC).<p>', 'gantam2', TIMESTAMP '2020-01-27 15:43:33');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (2, 'RESET_PASSWORD_EMAIL', 'Password Reset Request', 'Password Reset Request', '<p>A temporary password has been generated as below. </p> <p>${TEMP_PSWD}</p> <p> We encourage you to reset the password after <a href="https://fsdmel-modac01t.ncifcrf.gov/loginTab">logging in</a>. </p>', 'gantam2', TIMESTAMP '2020-01-27 15:43:33');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (3, 'ACCESS_GROUP_EMAIL', 'Access Group Change Request', 'Access Group Change Request', '<p>MoDaC user ${loggedOnUser} is requesting that you change the group access for <b>${COLLECTION_NAME}</b> from 
<b>${existingAccessGroups}</b> to <b>${newAccessGroups}</b>.<p>
 <p>For instructions, refer to <a href="https://wiki.nci.nih.gov/x/qQbgGQ">Managing Group Access to an Existing Collection.</a>
</p>', 'gantam2', TIMESTAMP '2020-01-27 15:43:33');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (4, 'ACTIVATION_EMAIL', 'Confirm Email Address', 'Confirm your email address on MoDaC', '<p>Verify your email address to activate your account on MoDaC.<p>
<p style="margin-left:10px;"><a style="padding: .375rem 0.45rem;background-color: #218739 !important;text-decoration: none!important;
    color: #fff;border-radius: 5px;"href="${confirm_email}">Verify Email Address</a><p>
', 'gantam2', TIMESTAMP '2020-07-24 15:43:33');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (5, 'COLLECTION_FAILURE_EMAIL', 'Collection Registration Failure', 'Collection Registration Failure', '<p>Collection registration failure in MoDaC for path <b>${COLLECTION_PATH}</b> for the user ${EMAIL} </p>
<p>TASK ID: ${TASK_ID}</p>
<p>STACK TRACE:</p>
<p>${EXCEPTION}</p>', 'gantam2', TIMESTAMP '2021-04-19 11:43:21');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (6, 'EXCEPTION_EMAIL', 'Error From MoDaC', 'Error From MoDaC', '<p> Error from MoDaC from user ${user_Id}.</p>
<p>STACK TRACE:</p>
<p>${EXCEPTION}</p>', 'gantam2', TIMESTAMP '2021-07-20 14:25:19');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (7, 'CONTACT_US_EMAIL', 'Contact Us Email', 'User Support', '<p>Message from ${username}.</p><p>Organization : ${org}</p><p>Type of Inquiry: ${inquiry}</p><p>Message: ${message}</p>', 'gantam2', TIMESTAMP '2021-11-30 12:55:40');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (9, 'RELEASE_NOTIFICATION_EMAIL', 'Release Notification Email', 'MoDaC Release', 'Hi All,

A new Release of the Predictive Oncology Model and Data Clearinghouse (<a href=''${modac_link}''> MoDaC </a>) has been deployed to production.
Details of the enhancements are available in the Release notes located on GitHub (also attached).
Please let us know if you have questions. 

Thank You ! 

Click <a href=''${unsubscribe_link}''> here </a>to unsubscribe from MoDaC notifications.', 'gantam2', TIMESTAMP '2023-08-04 15:04:53');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (10, 'UPLOAD_NOTIFICATION', 'Upload Notifiction', 'MoDaC Bulk Registration Request ${displayStatus} at ${dateTime}', '<p>Your bulk registration request ${status}.</p>
<p>Task ID: <a href="${modac_link}">${taskId}</a></p>
<p>Registration Items: ${registrationItems}</p>
<p>${failureMsg}</p>', 'gantam2', TIMESTAMP '2024-05-28 17:46:07');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (11, 'DOWNLOAD_NOTIFICATION', 'Download Notification', 'MoDaC ${downloadType} Download Request ${displayStatus} at ${dateTime}', '<p>Your download request ${status}.</p>
<p>Destination type: ${destinationType}</p>
<p>Source path: ${sourcePath}</p>
<p>Target path: ${targetPath}</p>
<p>Task ID: <a href="${modac_link}">${taskId}</a></p>
<p>${failureMsg}</p>', 'gantam2', TIMESTAMP '2024-05-28 17:48:22');
INSERT INTO NCI_DOE_DB.MAIL_TEMPLATE_T (ID, SHORT_IDENTIFIER, EMAIL_TITLE, EMAIL_SUBJECT, EMAIL_BODY, CREATED_BY, CREATED_DATE) VALUES (12, 'PREDICTION_NOTIFICATION', 'Prediction Notification', 'MoDaC Prediction Request ${displayStatus} at ${dateTime}', '<p>Your Prediction request ${status}</p>
<p>Task ID: <a href="${modac_link}">${taskId}</a></p>
<p>Result Path: ${resultPath}</p>
<p>Input Dataset Path: ${inputDatasetPath}</p>
<p>${failureMsg}</p>

', 'gantam2', TIMESTAMP '2024-05-28 17:48:23');
