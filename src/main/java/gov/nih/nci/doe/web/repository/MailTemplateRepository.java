package gov.nih.nci.doe.web.repository;


import org.springframework.data.jpa.repository.JpaRepository;

import gov.nih.nci.doe.web.domain.MailTemplate;

public interface MailTemplateRepository extends JpaRepository<MailTemplate, String> {
	
	public MailTemplate findMailTemplateTByShortIdentifier(String shortIdentifier);
  

}
