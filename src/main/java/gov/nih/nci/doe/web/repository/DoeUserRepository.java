package gov.nih.nci.doe.web.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import gov.nih.nci.doe.web.domain.DoeUsers;

public interface DoeUserRepository extends JpaRepository<DoeUsers, String> {
	
    @Query("select a.password from DoeUsers a where a.emailAddrr =?1")	
    public String getPasswordById(String emailAddr);
  
	
    // Check if an email exists in the table or not
    @Query("select count(u)>0 from DoeUsers u where u.emailAddrr = :emailAddrr")
	boolean doesUsernameExist(@Param("emailAddrr") String emailAddrr);
     
    @Query("select a from DoeUsers a where a.emailAddrr =?1")	
    public DoeUsers getUserInfo(String emailAddr);
     

 	
}
