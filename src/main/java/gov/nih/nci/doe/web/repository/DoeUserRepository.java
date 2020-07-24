package gov.nih.nci.doe.web.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import gov.nih.nci.doe.web.domain.DoeUsers;

public interface DoeUserRepository extends JpaRepository<DoeUsers, String> {
	
    @Query("select a.password from DoeUsers a where a.emailAddrr =?1")	
    public String getPasswordById(String emailAddr);
     
    @Query("select a from DoeUsers a where a.emailAddrr =?1")	
    public DoeUsers getUserInfo(String emailAddr);
    
    @Query("select a from DoeUsers a where a.uuid =?1 and a.emailAddrr =?2")
    public DoeUsers getUserInfoByToken(String token, String emailAddr);
    
     

 	
}
