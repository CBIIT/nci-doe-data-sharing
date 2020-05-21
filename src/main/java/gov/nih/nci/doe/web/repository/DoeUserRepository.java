package gov.nih.nci.doe.web.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import gov.nih.nci.doe.web.domain.DoeUsers;

public interface DoeUserRepository extends JpaRepository<DoeUsers, String> {
	
  @Query("select a.password from DoeUsers a where a.emailAddrr =?1")	
  public String getPasswordById(String emailAddr);
  
	/**
	 * Update the user record with encrypted password for given email.
	 * @param password
	 */
	@Modifying
	@Query(value = "update DOE_USER_T set PASSWORD = ?1 where EMAIL_ADDR = ?2", nativeQuery = true)
	public void updatePasswordByUsername(String password, String emailAddrr);
	
		
	@Modifying
	@Query(value = "update DOE_USER_T set LOCKOUT_COUNTER = ?1 where EMAIL_ADDR = ?2", nativeQuery = true)
	public void updateCounterInfo(Integer counter, String emailAddrr);
	
	
    // Check if an email exists in the table or not
    @Query("select count(u)>0 from DoeUsers u where u.emailAddrr = :emailAddrr")
	boolean doesUsernameExist(@Param("emailAddrr") String emailAddrr);
     
    @Query("select a from DoeUsers a where a.emailAddrr =?1")	
    public DoeUsers getUserInfo(String emailAddr);
     

 	@Modifying
 	@Query(value = "update DOE_USER_T set FIRST_NAME = ?1,LAST_NAME = ?2,INSTITUTION = ?3 where EMAIL_ADDR = ?4", nativeQuery = true)
 	public void updateUserInfo(String firstName, String lastName, String institution, String emailAddrr);
}
