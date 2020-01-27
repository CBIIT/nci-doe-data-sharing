package gov.nih.nci.hpc.web;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import gov.nih.nci.doe.web.DoeWebApplication;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = DoeWebApplication.class)
@WebAppConfiguration
public class DoeWebApplicationTests {

	@Test
	public void contextLoads() {
	}

}
