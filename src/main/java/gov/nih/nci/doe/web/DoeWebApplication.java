/**
 * DoeWebApplication.java
 *
 * Copyright SVG, Inc.
 * Copyright Leidos Biomedical Research, Inc
 * 
 * Distributed under the OSI-approved BSD 3-Clause License.
 * See https://ncisvn.nci.nih.gov/svn/HPC_Data_Management/branches/hpc-prototype-dev/LICENSE.txt for details.
 */

package gov.nih.nci.doe.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.ImportResource;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * <p>
 * DOE Web Application
 * </p>
 
 */

@SpringBootApplication
@EnableCaching
@EnableScheduling
//@ImportResource("META-INF/spring/doe-scheduler-beans-configuration.xml")
@ImportResource(locations = {"classpath:META-INF/spring/doe-scheduler-beans-configuration.xml"})
public class DoeWebApplication extends SpringBootServletInitializer{

	  @Override
	    protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
	        return application.sources(DoeWebApplication.class);
	    }
	  
	public static void main(String[] args) {
		SpringApplication.run(DoeWebApplication.class, args);
	}
}
