package gov.nih.nci.doe.web;


import org.apache.velocity.app.VelocityEngine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;





/**
 * <p>
 * DOE Web Config to add Interceptor
 * </p>
 */
@SuppressWarnings("deprecation")
@Configuration
public class DoeWebConfig extends WebMvcConfigurerAdapter {

	private static final String[] EXCLUDE_PATTERNS = { "/login", "/css/**", "/fonts/**", "/img/**", "/js/**",
			"/ng-table/**" };
	/**
	 * The User Interceptor.
	 */
	@Autowired
	DoeUserInterceptor userInterceptor;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter#
	 * addInterceptors(org.springframework.web.servlet.config.annotation.
	 * InterceptorRegistry)
	 */
	@Override
	public void addInterceptors(InterceptorRegistry registry) {
		// Disable interceptor for static resources
		registry.addInterceptor(userInterceptor).excludePathPatterns(EXCLUDE_PATTERNS);
	}
	
	   @Bean
	   public JavaMailSender mailSender() {
	       JavaMailSenderImpl mailSender = new JavaMailSenderImpl();
	       mailSender.setHost("mailfwd.nih.gov");
	       return mailSender;
	   }
	   
	   @Bean
	   public VelocityEngine velocityEngine() {
		   VelocityEngine velocityEngine = new VelocityEngine();
		   return velocityEngine;
	   }
	   
	   @Override
	    public void addCorsMappings(CorsRegistry registry) {
	        registry.addMapping("/**");
	    }

}
