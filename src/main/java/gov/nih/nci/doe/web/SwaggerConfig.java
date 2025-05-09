package gov.nih.nci.doe.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;

@Configuration
public class SwaggerConfig {

	@Bean
	public OpenAPI apiInfo() {
		return new OpenAPI().info(new Info().title("MoDaC API")
				.description("MoDaC API is a set of public rest API to access datasets stored in the repository"));

	}
}