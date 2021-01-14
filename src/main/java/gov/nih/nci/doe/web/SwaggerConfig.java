package gov.nih.nci.doe.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
 
import static springfox.documentation.builders.PathSelectors.regex;
 
@Configuration
public class SwaggerConfig {
 
    @Bean
    public Docket doeApi() {
        return new Docket(DocumentationType.SWAGGER_2)
                .select()
                .paths(regex("/api.*"))
                .build()
                .apiInfo(apiInfo());
    }
 
    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("MoDaC API")
                .description("MoDaC API is a set of public rest API to access datasets stored in the repository.")
                .build();
    }
}