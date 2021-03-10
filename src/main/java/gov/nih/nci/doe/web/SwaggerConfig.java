package gov.nih.nci.doe.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.fasterxml.classmate.TypeResolver;

import gov.nih.nci.hpc.dto.datamanagement.HpcBulkDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcCollectionListDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectDownloadResponseDTO;
import gov.nih.nci.hpc.dto.datamanagement.HpcDataObjectListDTO;
import gov.nih.nci.hpc.dto.datamanagement.v2.HpcDataObjectDTO;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
 
import static springfox.documentation.builders.PathSelectors.regex;
 
@Configuration
public class SwaggerConfig {
 
	private final TypeResolver typeResolver;


	public SwaggerConfig(final TypeResolver typeResolver) {
	    this.typeResolver = typeResolver;
	}
	
    @Bean
    public Docket doeApi() {
        return new Docket(DocumentationType.SWAGGER_2)
                .select()
                .paths(regex("/api.*"))
                .build()
                .apiInfo(apiInfo())
                .additionalModels(typeResolver.resolve(HpcDataObjectDownloadResponseDTO.class))
                .additionalModels(typeResolver.resolve(HpcCollectionDownloadResponseDTO.class))
                .additionalModels(typeResolver.resolve(HpcBulkDataObjectDownloadResponseDTO.class))
                .additionalModels(typeResolver.resolve(HpcCollectionDTO.class))
                .additionalModels(typeResolver.resolve(HpcDataObjectListDTO.class))
                .additionalModels(typeResolver.resolve(HpcCollectionListDTO.class))
                .additionalModels(typeResolver.resolve(HpcDataObjectDTO.class));
    }
 
    private ApiInfo apiInfo() {
        return new ApiInfoBuilder()
                .title("MoDaC API")
                .description("MoDaC API is a set of public rest API to access datasets stored in the repository.")
                .build();
    }
}