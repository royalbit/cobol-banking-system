package com.royalbit.banking.api.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * OpenAPI/Swagger configuration.
 */
@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI bankingOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("COBOL Banking System API")
                        .description("REST API for the COBOL Banking System migration to Java Spring Boot. " +
                                "This API provides equivalent functionality to the original COBOL mainframe system.")
                        .version("0.4.0")
                        .contact(new Contact()
                                .name("RoyalBit")
                                .url("https://github.com/royalbit/cobol-banking-system"))
                        .license(new License()
                                .name("MIT")
                                .url("https://opensource.org/licenses/MIT")));
    }
}
