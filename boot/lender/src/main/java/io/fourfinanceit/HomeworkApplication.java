package io.fourfinanceit;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class HomeworkApplication {

    public static void main(String[] args) {
        SpringApplication.run(HomeworkApplication.class, args);
    }

    @Bean
    InitializingBean bootstrapData(CustomerRepository customerRepository) {
        return () -> {
            customerRepository.save(new Customer("Chuck Norris"));
            customerRepository.save(new Customer("Michael Dudikoff"));
        };
    }
}
