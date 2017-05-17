package io.fourfinanceit;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.LoanApplicationAttempt;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanApplicationAttemptRepository;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.apache.commons.logging.Log;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.annotation.Transactional;

@SpringBootApplication
public class HomeworkApplication {

    private final Logger log = LoggerFactory.getLogger(HomeworkApplication.class);

    public static void main(String[] args) {
        SpringApplication.run(HomeworkApplication.class, args);
    }

    @Bean
    @Transactional
    InitializingBean bootstrapData(CustomerRepository customerRepository, LoanApplicationAttemptRepository loanApplicationAttemptRepository) {
        return () -> {

            // TODO initialize only for dev and test
            Customer customer1 = new Customer();
            customer1.setNumber("123123123");
            customer1 = customerRepository.save(customer1);
            LoanApplicationAttempt loandApplicationAttempt = new LoanApplicationAttempt(customer1, "127.0.0.1");
            loandApplicationAttempt = loanApplicationAttemptRepository.save(loandApplicationAttempt);
        };
    }
}
