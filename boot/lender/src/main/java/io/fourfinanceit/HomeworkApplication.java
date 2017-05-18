package io.fourfinanceit;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanApplicationAttempt;
import io.fourfinanceit.domain.LoanExtension;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanApplicationAttemptRepository;
import io.fourfinanceit.repository.LoanExtensionRepository;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.util.test.TestHelpers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

@SpringBootApplication
public class HomeworkApplication {

    private final Logger log = LoggerFactory.getLogger(HomeworkApplication.class);

    public static void main(String[] args) {
        SpringApplication.run(HomeworkApplication.class, args);
    }

    @Bean
    @Transactional
    InitializingBean bootstrapData(CustomerRepository customerRepository,
                                   LoanApplicationAttemptRepository loanApplicationAttemptRepository,
                                   LoanRepository loanRepository,
                                   LoanExtensionRepository loanExtensionRepository) {
        return () -> {
            log.info("Generate sample data");
            Customer customer = new Customer();
            customer.setNumber(TestHelpers.CUSTOMER_NUMBER);
            customer = customerRepository.save(customer);
            // Sample loan application
            LoanApplicationAttempt loandApplicationAttempt = new LoanApplicationAttempt(customer, "127.0.0.1");
            loandApplicationAttempt = loanApplicationAttemptRepository.save(loandApplicationAttempt);

            // Sample loan
            Loan loan = new Loan();
            loan.setCustomer(customer);
            loan.setStartDate(TestHelpers.WEEK_AGO);
            loan.setEndDate(TestHelpers.TOMORROW);
            loan.setAmount(new BigDecimal(10.10));
            loanRepository.save(loan);

            // Add sample extension to loan
            LoanExtension loanExtension = new LoanExtension();
            loanExtension.setLoan(loan);
            loanExtension.setStartDate(loan.getEndDate());
            loanExtension.setEndDate(TestHelpers.WEEK_FROM_NOW);
            loanExtensionRepository.save(loanExtension);
        };
    }
}
