package io.fourfinanceit.repository;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.LoanApplicationAttempt;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Set;

public interface LoanApplicationAttemptRepository extends JpaRepository<LoanApplicationAttempt, Long> {

    Set<LoanApplicationAttempt> findByCustomer(Customer customer);


}
