package io.fourfinanceit.repository;

import io.fourfinanceit.domain.Customer;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CustomerRepository extends JpaRepository<Customer, Long> {

    Customer findByNumber(String number);

}
