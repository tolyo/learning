package io.fourfinanceit.rest;

import io.fourfinanceit.domain.Loan;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/loans")
public class LoanResource {

    @PostMapping("")
    public Loan createLoan() {
        // create a new loan for a customer

        // create and save and application attempt
        // validate application request

        return null;
    }

    @PutMapping("")
    public Loan updateLoan() {
        // extend load

        // validate extention request

        // return created
        return null;
    }


}
