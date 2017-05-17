package io.fourfinanceit.service;

import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class LoanService {

    @Autowired
    LoanRepository loanRepository;

    public Loan createLoan(LoanApplicationCommand cmd) {
        return loanRepository.save(cmd.getLoan());
    }
}
