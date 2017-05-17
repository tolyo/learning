package io.fourfinanceit.service;

import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanExtension;
import io.fourfinanceit.repository.LoanExtensionRepository;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.validation.LoanApplicationCommand;
import io.fourfinanceit.validation.LoanExtensionCommand;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class LoanService {

    @Autowired
    LoanRepository loanRepository;

    @Autowired
    LoanExtensionRepository loanExtensionRepository;

    public Loan createLoan(LoanApplicationCommand cmd) {
        return loanRepository.save(cmd.getLoan());
    }

    public LoanExtension extendLoan(LoanExtensionCommand cmd) {
        return loanExtensionRepository.save(cmd.getLoanExtension());
    }
}
