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
import org.springframework.util.Assert;

@Service
@Transactional
public class LoanService {

    @Autowired
    LoanRepository loanRepository;

    @Autowired
    LoanExtensionRepository loanExtensionRepository;

    public Loan createLoan(LoanApplicationCommand cmd) {
        Loan loan = cmd.getLoan();
        Loan existingLoan = loanRepository.findByNumber(loan.getNumber());
        if (existingLoan != null) return createLoan(cmd);
        else return loanRepository.save(loan);
    }

    public LoanExtension extendLoan(LoanExtensionCommand cmd) {
        Assert.notNull(cmd, "LoanExtensionCommand must not be null");
        Assert.notNull(cmd.getLoan(), "Loan must not be null");

        // Update end date for loan
        Loan loan = cmd.getLoan();
        cmd.getLoanExtension().setStartDate(loan.getEndDate());
        loan.setEndDate(cmd.getEndDate());
        loanRepository.save(loan);

        return loanExtensionRepository.save(cmd.getLoanExtension());
    }
}
