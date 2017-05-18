package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.repository.LoanApplicationAttemptRepository;
import io.fourfinanceit.service.LoanService;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.Errors;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import static io.fourfinanceit.util.ControllerUtils.getErrorMap;

@RestController
@RequestMapping("/loans")
@Transactional
public class LoanResource {

    private final Logger log = LoggerFactory.getLogger(LoanResource.class);

    @Autowired
    LoanService loanService;

    @Autowired
    LoanApplicationAttemptRepository loanApplicationAttemptRepository;

    @PostMapping("")
    public ResponseEntity<ObjectNode> createLoan(
            @RequestBody @Valid LoanApplicationCommand loanApplicationCommand,
            Errors errors,
            HttpServletRequest request) {
        log.info("createLoan > " + loanApplicationCommand.toString() + " from " + request.getRemoteAddr());
        if (loanApplicationCommand.isValidApplicationAttempt()) loanApplicationAttemptRepository.save(loanApplicationCommand.getLoanApplicationAttempt());
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        } else {
            loanService.createLoan(loanApplicationCommand);
            return new ResponseEntity<>(HttpStatus.CREATED);
        }
    }

    @InitBinder("loanApplicationCommand")
    protected void initPostBinder(WebDataBinder binder) {
        binder.addValidators(new LoanApplicationCommand());
    }



}
