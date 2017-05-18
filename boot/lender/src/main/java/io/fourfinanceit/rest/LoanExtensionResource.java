package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.service.LoanService;
import io.fourfinanceit.validation.LoanExtensionCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.Errors;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

import static io.fourfinanceit.util.ControllerUtils.getErrorMap;

@RestController
@RequestMapping("/loanextensions")
@Transactional
public class LoanExtensionResource {

    private final Logger log = LoggerFactory.getLogger(LoanExtensionResource.class);

    @Autowired
    LoanService loanService;

    @PostMapping("")
    public ResponseEntity<ObjectNode> createLoanExtension(
            @RequestBody @Valid LoanExtensionCommand cmd,
            Errors errors) {
        log.info("updateLoan > " + cmd.toString());
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        } else {
            loanService.extendLoan(cmd);
            return new ResponseEntity<>(HttpStatus.CREATED);
        }
    }

    @InitBinder("loanExtensionCommand")
    protected void initPostBinder(WebDataBinder binder) {
        binder.addValidators(new LoanExtensionCommand());
    }
}
