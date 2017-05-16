package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.validation.LoanApplicationCommand;
import io.fourfinanceit.validation.LoanExtensionCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

    @PostMapping("")
    public ResponseEntity<Object> createLoan(
            @RequestBody @Valid LoanApplicationCommand loanApplicationCommand,
            Errors errors,
            HttpServletRequest request) {
        log.info("createLoan > " + loanApplicationCommand.toString() + " from " + request.getRemoteAddr());
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        } else {
        }
        return null;
    }

    @PutMapping("")
    public ResponseEntity<ObjectNode> updateLoan(
            @RequestBody @Valid LoanExtensionCommand loanExtensionCommand,
            Errors errors) {
        log.info("updateLoan > " + loanExtensionCommand.toString());
        // extend load
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        }
        // validate extention request

        // return created
        return null;
    }

    @InitBinder("loanApplicationCommand")
    protected void initPostBinder(WebDataBinder binder) {
        binder.addValidators(new LoanApplicationCommand());
    }

    @InitBinder("loanExtensionCommand")
    protected void initPutBinder(WebDataBinder binder) {
        binder.addValidators(new LoanExtensionCommand());
    }


}
