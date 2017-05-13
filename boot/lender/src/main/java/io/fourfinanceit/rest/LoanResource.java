package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.validation.LoanApplicationCommand;
import io.fourfinanceit.validation.LoanExtentionCommand;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.Errors;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

import static io.fourfinanceit.util.ControllerUtils.getErrorMap;

@RestController
@RequestMapping("/loans")
public class LoanResource {

    @PostMapping("")
    public ResponseEntity<Object> createLoan(@Valid LoanApplicationCommand cmd, Errors errors) {
        // create a new loan for a customer
        // extend load
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        }
        // create and save and application attempt
        // validate application request

        return null;
    }

    @PutMapping("")
    public ResponseEntity<ObjectNode> updateLoan(@Valid LoanExtentionCommand cmd, Errors errors) {
        // extend load
        if (errors.hasErrors()) {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        }
        // validate extention request

        // return created
        return null;
    }


}
