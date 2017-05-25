package com.tel.resource;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.tel.model.validators.PhoneValidator;
import com.tel.service.PhoneService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.Errors;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

import static com.tel.util.ControllerUtils.getErrorMap;

@RestController
@RequestMapping("/phones")
public class PhoneResource {

    @Autowired
    PhoneService phoneService;

    @GetMapping("")
    public ResponseEntity<ObjectNode> get(
            @RequestBody @Valid PhoneValidator cmd,
            Errors errors
    ) {
        if (errors.hasErrors())  {
            return ResponseEntity.unprocessableEntity().body(getErrorMap(errors));
        } else {
            return ResponseEntity.ok(cmd.toJsonNode());
        }
    }
}
