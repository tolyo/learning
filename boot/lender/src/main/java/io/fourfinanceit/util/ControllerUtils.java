package io.fourfinanceit.util;

import org.springframework.validation.Errors;

import java.util.HashMap;
import java.util.Map;

abstract public class ControllerUtils {

    public static Map<String, String> getErrorMap(Errors errors ){
        Map<String, String> errorMap = new HashMap<>();
        errors.getAllErrors().forEach(x -> {
            errorMap.put(getLast(x.getCodes()[0].split("\\.")), x.getDefaultMessage());
        });
        return errorMap;
    }

    private static String getLast(String[] errors) {
        return errors[errors.length - 1];
    }
}
