package io.fourfinanceit.util;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.validation.Errors;

import java.util.List;

abstract public class ControllerUtils {

    public static ObjectNode getErrorMap(Errors errors ){
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        errors.getAllErrors().forEach(x -> {
            node.put(getLast(x.getCodes()[0].split("\\.")), x.getDefaultMessage());
        });
        return node;
    }

    public static ArrayNode toArrayNode(List<? extends DomainFilter> list) {
        ArrayNode arrayNode = new ArrayNode(JsonNodeFactory.instance);
        list.forEach(x -> arrayNode.add(x.toJson()));
        return arrayNode;
    }

    private static String getLast(String[] errors) {
        return errors[errors.length - 1];
    }
}
