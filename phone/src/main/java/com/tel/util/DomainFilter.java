package com.tel.util;

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * An lightweight alternative to generating DTOs
 */
public interface DomainFilter {

    /**
     * A JSON object with public properties that can be revealed to HTTP clients
     * and messages.
     * @return Map of properties for json serialization
     */
    ObjectNode toJsonNode();
}