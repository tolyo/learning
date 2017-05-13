package io.fourfinanceit.util;

import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * An lightweight alternative to generating DTOs
 */
public interface DomainFilter {

    /**
     * A JSON object with public properties that can be revealed to HTTP clients
     * and messages.
     * @return
     */
    public ObjectNode toJson();
}
