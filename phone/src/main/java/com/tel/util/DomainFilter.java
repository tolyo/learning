package com.tel.util;

import com.fasterxml.jackson.databind.node.ObjectNode;

public interface DomainFilter {
    ObjectNode toJsonNode();
}