package io.fourfinanceit.validation;

import javax.validation.constraints.NotNull;

/**
 * Created by anatoly on 17.11.5.
 */
public class LoanExtentionCommand {

    @NotNull
    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
