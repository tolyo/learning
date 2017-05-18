package io.fourfinanceit.util;

/**
 * Helper for setting risk range
 */
public class RiskRange {

    private Integer startHour;
    private Integer endHour;

    public Integer getStartHour() {
        if (this.startHour == null) {
            this.startHour = Integer.valueOf(SpringEnvironment.get().getProperty("loan.risk.hourstart"));
        }
        return startHour;
    }

    public void setStartHour(Integer startHour) {
        this.startHour = startHour;
    }

    public Integer getEndHour() {
        if (this.endHour == null) {
            this.endHour = Integer.valueOf(SpringEnvironment.get().getProperty("loan.risk.hourend"));
        }
        return endHour;
    }

    public void setEndHour(Integer endHour) {
        this.endHour = endHour;
    }
}
