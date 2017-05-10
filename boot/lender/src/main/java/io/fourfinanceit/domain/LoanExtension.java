package io.fourfinanceit.domain;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.util.Objects;

/**
 * Loan extentions modify a loan
 */
public class LoanExtension {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    @NotNull
    private LocalDate startDate;

    @NotNull
    private LocalDate endDate;

    @NotNull
    private LocalDate dateCreated;

    @NotNull
    private LocalDate lastUpdated;

    @ManyToOne
    private Loan loan;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public LoanExtension startDate(LocalDate startDate) {
        this.startDate = startDate;
        return this;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public LoanExtension endDate(LocalDate endDate) {
        this.endDate = endDate;
        return this;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public LocalDate getDateCreated() {
        return dateCreated;
    }

    public LoanExtension dateCreated(LocalDate dateCreated) {
        this.dateCreated = dateCreated;
        return this;
    }

    public void setDateCreated(LocalDate dateCreated) {
        this.dateCreated = dateCreated;
    }

    public LocalDate getLastUpdated() {
        return lastUpdated;
    }

    public LoanExtension lastUpdated(LocalDate lastUpdated) {
        this.lastUpdated = lastUpdated;
        return this;
    }

    public void setLastUpdated(LocalDate lastUpdated) {
        this.lastUpdated = lastUpdated;
    }

    public Loan getLoan() {
        return loan;
    }

    public LoanExtension loan(Loan loan) {
        this.loan = loan;
        return this;
    }

    public void setLoan(Loan loan) {
        this.loan = loan;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        LoanExtension loanExtension = (LoanExtension) o;
        if (loanExtension.id == null || id == null) {
            return false;
        }
        return Objects.equals(id, loanExtension.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "LoanExtension{" +
                "id=" + id +
                ", startDate='" + startDate + "'" +
                ", endDate='" + endDate + "'" +
                ", dateCreated='" + dateCreated + "'" +
                ", lastUpdated='" + lastUpdated + "'" +
                '}';
    }
}
