package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * A loan that can be taken out by a customer
 */
@Entity
public class Loan implements Serializable {

    @Id
    @NotNull
    @GeneratedValue(strategy = GenerationType.AUTO)
    @JsonIgnore
    private Long id;

    @ManyToOne
    private Customer customer;

    @NotNull
    private LocalDate startDate;

    @NotNull
    private LocalDate endDate;

    @NotNull
    @Column(precision=10, scale=2)
    private BigDecimal amount;

    @NotNull
    @Column(precision=10, scale=2)
    private BigDecimal rate;

    @NotNull
    @CreationTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date created = new Date();

    @NotNull
    @UpdateTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date updated = new Date();

    @OneToMany(mappedBy = "loan")
    @JsonIgnore
    private Set<LoanExtension> loanExtensions = new HashSet<>();

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Customer getCustomer() {
        return customer;
    }

    public void setCustomer(Customer customer) {
        this.customer = customer;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public BigDecimal getRate() {
        return rate;
    }

    public void setRate(BigDecimal rate) {
        this.rate = rate;
    }

    public Date getUpdated() {
        return updated;
    }

    public void setUpdated(Date updated) {
        this.updated = updated;
    }

    public Set<LoanExtension> getLoanExtensions() {
        return loanExtensions;
    }

    public void setLoanExtensions(Set<LoanExtension> loanExtensions) {
        this.loanExtensions = loanExtensions;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Loan loan = (Loan) o;

        return id != null ? id.equals(loan.id) : loan.id == null;

    }

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "Loan{" +
                "id=" + id +
                ", customer=" + customer +
                ", startDate=" + startDate +
                ", endDate=" + endDate +
                ", amount=" + amount +
                ", rate=" + rate +
                ", created=" + created +
                ", updated=" + updated +
                ", loanExtensions=" + loanExtensions +
                '}';
    }
}
