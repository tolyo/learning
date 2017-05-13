package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.util.DomainFilter;
import io.fourfinanceit.util.JsonDateSerializer;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * A loan that can be taken out by a customer
 */
@Entity
public class Loan implements Serializable, DomainFilter {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @JsonIgnore
    private Long id;

    @ManyToOne
    private Customer customer;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    private Date startDate;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    private Date endDate;

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

    public Loan() {};

    public Loan(Date startDate, Date endDate, BigDecimal amount) {
        this.startDate = startDate;
        this.endDate = endDate;
        this.amount = amount;
    }

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

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
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

    @Override
    public ObjectNode toJson() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        return node;
    }
}
