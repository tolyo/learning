package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.util.DomainFilter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/*
 * A generic recipient of services provided by the application.
 */
@Entity
public class Customer implements DomainFilter {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @JsonIgnore
    private Long id;

    @NotNull
    private String number;

    @OneToMany(mappedBy = "customer")
    private Set<Loan> loans = new HashSet<>();

    @OneToMany(mappedBy = "customer")
    private Set<LoanApplicationAttempt> loandApplicationAttempts = new HashSet<>();

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

    public Customer() {}

    public Customer(String number) {
        this.number = number;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public Set<Loan> getLoans() {
        return loans;
    }

    public void setLoans(Set<Loan> loans) {
        this.loans = loans;
    }

    public Customer addLoan(Loan loan) {
        this.loans.add(loan);
        loan.setCustomer(this);
        return this;
    }

    public Customer removeLoan(Loan loan) {
        this.loans.remove(loan);
        loan.setCustomer(null);
        return this;
    }

    public Customer addLoandApplicationAttempt(LoanApplicationAttempt loanApplicationAttempt) {
        this.loandApplicationAttempts.add(loanApplicationAttempt);
        loanApplicationAttempt.setBorrower(this);
        return this;
    }

    public Customer removeLoandApplicationAttempt(LoanApplicationAttempt loanApplicationAttempt) {
        this.loandApplicationAttempts.remove(loanApplicationAttempt);
        loanApplicationAttempt.setBorrower(null);
        return this;
    }


    public Set<LoanApplicationAttempt> getLoandApplicationAttempts() {
        return loandApplicationAttempts;
    }

    public void setLoandApplicationAttempts(Set<LoanApplicationAttempt> loandApplicationAttempts) {
        this.loandApplicationAttempts = loandApplicationAttempts;
    }

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public Date getUpdated() {
        return updated;
    }

    public void setUpdated(Date updated) {
        this.updated = updated;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Customer)) return false;
        Customer customer = (Customer) o;
        return getId().equals(customer.getId());
    }

    @Override
    public int hashCode() {
        return getId().hashCode();
    }

    @Override
    public String toString() {
        return "Customer{" +
                "id=" + id +
                ", number='" + number + '\'' +
                ", created=" + created +
                '}';
    }

    @Override
    public ObjectNode toJson() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("number", number);
        ArrayNode loanApplicationAttemptsNode = new ArrayNode(JsonNodeFactory.instance);
        loandApplicationAttempts.stream().forEach(x -> loanApplicationAttemptsNode.add(x.toJson()));
        node.set("loadnAppllicationAttempts", loanApplicationAttemptsNode);
        return node;
    }
}
