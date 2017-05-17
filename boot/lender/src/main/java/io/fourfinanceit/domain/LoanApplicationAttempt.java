package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.util.DomainFilter;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.Objects;

/**
 * For every loan there may be several application attempts
 */
@Entity
public class LoanApplicationAttempt implements DomainFilter {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @JsonIgnore
    private Long id;

    @NotNull
    private String ip;

    @ManyToOne
    @JsonIgnore
    private Customer customer;

    @NotNull
    @CreationTimestamp
    @Temporal(TemporalType.TIMESTAMP)
    @JsonIgnore
    private Date created = new Date();

    @NotNull
    @UpdateTimestamp
    @Temporal(TemporalType.TIMESTAMP)
    @JsonIgnore
    private Date updated = new Date();

    public LoanApplicationAttempt() {}

    public LoanApplicationAttempt(LoanApplicationCommand cmd) {
        this.customer = cmd.getCustomer();
        this.ip = cmd.getIp();
    }

    public LoanApplicationAttempt(Customer customer, String ip) {
        this.customer = customer;
        this.ip = ip;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getIp() {
        return ip;
    }

    public LoanApplicationAttempt ip(String ip) {
        this.ip = ip;
        return this;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public Customer getBorrower() {
        return customer;
    }

    public LoanApplicationAttempt customer(Customer customer) {
        this.customer = customer;
        return this;
    }

    public void setBorrower(Customer customer) {
        this.customer = customer;
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
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        LoanApplicationAttempt loanApplicationAttempt = (LoanApplicationAttempt) o;
        if (loanApplicationAttempt.id == null || id == null) {
            return false;
        }
        return Objects.equals(id, loanApplicationAttempt.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "LoanApplicationAttempt{" +
                "id=" + id +
                ", ip='" + ip + "'" +
                '}';
    }

    @Override
    public ObjectNode toJson() {
        return new ObjectNode(JsonNodeFactory.instance)
                .put("ip", ip);
    }
}
