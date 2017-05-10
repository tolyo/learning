package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.hibernate.annotations.CreationTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.util.Date;
import java.util.Objects;

/**
 * For every loan there may be several application attempts
 */
public class LoanApplicationAttempt {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    @NotNull
    private String ip;

    @NotNull
    private LocalDate dateCreated;

    @ManyToOne
    private Customer customer;

    @NotNull
    @CreationTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date created = new Date();

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

    public LocalDate getDateCreated() {
        return dateCreated;
    }

    public LoanApplicationAttempt dateCreated(LocalDate dateCreated) {
        this.dateCreated = dateCreated;
        return this;
    }

    public void setDateCreated(LocalDate dateCreated) {
        this.dateCreated = dateCreated;
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
                ", dateCreated='" + dateCreated + "'" +
                '}';
    }
}
