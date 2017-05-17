package io.fourfinanceit.repository;

import io.fourfinanceit.domain.LoanExtension;
import org.springframework.data.jpa.repository.JpaRepository;

public interface LoanExtensionRepository
        extends JpaRepository<LoanExtension, Long> { }
