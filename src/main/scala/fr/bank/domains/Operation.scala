package fr.bank.domains

import java.time.LocalDateTime

sealed case class Operation(
                             operationDate: LocalDateTime = LocalDateTime.now(),
                             amount: Int,
                             operationType: String)

