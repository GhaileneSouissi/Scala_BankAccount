package fr.bank

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID


//the bank account
sealed case class BankAccount(accountId: UUID, amount: Int = 0, operations: Seq[Operation] = Nil)

//a user
sealed case class User(userId: UUID = UUID.randomUUID(), name: String, age: Int, bankAccounts: Map[UUID, BankAccount] = Map.empty[UUID, BankAccount])

//the operation, it takes the system date time, remove seconds (for testing reason)
sealed case class Operation(operationDate: LocalDateTime = LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES), amount: Int, operationType: String)


// TODO : define all
