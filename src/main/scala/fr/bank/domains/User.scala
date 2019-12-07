package fr.bank.domains

import java.util.UUID

sealed case class User(
                        userId: UUID = UUID.randomUUID(),
                        name: String, age: Int,
                        bankAccounts: Map[UUID, BankAccount] = Map.empty[UUID, BankAccount])

