package fr.bank.domains

import java.util.UUID

sealed case class BankAccount(
                          accountId: UUID,
                          amount: Int = 0,
                          operations: Seq[Operation] = Nil)

