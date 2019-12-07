package fr.bank.model

/** *
  * operation types
  */
trait OperationType {
  val Deposit = "DEPOSIT"
  val Withdrawal = "WITHDRAWAL"
  val Credit = "CREDIT"
  val TransactionDebit = "TRANSACTION_DEBIT"
  val TransactionTransfer = "TRANSACTION_TRANSFER"
}
