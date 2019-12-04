package fr.bank

/***
  * operation types
  */
trait OperationType extends Enumeration {
  val Deposit = "DEPOSIT"
  val Withdrawal = "WITHDRAWAL"
  val Credit = "CREDIT"
  val Transaction = "TRANSACTION"
}
