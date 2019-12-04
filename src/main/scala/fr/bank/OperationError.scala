package fr.bank

/***
  * the different operation errors
  */
trait OperationError

object OperationError {

  class WithrawalError(code: Int, details: String) extends OperationError

  class AccountNotFoundError(code: Int, details: String) extends OperationError

  class InvalidAmount(code: Int, details: String) extends OperationError

}