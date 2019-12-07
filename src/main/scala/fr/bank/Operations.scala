package fr.bank

import java.util.UUID

import fr.bank.domains.{BankAccount, Operation, User}
import fr.bank.model.OperationError.{AccountNotFoundError, InvalidAmount, UnknownOperationError, WithrawalError}
import fr.bank.model.OperationHelper.{OperationStatus, UsersResult}
import fr.bank.model._

import scala.annotation.tailrec

object Operations extends OperationType {


  /** *
    * a user can create an account
    * NB: a user can have several accounts.
    *
    * @param user
    * @return
    */

  def createAccount(user: User): User = {
    val bankAccount = BankAccount(UUID.randomUUID())
    val existingBankAccounts = user.bankAccounts
    user.copy(bankAccounts = existingBankAccounts + (bankAccount.accountId -> bankAccount))
  }


  /** *
    * based on a type, execute an operation that updates a user account
    *
    * @param `type`
    * @param operationAmount
    * @param userSrc
    * @param userDest
    * @param accountSrcId
    * @return
    */
  def makeOperation(`type`: String, operationAmount: Int, userSrc: User, userDest: Option[User] = None,
                    accountSrcId: UUID, accountDestId: Option[UUID]= None): Either[UsersResult, OperationError]
  = {
    if (operationAmount <= 0) {
      Right(new InvalidAmount(400, "cannot have a negative amount"))
    } else {
      val maybeOperationStatus = `type` match {
        case Deposit =>
          userSrc.bankAccounts.get(accountSrcId) match {
            case Some(account) =>
              val existingAmount = account.amount
              val newAccount = account.copy(amount = existingAmount + operationAmount)
              OperationStatus(accountSrc = Some(newAccount), `type` = Deposit)
            case _ =>
              OperationStatus(
                None,
                `type` = Deposit,
                error = Some(new AccountNotFoundError(404, "cannot find account, enter a correct account ID")))

          }

        case Withdrawal =>
          userSrc.bankAccounts.get(accountSrcId) match {
            case Some(account) =>
              val existingAmount = account.amount
              if (existingAmount >= operationAmount) {
                val newAccount = account.copy(amount = existingAmount - operationAmount)
                OperationStatus(accountSrc = Some(newAccount), `type` = Withdrawal)
              }
              else OperationStatus(
                None,
                `type` = Withdrawal,
                error = Some(new WithrawalError(401, "cannot make operation, not enough money in your account"))
              )

            case _ =>
              OperationStatus(
                None,
                `type` = Deposit,
                error = Some(new AccountNotFoundError(404, "cannot find account, enter a correct account ID")))
          }

        case TransactionDebit =>
           val maybeOperationStats = userDest.map {
             case userDest if accountDestId.isDefined =>
              val operationStatus = userSrc.bankAccounts.get(accountSrcId) match {
                case Some(account) =>
                  val existingSrcAmount = account.amount
                  if (existingSrcAmount >= operationAmount) {

                    //accountDestId.get is safe, there is the if condition above to check the existence of accountDestId
                    val newAccountDest = userDest.bankAccounts.get(accountDestId.get).map(account => account.copy(amount = account.amount + operationAmount))
                    val newAccountSrc = account.copy(amount = existingSrcAmount - operationAmount)

                    OperationStatus(Some(newAccountSrc),newAccountDest, `type` = TransactionDebit)
                  }
                  else OperationStatus(
                    None,
                    `type` = TransactionDebit,
                    error = Some(new WithrawalError(401, "cannot make operation, not enough money in your account"))
                  )

                case _ =>
                  OperationStatus(
                    None,
                    `type` = TransactionDebit,
                    error = Some(new AccountNotFoundError(404, "cannot find account, enter a correct account ID")))
              }

            operationStatus
          }

          maybeOperationStats match {
            case Some(status) => status
            case _ => OperationStatus(
              None,
              `type` = TransactionDebit,
              error = Some(new AccountNotFoundError(404, "cannot find the destination account, please specify one")))
          }


        case Credit => OperationStatus(None, None, Credit) //TODO : Develop this feature
        case _ => OperationStatus(None, None, Credit) //TODO : refactor ...
      }



      val newAccountSrc = maybeOperationStatus.accountSrc
      val newAccountDest = maybeOperationStatus.accountDest
      val operationError = maybeOperationStatus.error
      val operationType = maybeOperationStatus.`type`

      (newAccountSrc, newAccountDest, operationType, operationError) match {
          /*** Error case ***/
        case (_, _, _, Some(error)) => Right(error)

          /*** Deposit or Withdrawal operation***/
        case (Some(account), None, `type`, None) =>
          val operation = Operation(amount = operationAmount, operationType = `type`)
          val newAccount = account.copy(operations = account.operations :+ operation)
          val newUserSrc = userSrc.copy(bankAccounts = Map(newAccount.accountId -> newAccount))

          Left(UsersResult(userSrc = newUserSrc))

          /*** Transaction operation ***/
        case (Some(srcAccount), Some(destAccount), `type`, None) =>
          val operationSrc = Operation(amount = operationAmount, operationType = TransactionDebit)
          val operationDest = Operation(amount = operationAmount, operationType = TransactionTransfer)
          val newAccountSrc = srcAccount.copy(operations = srcAccount.operations :+ operationSrc)
          val newAccountDest = destAccount.copy(operations = destAccount.operations :+ operationDest)
          val newUserSrc = userSrc.copy(bankAccounts = Map(newAccountSrc.accountId -> newAccountSrc))
          val newUserDest = userSrc.copy(bankAccounts = Map(newAccountDest.accountId -> newAccountDest))

          Left(UsersResult(userSrc = newUserSrc, userDest = Some(newUserDest)))


        case _ =>
          Right(new UnknownOperationError(500, "Unknown Error"))

      }
      
    }
  }

  /** *
    * get operation history
    *
    * @param bankAccount
    * @return
    */
  def showHistory(bankAccount: BankAccount): Seq[Operation] = bankAccount.operations

  /** *
    * count how many operations there are by type
    *
    * @param bankAccount
    * @param operationType
    * @return
    */
  def getOperationsByType(bankAccount: BankAccount, operationType: String): Int = {
    bankAccount.operations.foldLeft(0) {
      case (acc, operation) =>
        if (operation.operationType.equals(operationType))
          acc + 1
        else acc
    }
  }

  /** *
    * calculate the expected amount in a count, useful if we want to have the variables immutability in our database
    *
    * @param operations
    * @return
    */
  @tailrec
  private def caluclateAmount(operations: Iterator[Operation], sum: Int = 0): Int = {
    if (operations.isEmpty) sum
    else {
      val operation = operations.next()
      if (operation.operationType == Deposit || operation.operationType == TransactionTransfer) {
        caluclateAmount(operations, sum + operation.amount)
      }
      else if (operation.operationType == Withdrawal || operation.operationType == TransactionDebit) {
        caluclateAmount(operations, sum - operation.amount)
      }
      else {
        caluclateAmount(operations)
      }
    }

  }


  /** *
    * check if there is a consistency between operations and the actual existing amount in the account
    *
    * @param bankAccount
    * @return
    */
  def checkAccountValidity(bankAccount: BankAccount): Boolean = {
    val accountSum = caluclateAmount(bankAccount.operations.iterator)
    println(accountSum)
    if (accountSum == bankAccount.amount) true
    else false
  }


}
