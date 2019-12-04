package fr.bank

import java.util.UUID

import fr.bank.OperationError.{AccountNotFoundError, InvalidAmount}

object Operations extends OperationType {


  /***
    * a user can create an account
    * NB: a user can have several accounts.
    * @param user
    * @return
    */

  def createAccount(user: User): User = {
    val bankAccount = BankAccount(UUID.randomUUID())
    val existingBankAccounts = user.bankAccounts
    user.copy(bankAccounts = existingBankAccounts + (bankAccount.accountId -> bankAccount))
  }


  /***
    * based on a type, execute an operation that updates a user account
    * @param `type`
    * @param operationAmount
    * @param userSrc
    * @param userDest
    * @param accountId
    * @return
    */
  def makeOperation(`type`: String, operationAmount: Int, userSrc: User, userDest: Option[User] = None, accountId: UUID): Either[User, OperationError]
  = {
    if (operationAmount <= 0) {
      Right(new InvalidAmount(400, "cannot have a negative amount"))
    } else {
      val maybeNewBankAccount = `type` match {
        case Deposit =>
          val newAccount = userSrc.bankAccounts.get(accountId).map { account =>
            val existingAmount = account.amount
            account.copy(amount = existingAmount + operationAmount)
          }
          (newAccount, Deposit)

        case Withdrawal =>
          val newAccount = userSrc.bankAccounts.get(accountId).map { account =>
            val existingAmount = account.amount
            if (existingAmount >= operationAmount)
              account.copy(amount = existingAmount - operationAmount)
            else account
          }

          (newAccount, Withdrawal)

        case Transaction =>
          val newAccount = userDest.flatMap { userDest =>
            userSrc.bankAccounts.get(accountId).map { account =>

              val existingSrcAmount = account.amount
              if (existingSrcAmount >= operationAmount) {
                userDest.bankAccounts.get(accountId).map(account => account.copy(amount = account.amount + operationAmount))
                account.copy(amount = existingSrcAmount - operationAmount)
              }
              else account

            }
          }
          (newAccount, Transaction)


        case Credit => (None, Credit) //TODO : Develop this feature
      }


      maybeNewBankAccount match {
        case (Some(account), operationType) =>
          val operation = Operation(amount = operationAmount, operationType = operationType)
          val newAccount = account.copy(operations = account.operations :+ operation)
          val newUserSrc = userSrc.copy(bankAccounts = Map(newAccount.accountId -> newAccount))

          Left(newUserSrc)
        case _ => Right(new AccountNotFoundError(404, "cannot find account, enter a correct account ID")) //TODO : need to specify error codes, refactor makeOperations to have a more specific error code
      }
    }
  }

  /***
    * get operation history
    * @param bankAccount
    * @return
    */
  def showHistory(bankAccount: BankAccount) = bankAccount.operations

  /***
    * count how many operations there are by type
    * @param bankAccount
    * @param operationType
    * @return
    */
  def getOperationsByType(bankAccount: BankAccount, operationType: String) = {
    bankAccount.operations.foldLeft(0) {
      case (acc, operation) =>
        if (operation.operationType.equals(operationType))
          acc + 1
        else acc
    }
  }

  /***
    * calculate the expected amount in a count
    * @param operations
    * @return
    */
  def caluclateAmount(operations: Iterator[Operation]): Int = { // TODO: maybe tail recursive ....

    if (operations.isEmpty) 0
    else {
      val operation = operations.next()
      if (operation.operationType == Deposit) {
        println("here deposit")
        caluclateAmount(operations) + operation.amount
      }
      else if (operation.operationType == Withdrawal) {
        println("here with")
        caluclateAmount(operations) - operation.amount
      }
      else {
        caluclateAmount(operations)
      } //TODO : change transactionType to know if it's a deposit or a withdrawal
    }

  }


  /***
    * check if there is a consistency between operations and the actual existing amount in the account
    * @param bankAccount
    * @return
    */
  def checkAccountValidity(bankAccount: BankAccount) = { //TODO : expect if there is a transaction
    val accountSum = caluclateAmount(bankAccount.operations.iterator)
    println(accountSum)
    if (accountSum == bankAccount.amount) true
    else false
  }


}
