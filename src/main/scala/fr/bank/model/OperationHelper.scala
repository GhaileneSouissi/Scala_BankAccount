package fr.bank.model

import fr.bank.domains.{BankAccount, User}

object OperationHelper {

  case class OperationStatus(accountSrc: Option[BankAccount], accountDest: Option[BankAccount] = None, `type`: String, error: Option[OperationError] = None)

  case class UsersResult(userSrc: User, userDest: Option[User] = None)

}