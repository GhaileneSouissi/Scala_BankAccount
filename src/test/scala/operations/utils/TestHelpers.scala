package operations.utils

import java.util.UUID

import fr.bank.Operations.makeOperation
import fr.bank.domains.User
import org.scalatest.EitherValues._

object TestHelpers {

  /**
    * apply multiple operations, useful form credit, withdrawal and deposit transformation
    *
    * @param userSrc
    * @param userDest
    * @param operations
    * @param accountId
    * @return
    */
  private[operations] def applyOperations(userSrc: User, userDest: Option[User] = None, operations: Seq[(String, Int)], accountId: UUID) = {
    operations.foldLeft(userSrc) {
      case (newUser, op) =>
        val operationType = op._1
        val amount = op._2
        makeOperation(`type` = operationType, operationAmount = amount, userSrc = newUser,userDest = userDest, accountSrcId = accountId).left.value.userSrc
    }
  } //TODO : take transaction into consideration.

}
