package operations


import java.util.UUID

import fr.bank.Operations.{TransactionDebit, _}
import fr.bank.domains.{BankAccount, User}
import fr.bank.model.OperationError.AccountNotFoundError
import fr.bank.model.OperationType
import org.scalatest.EitherValues._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class TransactionSpec extends FlatSpec
  with Matchers
  with GivenWhenThen
  with OperationType {

  "a transaction operation" should "add the amount to the dest user and withdrawal the amount from the src user" in {
    Given("a source user with a bank account")
    val accountSrcId = UUID.randomUUID()
    val userSrcId = UUID.randomUUID()
    val userSrc = User(userId = userSrcId, name = "ghailene", age = 25, bankAccounts = Map(accountSrcId -> BankAccount(accountSrcId, 100)))

    Given("a destination user with a bank account")
    val accountDestId = UUID.randomUUID()
    val userDestId = UUID.randomUUID()
    val userDest = User(userId = userDestId, name = "titi", age = 18, bankAccounts = Map(accountDestId -> BankAccount(accountDestId)))

    val operationType = TransactionDebit

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50,
      userSrc = userSrc,userDest = Some(userDest), accountSrcId = accountSrcId, accountDestId = Some(accountDestId))

    Then("a success operation, and the amount is equal to the amount deposited")

    operationStatus.isLeft shouldBe true

    operationStatus.left.value.userSrc.bankAccounts(accountSrcId).amount shouldBe 50
    operationStatus.left.value.userSrc.bankAccounts(accountSrcId).operations(0).operationType shouldBe TransactionDebit


    operationStatus.left.value.userDest.map(_.bankAccounts(accountDestId).amount) shouldBe Some(50)
    operationStatus.left.value.userDest.map(_.bankAccounts(accountDestId).operations(0).operationType) shouldBe Some(TransactionTransfer)



  }

}
