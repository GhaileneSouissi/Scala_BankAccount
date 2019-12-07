package operations


import java.util.UUID

import fr.bank.Operations._
import fr.bank.domains.{BankAccount, User}
import fr.bank.model.OperationError.AccountNotFoundError
import fr.bank.model.OperationType
import org.scalatest.EitherValues._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class DepositSpec extends FlatSpec
  with Matchers
  with GivenWhenThen
  with OperationType {

  "a desposit operation" should "add the amount to the user account" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val user = User(userId = userId, name = "ghailene", age = 25, bankAccounts = Map(accountId -> BankAccount(accountId)))
    val operationType = Deposit

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountSrcId = accountId)

    Then("a success operation, and the amount is equal to the amount deposited")

    operationStatus.isLeft shouldBe true
    operationStatus.left.value.userSrc.bankAccounts(accountId).amount shouldBe 50

  }

  "a desposit operation" should "return a not found error" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val accountId2 = UUID.randomUUID()
    val user = User(name = "ghailene", age = 25, bankAccounts = Map(accountId2 -> BankAccount(accountId)))
    val operationType = Deposit

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountSrcId = accountId)

    Then("an error, and thus the amount is unchanged")
    operationStatus.isRight shouldBe true

    println(operationStatus.right.value)
    operationStatus.right.value.isInstanceOf[AccountNotFoundError] shouldBe true
  }


}
