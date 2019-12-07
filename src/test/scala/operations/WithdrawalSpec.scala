package operations

import java.util.UUID

import fr.bank.Operations._
import fr.bank.domains.{BankAccount, User}
import fr.bank.model.OperationError.{AccountNotFoundError, WithrawalError}
import fr.bank.model.OperationType
import org.scalatest.EitherValues._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class WithdrawalSpec extends FlatSpec
  with Matchers
  with GivenWhenThen
  with OperationType {

  "a withdrawal operation" should "return success, and remove the amount from the bank user" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val user = User(userId = userId, name = "ghailene", age = 25, bankAccounts = Map(accountId -> BankAccount(accountId, 50)))
    val operationType = Withdrawal

    When("make a withdrawal in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 25, userSrc = user, accountSrcId = accountId)

    Then("a success operation, and the amount is equal to the amount after the withdrawal")

    operationStatus.isLeft shouldBe true
    operationStatus.left.value.userSrc.bankAccounts(accountId).amount shouldBe 25
  }

  "a withdrawal operation" should "return an error if there us no sufficient amount" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val user = User(userId = userId, name = "ghailene", age = 25, bankAccounts = Map(accountId -> BankAccount(accountId, 50)))
    val operationType = Withdrawal

    When("make a withdrawal in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 60, userSrc = user, accountSrcId = accountId)

    Then("a success operation, and the amount is equal to the amount after the withdrawal")

    operationStatus.isRight shouldBe true
    operationStatus.right.value.isInstanceOf[WithrawalError] shouldBe true
  }

  "a withdrawal operation" should "return an error if the account Id does not exist" in {

    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val accountId2 = UUID.randomUUID()
    val user = User(name = "ghailene", age = 25, bankAccounts = Map(accountId2 -> BankAccount(accountId)))
    val operationType = Deposit

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountSrcId = accountId)

    Then("an error, and thus the amount is unchanged")
    operationStatus.isRight shouldBe true
    operationStatus.right.value.isInstanceOf[AccountNotFoundError] shouldBe true
  }


}
