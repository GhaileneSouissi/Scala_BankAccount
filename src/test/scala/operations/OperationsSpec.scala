package operations
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID

import fr.bank.Operations._
import fr.bank.{BankAccount, Operation, User}
import org.scalatest.EitherValues._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class OperationsSpec extends FlatSpec with Matchers with GivenWhenThen {

  "a desposit operation" should "add the amount to the user account" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val user = User(userId = userId, name = "ghailene", age =  25, bankAccounts = Map(accountId -> BankAccount(accountId)))
    val operationType = "DEPOSIT"

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountId = accountId)

    Then("a success operation, and the amount is equal to the amount deposited")
    val operation = Operation(LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES), 50, operationType)
    val bankAccount = BankAccount(accountId, 50, Seq(operation))
    val resultUser =  User(userId = userId, name = "ghailene", age =  25, bankAccounts = Map(bankAccount.accountId -> bankAccount))
    operationStatus shouldBe Left(resultUser)

  }

  "a deposit operation" should "add the amount to the user account" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val user = User(userId = userId, name = "ghailene", age =  25, bankAccounts = Map(accountId -> BankAccount(accountId)))
    val operationType = "DEPOSIT"

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountId = accountId)

    Then("a success operation, and the amount is equal to the amount deposited")
    val operation = Operation(LocalDateTime.now().truncatedTo(ChronoUnit.MINUTES), 50, operationType)
    val bankAccount = BankAccount(accountId, 50, Seq(operation))
    val resultUser =  User(userId = userId, name = "ghailene", age =  25, bankAccounts = Map(bankAccount.accountId -> bankAccount))
    operationStatus shouldBe Left(resultUser)

  }

  "a desposit operation" should "return a not found error" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val accountId2 = UUID.randomUUID()
    val user = User(name = "ghailene", age =  25, bankAccounts = Map(accountId2 -> BankAccount(accountId)))
    val operationType = "DEPOSIT"

    When("make a deposit in his account")
    val operationStatus = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountId = accountId)

    Then("an error, and thus the amount is unchanged")
    operationStatus.isRight shouldBe true
  }


  "check validity" should "return true" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val user = User(name = "ghailene", age =  25, bankAccounts = Map(accountId -> BankAccount(accountId)))
    val operationType = "DEPOSIT"
    val operationType2 = "WITHDRAWAL"

    When("make some operations")

    val newUser = makeOperation(`type` = operationType, operationAmount = 50, userSrc = user, accountId = accountId) match {
      case Left(user1) =>
        makeOperation(`type` = operationType, operationAmount = 50, userSrc = user1, accountId = accountId) match {
          case Left(user2) => makeOperation(`type` = operationType2, operationAmount = 25, userSrc = user2, accountId = accountId)
        }
    }


    Then("we check the consistency of operations and return true")
    checkAccountValidity(newUser.left.value.bankAccounts(accountId)) shouldBe true





  }
}
