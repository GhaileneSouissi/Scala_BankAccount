package operations

import java.util.UUID

import fr.bank.Operations._
import fr.bank.domains.{BankAccount, User}
import operations.utils.TestHelpers.applyOperations
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class ValiditySpec extends FlatSpec
  with Matchers
  with GivenWhenThen {



  "check validity" should "return true" in {
    Given("a user with a bank account")
    val accountId = UUID.randomUUID()
    val user = User(name = "ghailene", age = 25, bankAccounts = Map(accountId -> BankAccount(accountId)))
    val operationType = Deposit
    val operationType2 = Withdrawal

    When("make some operations")

    val operations = Seq((operationType, 50), (operationType, 50), (operationType2, 25))

    val newUser = applyOperations(userSrc = user, operations = operations, accountId = accountId)


    Then("we check the consistency of operations and return true")
    checkAccountValidity(newUser.bankAccounts(accountId)) shouldBe true

  }


}
