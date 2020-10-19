package error_handling
import java.util.Calendar

object ErrorHandling {

  final case class CardHolderName(value: String) extends AnyVal
  final case class CardNumber(value: String) extends AnyVal
  final case class ValidityDate(month: Int, year: Int)
  final case class SecurityCode(value: String) extends AnyVal

  case class CreditCard(cardHolderName: CardHolderName, cardNumber: CardNumber, validityDate: ValidityDate, securityCode: SecurityCode)

  sealed trait ValidationError
  object ValidationError {
    final case object CardHolderNameContainsSpecialChars extends ValidationError {
      override def toString: String = "Card holder name cannot contain special characters"
    }
    final case object CardNumberContainsNonNumericChars extends ValidationError {
      override def toString: String = "Card number can only contain numeric characters"
    }
    final case object CardNumberInvalidLength extends ValidationError {
      override def toString: String = "Card number can only contain 16 digits"
    }
    final case object ValidityDateExpired extends ValidationError {
      override def toString: String = "Validity date is invalid"
    }
    final case object SecurityNumberContainsNonNumericChars extends ValidationError {
      override def toString: String = "Security number can only contain numeric characters"
    }
    final case object SecurityNumberInvalidLength extends ValidationError {
      override def toString: String = "Security number can only contain 3 digits"
    }
  }

  import cats.data.ValidatedNec
  import cats.syntax.all._

  object CreditCardValidator {

    import ValidationError._
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardHolderName(cardHolderName: CardHolderName): AllErrorsOr[CardHolderName] = {
      def validateCardHolderNameChars: AllErrorsOr[CardHolderName] =
        if (cardHolderName.value.matches("^[a-zA-Z]*$")) cardHolderName.validNec
        else CardHolderNameContainsSpecialChars.invalidNec

      validateCardHolderNameChars
    }

    private def validateCardNumber(cardNumber: CardNumber): AllErrorsOr[CardNumber] = {
      def validateCardNumberContents: AllErrorsOr[CardNumber] =
        if (cardNumber.value.matches("^[0-9]*$")) cardNumber.validNec
        else CardNumberContainsNonNumericChars.invalidNec

      def validateCardNumberLength: AllErrorsOr[CardNumber] =
        if (cardNumber.value.length == 16) cardNumber.validNec
        else CardNumberInvalidLength.invalidNec

      validateCardNumberLength.productR(validateCardNumberContents)
    }

    private def validateValidityDate(validityDate: ValidityDate): AllErrorsOr[ValidityDate] = {
      def validateValidityDateExpirationDate: AllErrorsOr[ValidityDate] = {
        val dT = Calendar.getInstance()
        if (validityDate.year > dT.get(Calendar.YEAR) || validityDate.year == dT.get(Calendar.YEAR) && validityDate.month > dT.get(Calendar.MONTH))
          validityDate.validNec
        else
          ValidityDateExpired.invalidNec
      }

      validateValidityDateExpirationDate
    }

    private def validateSecurityNumber(securityCode: SecurityCode): AllErrorsOr[SecurityCode] = {
      def validateSecurityCodeContents: AllErrorsOr[SecurityCode] =
        if (securityCode.value.matches("^[0-9]*$")) securityCode.validNec
        else SecurityNumberContainsNonNumericChars.invalidNec

      def validateSecurityCodeLength: AllErrorsOr[SecurityCode] =
        if (securityCode.value.length == 3) securityCode.validNec
        else SecurityNumberInvalidLength.invalidNec

      validateSecurityCodeLength.productR(validateSecurityCodeContents)
    }

    def validate(cardHolderName: CardHolderName, cardNumber: CardNumber, validityDate: ValidityDate, securityCode: SecurityCode): AllErrorsOr[CreditCard] = {
      (validateCardHolderName(cardHolderName), validateCardNumber(cardNumber), validateValidityDate(validityDate), validateSecurityNumber(securityCode)).mapN(CreditCard)
    }
  }
}