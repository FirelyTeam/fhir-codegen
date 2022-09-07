// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  ///  The type of workflow from which this payment arose.
  /// </summary>
  public static class PaymentKindCodes
  {
    /// <summary>
    /// The payment or adjustment is to an indicated account not to a specific charge.
    /// </summary>
    public static readonly Coding DepositOnAccount = new Coding
    {
      Code = "deposit",
      Display = "Deposit on Account",
      System = "http://hl7.org/fhir/payment-kind"
    };
    /// <summary>
    /// Payment made at an authorized Kiosk.
    /// </summary>
    public static readonly Coding KioskPayment = new Coding
    {
      Code = "kiosk",
      Display = "Kiosk Payment",
      System = "http://hl7.org/fhir/payment-kind"
    };
    /// <summary>
    /// Payment, full or partial, of an invoice or statement provided to the payment issuer.
    /// </summary>
    public static readonly Coding OnlineBillPayment = new Coding
    {
      Code = "online",
      Display = "Online Bill Payment",
      System = "http://hl7.org/fhir/payment-kind"
    };
    /// <summary>
    /// The payment is one of a set of previously agreed payments, for example in fullfilment of a payment plan.
    /// </summary>
    public static readonly Coding PeriodicPayment = new Coding
    {
      Code = "periodic-payment",
      Display = "Periodic Payment",
      System = "http://hl7.org/fhir/payment-kind"
    };

    /// <summary>
    /// Literal for code: DepositOnAccount
    /// </summary>
    public const string LiteralDepositOnAccount = "deposit";

    /// <summary>
    /// Literal for code: PaymentKindDepositOnAccount
    /// </summary>
    public const string LiteralPaymentKindDepositOnAccount = "http://hl7.org/fhir/payment-kind#deposit";

    /// <summary>
    /// Literal for code: KioskPayment
    /// </summary>
    public const string LiteralKioskPayment = "kiosk";

    /// <summary>
    /// Literal for code: PaymentKindKioskPayment
    /// </summary>
    public const string LiteralPaymentKindKioskPayment = "http://hl7.org/fhir/payment-kind#kiosk";

    /// <summary>
    /// Literal for code: OnlineBillPayment
    /// </summary>
    public const string LiteralOnlineBillPayment = "online";

    /// <summary>
    /// Literal for code: PaymentKindOnlineBillPayment
    /// </summary>
    public const string LiteralPaymentKindOnlineBillPayment = "http://hl7.org/fhir/payment-kind#online";

    /// <summary>
    /// Literal for code: PeriodicPayment
    /// </summary>
    public const string LiteralPeriodicPayment = "periodic-payment";

    /// <summary>
    /// Literal for code: PaymentKindPeriodicPayment
    /// </summary>
    public const string LiteralPaymentKindPeriodicPayment = "http://hl7.org/fhir/payment-kind#periodic-payment";

    /// <summary>
    /// Dictionary for looking up PaymentKind Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "deposit", DepositOnAccount }, 
      { "http://hl7.org/fhir/payment-kind#deposit", DepositOnAccount }, 
      { "kiosk", KioskPayment }, 
      { "http://hl7.org/fhir/payment-kind#kiosk", KioskPayment }, 
      { "online", OnlineBillPayment }, 
      { "http://hl7.org/fhir/payment-kind#online", OnlineBillPayment }, 
      { "periodic-payment", PeriodicPayment }, 
      { "http://hl7.org/fhir/payment-kind#periodic-payment", PeriodicPayment }, 
    };
  };
}
