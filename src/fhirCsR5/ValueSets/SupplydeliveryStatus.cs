// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Status of the supply delivery.
  /// </summary>
  public static class SupplydeliveryStatusCodes
  {
    /// <summary>
    /// Delivery was not completed.
    /// </summary>
    public static readonly Coding Abandoned = new Coding
    {
      Code = "abandoned",
      Display = "Abandoned",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// Supply has been delivered ("completed").
    /// </summary>
    public static readonly Coding Delivered = new Coding
    {
      Code = "completed",
      Display = "Delivered",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// This electronic record should never have existed, though it is possible that real-world decisions were based on it. (If real-world activity has occurred, the status should be "abandoned" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered In Error",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// Supply has been requested, but not delivered.
    /// </summary>
    public static readonly Coding InProgress = new Coding
    {
      Code = "in-progress",
      Display = "In Progress",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };

    /// <summary>
    /// Literal for code: Abandoned
    /// </summary>
    public const string LiteralAbandoned = "abandoned";

    /// <summary>
    /// Literal for code: SupplydeliveryStatusAbandoned
    /// </summary>
    public const string LiteralSupplydeliveryStatusAbandoned = "http://hl7.org/fhir/supplydelivery-status#abandoned";

    /// <summary>
    /// Literal for code: Delivered
    /// </summary>
    public const string LiteralDelivered = "completed";

    /// <summary>
    /// Literal for code: SupplydeliveryStatusDelivered
    /// </summary>
    public const string LiteralSupplydeliveryStatusDelivered = "http://hl7.org/fhir/supplydelivery-status#completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: SupplydeliveryStatusEnteredInError
    /// </summary>
    public const string LiteralSupplydeliveryStatusEnteredInError = "http://hl7.org/fhir/supplydelivery-status#entered-in-error";

    /// <summary>
    /// Literal for code: InProgress
    /// </summary>
    public const string LiteralInProgress = "in-progress";

    /// <summary>
    /// Literal for code: SupplydeliveryStatusInProgress
    /// </summary>
    public const string LiteralSupplydeliveryStatusInProgress = "http://hl7.org/fhir/supplydelivery-status#in-progress";

    /// <summary>
    /// Dictionary for looking up SupplydeliveryStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "abandoned", Abandoned }, 
      { "http://hl7.org/fhir/supplydelivery-status#abandoned", Abandoned }, 
      { "completed", Delivered }, 
      { "http://hl7.org/fhir/supplydelivery-status#completed", Delivered }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/supplydelivery-status#entered-in-error", EnteredInError }, 
      { "in-progress", InProgress }, 
      { "http://hl7.org/fhir/supplydelivery-status#in-progress", InProgress }, 
    };
  };
}
