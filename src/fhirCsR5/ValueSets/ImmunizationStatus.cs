// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the administered dose of vaccine.
  /// </summary>
  public static class ImmunizationStatusCodes
  {
    /// <summary>
    /// The event has now concluded.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/event-status"
    };
    /// <summary>
    /// This electronic record should never have existed, though it is possible that real-world decisions were based on it.  (If real-world activity has occurred, the status should be "stopped" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/event-status"
    };
    /// <summary>
    /// The event was terminated prior to any activity beyond preparation.  I.e. The 'main' activity has not yet begun.  The boundary between preparatory and the 'main' activity is context-specific.
    /// </summary>
    public static readonly Coding NotDone = new Coding
    {
      Code = "not-done",
      Display = "Not Done",
      System = "http://hl7.org/fhir/event-status"
    };

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: EventStatusCompleted
    /// </summary>
    public const string LiteralEventStatusCompleted = "http://hl7.org/fhir/event-status#completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: EventStatusEnteredInError
    /// </summary>
    public const string LiteralEventStatusEnteredInError = "http://hl7.org/fhir/event-status#entered-in-error";

    /// <summary>
    /// Literal for code: NotDone
    /// </summary>
    public const string LiteralNotDone = "not-done";

    /// <summary>
    /// Literal for code: EventStatusNotDone
    /// </summary>
    public const string LiteralEventStatusNotDone = "http://hl7.org/fhir/event-status#not-done";

    /// <summary>
    /// Dictionary for looking up ImmunizationStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "completed", Completed }, 
      { "http://hl7.org/fhir/event-status#completed", Completed }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/event-status#entered-in-error", EnteredInError }, 
      { "not-done", NotDone }, 
      { "http://hl7.org/fhir/event-status#not-done", NotDone }, 
    };
  };
}
