// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Indicates the state of the consent.
  /// </summary>
  public static class ConsentStateCodesCodes
  {
    /// <summary>
    /// The consent is to be followed and enforced.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/consent-state-codes"
    };
    /// <summary>
    /// The consent is in development or awaiting use but is not yet intended to be acted upon.
    /// </summary>
    public static readonly Coding Pending = new Coding
    {
      Code = "draft",
      Display = "Pending",
      System = "http://hl7.org/fhir/consent-state-codes"
    };
    /// <summary>
    /// The consent was created wrongly (e.g. wrong patient) and should be ignored.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/consent-state-codes"
    };
    /// <summary>
    /// The consent is terminated or replaced.
    /// </summary>
    public static readonly Coding Inactive = new Coding
    {
      Code = "inactive",
      Display = "Inactive",
      System = "http://hl7.org/fhir/consent-state-codes"
    };
    /// <summary>
    /// The consent development has been terminated prior to completion.
    /// </summary>
    public static readonly Coding Abandoned = new Coding
    {
      Code = "not-done",
      Display = "Abandoned",
      System = "http://hl7.org/fhir/consent-state-codes"
    };
    /// <summary>
    /// The resource is in an indeterminate state.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/consent-state-codes"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: ConsentStateCodesActive
    /// </summary>
    public const string LiteralConsentStateCodesActive = "http://hl7.org/fhir/consent-state-codes#active";

    /// <summary>
    /// Literal for code: Pending
    /// </summary>
    public const string LiteralPending = "draft";

    /// <summary>
    /// Literal for code: ConsentStateCodesPending
    /// </summary>
    public const string LiteralConsentStateCodesPending = "http://hl7.org/fhir/consent-state-codes#draft";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: ConsentStateCodesEnteredInError
    /// </summary>
    public const string LiteralConsentStateCodesEnteredInError = "http://hl7.org/fhir/consent-state-codes#entered-in-error";

    /// <summary>
    /// Literal for code: Inactive
    /// </summary>
    public const string LiteralInactive = "inactive";

    /// <summary>
    /// Literal for code: ConsentStateCodesInactive
    /// </summary>
    public const string LiteralConsentStateCodesInactive = "http://hl7.org/fhir/consent-state-codes#inactive";

    /// <summary>
    /// Literal for code: Abandoned
    /// </summary>
    public const string LiteralAbandoned = "not-done";

    /// <summary>
    /// Literal for code: ConsentStateCodesAbandoned
    /// </summary>
    public const string LiteralConsentStateCodesAbandoned = "http://hl7.org/fhir/consent-state-codes#not-done";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: ConsentStateCodesUnknown
    /// </summary>
    public const string LiteralConsentStateCodesUnknown = "http://hl7.org/fhir/consent-state-codes#unknown";

    /// <summary>
    /// Dictionary for looking up ConsentStateCodes Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/consent-state-codes#active", Active }, 
      { "draft", Pending }, 
      { "http://hl7.org/fhir/consent-state-codes#draft", Pending }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/consent-state-codes#entered-in-error", EnteredInError }, 
      { "inactive", Inactive }, 
      { "http://hl7.org/fhir/consent-state-codes#inactive", Inactive }, 
      { "not-done", Abandoned }, 
      { "http://hl7.org/fhir/consent-state-codes#not-done", Abandoned }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/consent-state-codes#unknown", Unknown }, 
    };
  };
}
