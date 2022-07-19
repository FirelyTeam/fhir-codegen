// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Status of the supply request.
  /// </summary>
  public static class SupplyrequestStatusCodes
  {
    /// <summary>
    /// The request is ready to be acted upon.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// The authorization/request to act has been terminated prior to the full completion of the intended actions.  No further activity should occur.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// Activity against the request has been sufficiently completed to the satisfaction of the requester.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// The request has been created but is not yet complete or ready for action.
    /// </summary>
    public static readonly Coding Draft = new Coding
    {
      Code = "draft",
      Display = "Draft",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// This electronic record should never have existed, though it is possible that real-world decisions were based on it.  (If real-world activity has occurred, the status should be "cancelled" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// The authorization/request to act has been temporarily withdrawn but is expected to resume in the future.
    /// </summary>
    public static readonly Coding Suspended = new Coding
    {
      Code = "suspended",
      Display = "Suspended",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };
    /// <summary>
    /// The authoring/source system does not know which of the status values currently applies for this observation. Note: This concept is not to be used for "other" - one of the listed statuses is presumed to apply, but the authoring/source system does not know which.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/supplyrequest-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: SupplyrequestStatusActive
    /// </summary>
    public const string LiteralSupplyrequestStatusActive = "http://hl7.org/fhir/supplyrequest-status#active";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: SupplyrequestStatusCancelled
    /// </summary>
    public const string LiteralSupplyrequestStatusCancelled = "http://hl7.org/fhir/supplyrequest-status#cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: SupplyrequestStatusCompleted
    /// </summary>
    public const string LiteralSupplyrequestStatusCompleted = "http://hl7.org/fhir/supplyrequest-status#completed";

    /// <summary>
    /// Literal for code: Draft
    /// </summary>
    public const string LiteralDraft = "draft";

    /// <summary>
    /// Literal for code: SupplyrequestStatusDraft
    /// </summary>
    public const string LiteralSupplyrequestStatusDraft = "http://hl7.org/fhir/supplyrequest-status#draft";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: SupplyrequestStatusEnteredInError
    /// </summary>
    public const string LiteralSupplyrequestStatusEnteredInError = "http://hl7.org/fhir/supplyrequest-status#entered-in-error";

    /// <summary>
    /// Literal for code: Suspended
    /// </summary>
    public const string LiteralSuspended = "suspended";

    /// <summary>
    /// Literal for code: SupplyrequestStatusSuspended
    /// </summary>
    public const string LiteralSupplyrequestStatusSuspended = "http://hl7.org/fhir/supplyrequest-status#suspended";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: SupplyrequestStatusUnknown
    /// </summary>
    public const string LiteralSupplyrequestStatusUnknown = "http://hl7.org/fhir/supplyrequest-status#unknown";

    /// <summary>
    /// Dictionary for looking up SupplyrequestStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/supplyrequest-status#active", Active }, 
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/supplyrequest-status#cancelled", Cancelled }, 
      { "completed", Completed }, 
      { "http://hl7.org/fhir/supplyrequest-status#completed", Completed }, 
      { "draft", Draft }, 
      { "http://hl7.org/fhir/supplyrequest-status#draft", Draft }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/supplyrequest-status#entered-in-error", EnteredInError }, 
      { "suspended", Suspended }, 
      { "http://hl7.org/fhir/supplyrequest-status#suspended", Suspended }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/supplyrequest-status#unknown", Unknown }, 
    };
  };
}
