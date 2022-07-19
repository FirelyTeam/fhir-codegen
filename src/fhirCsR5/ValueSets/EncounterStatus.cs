// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Current state of the encounter.
  /// </summary>
  public static class EncounterStatusCodes
  {
    /// <summary>
    /// The Encounter has ended before it has begun.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// The Encounter has ended.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// This instance should not have been part of this patient's medical record.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// The Encounter has begun and the patient is present / the practitioner and the patient are meeting.
    /// </summary>
    public static readonly Coding InProgress = new Coding
    {
      Code = "in-progress",
      Display = "In Progress",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// The Encounter has begun, but is currently on hold, e.g. because the patient is temporarily on leave.
    /// </summary>
    public static readonly Coding OnHold = new Coding
    {
      Code = "onhold",
      Display = "On Hold",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// The Encounter has not yet started.
    /// </summary>
    public static readonly Coding Planned = new Coding
    {
      Code = "planned",
      Display = "Planned",
      System = "http://hl7.org/fhir/encounter-status"
    };
    /// <summary>
    /// The encounter status is unknown. Note that "unknown" is a value of last resort and every attempt should be made to provide a meaningful value other than "unknown".
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/encounter-status"
    };

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: EncounterStatusCancelled
    /// </summary>
    public const string LiteralEncounterStatusCancelled = "http://hl7.org/fhir/encounter-status#cancelled";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: EncounterStatusCompleted
    /// </summary>
    public const string LiteralEncounterStatusCompleted = "http://hl7.org/fhir/encounter-status#completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: EncounterStatusEnteredInError
    /// </summary>
    public const string LiteralEncounterStatusEnteredInError = "http://hl7.org/fhir/encounter-status#entered-in-error";

    /// <summary>
    /// Literal for code: InProgress
    /// </summary>
    public const string LiteralInProgress = "in-progress";

    /// <summary>
    /// Literal for code: EncounterStatusInProgress
    /// </summary>
    public const string LiteralEncounterStatusInProgress = "http://hl7.org/fhir/encounter-status#in-progress";

    /// <summary>
    /// Literal for code: OnHold
    /// </summary>
    public const string LiteralOnHold = "onhold";

    /// <summary>
    /// Literal for code: EncounterStatusOnHold
    /// </summary>
    public const string LiteralEncounterStatusOnHold = "http://hl7.org/fhir/encounter-status#onhold";

    /// <summary>
    /// Literal for code: Planned
    /// </summary>
    public const string LiteralPlanned = "planned";

    /// <summary>
    /// Literal for code: EncounterStatusPlanned
    /// </summary>
    public const string LiteralEncounterStatusPlanned = "http://hl7.org/fhir/encounter-status#planned";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: EncounterStatusUnknown
    /// </summary>
    public const string LiteralEncounterStatusUnknown = "http://hl7.org/fhir/encounter-status#unknown";

    /// <summary>
    /// Dictionary for looking up EncounterStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/encounter-status#cancelled", Cancelled }, 
      { "completed", Completed }, 
      { "http://hl7.org/fhir/encounter-status#completed", Completed }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/encounter-status#entered-in-error", EnteredInError }, 
      { "in-progress", InProgress }, 
      { "http://hl7.org/fhir/encounter-status#in-progress", InProgress }, 
      { "onhold", OnHold }, 
      { "http://hl7.org/fhir/encounter-status#onhold", OnHold }, 
      { "planned", Planned }, 
      { "http://hl7.org/fhir/encounter-status#planned", Planned }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/encounter-status#unknown", Unknown }, 
    };
  };
}
