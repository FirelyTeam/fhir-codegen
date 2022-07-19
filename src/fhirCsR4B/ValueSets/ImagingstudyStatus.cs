// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The status of the ImagingStudy.
  /// </summary>
  public static class ImagingstudyStatusCodes
  {
    /// <summary>
    /// At least one instance has been associated with this imaging study.
    /// </summary>
    public static readonly Coding Available = new Coding
    {
      Code = "available",
      Display = "Available",
      System = "http://hl7.org/fhir/imagingstudy-status"
    };
    /// <summary>
    /// The imaging study is unavailable because the imaging study was not started or not completed (also sometimes called "aborted").
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/imagingstudy-status"
    };
    /// <summary>
    /// The imaging study has been withdrawn following a previous final release.  This electronic record should never have existed, though it is possible that real-world decisions were based on it. (If real-world activity has occurred, the status should be "cancelled" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/imagingstudy-status"
    };
    /// <summary>
    /// The existence of the imaging study is registered, but there is nothing yet available.
    /// </summary>
    public static readonly Coding Registered = new Coding
    {
      Code = "registered",
      Display = "Registered",
      System = "http://hl7.org/fhir/imagingstudy-status"
    };
    /// <summary>
    /// The system does not know which of the status values currently applies for this request. Note: This concept is not to be used for "other" - one of the listed statuses is presumed to apply, it's just not known which one.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/imagingstudy-status"
    };

    /// <summary>
    /// Literal for code: Available
    /// </summary>
    public const string LiteralAvailable = "available";

    /// <summary>
    /// Literal for code: ImagingstudyStatusAvailable
    /// </summary>
    public const string LiteralImagingstudyStatusAvailable = "http://hl7.org/fhir/imagingstudy-status#available";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: ImagingstudyStatusCancelled
    /// </summary>
    public const string LiteralImagingstudyStatusCancelled = "http://hl7.org/fhir/imagingstudy-status#cancelled";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: ImagingstudyStatusEnteredInError
    /// </summary>
    public const string LiteralImagingstudyStatusEnteredInError = "http://hl7.org/fhir/imagingstudy-status#entered-in-error";

    /// <summary>
    /// Literal for code: Registered
    /// </summary>
    public const string LiteralRegistered = "registered";

    /// <summary>
    /// Literal for code: ImagingstudyStatusRegistered
    /// </summary>
    public const string LiteralImagingstudyStatusRegistered = "http://hl7.org/fhir/imagingstudy-status#registered";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: ImagingstudyStatusUnknown
    /// </summary>
    public const string LiteralImagingstudyStatusUnknown = "http://hl7.org/fhir/imagingstudy-status#unknown";

    /// <summary>
    /// Dictionary for looking up ImagingstudyStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "available", Available }, 
      { "http://hl7.org/fhir/imagingstudy-status#available", Available }, 
      { "cancelled", Cancelled }, 
      { "http://hl7.org/fhir/imagingstudy-status#cancelled", Cancelled }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/imagingstudy-status#entered-in-error", EnteredInError }, 
      { "registered", Registered }, 
      { "http://hl7.org/fhir/imagingstudy-status#registered", Registered }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/imagingstudy-status#unknown", Unknown }, 
    };
  };
}
