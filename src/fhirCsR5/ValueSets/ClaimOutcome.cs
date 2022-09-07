// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes Claim Processing Outcome codes.
  /// </summary>
  public static class ClaimOutcomeCodes
  {
    /// <summary>
    /// The processing has completed without errors
    /// </summary>
    public static readonly Coding ProcessingComplete = new Coding
    {
      Code = "complete",
      Display = "Processing Complete",
      System = "http://hl7.org/fhir/claim-outcome"
    };
    /// <summary>
    /// One or more errors have been detected in the Claim
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/claim-outcome"
    };
    /// <summary>
    /// No errors have been detected in the Claim and some of the adjudication has been performed.
    /// </summary>
    public static readonly Coding PartialProcessing = new Coding
    {
      Code = "partial",
      Display = "Partial Processing",
      System = "http://hl7.org/fhir/claim-outcome"
    };
    /// <summary>
    /// The Claim/Pre-authorization/Pre-determination has been received but processing has not begun.
    /// </summary>
    public static readonly Coding Queued = new Coding
    {
      Code = "queued",
      Display = "Queued",
      System = "http://hl7.org/fhir/claim-outcome"
    };

    /// <summary>
    /// Literal for code: ProcessingComplete
    /// </summary>
    public const string LiteralProcessingComplete = "complete";

    /// <summary>
    /// Literal for code: ClaimOutcomeProcessingComplete
    /// </summary>
    public const string LiteralClaimOutcomeProcessingComplete = "http://hl7.org/fhir/claim-outcome#complete";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: ClaimOutcomeError
    /// </summary>
    public const string LiteralClaimOutcomeError = "http://hl7.org/fhir/claim-outcome#error";

    /// <summary>
    /// Literal for code: PartialProcessing
    /// </summary>
    public const string LiteralPartialProcessing = "partial";

    /// <summary>
    /// Literal for code: ClaimOutcomePartialProcessing
    /// </summary>
    public const string LiteralClaimOutcomePartialProcessing = "http://hl7.org/fhir/claim-outcome#partial";

    /// <summary>
    /// Literal for code: Queued
    /// </summary>
    public const string LiteralQueued = "queued";

    /// <summary>
    /// Literal for code: ClaimOutcomeQueued
    /// </summary>
    public const string LiteralClaimOutcomeQueued = "http://hl7.org/fhir/claim-outcome#queued";

    /// <summary>
    /// Dictionary for looking up ClaimOutcome Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "complete", ProcessingComplete }, 
      { "http://hl7.org/fhir/claim-outcome#complete", ProcessingComplete }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/claim-outcome#error", Error }, 
      { "partial", PartialProcessing }, 
      { "http://hl7.org/fhir/claim-outcome#partial", PartialProcessing }, 
      { "queued", Queued }, 
      { "http://hl7.org/fhir/claim-outcome#queued", Queued }, 
    };
  };
}
