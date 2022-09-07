// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The outcome of the processing.
  /// </summary>
  public static class RemittanceOutcomeCodes
  {
    /// <summary>
    /// The processing completed without errors.
    /// </summary>
    public static readonly Coding Complete = new Coding
    {
      Code = "complete",
      Display = "Complete",
      System = "http://hl7.org/fhir/remittance-outcome"
    };
    /// <summary>
    /// The processing identified errors.
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/remittance-outcome"
    };
    /// <summary>
    /// No errors have been detected and some of the adjudication has been performed.
    /// </summary>
    public static readonly Coding Partial = new Coding
    {
      Code = "partial",
      Display = "Partial",
      System = "http://hl7.org/fhir/remittance-outcome"
    };

    /// <summary>
    /// Literal for code: Complete
    /// </summary>
    public const string LiteralComplete = "complete";

    /// <summary>
    /// Literal for code: RemittanceOutcomeComplete
    /// </summary>
    public const string LiteralRemittanceOutcomeComplete = "http://hl7.org/fhir/remittance-outcome#complete";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: RemittanceOutcomeError
    /// </summary>
    public const string LiteralRemittanceOutcomeError = "http://hl7.org/fhir/remittance-outcome#error";

    /// <summary>
    /// Literal for code: Partial
    /// </summary>
    public const string LiteralPartial = "partial";

    /// <summary>
    /// Literal for code: RemittanceOutcomePartial
    /// </summary>
    public const string LiteralRemittanceOutcomePartial = "http://hl7.org/fhir/remittance-outcome#partial";

    /// <summary>
    /// Dictionary for looking up RemittanceOutcome Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "complete", Complete }, 
      { "http://hl7.org/fhir/remittance-outcome#complete", Complete }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/remittance-outcome#error", Error }, 
      { "partial", Partial }, 
      { "http://hl7.org/fhir/remittance-outcome#partial", Partial }, 
    };
  };
}
