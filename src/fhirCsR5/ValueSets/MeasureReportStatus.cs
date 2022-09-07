// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The status of the measure report.
  /// </summary>
  public static class MeasureReportStatusCodes
  {
    /// <summary>
    /// The report is complete and ready for use.
    /// </summary>
    public static readonly Coding Complete = new Coding
    {
      Code = "complete",
      Display = "Complete",
      System = "http://hl7.org/fhir/measure-report-status"
    };
    /// <summary>
    /// An error occurred attempting to generate the report.
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/measure-report-status"
    };
    /// <summary>
    /// The report is currently being generated.
    /// </summary>
    public static readonly Coding Pending = new Coding
    {
      Code = "pending",
      Display = "Pending",
      System = "http://hl7.org/fhir/measure-report-status"
    };

    /// <summary>
    /// Literal for code: Complete
    /// </summary>
    public const string LiteralComplete = "complete";

    /// <summary>
    /// Literal for code: MeasureReportStatusComplete
    /// </summary>
    public const string LiteralMeasureReportStatusComplete = "http://hl7.org/fhir/measure-report-status#complete";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: MeasureReportStatusError
    /// </summary>
    public const string LiteralMeasureReportStatusError = "http://hl7.org/fhir/measure-report-status#error";

    /// <summary>
    /// Literal for code: Pending
    /// </summary>
    public const string LiteralPending = "pending";

    /// <summary>
    /// Literal for code: MeasureReportStatusPending
    /// </summary>
    public const string LiteralMeasureReportStatusPending = "http://hl7.org/fhir/measure-report-status#pending";

    /// <summary>
    /// Dictionary for looking up MeasureReportStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "complete", Complete }, 
      { "http://hl7.org/fhir/measure-report-status#complete", Complete }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/measure-report-status#error", Error }, 
      { "pending", Pending }, 
      { "http://hl7.org/fhir/measure-report-status#pending", Pending }, 
    };
  };
}
