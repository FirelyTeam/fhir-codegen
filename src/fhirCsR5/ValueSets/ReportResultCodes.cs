// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The reported execution result.
  /// </summary>
  public static class ReportResultCodesCodes
  {
    /// <summary>
    /// One or more test operations failed one or more asserts.
    /// </summary>
    public static readonly Coding Fail = new Coding
    {
      Code = "fail",
      Display = "Fail",
      System = "http://hl7.org/fhir/report-result-codes"
    };
    /// <summary>
    /// All test operations successfully passed all asserts.
    /// </summary>
    public static readonly Coding Pass = new Coding
    {
      Code = "pass",
      Display = "Pass",
      System = "http://hl7.org/fhir/report-result-codes"
    };
    /// <summary>
    /// One or more test operations is pending execution completion.
    /// </summary>
    public static readonly Coding Pending = new Coding
    {
      Code = "pending",
      Display = "Pending",
      System = "http://hl7.org/fhir/report-result-codes"
    };

    /// <summary>
    /// Literal for code: Fail
    /// </summary>
    public const string LiteralFail = "fail";

    /// <summary>
    /// Literal for code: ReportResultCodesFail
    /// </summary>
    public const string LiteralReportResultCodesFail = "http://hl7.org/fhir/report-result-codes#fail";

    /// <summary>
    /// Literal for code: Pass
    /// </summary>
    public const string LiteralPass = "pass";

    /// <summary>
    /// Literal for code: ReportResultCodesPass
    /// </summary>
    public const string LiteralReportResultCodesPass = "http://hl7.org/fhir/report-result-codes#pass";

    /// <summary>
    /// Literal for code: Pending
    /// </summary>
    public const string LiteralPending = "pending";

    /// <summary>
    /// Literal for code: ReportResultCodesPending
    /// </summary>
    public const string LiteralReportResultCodesPending = "http://hl7.org/fhir/report-result-codes#pending";

    /// <summary>
    /// Dictionary for looking up ReportResultCodes Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "fail", Fail }, 
      { "http://hl7.org/fhir/report-result-codes#fail", Fail }, 
      { "pass", Pass }, 
      { "http://hl7.org/fhir/report-result-codes#pass", Pass }, 
      { "pending", Pending }, 
      { "http://hl7.org/fhir/report-result-codes#pending", Pending }, 
    };
  };
}
