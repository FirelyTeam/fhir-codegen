// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes identifying the category of observation range.
  /// </summary>
  public static class ObservationRangeCategoryCodes
  {
    /// <summary>
    /// Absolute Range for Ordinal and Continuous Observations. Results outside this range are not possible.
    /// </summary>
    public static readonly Coding AbsoluteRange = new Coding
    {
      Code = "absolute",
      Display = "absolute range",
      System = "http://hl7.org/fhir/observation-range-category"
    };
    /// <summary>
    /// Critical Range for Ordinal and Continuous Observations. Results outside this range are critical.
    /// </summary>
    public static readonly Coding CriticalRange = new Coding
    {
      Code = "critical",
      Display = "critical range",
      System = "http://hl7.org/fhir/observation-range-category"
    };
    /// <summary>
    /// Reference (Normal) Range for Ordinal and Continuous Observations.
    /// </summary>
    public static readonly Coding ReferenceRange = new Coding
    {
      Code = "reference",
      Display = "reference range",
      System = "http://hl7.org/fhir/observation-range-category"
    };

    /// <summary>
    /// Literal for code: AbsoluteRange
    /// </summary>
    public const string LiteralAbsoluteRange = "absolute";

    /// <summary>
    /// Literal for code: ObservationRangeCategoryAbsoluteRange
    /// </summary>
    public const string LiteralObservationRangeCategoryAbsoluteRange = "http://hl7.org/fhir/observation-range-category#absolute";

    /// <summary>
    /// Literal for code: CriticalRange
    /// </summary>
    public const string LiteralCriticalRange = "critical";

    /// <summary>
    /// Literal for code: ObservationRangeCategoryCriticalRange
    /// </summary>
    public const string LiteralObservationRangeCategoryCriticalRange = "http://hl7.org/fhir/observation-range-category#critical";

    /// <summary>
    /// Literal for code: ReferenceRange
    /// </summary>
    public const string LiteralReferenceRange = "reference";

    /// <summary>
    /// Literal for code: ObservationRangeCategoryReferenceRange
    /// </summary>
    public const string LiteralObservationRangeCategoryReferenceRange = "http://hl7.org/fhir/observation-range-category#reference";

    /// <summary>
    /// Dictionary for looking up ObservationRangeCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "absolute", AbsoluteRange }, 
      { "http://hl7.org/fhir/observation-range-category#absolute", AbsoluteRange }, 
      { "critical", CriticalRange }, 
      { "http://hl7.org/fhir/observation-range-category#critical", CriticalRange }, 
      { "reference", ReferenceRange }, 
      { "http://hl7.org/fhir/observation-range-category#reference", ReferenceRange }, 
    };
  };
}
