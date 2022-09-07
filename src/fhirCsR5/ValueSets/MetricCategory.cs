// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Describes the category of the metric.
  /// </summary>
  public static class MetricCategoryCodes
  {
    /// <summary>
    /// DeviceObservations generated for this DeviceMetric are calculated.
    /// </summary>
    public static readonly Coding Calculation = new Coding
    {
      Code = "calculation",
      Display = "Calculation",
      System = "http://hl7.org/fhir/metric-category"
    };
    /// <summary>
    /// DeviceObservations generated for this DeviceMetric are measured.
    /// </summary>
    public static readonly Coding Measurement = new Coding
    {
      Code = "measurement",
      Display = "Measurement",
      System = "http://hl7.org/fhir/metric-category"
    };
    /// <summary>
    /// DeviceObservations generated for this DeviceMetric is a setting that will influence the behavior of the Device.
    /// </summary>
    public static readonly Coding Setting = new Coding
    {
      Code = "setting",
      Display = "Setting",
      System = "http://hl7.org/fhir/metric-category"
    };
    /// <summary>
    /// The category of this DeviceMetric is unspecified.
    /// </summary>
    public static readonly Coding Unspecified = new Coding
    {
      Code = "unspecified",
      Display = "Unspecified",
      System = "http://hl7.org/fhir/metric-category"
    };

    /// <summary>
    /// Literal for code: Calculation
    /// </summary>
    public const string LiteralCalculation = "calculation";

    /// <summary>
    /// Literal for code: MetricCategoryCalculation
    /// </summary>
    public const string LiteralMetricCategoryCalculation = "http://hl7.org/fhir/metric-category#calculation";

    /// <summary>
    /// Literal for code: Measurement
    /// </summary>
    public const string LiteralMeasurement = "measurement";

    /// <summary>
    /// Literal for code: MetricCategoryMeasurement
    /// </summary>
    public const string LiteralMetricCategoryMeasurement = "http://hl7.org/fhir/metric-category#measurement";

    /// <summary>
    /// Literal for code: Setting
    /// </summary>
    public const string LiteralSetting = "setting";

    /// <summary>
    /// Literal for code: MetricCategorySetting
    /// </summary>
    public const string LiteralMetricCategorySetting = "http://hl7.org/fhir/metric-category#setting";

    /// <summary>
    /// Literal for code: Unspecified
    /// </summary>
    public const string LiteralUnspecified = "unspecified";

    /// <summary>
    /// Literal for code: MetricCategoryUnspecified
    /// </summary>
    public const string LiteralMetricCategoryUnspecified = "http://hl7.org/fhir/metric-category#unspecified";

    /// <summary>
    /// Dictionary for looking up MetricCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "calculation", Calculation }, 
      { "http://hl7.org/fhir/metric-category#calculation", Calculation }, 
      { "measurement", Measurement }, 
      { "http://hl7.org/fhir/metric-category#measurement", Measurement }, 
      { "setting", Setting }, 
      { "http://hl7.org/fhir/metric-category#setting", Setting }, 
      { "unspecified", Unspecified }, 
      { "http://hl7.org/fhir/metric-category#unspecified", Unspecified }, 
    };
  };
}
