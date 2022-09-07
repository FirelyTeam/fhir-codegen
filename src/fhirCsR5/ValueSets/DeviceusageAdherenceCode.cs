// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// A coded concept indicating the adherence of device usage.
  /// </summary>
  public static class DeviceusageAdherenceCodeCodes
  {
    /// <summary>
    /// The device is always used.
    /// </summary>
    public static readonly Coding Always = new Coding
    {
      Code = "always",
      Display = "Always",
      System = "http://hl7.org/fhir/deviceusage-adherence-code"
    };
    /// <summary>
    /// The device is never used.
    /// </summary>
    public static readonly Coding Never = new Coding
    {
      Code = "never",
      Display = "Never",
      System = "http://hl7.org/fhir/deviceusage-adherence-code"
    };
    /// <summary>
    /// The device is sometimes used.
    /// </summary>
    public static readonly Coding Sometimes = new Coding
    {
      Code = "sometimes",
      Display = "Sometimes",
      System = "http://hl7.org/fhir/deviceusage-adherence-code"
    };

    /// <summary>
    /// Literal for code: Always
    /// </summary>
    public const string LiteralAlways = "always";

    /// <summary>
    /// Literal for code: DeviceusageAdherenceCodeAlways
    /// </summary>
    public const string LiteralDeviceusageAdherenceCodeAlways = "http://hl7.org/fhir/deviceusage-adherence-code#always";

    /// <summary>
    /// Literal for code: Never
    /// </summary>
    public const string LiteralNever = "never";

    /// <summary>
    /// Literal for code: DeviceusageAdherenceCodeNever
    /// </summary>
    public const string LiteralDeviceusageAdherenceCodeNever = "http://hl7.org/fhir/deviceusage-adherence-code#never";

    /// <summary>
    /// Literal for code: Sometimes
    /// </summary>
    public const string LiteralSometimes = "sometimes";

    /// <summary>
    /// Literal for code: DeviceusageAdherenceCodeSometimes
    /// </summary>
    public const string LiteralDeviceusageAdherenceCodeSometimes = "http://hl7.org/fhir/deviceusage-adherence-code#sometimes";

    /// <summary>
    /// Dictionary for looking up DeviceusageAdherenceCode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "always", Always }, 
      { "http://hl7.org/fhir/deviceusage-adherence-code#always", Always }, 
      { "never", Never }, 
      { "http://hl7.org/fhir/deviceusage-adherence-code#never", Never }, 
      { "sometimes", Sometimes }, 
      { "http://hl7.org/fhir/deviceusage-adherence-code#sometimes", Sometimes }, 
    };
  };
}
