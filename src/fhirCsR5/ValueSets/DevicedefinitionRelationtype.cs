// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Description Needed Here
  /// </summary>
  public static class DevicedefinitionRelationtypeCodes
  {
    /// <summary>
    /// The linked device is a container for the current device.
    /// </summary>
    public static readonly Coding Container = new Coding
    {
      Code = "container",
      Display = "container",
      System = "http://hl7.org/fhir/devicedefinition-relationtype"
    };
    /// <summary>
    /// Gateway.
    /// </summary>
    public static readonly Coding Gateway = new Coding
    {
      Code = "gateway",
      Display = "Gateway",
      System = "http://hl7.org/fhir/devicedefinition-relationtype"
    };
    /// <summary>
    /// The current device is a previous device and has been replaced by the linked device.
    /// </summary>
    public static readonly Coding Previous = new Coding
    {
      Code = "previous",
      Display = "Previous",
      System = "http://hl7.org/fhir/devicedefinition-relationtype"
    };
    /// <summary>
    /// The current device replaces the linked device.
    /// </summary>
    public static readonly Coding Replaces = new Coding
    {
      Code = "replaces",
      Display = "Replaces",
      System = "http://hl7.org/fhir/devicedefinition-relationtype"
    };
    /// <summary>
    /// The current device is supported by the linked device.
    /// </summary>
    public static readonly Coding Supported = new Coding
    {
      Code = "supported",
      Display = "Supported",
      System = "http://hl7.org/fhir/devicedefinition-relationtype"
    };

    /// <summary>
    /// Literal for code: Container
    /// </summary>
    public const string LiteralContainer = "container";

    /// <summary>
    /// Literal for code: DevicedefinitionRelationtypeContainer
    /// </summary>
    public const string LiteralDevicedefinitionRelationtypeContainer = "http://hl7.org/fhir/devicedefinition-relationtype#container";

    /// <summary>
    /// Literal for code: Gateway
    /// </summary>
    public const string LiteralGateway = "gateway";

    /// <summary>
    /// Literal for code: DevicedefinitionRelationtypeGateway
    /// </summary>
    public const string LiteralDevicedefinitionRelationtypeGateway = "http://hl7.org/fhir/devicedefinition-relationtype#gateway";

    /// <summary>
    /// Literal for code: Previous
    /// </summary>
    public const string LiteralPrevious = "previous";

    /// <summary>
    /// Literal for code: DevicedefinitionRelationtypePrevious
    /// </summary>
    public const string LiteralDevicedefinitionRelationtypePrevious = "http://hl7.org/fhir/devicedefinition-relationtype#previous";

    /// <summary>
    /// Literal for code: Replaces
    /// </summary>
    public const string LiteralReplaces = "replaces";

    /// <summary>
    /// Literal for code: DevicedefinitionRelationtypeReplaces
    /// </summary>
    public const string LiteralDevicedefinitionRelationtypeReplaces = "http://hl7.org/fhir/devicedefinition-relationtype#replaces";

    /// <summary>
    /// Literal for code: Supported
    /// </summary>
    public const string LiteralSupported = "supported";

    /// <summary>
    /// Literal for code: DevicedefinitionRelationtypeSupported
    /// </summary>
    public const string LiteralDevicedefinitionRelationtypeSupported = "http://hl7.org/fhir/devicedefinition-relationtype#supported";

    /// <summary>
    /// Dictionary for looking up DevicedefinitionRelationtype Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "container", Container }, 
      { "http://hl7.org/fhir/devicedefinition-relationtype#container", Container }, 
      { "gateway", Gateway }, 
      { "http://hl7.org/fhir/devicedefinition-relationtype#gateway", Gateway }, 
      { "previous", Previous }, 
      { "http://hl7.org/fhir/devicedefinition-relationtype#previous", Previous }, 
      { "replaces", Replaces }, 
      { "http://hl7.org/fhir/devicedefinition-relationtype#replaces", Replaces }, 
      { "supported", Supported }, 
      { "http://hl7.org/fhir/devicedefinition-relationtype#supported", Supported }, 
    };
  };
}