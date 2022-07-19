// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The status of the endpoint
  /// </summary>
  public static class EndpointStatusCodes
  {
    /// <summary>
    /// This endpoint is expected to be active and can be used
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/endpoint-status"
    };
    /// <summary>
    /// This instance should not have been part of this patient's medical record.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in error",
      System = "http://hl7.org/fhir/endpoint-status"
    };
    /// <summary>
    /// This endpoint has exceeded connectivity thresholds and is considered in an error state and should no longer be attempted to connect to until corrective action is taken
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/endpoint-status"
    };
    /// <summary>
    /// This endpoint is no longer to be used
    /// </summary>
    public static readonly Coding Off = new Coding
    {
      Code = "off",
      Display = "Off",
      System = "http://hl7.org/fhir/endpoint-status"
    };
    /// <summary>
    /// This endpoint is temporarily unavailable
    /// </summary>
    public static readonly Coding Suspended = new Coding
    {
      Code = "suspended",
      Display = "Suspended",
      System = "http://hl7.org/fhir/endpoint-status"
    };
    /// <summary>
    /// This endpoint is not intended for production usage.
    /// </summary>
    public static readonly Coding Test = new Coding
    {
      Code = "test",
      Display = "Test",
      System = "http://hl7.org/fhir/endpoint-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: EndpointStatusActive
    /// </summary>
    public const string LiteralEndpointStatusActive = "http://hl7.org/fhir/endpoint-status#active";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: EndpointStatusEnteredInError
    /// </summary>
    public const string LiteralEndpointStatusEnteredInError = "http://hl7.org/fhir/endpoint-status#entered-in-error";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: EndpointStatusError
    /// </summary>
    public const string LiteralEndpointStatusError = "http://hl7.org/fhir/endpoint-status#error";

    /// <summary>
    /// Literal for code: Off
    /// </summary>
    public const string LiteralOff = "off";

    /// <summary>
    /// Literal for code: EndpointStatusOff
    /// </summary>
    public const string LiteralEndpointStatusOff = "http://hl7.org/fhir/endpoint-status#off";

    /// <summary>
    /// Literal for code: Suspended
    /// </summary>
    public const string LiteralSuspended = "suspended";

    /// <summary>
    /// Literal for code: EndpointStatusSuspended
    /// </summary>
    public const string LiteralEndpointStatusSuspended = "http://hl7.org/fhir/endpoint-status#suspended";

    /// <summary>
    /// Literal for code: Test
    /// </summary>
    public const string LiteralTest = "test";

    /// <summary>
    /// Literal for code: EndpointStatusTest
    /// </summary>
    public const string LiteralEndpointStatusTest = "http://hl7.org/fhir/endpoint-status#test";

    /// <summary>
    /// Dictionary for looking up EndpointStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/endpoint-status#active", Active }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/endpoint-status#entered-in-error", EnteredInError }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/endpoint-status#error", Error }, 
      { "off", Off }, 
      { "http://hl7.org/fhir/endpoint-status#off", Off }, 
      { "suspended", Suspended }, 
      { "http://hl7.org/fhir/endpoint-status#suspended", Suspended }, 
      { "test", Test }, 
      { "http://hl7.org/fhir/endpoint-status#test", Test }, 
    };
  };
}
