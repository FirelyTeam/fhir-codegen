// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Indicates that a parameter applies when the operation is being invoked at the specified level
  /// </summary>
  public static class OperationParameterScopeCodes
  {
    /// <summary>
    /// This is a parameter that can be used at the instance level.
    /// </summary>
    public static readonly Coding Instance = new Coding
    {
      Code = "instance",
      Display = "Instance",
      System = "http://hl7.org/fhir/operation-parameter-scope"
    };
    /// <summary>
    /// This is a parameter that can be used at the system level.
    /// </summary>
    public static readonly Coding System = new Coding
    {
      Code = "system",
      Display = "System",
      System = "http://hl7.org/fhir/operation-parameter-scope"
    };
    /// <summary>
    /// This is a parameter that can be used at the type level.
    /// </summary>
    public static readonly Coding Type = new Coding
    {
      Code = "type",
      Display = "Type",
      System = "http://hl7.org/fhir/operation-parameter-scope"
    };

    /// <summary>
    /// Literal for code: Instance
    /// </summary>
    public const string LiteralInstance = "instance";

    /// <summary>
    /// Literal for code: OperationParameterScopeInstance
    /// </summary>
    public const string LiteralOperationParameterScopeInstance = "http://hl7.org/fhir/operation-parameter-scope#instance";

    /// <summary>
    /// Literal for code: System
    /// </summary>
    public const string LiteralSystem = "system";

    /// <summary>
    /// Literal for code: OperationParameterScopeSystem
    /// </summary>
    public const string LiteralOperationParameterScopeSystem = "http://hl7.org/fhir/operation-parameter-scope#system";

    /// <summary>
    /// Literal for code: Type
    /// </summary>
    public const string LiteralType = "type";

    /// <summary>
    /// Literal for code: OperationParameterScopeType
    /// </summary>
    public const string LiteralOperationParameterScopeType = "http://hl7.org/fhir/operation-parameter-scope#type";

    /// <summary>
    /// Dictionary for looking up OperationParameterScope Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "instance", Instance }, 
      { "http://hl7.org/fhir/operation-parameter-scope#instance", Instance }, 
      { "system", System }, 
      { "http://hl7.org/fhir/operation-parameter-scope#system", System }, 
      { "type", Type }, 
      { "http://hl7.org/fhir/operation-parameter-scope#type", Type }, 
    };
  };
}
