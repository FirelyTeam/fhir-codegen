// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Whether an operation parameter is an input or an output parameter.
  /// </summary>
  public static class OperationParameterUseCodes
  {
    /// <summary>
    /// This is an input parameter.
    /// </summary>
    public static readonly Coding In = new Coding
    {
      Code = "in",
      Display = "In",
      System = "http://hl7.org/fhir/operation-parameter-use"
    };
    /// <summary>
    /// This is an output parameter.
    /// </summary>
    public static readonly Coding Out = new Coding
    {
      Code = "out",
      Display = "Out",
      System = "http://hl7.org/fhir/operation-parameter-use"
    };

    /// <summary>
    /// Literal for code: In
    /// </summary>
    public const string LiteralIn = "in";

    /// <summary>
    /// Literal for code: OperationParameterUseIn
    /// </summary>
    public const string LiteralOperationParameterUseIn = "http://hl7.org/fhir/operation-parameter-use#in";

    /// <summary>
    /// Literal for code: Out
    /// </summary>
    public const string LiteralOut = "out";

    /// <summary>
    /// Literal for code: OperationParameterUseOut
    /// </summary>
    public const string LiteralOperationParameterUseOut = "http://hl7.org/fhir/operation-parameter-use#out";

    /// <summary>
    /// Dictionary for looking up OperationParameterUse Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "in", In }, 
      { "http://hl7.org/fhir/operation-parameter-use#in", In }, 
      { "out", Out }, 
      { "http://hl7.org/fhir/operation-parameter-use#out", Out }, 
    };
  };
}
