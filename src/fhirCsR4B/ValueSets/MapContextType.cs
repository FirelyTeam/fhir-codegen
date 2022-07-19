// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// How to interpret the context.
  /// </summary>
  public static class MapContextTypeCodes
  {
    /// <summary>
    /// The context specifies a type.
    /// </summary>
    public static readonly Coding Type = new Coding
    {
      Code = "type",
      Display = "Type",
      System = "http://hl7.org/fhir/map-context-type"
    };
    /// <summary>
    /// The context specifies a variable.
    /// </summary>
    public static readonly Coding Variable = new Coding
    {
      Code = "variable",
      Display = "Variable",
      System = "http://hl7.org/fhir/map-context-type"
    };

    /// <summary>
    /// Literal for code: Type
    /// </summary>
    public const string LiteralType = "type";

    /// <summary>
    /// Literal for code: MapContextTypeType
    /// </summary>
    public const string LiteralMapContextTypeType = "http://hl7.org/fhir/map-context-type#type";

    /// <summary>
    /// Literal for code: Variable
    /// </summary>
    public const string LiteralVariable = "variable";

    /// <summary>
    /// Literal for code: MapContextTypeVariable
    /// </summary>
    public const string LiteralMapContextTypeVariable = "http://hl7.org/fhir/map-context-type#variable";

    /// <summary>
    /// Dictionary for looking up MapContextType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "type", Type }, 
      { "http://hl7.org/fhir/map-context-type#type", Type }, 
      { "variable", Variable }, 
      { "http://hl7.org/fhir/map-context-type#variable", Variable }, 
    };
  };
}
