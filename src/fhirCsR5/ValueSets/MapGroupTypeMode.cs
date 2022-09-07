// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// If this is the default rule set to apply for the source type, or this combination of types.
  /// </summary>
  public static class MapGroupTypeModeCodes
  {
    /// <summary>
    /// This group is a default mapping group for the specified types.
    /// </summary>
    public static readonly Coding DefaultForTypePlusCombination = new Coding
    {
      Code = "type-and-types",
      Display = "Default for type + combination",
      System = "http://hl7.org/fhir/map-group-type-mode"
    };
    /// <summary>
    /// This group is a default mapping group for the specified types and for the primary source type.
    /// </summary>
    public static readonly Coding DefaultForTypeCombination = new Coding
    {
      Code = "types",
      Display = "Default for Type Combination",
      System = "http://hl7.org/fhir/map-group-type-mode"
    };

    /// <summary>
    /// Literal for code: DefaultForTypePlusCombination
    /// </summary>
    public const string LiteralDefaultForTypePlusCombination = "type-and-types";

    /// <summary>
    /// Literal for code: MapGroupTypeModeDefaultForTypePlusCombination
    /// </summary>
    public const string LiteralMapGroupTypeModeDefaultForTypePlusCombination = "http://hl7.org/fhir/map-group-type-mode#type-and-types";

    /// <summary>
    /// Literal for code: DefaultForTypeCombination
    /// </summary>
    public const string LiteralDefaultForTypeCombination = "types";

    /// <summary>
    /// Literal for code: MapGroupTypeModeDefaultForTypeCombination
    /// </summary>
    public const string LiteralMapGroupTypeModeDefaultForTypeCombination = "http://hl7.org/fhir/map-group-type-mode#types";

    /// <summary>
    /// Dictionary for looking up MapGroupTypeMode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "type-and-types", DefaultForTypePlusCombination }, 
      { "http://hl7.org/fhir/map-group-type-mode#type-and-types", DefaultForTypePlusCombination }, 
      { "types", DefaultForTypeCombination }, 
      { "http://hl7.org/fhir/map-group-type-mode#types", DefaultForTypeCombination }, 
    };
  };
}
