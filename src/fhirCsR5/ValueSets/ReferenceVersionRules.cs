// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Whether a reference needs to be version specific or version independent, or whether either can be used.
  /// </summary>
  public static class ReferenceVersionRulesCodes
  {
    /// <summary>
    /// The reference may be either version independent or version specific.
    /// </summary>
    public static readonly Coding EitherSpecificOrIndependent = new Coding
    {
      Code = "either",
      Display = "Either Specific or independent",
      System = "http://hl7.org/fhir/reference-version-rules"
    };
    /// <summary>
    /// The reference must be version independent.
    /// </summary>
    public static readonly Coding VersionIndependent = new Coding
    {
      Code = "independent",
      Display = "Version independent",
      System = "http://hl7.org/fhir/reference-version-rules"
    };
    /// <summary>
    /// The reference must be version specific.
    /// </summary>
    public static readonly Coding VersionSpecific = new Coding
    {
      Code = "specific",
      Display = "Version Specific",
      System = "http://hl7.org/fhir/reference-version-rules"
    };

    /// <summary>
    /// Literal for code: EitherSpecificOrIndependent
    /// </summary>
    public const string LiteralEitherSpecificOrIndependent = "either";

    /// <summary>
    /// Literal for code: ReferenceVersionRulesEitherSpecificOrIndependent
    /// </summary>
    public const string LiteralReferenceVersionRulesEitherSpecificOrIndependent = "http://hl7.org/fhir/reference-version-rules#either";

    /// <summary>
    /// Literal for code: VersionIndependent
    /// </summary>
    public const string LiteralVersionIndependent = "independent";

    /// <summary>
    /// Literal for code: ReferenceVersionRulesVersionIndependent
    /// </summary>
    public const string LiteralReferenceVersionRulesVersionIndependent = "http://hl7.org/fhir/reference-version-rules#independent";

    /// <summary>
    /// Literal for code: VersionSpecific
    /// </summary>
    public const string LiteralVersionSpecific = "specific";

    /// <summary>
    /// Literal for code: ReferenceVersionRulesVersionSpecific
    /// </summary>
    public const string LiteralReferenceVersionRulesVersionSpecific = "http://hl7.org/fhir/reference-version-rules#specific";

    /// <summary>
    /// Dictionary for looking up ReferenceVersionRules Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "either", EitherSpecificOrIndependent }, 
      { "http://hl7.org/fhir/reference-version-rules#either", EitherSpecificOrIndependent }, 
      { "independent", VersionIndependent }, 
      { "http://hl7.org/fhir/reference-version-rules#independent", VersionIndependent }, 
      { "specific", VersionSpecific }, 
      { "http://hl7.org/fhir/reference-version-rules#specific", VersionSpecific }, 
    };
  };
}
