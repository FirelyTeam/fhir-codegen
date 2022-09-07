// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Description Needed Here
  /// </summary>
  public static class AllergIntolSubstanceExpRiskCodes
  {
    /// <summary>
    /// Known risk of allergy or intolerance reaction upon exposure to the specified substance.
    /// </summary>
    public static readonly Coding KnownReactionRisk = new Coding
    {
      Code = "known-reaction-risk",
      Display = "Known Reaction Risk",
      System = "http://hl7.org/fhir/allerg-intol-substance-exp-risk"
    };
    /// <summary>
    /// No known risk of allergy or intolerance reaction upon exposure to the specified substance.
    /// </summary>
    public static readonly Coding NoKnownReactionRisk = new Coding
    {
      Code = "no-known-reaction-risk",
      Display = "No Known Reaction Risk",
      System = "http://hl7.org/fhir/allerg-intol-substance-exp-risk"
    };

    /// <summary>
    /// Literal for code: KnownReactionRisk
    /// </summary>
    public const string LiteralKnownReactionRisk = "known-reaction-risk";

    /// <summary>
    /// Literal for code: AllergIntolSubstanceExpRiskKnownReactionRisk
    /// </summary>
    public const string LiteralAllergIntolSubstanceExpRiskKnownReactionRisk = "http://hl7.org/fhir/allerg-intol-substance-exp-risk#known-reaction-risk";

    /// <summary>
    /// Literal for code: NoKnownReactionRisk
    /// </summary>
    public const string LiteralNoKnownReactionRisk = "no-known-reaction-risk";

    /// <summary>
    /// Literal for code: AllergIntolSubstanceExpRiskNoKnownReactionRisk
    /// </summary>
    public const string LiteralAllergIntolSubstanceExpRiskNoKnownReactionRisk = "http://hl7.org/fhir/allerg-intol-substance-exp-risk#no-known-reaction-risk";

    /// <summary>
    /// Dictionary for looking up AllergIntolSubstanceExpRisk Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "known-reaction-risk", KnownReactionRisk }, 
      { "http://hl7.org/fhir/allerg-intol-substance-exp-risk#known-reaction-risk", KnownReactionRisk }, 
      { "no-known-reaction-risk", NoKnownReactionRisk }, 
      { "http://hl7.org/fhir/allerg-intol-substance-exp-risk#no-known-reaction-risk", NoKnownReactionRisk }, 
    };
  };
}
