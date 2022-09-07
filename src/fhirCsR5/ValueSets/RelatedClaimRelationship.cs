// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes sample Related Claim Relationship codes.
  /// </summary>
  public static class RelatedClaimRelationshipCodes
  {
    /// <summary>
    /// Associated Claim
    /// </summary>
    public static readonly Coding AssociatedClaim = new Coding
    {
      Code = "associated",
      Display = "Associated Claim",
      System = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship"
    };
    /// <summary>
    /// Prior Claim
    /// </summary>
    public static readonly Coding PriorClaim = new Coding
    {
      Code = "prior",
      Display = "Prior Claim",
      System = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship"
    };

    /// <summary>
    /// Literal for code: AssociatedClaim
    /// </summary>
    public const string LiteralAssociatedClaim = "associated";

    /// <summary>
    /// Literal for code: ExRelatedclaimrelationshipAssociatedClaim
    /// </summary>
    public const string LiteralExRelatedclaimrelationshipAssociatedClaim = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship#associated";

    /// <summary>
    /// Literal for code: PriorClaim
    /// </summary>
    public const string LiteralPriorClaim = "prior";

    /// <summary>
    /// Literal for code: ExRelatedclaimrelationshipPriorClaim
    /// </summary>
    public const string LiteralExRelatedclaimrelationshipPriorClaim = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship#prior";

    /// <summary>
    /// Dictionary for looking up RelatedClaimRelationship Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "associated", AssociatedClaim }, 
      { "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship#associated", AssociatedClaim }, 
      { "prior", PriorClaim }, 
      { "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship#prior", PriorClaim }, 
    };
  };
}