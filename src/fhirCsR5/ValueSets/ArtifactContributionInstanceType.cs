// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Artifact Contribution Instance Type
  /// </summary>
  public static class ArtifactContributionInstanceTypeCodes
  {
    /// <summary>
    /// Approved
    /// </summary>
    public static readonly Coding Approved = new Coding
    {
      Code = "approved",
      Display = "Approved",
      System = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type"
    };
    /// <summary>
    /// Edited
    /// </summary>
    public static readonly Coding Edited = new Coding
    {
      Code = "edited",
      Display = "Edited",
      System = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type"
    };
    /// <summary>
    /// Reviewed
    /// </summary>
    public static readonly Coding Reviewed = new Coding
    {
      Code = "reviewed",
      Display = "Reviewed",
      System = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type"
    };

    /// <summary>
    /// Literal for code: Approved
    /// </summary>
    public const string LiteralApproved = "approved";

    /// <summary>
    /// Literal for code: ArtifactContributionInstanceTypeApproved
    /// </summary>
    public const string LiteralArtifactContributionInstanceTypeApproved = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#approved";

    /// <summary>
    /// Literal for code: Edited
    /// </summary>
    public const string LiteralEdited = "edited";

    /// <summary>
    /// Literal for code: ArtifactContributionInstanceTypeEdited
    /// </summary>
    public const string LiteralArtifactContributionInstanceTypeEdited = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#edited";

    /// <summary>
    /// Literal for code: Reviewed
    /// </summary>
    public const string LiteralReviewed = "reviewed";

    /// <summary>
    /// Literal for code: ArtifactContributionInstanceTypeReviewed
    /// </summary>
    public const string LiteralArtifactContributionInstanceTypeReviewed = "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#reviewed";

    /// <summary>
    /// Dictionary for looking up ArtifactContributionInstanceType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "approved", Approved }, 
      { "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#approved", Approved }, 
      { "edited", Edited }, 
      { "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#edited", Edited }, 
      { "reviewed", Reviewed }, 
      { "http://terminology.hl7.org/CodeSystem/artifact-contribution-instance-type#reviewed", Reviewed }, 
    };
  };
}
