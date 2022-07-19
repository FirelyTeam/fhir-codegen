// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// A code specifying the types of information being requested.
  /// </summary>
  public static class EligibilityrequestPurposeCodes
  {
    /// <summary>
    /// The prior authorization requirements for the listed, or discovered if specified, converages for the categories of service and/or specifed biling codes are requested.
    /// </summary>
    public static readonly Coding CoverageAuthRequirements = new Coding
    {
      Code = "auth-requirements",
      Display = "Coverage auth-requirements",
      System = "http://hl7.org/fhir/eligibilityrequest-purpose"
    };
    /// <summary>
    /// The plan benefits and optionally benefits consumed  for the listed, or discovered if specified, converages are requested.
    /// </summary>
    public static readonly Coding CoverageBenefits = new Coding
    {
      Code = "benefits",
      Display = "Coverage benefits",
      System = "http://hl7.org/fhir/eligibilityrequest-purpose"
    };
    /// <summary>
    /// The insurer is requested to report on any coverages which they are aware of in addition to any specifed.
    /// </summary>
    public static readonly Coding CoverageDiscovery = new Coding
    {
      Code = "discovery",
      Display = "Coverage Discovery",
      System = "http://hl7.org/fhir/eligibilityrequest-purpose"
    };
    /// <summary>
    /// A check that the specified coverages are in-force is requested.
    /// </summary>
    public static readonly Coding CoverageValidation = new Coding
    {
      Code = "validation",
      Display = "Coverage Validation",
      System = "http://hl7.org/fhir/eligibilityrequest-purpose"
    };

    /// <summary>
    /// Literal for code: CoverageAuthRequirements
    /// </summary>
    public const string LiteralCoverageAuthRequirements = "auth-requirements";

    /// <summary>
    /// Literal for code: EligibilityrequestPurposeCoverageAuthRequirements
    /// </summary>
    public const string LiteralEligibilityrequestPurposeCoverageAuthRequirements = "http://hl7.org/fhir/eligibilityrequest-purpose#auth-requirements";

    /// <summary>
    /// Literal for code: CoverageBenefits
    /// </summary>
    public const string LiteralCoverageBenefits = "benefits";

    /// <summary>
    /// Literal for code: EligibilityrequestPurposeCoverageBenefits
    /// </summary>
    public const string LiteralEligibilityrequestPurposeCoverageBenefits = "http://hl7.org/fhir/eligibilityrequest-purpose#benefits";

    /// <summary>
    /// Literal for code: CoverageDiscovery
    /// </summary>
    public const string LiteralCoverageDiscovery = "discovery";

    /// <summary>
    /// Literal for code: EligibilityrequestPurposeCoverageDiscovery
    /// </summary>
    public const string LiteralEligibilityrequestPurposeCoverageDiscovery = "http://hl7.org/fhir/eligibilityrequest-purpose#discovery";

    /// <summary>
    /// Literal for code: CoverageValidation
    /// </summary>
    public const string LiteralCoverageValidation = "validation";

    /// <summary>
    /// Literal for code: EligibilityrequestPurposeCoverageValidation
    /// </summary>
    public const string LiteralEligibilityrequestPurposeCoverageValidation = "http://hl7.org/fhir/eligibilityrequest-purpose#validation";

    /// <summary>
    /// Dictionary for looking up EligibilityrequestPurpose Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "auth-requirements", CoverageAuthRequirements }, 
      { "http://hl7.org/fhir/eligibilityrequest-purpose#auth-requirements", CoverageAuthRequirements }, 
      { "benefits", CoverageBenefits }, 
      { "http://hl7.org/fhir/eligibilityrequest-purpose#benefits", CoverageBenefits }, 
      { "discovery", CoverageDiscovery }, 
      { "http://hl7.org/fhir/eligibilityrequest-purpose#discovery", CoverageDiscovery }, 
      { "validation", CoverageValidation }, 
      { "http://hl7.org/fhir/eligibilityrequest-purpose#validation", CoverageValidation }, 
    };
  };
}
