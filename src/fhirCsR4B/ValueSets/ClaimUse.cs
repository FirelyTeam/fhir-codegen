// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The purpose of the Claim: predetermination, preauthorization, claim.
  /// </summary>
  public static class ClaimUseCodes
  {
    /// <summary>
    /// The treatment is complete and this represents a Claim for the services.
    /// </summary>
    public static readonly Coding Claim = new Coding
    {
      Code = "claim",
      Display = "Claim",
      System = "http://hl7.org/fhir/claim-use"
    };
    /// <summary>
    /// The treatment is proposed and this represents a Pre-authorization for the services.
    /// </summary>
    public static readonly Coding Preauthorization = new Coding
    {
      Code = "preauthorization",
      Display = "Preauthorization",
      System = "http://hl7.org/fhir/claim-use"
    };
    /// <summary>
    /// The treatment is proposed and this represents a Pre-determination for the services.
    /// </summary>
    public static readonly Coding Predetermination = new Coding
    {
      Code = "predetermination",
      Display = "Predetermination",
      System = "http://hl7.org/fhir/claim-use"
    };

    /// <summary>
    /// Literal for code: Claim
    /// </summary>
    public const string LiteralClaim = "claim";

    /// <summary>
    /// Literal for code: ClaimUseClaim
    /// </summary>
    public const string LiteralClaimUseClaim = "http://hl7.org/fhir/claim-use#claim";

    /// <summary>
    /// Literal for code: Preauthorization
    /// </summary>
    public const string LiteralPreauthorization = "preauthorization";

    /// <summary>
    /// Literal for code: ClaimUsePreauthorization
    /// </summary>
    public const string LiteralClaimUsePreauthorization = "http://hl7.org/fhir/claim-use#preauthorization";

    /// <summary>
    /// Literal for code: Predetermination
    /// </summary>
    public const string LiteralPredetermination = "predetermination";

    /// <summary>
    /// Literal for code: ClaimUsePredetermination
    /// </summary>
    public const string LiteralClaimUsePredetermination = "http://hl7.org/fhir/claim-use#predetermination";

    /// <summary>
    /// Dictionary for looking up ClaimUse Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "claim", Claim }, 
      { "http://hl7.org/fhir/claim-use#claim", Claim }, 
      { "preauthorization", Preauthorization }, 
      { "http://hl7.org/fhir/claim-use#preauthorization", Preauthorization }, 
      { "predetermination", Predetermination }, 
      { "http://hl7.org/fhir/claim-use#predetermination", Predetermination }, 
    };
  };
}