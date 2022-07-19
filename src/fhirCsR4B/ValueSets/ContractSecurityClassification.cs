// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// This value set contract specific codes for security classification.
  /// </summary>
  public static class ContractSecurityClassificationCodes
  {
    /// <summary>
    /// To be completed
    /// </summary>
    public static readonly Coding Policy = new Coding
    {
      Code = "policy",
      Display = "Policy",
      System = "http://hl7.org/fhir/contract-security-classification"
    };

    /// <summary>
    /// Literal for code: Policy
    /// </summary>
    public const string LiteralPolicy = "policy";

    /// <summary>
    /// Literal for code: ContractSecurityClassificationPolicy
    /// </summary>
    public const string LiteralContractSecurityClassificationPolicy = "http://hl7.org/fhir/contract-security-classification#policy";

    /// <summary>
    /// Dictionary for looking up ContractSecurityClassification Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "policy", Policy }, 
      { "http://hl7.org/fhir/contract-security-classification#policy", Policy }, 
    };
  };
}
