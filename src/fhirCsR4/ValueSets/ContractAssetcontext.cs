// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set contract specific codes for asset context.
  /// </summary>
  public static class ContractAssetcontextCodes
  {
    /// <summary>
    /// To be completed
    /// </summary>
    public static readonly Coding Custodian = new Coding
    {
      Code = "custodian",
      Display = "Custodian",
      System = "http://hl7.org/fhir/contract-asset-context"
    };

    /// <summary>
    /// Literal for code: Custodian
    /// </summary>
    public const string LiteralCustodian = "custodian";

    /// <summary>
    /// Literal for code: ContractAssetcontextCustodian
    /// </summary>
    public const string LiteralContractAssetcontextCustodian = "http://hl7.org/fhir/contract-asset-context#custodian";

    /// <summary>
    /// Dictionary for looking up ContractAssetcontext Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "custodian", Custodian }, 
      { "http://hl7.org/fhir/contract-asset-context#custodian", Custodian }, 
    };
  };
}
