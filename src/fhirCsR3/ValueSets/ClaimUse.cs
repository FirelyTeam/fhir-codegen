// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Complete, proposed, exploratory, other
  /// </summary>
  public static class ClaimUseCodes
  {
    /// <summary>
    /// The treatment is complete and this represents a Claim for the services.
    /// </summary>
    public static readonly Coding Complete = new Coding
    {
      Code = "complete",
      Display = "Complete",
      System = "http://hl7.org/fhir/claim-use"
    };
    /// <summary>
    /// The treatment is proposed and this represents a Pre-determination for the services.
    /// </summary>
    public static readonly Coding Exploratory = new Coding
    {
      Code = "exploratory",
      Display = "Exploratory",
      System = "http://hl7.org/fhir/claim-use"
    };
    /// <summary>
    /// A locally defined or otherwise resolved status.
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "other",
      Display = "Other",
      System = "http://hl7.org/fhir/claim-use"
    };
    /// <summary>
    /// The treatment is proposed and this represents a Pre-authorization for the services.
    /// </summary>
    public static readonly Coding Proposed = new Coding
    {
      Code = "proposed",
      Display = "Proposed",
      System = "http://hl7.org/fhir/claim-use"
    };

    /// <summary>
    /// Literal for code: Complete
    /// </summary>
    public const string LiteralComplete = "complete";

    /// <summary>
    /// Literal for code: ClaimUseComplete
    /// </summary>
    public const string LiteralClaimUseComplete = "http://hl7.org/fhir/claim-use#complete";

    /// <summary>
    /// Literal for code: Exploratory
    /// </summary>
    public const string LiteralExploratory = "exploratory";

    /// <summary>
    /// Literal for code: ClaimUseExploratory
    /// </summary>
    public const string LiteralClaimUseExploratory = "http://hl7.org/fhir/claim-use#exploratory";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "other";

    /// <summary>
    /// Literal for code: ClaimUseOther
    /// </summary>
    public const string LiteralClaimUseOther = "http://hl7.org/fhir/claim-use#other";

    /// <summary>
    /// Literal for code: Proposed
    /// </summary>
    public const string LiteralProposed = "proposed";

    /// <summary>
    /// Literal for code: ClaimUseProposed
    /// </summary>
    public const string LiteralClaimUseProposed = "http://hl7.org/fhir/claim-use#proposed";

    /// <summary>
    /// Dictionary for looking up ClaimUse Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "complete", Complete }, 
      { "http://hl7.org/fhir/claim-use#complete", Complete }, 
      { "exploratory", Exploratory }, 
      { "http://hl7.org/fhir/claim-use#exploratory", Exploratory }, 
      { "other", Other }, 
      { "http://hl7.org/fhir/claim-use#other", Other }, 
      { "proposed", Proposed }, 
      { "http://hl7.org/fhir/claim-use#proposed", Proposed }, 
    };
  };
}
