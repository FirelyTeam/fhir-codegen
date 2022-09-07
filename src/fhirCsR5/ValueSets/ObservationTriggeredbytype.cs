// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Codes providing the type of triggeredBy observation.
  /// </summary>
  public static class ObservationTriggeredbytypeCodes
  {
    /// <summary>
    /// Performance of the same test but with different parameters/settings/solution.
    /// </summary>
    public static readonly Coding ReRunPerPolicy = new Coding
    {
      Code = "re-run",
      Display = "Re-run (per policy)",
      System = "http://hl7.org/fhir/observation-triggeredbytype"
    };
    /// <summary>
    /// Performance of one or more other tests depending on the results of the initial test.  This may include collection of additional specimen. While a new ServiceRequest is not required to perform the additional test, where it is still needed (e.g., requesting another laboratory to perform the reflex test), the Observation.basedOn would reference the new ServiceRequest that requested the additional test to be performed as well as the original ServiceRequest to reflect the one that provided the authorization.
    /// </summary>
    public static readonly Coding Reflex = new Coding
    {
      Code = "reflex",
      Display = "Reflex",
      System = "http://hl7.org/fhir/observation-triggeredbytype"
    };
    /// <summary>
    /// Performance of the same test again with the same parameters/settings/solution.
    /// </summary>
    public static readonly Coding RepeatPerPolicy = new Coding
    {
      Code = "repeat",
      Display = "Repeat (per policy)",
      System = "http://hl7.org/fhir/observation-triggeredbytype"
    };

    /// <summary>
    /// Literal for code: ReRunPerPolicy
    /// </summary>
    public const string LiteralReRunPerPolicy = "re-run";

    /// <summary>
    /// Literal for code: ObservationTriggeredbytypeReRunPerPolicy
    /// </summary>
    public const string LiteralObservationTriggeredbytypeReRunPerPolicy = "http://hl7.org/fhir/observation-triggeredbytype#re-run";

    /// <summary>
    /// Literal for code: Reflex
    /// </summary>
    public const string LiteralReflex = "reflex";

    /// <summary>
    /// Literal for code: ObservationTriggeredbytypeReflex
    /// </summary>
    public const string LiteralObservationTriggeredbytypeReflex = "http://hl7.org/fhir/observation-triggeredbytype#reflex";

    /// <summary>
    /// Literal for code: RepeatPerPolicy
    /// </summary>
    public const string LiteralRepeatPerPolicy = "repeat";

    /// <summary>
    /// Literal for code: ObservationTriggeredbytypeRepeatPerPolicy
    /// </summary>
    public const string LiteralObservationTriggeredbytypeRepeatPerPolicy = "http://hl7.org/fhir/observation-triggeredbytype#repeat";

    /// <summary>
    /// Dictionary for looking up ObservationTriggeredbytype Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "re-run", ReRunPerPolicy }, 
      { "http://hl7.org/fhir/observation-triggeredbytype#re-run", ReRunPerPolicy }, 
      { "reflex", Reflex }, 
      { "http://hl7.org/fhir/observation-triggeredbytype#reflex", Reflex }, 
      { "repeat", RepeatPerPolicy }, 
      { "http://hl7.org/fhir/observation-triggeredbytype#repeat", RepeatPerPolicy }, 
    };
  };
}
