// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Indicates the type of care team.
  /// </summary>
  public static class CareTeamCategoryCodes
  {
    /// <summary>
    /// Event-focused care team
    /// </summary>
    public static readonly Coding EventFocusedCareTeam = new Coding
    {
      Code = "LA27975-4",
      Display = "Event-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Encounter-focused care team
    /// </summary>
    public static readonly Coding EncounterFocusedCareTeam = new Coding
    {
      Code = "LA27976-2",
      Display = "Encounter-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Episode of care-focused care team
    /// </summary>
    public static readonly Coding EpisodeOfCareFocusedCareTeam = new Coding
    {
      Code = "LA27977-0",
      Display = "Episode of care-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Condition-focused care team
    /// </summary>
    public static readonly Coding ConditionFocusedCareTeam = new Coding
    {
      Code = "LA27978-8",
      Display = "Condition-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Clinical research-focused care team
    /// </summary>
    public static readonly Coding ClinicalResearchFocusedCareTeam = new Coding
    {
      Code = "LA27980-4",
      Display = "Clinical research-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Longitudinal care-coordination focused care team
    /// </summary>
    public static readonly Coding LongitudinalCareCoordinationFocusedCareTeam = new Coding
    {
      Code = "LA28865-6",
      Display = "Longitudinal care-coordination focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Home &amp; Community Based Services (HCBS)-focused care team
    /// </summary>
    public static readonly Coding HomeAndCommunityBasedServicesHCBSFocusedCareTeam = new Coding
    {
      Code = "LA28866-4",
      Display = "Home & Community Based Services (HCBS)-focused care team",
      System = "http://loinc.org"
    };
    /// <summary>
    /// Public health-focused care team
    /// </summary>
    public static readonly Coding PublicHealthFocusedCareTeam = new Coding
    {
      Code = "LA28867-2",
      Display = "Public health-focused care team",
      System = "http://loinc.org"
    };

    /// <summary>
    /// Literal for code: EventFocusedCareTeam
    /// </summary>
    public const string LiteralEventFocusedCareTeam = "LA27975-4";

    /// <summary>
    /// Literal for code: NONEEventFocusedCareTeam
    /// </summary>
    public const string LiteralNONEEventFocusedCareTeam = "http://loinc.org#LA27975-4";

    /// <summary>
    /// Literal for code: EncounterFocusedCareTeam
    /// </summary>
    public const string LiteralEncounterFocusedCareTeam = "LA27976-2";

    /// <summary>
    /// Literal for code: NONEEncounterFocusedCareTeam
    /// </summary>
    public const string LiteralNONEEncounterFocusedCareTeam = "http://loinc.org#LA27976-2";

    /// <summary>
    /// Literal for code: EpisodeOfCareFocusedCareTeam
    /// </summary>
    public const string LiteralEpisodeOfCareFocusedCareTeam = "LA27977-0";

    /// <summary>
    /// Literal for code: NONEEpisodeOfCareFocusedCareTeam
    /// </summary>
    public const string LiteralNONEEpisodeOfCareFocusedCareTeam = "http://loinc.org#LA27977-0";

    /// <summary>
    /// Literal for code: ConditionFocusedCareTeam
    /// </summary>
    public const string LiteralConditionFocusedCareTeam = "LA27978-8";

    /// <summary>
    /// Literal for code: NONEConditionFocusedCareTeam
    /// </summary>
    public const string LiteralNONEConditionFocusedCareTeam = "http://loinc.org#LA27978-8";

    /// <summary>
    /// Literal for code: ClinicalResearchFocusedCareTeam
    /// </summary>
    public const string LiteralClinicalResearchFocusedCareTeam = "LA27980-4";

    /// <summary>
    /// Literal for code: NONEClinicalResearchFocusedCareTeam
    /// </summary>
    public const string LiteralNONEClinicalResearchFocusedCareTeam = "http://loinc.org#LA27980-4";

    /// <summary>
    /// Literal for code: LongitudinalCareCoordinationFocusedCareTeam
    /// </summary>
    public const string LiteralLongitudinalCareCoordinationFocusedCareTeam = "LA28865-6";

    /// <summary>
    /// Literal for code: NONELongitudinalCareCoordinationFocusedCareTeam
    /// </summary>
    public const string LiteralNONELongitudinalCareCoordinationFocusedCareTeam = "http://loinc.org#LA28865-6";

    /// <summary>
    /// Literal for code: HomeAndCommunityBasedServicesHCBSFocusedCareTeam
    /// </summary>
    public const string LiteralHomeAndCommunityBasedServicesHCBSFocusedCareTeam = "LA28866-4";

    /// <summary>
    /// Literal for code: NONEHomeAndCommunityBasedServicesHCBSFocusedCareTeam
    /// </summary>
    public const string LiteralNONEHomeAndCommunityBasedServicesHCBSFocusedCareTeam = "http://loinc.org#LA28866-4";

    /// <summary>
    /// Literal for code: PublicHealthFocusedCareTeam
    /// </summary>
    public const string LiteralPublicHealthFocusedCareTeam = "LA28867-2";

    /// <summary>
    /// Literal for code: NONEPublicHealthFocusedCareTeam
    /// </summary>
    public const string LiteralNONEPublicHealthFocusedCareTeam = "http://loinc.org#LA28867-2";

    /// <summary>
    /// Dictionary for looking up CareTeamCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "LA27975-4", EventFocusedCareTeam }, 
      { "http://loinc.org#LA27975-4", EventFocusedCareTeam }, 
      { "LA27976-2", EncounterFocusedCareTeam }, 
      { "http://loinc.org#LA27976-2", EncounterFocusedCareTeam }, 
      { "LA27977-0", EpisodeOfCareFocusedCareTeam }, 
      { "http://loinc.org#LA27977-0", EpisodeOfCareFocusedCareTeam }, 
      { "LA27978-8", ConditionFocusedCareTeam }, 
      { "http://loinc.org#LA27978-8", ConditionFocusedCareTeam }, 
      { "LA27980-4", ClinicalResearchFocusedCareTeam }, 
      { "http://loinc.org#LA27980-4", ClinicalResearchFocusedCareTeam }, 
      { "LA28865-6", LongitudinalCareCoordinationFocusedCareTeam }, 
      { "http://loinc.org#LA28865-6", LongitudinalCareCoordinationFocusedCareTeam }, 
      { "LA28866-4", HomeAndCommunityBasedServicesHCBSFocusedCareTeam }, 
      { "http://loinc.org#LA28866-4", HomeAndCommunityBasedServicesHCBSFocusedCareTeam }, 
      { "LA28867-2", PublicHealthFocusedCareTeam }, 
      { "http://loinc.org#LA28867-2", PublicHealthFocusedCareTeam }, 
    };
  };
}
