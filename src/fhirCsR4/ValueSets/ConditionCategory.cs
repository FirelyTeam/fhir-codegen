// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Preferred value set for Condition Categories.
  /// </summary>
  public static class ConditionCategoryCodes
  {
    /// <summary>
    /// A point in time diagnosis (e.g. from a physician or nurse) in context of an encounter.
    /// </summary>
    public static readonly Coding EncounterDiagnosis = new Coding
    {
      Code = "encounter-diagnosis",
      Display = "Encounter Diagnosis",
      System = "http://terminology.hl7.org/CodeSystem/condition-category"
    };
    /// <summary>
    /// An item on a problem list that can be managed over time and can be expressed by a practitioner (e.g. physician, nurse), patient, or related person.
    /// </summary>
    public static readonly Coding ProblemListItem = new Coding
    {
      Code = "problem-list-item",
      Display = "Problem List Item",
      System = "http://terminology.hl7.org/CodeSystem/condition-category"
    };

    /// <summary>
    /// Literal for code: EncounterDiagnosis
    /// </summary>
    public const string LiteralEncounterDiagnosis = "encounter-diagnosis";

    /// <summary>
    /// Literal for code: ConditionCategoryEncounterDiagnosis
    /// </summary>
    public const string LiteralConditionCategoryEncounterDiagnosis = "http://terminology.hl7.org/CodeSystem/condition-category#encounter-diagnosis";

    /// <summary>
    /// Literal for code: ProblemListItem
    /// </summary>
    public const string LiteralProblemListItem = "problem-list-item";

    /// <summary>
    /// Literal for code: ConditionCategoryProblemListItem
    /// </summary>
    public const string LiteralConditionCategoryProblemListItem = "http://terminology.hl7.org/CodeSystem/condition-category#problem-list-item";

    /// <summary>
    /// Dictionary for looking up ConditionCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "encounter-diagnosis", EncounterDiagnosis }, 
      { "http://terminology.hl7.org/CodeSystem/condition-category#encounter-diagnosis", EncounterDiagnosis }, 
      { "problem-list-item", ProblemListItem }, 
      { "http://terminology.hl7.org/CodeSystem/condition-category#problem-list-item", ProblemListItem }, 
    };
  };
}
