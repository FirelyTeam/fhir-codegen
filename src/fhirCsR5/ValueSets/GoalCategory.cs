// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Example codes for grouping goals to use for filtering or presentation.
  /// </summary>
  public static class GoalCategoryCodes
  {
    /// <summary>
    /// Behavioral
    /// </summary>
    public static readonly Coding Behavioral = new Coding
    {
      Code = "behavioral",
      Display = "Behavioral",
      System = "http://terminology.hl7.org/CodeSystem/goal-category"
    };
    /// <summary>
    /// Dietary
    /// </summary>
    public static readonly Coding Dietary = new Coding
    {
      Code = "dietary",
      Display = "Dietary",
      System = "http://terminology.hl7.org/CodeSystem/goal-category"
    };
    /// <summary>
    /// Nursing
    /// </summary>
    public static readonly Coding Nursing = new Coding
    {
      Code = "nursing",
      Display = "Nursing",
      System = "http://terminology.hl7.org/CodeSystem/goal-category"
    };
    /// <summary>
    /// Physiotherapy
    /// </summary>
    public static readonly Coding Physiotherapy = new Coding
    {
      Code = "physiotherapy",
      Display = "Physiotherapy",
      System = "http://terminology.hl7.org/CodeSystem/goal-category"
    };
    /// <summary>
    /// Safety
    /// </summary>
    public static readonly Coding Safety = new Coding
    {
      Code = "safety",
      Display = "Safety",
      System = "http://terminology.hl7.org/CodeSystem/goal-category"
    };

    /// <summary>
    /// Literal for code: Behavioral
    /// </summary>
    public const string LiteralBehavioral = "behavioral";

    /// <summary>
    /// Literal for code: GoalCategoryBehavioral
    /// </summary>
    public const string LiteralGoalCategoryBehavioral = "http://terminology.hl7.org/CodeSystem/goal-category#behavioral";

    /// <summary>
    /// Literal for code: Dietary
    /// </summary>
    public const string LiteralDietary = "dietary";

    /// <summary>
    /// Literal for code: GoalCategoryDietary
    /// </summary>
    public const string LiteralGoalCategoryDietary = "http://terminology.hl7.org/CodeSystem/goal-category#dietary";

    /// <summary>
    /// Literal for code: Nursing
    /// </summary>
    public const string LiteralNursing = "nursing";

    /// <summary>
    /// Literal for code: GoalCategoryNursing
    /// </summary>
    public const string LiteralGoalCategoryNursing = "http://terminology.hl7.org/CodeSystem/goal-category#nursing";

    /// <summary>
    /// Literal for code: Physiotherapy
    /// </summary>
    public const string LiteralPhysiotherapy = "physiotherapy";

    /// <summary>
    /// Literal for code: GoalCategoryPhysiotherapy
    /// </summary>
    public const string LiteralGoalCategoryPhysiotherapy = "http://terminology.hl7.org/CodeSystem/goal-category#physiotherapy";

    /// <summary>
    /// Literal for code: Safety
    /// </summary>
    public const string LiteralSafety = "safety";

    /// <summary>
    /// Literal for code: GoalCategorySafety
    /// </summary>
    public const string LiteralGoalCategorySafety = "http://terminology.hl7.org/CodeSystem/goal-category#safety";

    /// <summary>
    /// Dictionary for looking up GoalCategory Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "behavioral", Behavioral }, 
      { "http://terminology.hl7.org/CodeSystem/goal-category#behavioral", Behavioral }, 
      { "dietary", Dietary }, 
      { "http://terminology.hl7.org/CodeSystem/goal-category#dietary", Dietary }, 
      { "nursing", Nursing }, 
      { "http://terminology.hl7.org/CodeSystem/goal-category#nursing", Nursing }, 
      { "physiotherapy", Physiotherapy }, 
      { "http://terminology.hl7.org/CodeSystem/goal-category#physiotherapy", Physiotherapy }, 
      { "safety", Safety }, 
      { "http://terminology.hl7.org/CodeSystem/goal-category#safety", Safety }, 
    };
  };
}
