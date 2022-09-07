// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Controls how multiple enableWhen values are interpreted -  whether all or any must be true.
  /// </summary>
  public static class QuestionnaireEnableBehaviorCodes
  {
    /// <summary>
    /// Enable the question when all the enableWhen criteria are satisfied.
    /// </summary>
    public static readonly Coding All = new Coding
    {
      Code = "all",
      Display = "All",
      System = "http://hl7.org/fhir/questionnaire-enable-behavior"
    };
    /// <summary>
    /// Enable the question when any of the enableWhen criteria are satisfied.
    /// </summary>
    public static readonly Coding Any = new Coding
    {
      Code = "any",
      Display = "Any",
      System = "http://hl7.org/fhir/questionnaire-enable-behavior"
    };

    /// <summary>
    /// Literal for code: All
    /// </summary>
    public const string LiteralAll = "all";

    /// <summary>
    /// Literal for code: QuestionnaireEnableBehaviorAll
    /// </summary>
    public const string LiteralQuestionnaireEnableBehaviorAll = "http://hl7.org/fhir/questionnaire-enable-behavior#all";

    /// <summary>
    /// Literal for code: Any
    /// </summary>
    public const string LiteralAny = "any";

    /// <summary>
    /// Literal for code: QuestionnaireEnableBehaviorAny
    /// </summary>
    public const string LiteralQuestionnaireEnableBehaviorAny = "http://hl7.org/fhir/questionnaire-enable-behavior#any";

    /// <summary>
    /// Dictionary for looking up QuestionnaireEnableBehavior Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "all", All }, 
      { "http://hl7.org/fhir/questionnaire-enable-behavior#all", All }, 
      { "any", Any }, 
      { "http://hl7.org/fhir/questionnaire-enable-behavior#any", Any }, 
    };
  };
}
