// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Observation values that indicate what change in a measurement value or score is indicative of an improvement in the measured item or scored issue.
  /// </summary>
  public static class MeasureImprovementNotationCodes
  {
    /// <summary>
    /// Improvement is indicated as a decrease in the score or measurement (e.g. Lower score indicates better quality).
    /// </summary>
    public static readonly Coding DecreasedScoreIndicatesImprovement = new Coding
    {
      Code = "decrease",
      Display = "Decreased score indicates improvement",
      System = "http://terminology.hl7.org/CodeSystem/measure-improvement-notation"
    };
    /// <summary>
    /// Improvement is indicated as an increase in the score or measurement (e.g. Higher score indicates better quality).
    /// </summary>
    public static readonly Coding IncreasedScoreIndicatesImprovement = new Coding
    {
      Code = "increase",
      Display = "Increased score indicates improvement",
      System = "http://terminology.hl7.org/CodeSystem/measure-improvement-notation"
    };

    /// <summary>
    /// Literal for code: DecreasedScoreIndicatesImprovement
    /// </summary>
    public const string LiteralDecreasedScoreIndicatesImprovement = "decrease";

    /// <summary>
    /// Literal for code: MeasureImprovementNotationDecreasedScoreIndicatesImprovement
    /// </summary>
    public const string LiteralMeasureImprovementNotationDecreasedScoreIndicatesImprovement = "http://terminology.hl7.org/CodeSystem/measure-improvement-notation#decrease";

    /// <summary>
    /// Literal for code: IncreasedScoreIndicatesImprovement
    /// </summary>
    public const string LiteralIncreasedScoreIndicatesImprovement = "increase";

    /// <summary>
    /// Literal for code: MeasureImprovementNotationIncreasedScoreIndicatesImprovement
    /// </summary>
    public const string LiteralMeasureImprovementNotationIncreasedScoreIndicatesImprovement = "http://terminology.hl7.org/CodeSystem/measure-improvement-notation#increase";

    /// <summary>
    /// Dictionary for looking up MeasureImprovementNotation Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "decrease", DecreasedScoreIndicatesImprovement }, 
      { "http://terminology.hl7.org/CodeSystem/measure-improvement-notation#decrease", DecreasedScoreIndicatesImprovement }, 
      { "increase", IncreasedScoreIndicatesImprovement }, 
      { "http://terminology.hl7.org/CodeSystem/measure-improvement-notation#increase", IncreasedScoreIndicatesImprovement }, 
    };
  };
}
