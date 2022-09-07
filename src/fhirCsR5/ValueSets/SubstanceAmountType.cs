// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The relationship between two substance types.
  /// </summary>
  public static class SubstanceAmountTypeCodes
  {
    /// <summary>
    /// Approximately
    /// </summary>
    public static readonly Coding Approximately = new Coding
    {
      Code = "Approximately",
      Display = "Approximately",
      System = "http://hl7.org/fhir/substance-amount-type"
    };
    /// <summary>
    /// Average
    /// </summary>
    public static readonly Coding Average = new Coding
    {
      Code = "Average",
      Display = "Average",
      System = "http://hl7.org/fhir/substance-amount-type"
    };
    /// <summary>
    /// Less Than
    /// </summary>
    public static readonly Coding LessThan = new Coding
    {
      Code = "LessThan",
      Display = "Less Than",
      System = "http://hl7.org/fhir/substance-amount-type"
    };
    /// <summary>
    /// More Than
    /// </summary>
    public static readonly Coding MoreThan = new Coding
    {
      Code = "MoreThan",
      Display = "More Than",
      System = "http://hl7.org/fhir/substance-amount-type"
    };

    /// <summary>
    /// Literal for code: Approximately
    /// </summary>
    public const string LiteralApproximately = "Approximately";

    /// <summary>
    /// Literal for code: SubstanceAmountTypeApproximately
    /// </summary>
    public const string LiteralSubstanceAmountTypeApproximately = "http://hl7.org/fhir/substance-amount-type#Approximately";

    /// <summary>
    /// Literal for code: Average
    /// </summary>
    public const string LiteralAverage = "Average";

    /// <summary>
    /// Literal for code: SubstanceAmountTypeAverage
    /// </summary>
    public const string LiteralSubstanceAmountTypeAverage = "http://hl7.org/fhir/substance-amount-type#Average";

    /// <summary>
    /// Literal for code: LessThan
    /// </summary>
    public const string LiteralLessThan = "LessThan";

    /// <summary>
    /// Literal for code: SubstanceAmountTypeLessThan
    /// </summary>
    public const string LiteralSubstanceAmountTypeLessThan = "http://hl7.org/fhir/substance-amount-type#LessThan";

    /// <summary>
    /// Literal for code: MoreThan
    /// </summary>
    public const string LiteralMoreThan = "MoreThan";

    /// <summary>
    /// Literal for code: SubstanceAmountTypeMoreThan
    /// </summary>
    public const string LiteralSubstanceAmountTypeMoreThan = "http://hl7.org/fhir/substance-amount-type#MoreThan";

    /// <summary>
    /// Dictionary for looking up SubstanceAmountType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "Approximately", Approximately }, 
      { "http://hl7.org/fhir/substance-amount-type#Approximately", Approximately }, 
      { "Average", Average }, 
      { "http://hl7.org/fhir/substance-amount-type#Average", Average }, 
      { "LessThan", LessThan }, 
      { "http://hl7.org/fhir/substance-amount-type#LessThan", LessThan }, 
      { "MoreThan", MoreThan }, 
      { "http://hl7.org/fhir/substance-amount-type#MoreThan", MoreThan }, 
    };
  };
}
