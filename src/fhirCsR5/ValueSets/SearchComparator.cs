// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// What Search Comparator Codes are supported in search.
  /// </summary>
  public static class SearchComparatorCodes
  {
    /// <summary>
    /// the value for the parameter in the resource is approximately the same to the provided value.
    /// </summary>
    public static readonly Coding Approximately = new Coding
    {
      Code = "ap",
      Display = "Approximately",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource ends before the provided value.
    /// </summary>
    public static readonly Coding EndsBefore = new Coding
    {
      Code = "eb",
      Display = "Ends Before",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is equal to the provided value.
    /// </summary>
    public static readonly new Coding Equals = new Coding
    {
      Code = "eq",
      Display = "Equals",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is greater or equal to the provided value.
    /// </summary>
    public static readonly Coding GreaterOrEquals = new Coding
    {
      Code = "ge",
      Display = "Greater or Equals",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is greater than the provided value.
    /// </summary>
    public static readonly Coding GreaterThan = new Coding
    {
      Code = "gt",
      Display = "Greater Than",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is less or equal to the provided value.
    /// </summary>
    public static readonly Coding LessOfEqual = new Coding
    {
      Code = "le",
      Display = "Less of Equal",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is less than the provided value.
    /// </summary>
    public static readonly Coding LessThan = new Coding
    {
      Code = "lt",
      Display = "Less Than",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource is not equal to the provided value.
    /// </summary>
    public static readonly Coding NotEquals = new Coding
    {
      Code = "ne",
      Display = "Not Equals",
      System = "http://hl7.org/fhir/search-comparator"
    };
    /// <summary>
    /// the value for the parameter in the resource starts after the provided value.
    /// </summary>
    public static readonly Coding StartsAfter = new Coding
    {
      Code = "sa",
      Display = "Starts After",
      System = "http://hl7.org/fhir/search-comparator"
    };

    /// <summary>
    /// Literal for code: Approximately
    /// </summary>
    public const string LiteralApproximately = "ap";

    /// <summary>
    /// Literal for code: SearchComparatorApproximately
    /// </summary>
    public const string LiteralSearchComparatorApproximately = "http://hl7.org/fhir/search-comparator#ap";

    /// <summary>
    /// Literal for code: EndsBefore
    /// </summary>
    public const string LiteralEndsBefore = "eb";

    /// <summary>
    /// Literal for code: SearchComparatorEndsBefore
    /// </summary>
    public const string LiteralSearchComparatorEndsBefore = "http://hl7.org/fhir/search-comparator#eb";

    /// <summary>
    /// Literal for code: Equals
    /// </summary>
    public const string LiteralEquals = "eq";

    /// <summary>
    /// Literal for code: SearchComparatorEquals
    /// </summary>
    public const string LiteralSearchComparatorEquals = "http://hl7.org/fhir/search-comparator#eq";

    /// <summary>
    /// Literal for code: GreaterOrEquals
    /// </summary>
    public const string LiteralGreaterOrEquals = "ge";

    /// <summary>
    /// Literal for code: SearchComparatorGreaterOrEquals
    /// </summary>
    public const string LiteralSearchComparatorGreaterOrEquals = "http://hl7.org/fhir/search-comparator#ge";

    /// <summary>
    /// Literal for code: GreaterThan
    /// </summary>
    public const string LiteralGreaterThan = "gt";

    /// <summary>
    /// Literal for code: SearchComparatorGreaterThan
    /// </summary>
    public const string LiteralSearchComparatorGreaterThan = "http://hl7.org/fhir/search-comparator#gt";

    /// <summary>
    /// Literal for code: LessOfEqual
    /// </summary>
    public const string LiteralLessOfEqual = "le";

    /// <summary>
    /// Literal for code: SearchComparatorLessOfEqual
    /// </summary>
    public const string LiteralSearchComparatorLessOfEqual = "http://hl7.org/fhir/search-comparator#le";

    /// <summary>
    /// Literal for code: LessThan
    /// </summary>
    public const string LiteralLessThan = "lt";

    /// <summary>
    /// Literal for code: SearchComparatorLessThan
    /// </summary>
    public const string LiteralSearchComparatorLessThan = "http://hl7.org/fhir/search-comparator#lt";

    /// <summary>
    /// Literal for code: NotEquals
    /// </summary>
    public const string LiteralNotEquals = "ne";

    /// <summary>
    /// Literal for code: SearchComparatorNotEquals
    /// </summary>
    public const string LiteralSearchComparatorNotEquals = "http://hl7.org/fhir/search-comparator#ne";

    /// <summary>
    /// Literal for code: StartsAfter
    /// </summary>
    public const string LiteralStartsAfter = "sa";

    /// <summary>
    /// Literal for code: SearchComparatorStartsAfter
    /// </summary>
    public const string LiteralSearchComparatorStartsAfter = "http://hl7.org/fhir/search-comparator#sa";

    /// <summary>
    /// Dictionary for looking up SearchComparator Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "ap", Approximately }, 
      { "http://hl7.org/fhir/search-comparator#ap", Approximately }, 
      { "eb", EndsBefore }, 
      { "http://hl7.org/fhir/search-comparator#eb", EndsBefore }, 
      { "eq", Equals }, 
      { "http://hl7.org/fhir/search-comparator#eq", Equals }, 
      { "ge", GreaterOrEquals }, 
      { "http://hl7.org/fhir/search-comparator#ge", GreaterOrEquals }, 
      { "gt", GreaterThan }, 
      { "http://hl7.org/fhir/search-comparator#gt", GreaterThan }, 
      { "le", LessOfEqual }, 
      { "http://hl7.org/fhir/search-comparator#le", LessOfEqual }, 
      { "lt", LessThan }, 
      { "http://hl7.org/fhir/search-comparator#lt", LessThan }, 
      { "ne", NotEquals }, 
      { "http://hl7.org/fhir/search-comparator#ne", NotEquals }, 
      { "sa", StartsAfter }, 
      { "http://hl7.org/fhir/search-comparator#sa", StartsAfter }, 
    };
  };
}
