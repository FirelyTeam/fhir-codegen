// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The possible sort directions, ascending or descending.
  /// </summary>
  public static class SortDirectionCodes
  {
    /// <summary>
    /// Sort by the value ascending, so that lower values appear first.
    /// </summary>
    public static readonly Coding Ascending = new Coding
    {
      Code = "ascending",
      Display = "Ascending",
      System = "http://hl7.org/fhir/sort-direction"
    };
    /// <summary>
    /// Sort by the value descending, so that lower values appear last.
    /// </summary>
    public static readonly Coding Descending = new Coding
    {
      Code = "descending",
      Display = "Descending",
      System = "http://hl7.org/fhir/sort-direction"
    };

    /// <summary>
    /// Literal for code: Ascending
    /// </summary>
    public const string LiteralAscending = "ascending";

    /// <summary>
    /// Literal for code: SortDirectionAscending
    /// </summary>
    public const string LiteralSortDirectionAscending = "http://hl7.org/fhir/sort-direction#ascending";

    /// <summary>
    /// Literal for code: Descending
    /// </summary>
    public const string LiteralDescending = "descending";

    /// <summary>
    /// Literal for code: SortDirectionDescending
    /// </summary>
    public const string LiteralSortDirectionDescending = "http://hl7.org/fhir/sort-direction#descending";

    /// <summary>
    /// Dictionary for looking up SortDirection Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "ascending", Ascending }, 
      { "http://hl7.org/fhir/sort-direction#ascending", Ascending }, 
      { "descending", Descending }, 
      { "http://hl7.org/fhir/sort-direction#descending", Descending }, 
    };
  };
}
