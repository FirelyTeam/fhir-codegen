// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The kind of operation to perform as a part of a property based filter.
  /// </summary>
  public static class FilterOperatorCodes
  {
    /// <summary>
    /// The specified property of the code equals the provided value.
    /// </summary>
    public static readonly new Coding Equals = new Coding
    {
      Code = "=",
      Display = "Equals",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Only concepts with a direct hierarchical relationship to the index code and no other concepts. This does not include the index code in the output.
    /// </summary>
    public static readonly Coding ChildOf = new Coding
    {
      Code = "child-of",
      Display = "Child Of",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Includes concept ids that have a transitive is-a relationship with the concept Id provided as the value, but which do not have any concept ids with transitive is-a relationships with themselves.
    /// </summary>
    public static readonly Coding DescendentLeaf = new Coding
    {
      Code = "descendent-leaf",
      Display = "Descendent Leaf",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Includes all concept ids that have a transitive is-a relationship with the concept Id provided as the value, excluding the provided concept itself i.e. include descendant codes only).
    /// </summary>
    public static readonly Coding DescendentOfBySubsumption = new Coding
    {
      Code = "descendent-of",
      Display = "Descendent Of (by subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code has at least one value (if the specified value is true; if the specified value is false, then matches when the specified property of the code has no values).
    /// </summary>
    public static readonly Coding Exists = new Coding
    {
      Code = "exists",
      Display = "Exists",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Includes all concept ids that have a transitive is-a relationship from the concept Id provided as the value, including the provided concept itself (i.e. include ancestor codes and self).
    /// </summary>
    public static readonly Coding GeneralizesBySubsumption = new Coding
    {
      Code = "generalizes",
      Display = "Generalizes (by Subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code is in the set of codes or concepts specified in the provided value (comma separated list).
    /// </summary>
    public static readonly Coding InSet = new Coding
    {
      Code = "in",
      Display = "In Set",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// Includes all concept ids that have a transitive is-a relationship with the concept Id provided as the value, including the provided concept itself (include descendant codes and self).
    /// </summary>
    public static readonly Coding IsABySubsumption = new Coding
    {
      Code = "is-a",
      Display = "Is A (by subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code does not have an is-a relationship with the provided value.
    /// </summary>
    public static readonly Coding NotIsABySubsumption = new Coding
    {
      Code = "is-not-a",
      Display = "Not (Is A) (by subsumption)",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code is not in the set of codes or concepts specified in the provided value (comma separated list).
    /// </summary>
    public static readonly Coding NotInSet = new Coding
    {
      Code = "not-in",
      Display = "Not in Set",
      System = "http://hl7.org/fhir/filter-operator"
    };
    /// <summary>
    /// The specified property of the code  matches the regex specified in the provided value.
    /// </summary>
    public static readonly Coding RegularExpression = new Coding
    {
      Code = "regex",
      Display = "Regular Expression",
      System = "http://hl7.org/fhir/filter-operator"
    };

    /// <summary>
    /// Literal for code: Equals
    /// </summary>
    public const string LiteralEquals = "=";

    /// <summary>
    /// Literal for code: FilterOperatorEquals
    /// </summary>
    public const string LiteralFilterOperatorEquals = "http://hl7.org/fhir/filter-operator#=";

    /// <summary>
    /// Literal for code: ChildOf
    /// </summary>
    public const string LiteralChildOf = "child-of";

    /// <summary>
    /// Literal for code: FilterOperatorChildOf
    /// </summary>
    public const string LiteralFilterOperatorChildOf = "http://hl7.org/fhir/filter-operator#child-of";

    /// <summary>
    /// Literal for code: DescendentLeaf
    /// </summary>
    public const string LiteralDescendentLeaf = "descendent-leaf";

    /// <summary>
    /// Literal for code: FilterOperatorDescendentLeaf
    /// </summary>
    public const string LiteralFilterOperatorDescendentLeaf = "http://hl7.org/fhir/filter-operator#descendent-leaf";

    /// <summary>
    /// Literal for code: DescendentOfBySubsumption
    /// </summary>
    public const string LiteralDescendentOfBySubsumption = "descendent-of";

    /// <summary>
    /// Literal for code: FilterOperatorDescendentOfBySubsumption
    /// </summary>
    public const string LiteralFilterOperatorDescendentOfBySubsumption = "http://hl7.org/fhir/filter-operator#descendent-of";

    /// <summary>
    /// Literal for code: Exists
    /// </summary>
    public const string LiteralExists = "exists";

    /// <summary>
    /// Literal for code: FilterOperatorExists
    /// </summary>
    public const string LiteralFilterOperatorExists = "http://hl7.org/fhir/filter-operator#exists";

    /// <summary>
    /// Literal for code: GeneralizesBySubsumption
    /// </summary>
    public const string LiteralGeneralizesBySubsumption = "generalizes";

    /// <summary>
    /// Literal for code: FilterOperatorGeneralizesBySubsumption
    /// </summary>
    public const string LiteralFilterOperatorGeneralizesBySubsumption = "http://hl7.org/fhir/filter-operator#generalizes";

    /// <summary>
    /// Literal for code: InSet
    /// </summary>
    public const string LiteralInSet = "in";

    /// <summary>
    /// Literal for code: FilterOperatorInSet
    /// </summary>
    public const string LiteralFilterOperatorInSet = "http://hl7.org/fhir/filter-operator#in";

    /// <summary>
    /// Literal for code: IsABySubsumption
    /// </summary>
    public const string LiteralIsABySubsumption = "is-a";

    /// <summary>
    /// Literal for code: FilterOperatorIsABySubsumption
    /// </summary>
    public const string LiteralFilterOperatorIsABySubsumption = "http://hl7.org/fhir/filter-operator#is-a";

    /// <summary>
    /// Literal for code: NotIsABySubsumption
    /// </summary>
    public const string LiteralNotIsABySubsumption = "is-not-a";

    /// <summary>
    /// Literal for code: FilterOperatorNotIsABySubsumption
    /// </summary>
    public const string LiteralFilterOperatorNotIsABySubsumption = "http://hl7.org/fhir/filter-operator#is-not-a";

    /// <summary>
    /// Literal for code: NotInSet
    /// </summary>
    public const string LiteralNotInSet = "not-in";

    /// <summary>
    /// Literal for code: FilterOperatorNotInSet
    /// </summary>
    public const string LiteralFilterOperatorNotInSet = "http://hl7.org/fhir/filter-operator#not-in";

    /// <summary>
    /// Literal for code: RegularExpression
    /// </summary>
    public const string LiteralRegularExpression = "regex";

    /// <summary>
    /// Literal for code: FilterOperatorRegularExpression
    /// </summary>
    public const string LiteralFilterOperatorRegularExpression = "http://hl7.org/fhir/filter-operator#regex";

    /// <summary>
    /// Dictionary for looking up FilterOperator Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "=", Equals }, 
      { "http://hl7.org/fhir/filter-operator#=", Equals }, 
      { "child-of", ChildOf }, 
      { "http://hl7.org/fhir/filter-operator#child-of", ChildOf }, 
      { "descendent-leaf", DescendentLeaf }, 
      { "http://hl7.org/fhir/filter-operator#descendent-leaf", DescendentLeaf }, 
      { "descendent-of", DescendentOfBySubsumption }, 
      { "http://hl7.org/fhir/filter-operator#descendent-of", DescendentOfBySubsumption }, 
      { "exists", Exists }, 
      { "http://hl7.org/fhir/filter-operator#exists", Exists }, 
      { "generalizes", GeneralizesBySubsumption }, 
      { "http://hl7.org/fhir/filter-operator#generalizes", GeneralizesBySubsumption }, 
      { "in", InSet }, 
      { "http://hl7.org/fhir/filter-operator#in", InSet }, 
      { "is-a", IsABySubsumption }, 
      { "http://hl7.org/fhir/filter-operator#is-a", IsABySubsumption }, 
      { "is-not-a", NotIsABySubsumption }, 
      { "http://hl7.org/fhir/filter-operator#is-not-a", NotIsABySubsumption }, 
      { "not-in", NotInSet }, 
      { "http://hl7.org/fhir/filter-operator#not-in", NotInSet }, 
      { "regex", RegularExpression }, 
      { "http://hl7.org/fhir/filter-operator#regex", RegularExpression }, 
    };
  };
}
