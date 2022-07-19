// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Biologically Derived Product Status.
  /// </summary>
  public static class ProductStatusCodes
  {
    /// <summary>
    /// Product is currently available for use.
    /// </summary>
    public static readonly Coding Available = new Coding
    {
      Code = "available",
      Display = "Available",
      System = "http://hl7.org/fhir/product-status"
    };
    /// <summary>
    /// Product is not currently available for use.
    /// </summary>
    public static readonly Coding Unavailable = new Coding
    {
      Code = "unavailable",
      Display = "Unavailable",
      System = "http://hl7.org/fhir/product-status"
    };

    /// <summary>
    /// Literal for code: Available
    /// </summary>
    public const string LiteralAvailable = "available";

    /// <summary>
    /// Literal for code: ProductStatusAvailable
    /// </summary>
    public const string LiteralProductStatusAvailable = "http://hl7.org/fhir/product-status#available";

    /// <summary>
    /// Literal for code: Unavailable
    /// </summary>
    public const string LiteralUnavailable = "unavailable";

    /// <summary>
    /// Literal for code: ProductStatusUnavailable
    /// </summary>
    public const string LiteralProductStatusUnavailable = "http://hl7.org/fhir/product-status#unavailable";

    /// <summary>
    /// Dictionary for looking up ProductStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "available", Available }, 
      { "http://hl7.org/fhir/product-status#available", Available }, 
      { "unavailable", Unavailable }, 
      { "http://hl7.org/fhir/product-status#unavailable", Unavailable }, 
    };
  };
}
