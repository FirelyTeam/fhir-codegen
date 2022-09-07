// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The type of a name given to a substance.
  /// </summary>
  public static class SubstanceNameTypeCodes
  {
    /// <summary>
    /// brand
    /// </summary>
    public static readonly Coding Brand = new Coding
    {
      Code = "Brand",
      Display = "brand",
      System = "http://hl7.org/fhir/substance-name-type"
    };
    /// <summary>
    /// scientific
    /// </summary>
    public static readonly Coding Scientific = new Coding
    {
      Code = "Scientific",
      Display = "scientific",
      System = "http://hl7.org/fhir/substance-name-type"
    };
    /// <summary>
    /// systematic
    /// </summary>
    public static readonly Coding Systematic = new Coding
    {
      Code = "Systematic",
      Display = "systematic",
      System = "http://hl7.org/fhir/substance-name-type"
    };

    /// <summary>
    /// Literal for code: Brand
    /// </summary>
    public const string LiteralBrand = "Brand";

    /// <summary>
    /// Literal for code: SubstanceNameTypeBrand
    /// </summary>
    public const string LiteralSubstanceNameTypeBrand = "http://hl7.org/fhir/substance-name-type#Brand";

    /// <summary>
    /// Literal for code: Scientific
    /// </summary>
    public const string LiteralScientific = "Scientific";

    /// <summary>
    /// Literal for code: SubstanceNameTypeScientific
    /// </summary>
    public const string LiteralSubstanceNameTypeScientific = "http://hl7.org/fhir/substance-name-type#Scientific";

    /// <summary>
    /// Literal for code: Systematic
    /// </summary>
    public const string LiteralSystematic = "Systematic";

    /// <summary>
    /// Literal for code: SubstanceNameTypeSystematic
    /// </summary>
    public const string LiteralSubstanceNameTypeSystematic = "http://hl7.org/fhir/substance-name-type#Systematic";

    /// <summary>
    /// Dictionary for looking up SubstanceNameType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "Brand", Brand }, 
      { "http://hl7.org/fhir/substance-name-type#Brand", Brand }, 
      { "Scientific", Scientific }, 
      { "http://hl7.org/fhir/substance-name-type#Scientific", Scientific }, 
      { "Systematic", Systematic }, 
      { "http://hl7.org/fhir/substance-name-type#Systematic", Systematic }, 
    };
  };
}
