// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// How a resource reference is interpreted when testing consent restrictions.
  /// </summary>
  public static class ConsentDataMeaningCodes
  {
    /// <summary>
    /// The consent applies to instances of resources that are authored by.
    /// </summary>
    public static readonly Coding AuthoredBy = new Coding
    {
      Code = "authoredby",
      Display = "AuthoredBy",
      System = "http://hl7.org/fhir/consent-data-meaning"
    };
    /// <summary>
    /// The consent applies directly to the instance of the resource and instances that refer to it.
    /// </summary>
    public static readonly Coding Dependents = new Coding
    {
      Code = "dependents",
      Display = "Dependents",
      System = "http://hl7.org/fhir/consent-data-meaning"
    };
    /// <summary>
    /// The consent applies directly to the instance of the resource.
    /// </summary>
    public static readonly Coding Instance = new Coding
    {
      Code = "instance",
      Display = "Instance",
      System = "http://hl7.org/fhir/consent-data-meaning"
    };
    /// <summary>
    /// The consent applies directly to the instance of the resource and instances it refers to.
    /// </summary>
    public static readonly Coding Related = new Coding
    {
      Code = "related",
      Display = "Related",
      System = "http://hl7.org/fhir/consent-data-meaning"
    };

    /// <summary>
    /// Literal for code: AuthoredBy
    /// </summary>
    public const string LiteralAuthoredBy = "authoredby";

    /// <summary>
    /// Literal for code: ConsentDataMeaningAuthoredBy
    /// </summary>
    public const string LiteralConsentDataMeaningAuthoredBy = "http://hl7.org/fhir/consent-data-meaning#authoredby";

    /// <summary>
    /// Literal for code: Dependents
    /// </summary>
    public const string LiteralDependents = "dependents";

    /// <summary>
    /// Literal for code: ConsentDataMeaningDependents
    /// </summary>
    public const string LiteralConsentDataMeaningDependents = "http://hl7.org/fhir/consent-data-meaning#dependents";

    /// <summary>
    /// Literal for code: Instance
    /// </summary>
    public const string LiteralInstance = "instance";

    /// <summary>
    /// Literal for code: ConsentDataMeaningInstance
    /// </summary>
    public const string LiteralConsentDataMeaningInstance = "http://hl7.org/fhir/consent-data-meaning#instance";

    /// <summary>
    /// Literal for code: Related
    /// </summary>
    public const string LiteralRelated = "related";

    /// <summary>
    /// Literal for code: ConsentDataMeaningRelated
    /// </summary>
    public const string LiteralConsentDataMeaningRelated = "http://hl7.org/fhir/consent-data-meaning#related";

    /// <summary>
    /// Dictionary for looking up ConsentDataMeaning Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "authoredby", AuthoredBy }, 
      { "http://hl7.org/fhir/consent-data-meaning#authoredby", AuthoredBy }, 
      { "dependents", Dependents }, 
      { "http://hl7.org/fhir/consent-data-meaning#dependents", Dependents }, 
      { "instance", Instance }, 
      { "http://hl7.org/fhir/consent-data-meaning#instance", Instance }, 
      { "related", Related }, 
      { "http://hl7.org/fhir/consent-data-meaning#related", Related }, 
    };
  };
}
