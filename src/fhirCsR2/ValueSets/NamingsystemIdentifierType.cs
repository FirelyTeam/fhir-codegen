// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Identifies the style of unique identifier used to identify a namespace.
  /// </summary>
  public static class NamingsystemIdentifierTypeCodes
  {
    /// <summary>
    /// An ISO object identifier; e.g. 1.2.3.4.5.
    /// </summary>
    public static readonly Coding OID = new Coding
    {
      Code = "oid",
      Display = "OID",
      System = "http://hl7.org/fhir/namingsystem-identifier-type"
    };
    /// <summary>
    /// Some other type of unique identifier; e.g. HL7-assigned reserved string such as LN for LOINC.
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "other",
      Display = "Other",
      System = "http://hl7.org/fhir/namingsystem-identifier-type"
    };
    /// <summary>
    /// A uniform resource identifier (ideally a URL - uniform resource locator); e.g. http://unitsofmeasure.org.
    /// </summary>
    public static readonly Coding URI = new Coding
    {
      Code = "uri",
      Display = "URI",
      System = "http://hl7.org/fhir/namingsystem-identifier-type"
    };
    /// <summary>
    /// A universally unique identifier of the form a5afddf4-e880-459b-876e-e4591b0acc11.
    /// </summary>
    public static readonly Coding UUID = new Coding
    {
      Code = "uuid",
      Display = "UUID",
      System = "http://hl7.org/fhir/namingsystem-identifier-type"
    };

    /// <summary>
    /// Literal for code: OID
    /// </summary>
    public const string LiteralOID = "oid";

    /// <summary>
    /// Literal for code: NamingsystemIdentifierTypeOID
    /// </summary>
    public const string LiteralNamingsystemIdentifierTypeOID = "http://hl7.org/fhir/namingsystem-identifier-type#oid";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "other";

    /// <summary>
    /// Literal for code: NamingsystemIdentifierTypeOther
    /// </summary>
    public const string LiteralNamingsystemIdentifierTypeOther = "http://hl7.org/fhir/namingsystem-identifier-type#other";

    /// <summary>
    /// Literal for code: URI
    /// </summary>
    public const string LiteralURI = "uri";

    /// <summary>
    /// Literal for code: NamingsystemIdentifierTypeURI
    /// </summary>
    public const string LiteralNamingsystemIdentifierTypeURI = "http://hl7.org/fhir/namingsystem-identifier-type#uri";

    /// <summary>
    /// Literal for code: UUID
    /// </summary>
    public const string LiteralUUID = "uuid";

    /// <summary>
    /// Literal for code: NamingsystemIdentifierTypeUUID
    /// </summary>
    public const string LiteralNamingsystemIdentifierTypeUUID = "http://hl7.org/fhir/namingsystem-identifier-type#uuid";

    /// <summary>
    /// Dictionary for looking up NamingsystemIdentifierType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "oid", OID }, 
      { "http://hl7.org/fhir/namingsystem-identifier-type#oid", OID }, 
      { "other", Other }, 
      { "http://hl7.org/fhir/namingsystem-identifier-type#other", Other }, 
      { "uri", URI }, 
      { "http://hl7.org/fhir/namingsystem-identifier-type#uri", URI }, 
      { "uuid", UUID }, 
      { "http://hl7.org/fhir/namingsystem-identifier-type#uuid", UUID }, 
    };
  };
}