// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// How an entity was used in an activity.
  /// </summary>
  public static class ProvenanceEntityRoleCodes
  {
    /// <summary>
    /// The record resulting from this event adheres to the protocol, guideline, order set or other definition represented by this entity.
    /// </summary>
    public static readonly Coding Instantiates = new Coding
    {
      Code = "instantiates",
      Display = "Instantiates",
      System = "http://hl7.org/fhir/provenance-entity-role"
    };
    /// <summary>
    /// An entity that is copied in full or part by an agent that is not the author of the entity.
    /// </summary>
    public static readonly Coding Quotation = new Coding
    {
      Code = "quotation",
      Display = "Quotation",
      System = "http://hl7.org/fhir/provenance-entity-role"
    };
    /// <summary>
    /// An entity that is removed from accessibility, usually through the DELETE operator.
    /// </summary>
    public static readonly Coding Removal = new Coding
    {
      Code = "removal",
      Display = "Removal",
      System = "http://hl7.org/fhir/provenance-entity-role"
    };
    /// <summary>
    /// An entity that is used by the activity to produce a new version of that entity.
    /// </summary>
    public static readonly Coding Revision = new Coding
    {
      Code = "revision",
      Display = "Revision",
      System = "http://hl7.org/fhir/provenance-entity-role"
    };
    /// <summary>
    /// An entity that is used as input to the activity that produced the target.
    /// </summary>
    public static readonly Coding Source = new Coding
    {
      Code = "source",
      Display = "Source",
      System = "http://hl7.org/fhir/provenance-entity-role"
    };

    /// <summary>
    /// Literal for code: Instantiates
    /// </summary>
    public const string LiteralInstantiates = "instantiates";

    /// <summary>
    /// Literal for code: ProvenanceEntityRoleInstantiates
    /// </summary>
    public const string LiteralProvenanceEntityRoleInstantiates = "http://hl7.org/fhir/provenance-entity-role#instantiates";

    /// <summary>
    /// Literal for code: Quotation
    /// </summary>
    public const string LiteralQuotation = "quotation";

    /// <summary>
    /// Literal for code: ProvenanceEntityRoleQuotation
    /// </summary>
    public const string LiteralProvenanceEntityRoleQuotation = "http://hl7.org/fhir/provenance-entity-role#quotation";

    /// <summary>
    /// Literal for code: Removal
    /// </summary>
    public const string LiteralRemoval = "removal";

    /// <summary>
    /// Literal for code: ProvenanceEntityRoleRemoval
    /// </summary>
    public const string LiteralProvenanceEntityRoleRemoval = "http://hl7.org/fhir/provenance-entity-role#removal";

    /// <summary>
    /// Literal for code: Revision
    /// </summary>
    public const string LiteralRevision = "revision";

    /// <summary>
    /// Literal for code: ProvenanceEntityRoleRevision
    /// </summary>
    public const string LiteralProvenanceEntityRoleRevision = "http://hl7.org/fhir/provenance-entity-role#revision";

    /// <summary>
    /// Literal for code: Source
    /// </summary>
    public const string LiteralSource = "source";

    /// <summary>
    /// Literal for code: ProvenanceEntityRoleSource
    /// </summary>
    public const string LiteralProvenanceEntityRoleSource = "http://hl7.org/fhir/provenance-entity-role#source";

    /// <summary>
    /// Dictionary for looking up ProvenanceEntityRole Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "instantiates", Instantiates }, 
      { "http://hl7.org/fhir/provenance-entity-role#instantiates", Instantiates }, 
      { "quotation", Quotation }, 
      { "http://hl7.org/fhir/provenance-entity-role#quotation", Quotation }, 
      { "removal", Removal }, 
      { "http://hl7.org/fhir/provenance-entity-role#removal", Removal }, 
      { "revision", Revision }, 
      { "http://hl7.org/fhir/provenance-entity-role#revision", Revision }, 
      { "source", Source }, 
      { "http://hl7.org/fhir/provenance-entity-role#source", Source }, 
    };
  };
}
