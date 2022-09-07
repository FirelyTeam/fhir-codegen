// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Citation classification type
  /// </summary>
  public static class CitationClassificationTypeCodes
  {
    /// <summary>
    /// Citation repository where this citation was created or copied from
    /// </summary>
    public static readonly Coding CitationSource = new Coding
    {
      Code = "citation-source",
      Display = "Citation Source",
      System = "http://terminology.hl7.org/CodeSystem/citation-classification-type"
    };
    /// <summary>
    /// Used for Citation sharing on the Fast Evidence Interoperability Resources (FEvIR) Platform
    /// </summary>
    public static readonly Coding FEvIRPlatformUse = new Coding
    {
      Code = "fevir-platform-use",
      Display = "FEvIR Platform Use",
      System = "http://terminology.hl7.org/CodeSystem/citation-classification-type"
    };
    /// <summary>
    /// The party responsible for creating and validating the MEDLINE citation
    /// </summary>
    public static readonly Coding MEDLINECitationOwner = new Coding
    {
      Code = "medline-owner",
      Display = "MEDLINE Citation Owner",
      System = "http://terminology.hl7.org/CodeSystem/citation-classification-type"
    };

    /// <summary>
    /// Literal for code: CitationSource
    /// </summary>
    public const string LiteralCitationSource = "citation-source";

    /// <summary>
    /// Literal for code: CitationClassificationTypeCitationSource
    /// </summary>
    public const string LiteralCitationClassificationTypeCitationSource = "http://terminology.hl7.org/CodeSystem/citation-classification-type#citation-source";

    /// <summary>
    /// Literal for code: FEvIRPlatformUse
    /// </summary>
    public const string LiteralFEvIRPlatformUse = "fevir-platform-use";

    /// <summary>
    /// Literal for code: CitationClassificationTypeFEvIRPlatformUse
    /// </summary>
    public const string LiteralCitationClassificationTypeFEvIRPlatformUse = "http://terminology.hl7.org/CodeSystem/citation-classification-type#fevir-platform-use";

    /// <summary>
    /// Literal for code: MEDLINECitationOwner
    /// </summary>
    public const string LiteralMEDLINECitationOwner = "medline-owner";

    /// <summary>
    /// Literal for code: CitationClassificationTypeMEDLINECitationOwner
    /// </summary>
    public const string LiteralCitationClassificationTypeMEDLINECitationOwner = "http://terminology.hl7.org/CodeSystem/citation-classification-type#medline-owner";

    /// <summary>
    /// Dictionary for looking up CitationClassificationType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "citation-source", CitationSource }, 
      { "http://terminology.hl7.org/CodeSystem/citation-classification-type#citation-source", CitationSource }, 
      { "fevir-platform-use", FEvIRPlatformUse }, 
      { "http://terminology.hl7.org/CodeSystem/citation-classification-type#fevir-platform-use", FEvIRPlatformUse }, 
      { "medline-owner", MEDLINECitationOwner }, 
      { "http://terminology.hl7.org/CodeSystem/citation-classification-type#medline-owner", MEDLINECitationOwner }, 
    };
  };
}
