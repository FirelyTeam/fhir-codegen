// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The type of relationship between documents.
  /// </summary>
  public static class DocumentRelationshipTypeCodes
  {
    /// <summary>
    /// This document adds additional information to the target document.
    /// </summary>
    public static readonly Coding Appends = new Coding
    {
      Code = "appends",
      Display = "Appends",
      System = "http://hl7.org/fhir/document-relationship-type"
    };
    /// <summary>
    /// This document embeds the content from the (source) target document.
    /// </summary>
    public static readonly Coding Incorporates = new Coding
    {
      Code = "incorporates",
      Display = "Incorporates",
      System = "http://hl7.org/fhir/document-relationship-type"
    };
    /// <summary>
    /// This document logically replaces or supersedes the target document.
    /// </summary>
    public static readonly Coding Replaces = new Coding
    {
      Code = "replaces",
      Display = "Replaces",
      System = "http://hl7.org/fhir/document-relationship-type"
    };
    /// <summary>
    /// This document is a signature of the target document.
    /// </summary>
    public static readonly Coding Signs = new Coding
    {
      Code = "signs",
      Display = "Signs",
      System = "http://hl7.org/fhir/document-relationship-type"
    };
    /// <summary>
    /// This document summarizes the content from the (source) target document.
    /// </summary>
    public static readonly Coding Summarizes = new Coding
    {
      Code = "summarizes",
      Display = "Summarizes",
      System = "http://hl7.org/fhir/document-relationship-type"
    };
    /// <summary>
    /// This document was generated by transforming the target document (e.g. format or language conversion).
    /// </summary>
    public static readonly Coding Transforms = new Coding
    {
      Code = "transforms",
      Display = "Transforms",
      System = "http://hl7.org/fhir/document-relationship-type"
    };

    /// <summary>
    /// Literal for code: Appends
    /// </summary>
    public const string LiteralAppends = "appends";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeAppends
    /// </summary>
    public const string LiteralDocumentRelationshipTypeAppends = "http://hl7.org/fhir/document-relationship-type#appends";

    /// <summary>
    /// Literal for code: Incorporates
    /// </summary>
    public const string LiteralIncorporates = "incorporates";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeIncorporates
    /// </summary>
    public const string LiteralDocumentRelationshipTypeIncorporates = "http://hl7.org/fhir/document-relationship-type#incorporates";

    /// <summary>
    /// Literal for code: Replaces
    /// </summary>
    public const string LiteralReplaces = "replaces";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeReplaces
    /// </summary>
    public const string LiteralDocumentRelationshipTypeReplaces = "http://hl7.org/fhir/document-relationship-type#replaces";

    /// <summary>
    /// Literal for code: Signs
    /// </summary>
    public const string LiteralSigns = "signs";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeSigns
    /// </summary>
    public const string LiteralDocumentRelationshipTypeSigns = "http://hl7.org/fhir/document-relationship-type#signs";

    /// <summary>
    /// Literal for code: Summarizes
    /// </summary>
    public const string LiteralSummarizes = "summarizes";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeSummarizes
    /// </summary>
    public const string LiteralDocumentRelationshipTypeSummarizes = "http://hl7.org/fhir/document-relationship-type#summarizes";

    /// <summary>
    /// Literal for code: Transforms
    /// </summary>
    public const string LiteralTransforms = "transforms";

    /// <summary>
    /// Literal for code: DocumentRelationshipTypeTransforms
    /// </summary>
    public const string LiteralDocumentRelationshipTypeTransforms = "http://hl7.org/fhir/document-relationship-type#transforms";

    /// <summary>
    /// Dictionary for looking up DocumentRelationshipType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "appends", Appends }, 
      { "http://hl7.org/fhir/document-relationship-type#appends", Appends }, 
      { "incorporates", Incorporates }, 
      { "http://hl7.org/fhir/document-relationship-type#incorporates", Incorporates }, 
      { "replaces", Replaces }, 
      { "http://hl7.org/fhir/document-relationship-type#replaces", Replaces }, 
      { "signs", Signs }, 
      { "http://hl7.org/fhir/document-relationship-type#signs", Signs }, 
      { "summarizes", Summarizes }, 
      { "http://hl7.org/fhir/document-relationship-type#summarizes", Summarizes }, 
      { "transforms", Transforms }, 
      { "http://hl7.org/fhir/document-relationship-type#transforms", Transforms }, 
    };
  };
}