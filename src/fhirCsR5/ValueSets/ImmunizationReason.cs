// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the reason why a dose of vaccine was administered. This value set is provided as a suggestive example.
  /// </summary>
  public static class ImmunizationReasonCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding VAL281657000 = new Coding
    {
      Code = "281657000",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding VAL429060002 = new Coding
    {
      Code = "429060002",
      System = "http://snomed.info/sct"
    };

    /// <summary>
    /// Literal for code: VAL281657000
    /// </summary>
    public const string LiteralVAL281657000 = "281657000";

    /// <summary>
    /// Literal for code: NoneVAL281657000
    /// </summary>
    public const string LiteralNoneVAL281657000 = "http://snomed.info/sct#281657000";

    /// <summary>
    /// Literal for code: VAL429060002
    /// </summary>
    public const string LiteralVAL429060002 = "429060002";

    /// <summary>
    /// Literal for code: NoneVAL429060002
    /// </summary>
    public const string LiteralNoneVAL429060002 = "http://snomed.info/sct#429060002";

    /// <summary>
    /// Dictionary for looking up ImmunizationReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "281657000", VAL281657000 }, 
      { "http://snomed.info/sct#281657000", VAL281657000 }, 
      { "429060002", VAL429060002 }, 
      { "http://snomed.info/sct#429060002", VAL429060002 }, 
    };
  };
}
