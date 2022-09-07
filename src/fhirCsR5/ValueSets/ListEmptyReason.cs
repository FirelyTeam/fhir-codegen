// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// General reasons for a list to be empty. Reasons are either related to a summary list (i.e. problem or medication list) or to a workflow related list (i.e. consultation list).
  /// </summary>
  public static class ListEmptyReasonCodes
  {
    /// <summary>
    /// Closed
    /// </summary>
    public static readonly Coding Closed = new Coding
    {
      Code = "closed",
      Display = "Closed",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };
    /// <summary>
    /// Nil Known
    /// </summary>
    public static readonly Coding NilKnown = new Coding
    {
      Code = "nilknown",
      Display = "Nil Known",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };
    /// <summary>
    /// Not Asked
    /// </summary>
    public static readonly Coding NotAsked = new Coding
    {
      Code = "notasked",
      Display = "Not Asked",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };
    /// <summary>
    /// Not Started
    /// </summary>
    public static readonly Coding NotStarted = new Coding
    {
      Code = "notstarted",
      Display = "Not Started",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };
    /// <summary>
    /// Unavailable
    /// </summary>
    public static readonly Coding Unavailable = new Coding
    {
      Code = "unavailable",
      Display = "Unavailable",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };
    /// <summary>
    /// Information Withheld
    /// </summary>
    public static readonly Coding InformationWithheld = new Coding
    {
      Code = "withheld",
      Display = "Information Withheld",
      System = "http://terminology.hl7.org/CodeSystem/list-empty-reason"
    };

    /// <summary>
    /// Literal for code: Closed
    /// </summary>
    public const string LiteralClosed = "closed";

    /// <summary>
    /// Literal for code: ListEmptyReasonClosed
    /// </summary>
    public const string LiteralListEmptyReasonClosed = "http://terminology.hl7.org/CodeSystem/list-empty-reason#closed";

    /// <summary>
    /// Literal for code: NilKnown
    /// </summary>
    public const string LiteralNilKnown = "nilknown";

    /// <summary>
    /// Literal for code: ListEmptyReasonNilKnown
    /// </summary>
    public const string LiteralListEmptyReasonNilKnown = "http://terminology.hl7.org/CodeSystem/list-empty-reason#nilknown";

    /// <summary>
    /// Literal for code: NotAsked
    /// </summary>
    public const string LiteralNotAsked = "notasked";

    /// <summary>
    /// Literal for code: ListEmptyReasonNotAsked
    /// </summary>
    public const string LiteralListEmptyReasonNotAsked = "http://terminology.hl7.org/CodeSystem/list-empty-reason#notasked";

    /// <summary>
    /// Literal for code: NotStarted
    /// </summary>
    public const string LiteralNotStarted = "notstarted";

    /// <summary>
    /// Literal for code: ListEmptyReasonNotStarted
    /// </summary>
    public const string LiteralListEmptyReasonNotStarted = "http://terminology.hl7.org/CodeSystem/list-empty-reason#notstarted";

    /// <summary>
    /// Literal for code: Unavailable
    /// </summary>
    public const string LiteralUnavailable = "unavailable";

    /// <summary>
    /// Literal for code: ListEmptyReasonUnavailable
    /// </summary>
    public const string LiteralListEmptyReasonUnavailable = "http://terminology.hl7.org/CodeSystem/list-empty-reason#unavailable";

    /// <summary>
    /// Literal for code: InformationWithheld
    /// </summary>
    public const string LiteralInformationWithheld = "withheld";

    /// <summary>
    /// Literal for code: ListEmptyReasonInformationWithheld
    /// </summary>
    public const string LiteralListEmptyReasonInformationWithheld = "http://terminology.hl7.org/CodeSystem/list-empty-reason#withheld";

    /// <summary>
    /// Dictionary for looking up ListEmptyReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "closed", Closed }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#closed", Closed }, 
      { "nilknown", NilKnown }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#nilknown", NilKnown }, 
      { "notasked", NotAsked }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#notasked", NotAsked }, 
      { "notstarted", NotStarted }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#notstarted", NotStarted }, 
      { "unavailable", Unavailable }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#unavailable", Unavailable }, 
      { "withheld", InformationWithheld }, 
      { "http://terminology.hl7.org/CodeSystem/list-empty-reason#withheld", InformationWithheld }, 
    };
  };
}
