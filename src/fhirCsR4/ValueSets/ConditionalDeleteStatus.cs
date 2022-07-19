// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// A code that indicates how the server supports conditional delete.
  /// </summary>
  public static class ConditionalDeleteStatusCodes
  {
    /// <summary>
    /// Conditional deletes are supported, and multiple resources can be deleted in a single interaction.
    /// </summary>
    public static readonly Coding MultipleDeletesSupported = new Coding
    {
      Code = "multiple",
      Display = "Multiple Deletes Supported",
      System = "http://hl7.org/fhir/conditional-delete-status"
    };
    /// <summary>
    /// No support for conditional deletes.
    /// </summary>
    public static readonly Coding NotSupported = new Coding
    {
      Code = "not-supported",
      Display = "Not Supported",
      System = "http://hl7.org/fhir/conditional-delete-status"
    };
    /// <summary>
    /// Conditional deletes are supported, but only single resources at a time.
    /// </summary>
    public static readonly Coding SingleDeletesSupported = new Coding
    {
      Code = "single",
      Display = "Single Deletes Supported",
      System = "http://hl7.org/fhir/conditional-delete-status"
    };

    /// <summary>
    /// Literal for code: MultipleDeletesSupported
    /// </summary>
    public const string LiteralMultipleDeletesSupported = "multiple";

    /// <summary>
    /// Literal for code: ConditionalDeleteStatusMultipleDeletesSupported
    /// </summary>
    public const string LiteralConditionalDeleteStatusMultipleDeletesSupported = "http://hl7.org/fhir/conditional-delete-status#multiple";

    /// <summary>
    /// Literal for code: NotSupported
    /// </summary>
    public const string LiteralNotSupported = "not-supported";

    /// <summary>
    /// Literal for code: ConditionalDeleteStatusNotSupported
    /// </summary>
    public const string LiteralConditionalDeleteStatusNotSupported = "http://hl7.org/fhir/conditional-delete-status#not-supported";

    /// <summary>
    /// Literal for code: SingleDeletesSupported
    /// </summary>
    public const string LiteralSingleDeletesSupported = "single";

    /// <summary>
    /// Literal for code: ConditionalDeleteStatusSingleDeletesSupported
    /// </summary>
    public const string LiteralConditionalDeleteStatusSingleDeletesSupported = "http://hl7.org/fhir/conditional-delete-status#single";

    /// <summary>
    /// Dictionary for looking up ConditionalDeleteStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "multiple", MultipleDeletesSupported }, 
      { "http://hl7.org/fhir/conditional-delete-status#multiple", MultipleDeletesSupported }, 
      { "not-supported", NotSupported }, 
      { "http://hl7.org/fhir/conditional-delete-status#not-supported", NotSupported }, 
      { "single", SingleDeletesSupported }, 
      { "http://hl7.org/fhir/conditional-delete-status#single", SingleDeletesSupported }, 
    };
  };
}
