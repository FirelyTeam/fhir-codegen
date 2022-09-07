// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes a sample set of Forms codes.
  /// </summary>
  public static class FormsCodes
  {
    /// <summary>
    /// Form #1
    /// </summary>
    public static readonly Coding FormNumber1 = new Coding
    {
      Code = "1",
      Display = "Form #1",
      System = "http://terminology.hl7.org/CodeSystem/forms-codes"
    };
    /// <summary>
    /// Form #1
    /// </summary>
    public static readonly Coding FormNumber1_2 = new Coding
    {
      Code = "2",
      Display = "Form #1",
      System = "http://terminology.hl7.org/CodeSystem/forms-codes"
    };

    /// <summary>
    /// Literal for code: FormNumber1
    /// </summary>
    public const string LiteralFormNumber1 = "1";

    /// <summary>
    /// Literal for code: FormsCodesFormNumber1
    /// </summary>
    public const string LiteralFormsCodesFormNumber1 = "http://terminology.hl7.org/CodeSystem/forms-codes#1";

    /// <summary>
    /// Literal for code: FormNumber1_2
    /// </summary>
    public const string LiteralFormNumber1_2 = "2";

    /// <summary>
    /// Literal for code: FormsCodesFormNumber1_2
    /// </summary>
    public const string LiteralFormsCodesFormNumber1_2 = "http://terminology.hl7.org/CodeSystem/forms-codes#2";

    /// <summary>
    /// Dictionary for looking up Forms Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "1", FormNumber1 }, 
      { "http://terminology.hl7.org/CodeSystem/forms-codes#1", FormNumber1 }, 
      { "2", FormNumber1_2 }, 
      { "http://terminology.hl7.org/CodeSystem/forms-codes#2", FormNumber1_2 }, 
    };
  };
}
