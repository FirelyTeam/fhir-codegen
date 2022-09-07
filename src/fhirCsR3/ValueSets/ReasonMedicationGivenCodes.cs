// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// This value set is provided as an example. The value set to instantiate this attribute should be drawn from a robust terminology code system that consists of or contains concepts to support the medication process.
  /// </summary>
  public static class ReasonMedicationGivenCodesCodes
  {
    /// <summary>
    /// No reason known.
    /// </summary>
    public static readonly Coding None = new Coding
    {
      Code = "a",
      Display = "None",
      System = "http://hl7.org/fhir/reason-medication-given"
    };
    /// <summary>
    /// The administration was following an ordered protocol.
    /// </summary>
    public static readonly Coding GivenAsOrdered = new Coding
    {
      Code = "b",
      Display = "Given as Ordered",
      System = "http://hl7.org/fhir/reason-medication-given"
    };
    /// <summary>
    /// The administration was needed to treat an emergency.
    /// </summary>
    public static readonly Coding Emergency = new Coding
    {
      Code = "c",
      Display = "Emergency",
      System = "http://hl7.org/fhir/reason-medication-given"
    };

    /// <summary>
    /// Literal for code: None
    /// </summary>
    public const string LiteralNone = "a";

    /// <summary>
    /// Literal for code: ReasonMedicationGivenCodesNone
    /// </summary>
    public const string LiteralReasonMedicationGivenCodesNone = "http://hl7.org/fhir/reason-medication-given#a";

    /// <summary>
    /// Literal for code: GivenAsOrdered
    /// </summary>
    public const string LiteralGivenAsOrdered = "b";

    /// <summary>
    /// Literal for code: ReasonMedicationGivenCodesGivenAsOrdered
    /// </summary>
    public const string LiteralReasonMedicationGivenCodesGivenAsOrdered = "http://hl7.org/fhir/reason-medication-given#b";

    /// <summary>
    /// Literal for code: Emergency
    /// </summary>
    public const string LiteralEmergency = "c";

    /// <summary>
    /// Literal for code: ReasonMedicationGivenCodesEmergency
    /// </summary>
    public const string LiteralReasonMedicationGivenCodesEmergency = "http://hl7.org/fhir/reason-medication-given#c";

    /// <summary>
    /// Dictionary for looking up ReasonMedicationGivenCodes Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "a", None }, 
      { "http://hl7.org/fhir/reason-medication-given#a", None }, 
      { "b", GivenAsOrdered }, 
      { "http://hl7.org/fhir/reason-medication-given#b", GivenAsOrdered }, 
      { "c", Emergency }, 
      { "http://hl7.org/fhir/reason-medication-given#c", Emergency }, 
    };
  };
}