// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the evaluation for vaccine administration event.
  /// </summary>
  public static class ImmunizationEvaluationStatusCodes
  {
    /// <summary>
    /// All actions that are implied by the administration have occurred.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://terminology.hl7.org/CodeSystem/medication-admin-status"
    };
    /// <summary>
    /// The administration was entered in error and therefore nullified.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://terminology.hl7.org/CodeSystem/medication-admin-status"
    };

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: MedicationAdminStatusCompleted
    /// </summary>
    public const string LiteralMedicationAdminStatusCompleted = "http://terminology.hl7.org/CodeSystem/medication-admin-status#completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: MedicationAdminStatusEnteredInError
    /// </summary>
    public const string LiteralMedicationAdminStatusEnteredInError = "http://terminology.hl7.org/CodeSystem/medication-admin-status#entered-in-error";

    /// <summary>
    /// Dictionary for looking up ImmunizationEvaluationStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "completed", Completed }, 
      { "http://terminology.hl7.org/CodeSystem/medication-admin-status#completed", Completed }, 
      { "entered-in-error", EnteredInError }, 
      { "http://terminology.hl7.org/CodeSystem/medication-admin-status#entered-in-error", EnteredInError }, 
    };
  };
}
