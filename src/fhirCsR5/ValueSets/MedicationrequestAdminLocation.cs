// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// MedicationRequest Administration Location Codes
  /// </summary>
  public static class MedicationrequestAdminLocationCodes
  {
    /// <summary>
    /// Community
    /// </summary>
    public static readonly Coding Community = new Coding
    {
      Code = "community",
      Display = "Community",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location"
    };
    /// <summary>
    /// Inpatient
    /// </summary>
    public static readonly Coding Inpatient = new Coding
    {
      Code = "inpatient",
      Display = "Inpatient",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location"
    };
    /// <summary>
    /// Outpatient
    /// </summary>
    public static readonly Coding Outpatient = new Coding
    {
      Code = "outpatient",
      Display = "Outpatient",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location"
    };

    /// <summary>
    /// Literal for code: Community
    /// </summary>
    public const string LiteralCommunity = "community";

    /// <summary>
    /// Literal for code: MedicationrequestAdminLocationCommunity
    /// </summary>
    public const string LiteralMedicationrequestAdminLocationCommunity = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#community";

    /// <summary>
    /// Literal for code: Inpatient
    /// </summary>
    public const string LiteralInpatient = "inpatient";

    /// <summary>
    /// Literal for code: MedicationrequestAdminLocationInpatient
    /// </summary>
    public const string LiteralMedicationrequestAdminLocationInpatient = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#inpatient";

    /// <summary>
    /// Literal for code: Outpatient
    /// </summary>
    public const string LiteralOutpatient = "outpatient";

    /// <summary>
    /// Literal for code: MedicationrequestAdminLocationOutpatient
    /// </summary>
    public const string LiteralMedicationrequestAdminLocationOutpatient = "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#outpatient";

    /// <summary>
    /// Dictionary for looking up MedicationrequestAdminLocation Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "community", Community }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#community", Community }, 
      { "inpatient", Inpatient }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#inpatient", Inpatient }, 
      { "outpatient", Outpatient }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-admin-location#outpatient", Outpatient }, 
    };
  };
}
