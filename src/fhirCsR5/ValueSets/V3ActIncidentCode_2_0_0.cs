// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Set of codes indicating the type of incident or accident.
  /// </summary>
  public static class V3ActIncidentCodeCodes
  {
    /// <summary>
    /// ActPatientSafetyIncidentCode
    /// </summary>
    public static readonly Coding ActPatientSafetyIncidentCode = new Coding
    {
      Code = "_ActPatientSafetyIncidentCode",
      Display = "ActPatientSafetyIncidentCode",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Motor vehicle accident
    /// </summary>
    public static readonly Coding MotorVehicleAccident = new Coding
    {
      Code = "MVA",
      Display = "Motor vehicle accident",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// School Accident
    /// </summary>
    public static readonly Coding SchoolAccident = new Coding
    {
      Code = "SCHOOL",
      Display = "School Accident",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Sporting Accident
    /// </summary>
    public static readonly Coding SportingAccident = new Coding
    {
      Code = "SPT",
      Display = "Sporting Accident",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Workplace accident
    /// </summary>
    public static readonly Coding WorkplaceAccident = new Coding
    {
      Code = "WPA",
      Display = "Workplace accident",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };

    /// <summary>
    /// Literal for code: ActPatientSafetyIncidentCode
    /// </summary>
    public const string LiteralActPatientSafetyIncidentCode = "_ActPatientSafetyIncidentCode";

    /// <summary>
    /// Literal for code: V3ActCodeActPatientSafetyIncidentCode
    /// </summary>
    public const string LiteralV3ActCodeActPatientSafetyIncidentCode = "http://terminology.hl7.org/CodeSystem/v3-ActCode#_ActPatientSafetyIncidentCode";

    /// <summary>
    /// Literal for code: MotorVehicleAccident
    /// </summary>
    public const string LiteralMotorVehicleAccident = "MVA";

    /// <summary>
    /// Literal for code: V3ActCodeMotorVehicleAccident
    /// </summary>
    public const string LiteralV3ActCodeMotorVehicleAccident = "http://terminology.hl7.org/CodeSystem/v3-ActCode#MVA";

    /// <summary>
    /// Literal for code: SchoolAccident
    /// </summary>
    public const string LiteralSchoolAccident = "SCHOOL";

    /// <summary>
    /// Literal for code: V3ActCodeSchoolAccident
    /// </summary>
    public const string LiteralV3ActCodeSchoolAccident = "http://terminology.hl7.org/CodeSystem/v3-ActCode#SCHOOL";

    /// <summary>
    /// Literal for code: SportingAccident
    /// </summary>
    public const string LiteralSportingAccident = "SPT";

    /// <summary>
    /// Literal for code: V3ActCodeSportingAccident
    /// </summary>
    public const string LiteralV3ActCodeSportingAccident = "http://terminology.hl7.org/CodeSystem/v3-ActCode#SPT";

    /// <summary>
    /// Literal for code: WorkplaceAccident
    /// </summary>
    public const string LiteralWorkplaceAccident = "WPA";

    /// <summary>
    /// Literal for code: V3ActCodeWorkplaceAccident
    /// </summary>
    public const string LiteralV3ActCodeWorkplaceAccident = "http://terminology.hl7.org/CodeSystem/v3-ActCode#WPA";

    /// <summary>
    /// Dictionary for looking up V3ActIncidentCode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "_ActPatientSafetyIncidentCode", ActPatientSafetyIncidentCode }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#_ActPatientSafetyIncidentCode", ActPatientSafetyIncidentCode }, 
      { "MVA", MotorVehicleAccident }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#MVA", MotorVehicleAccident }, 
      { "SCHOOL", SchoolAccident }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#SCHOOL", SchoolAccident }, 
      { "SPT", SportingAccident }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#SPT", SportingAccident }, 
      { "WPA", WorkplaceAccident }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#WPA", WorkplaceAccident }, 
    };
  };
}
