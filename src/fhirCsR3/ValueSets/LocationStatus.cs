// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Indicates whether the location is still in use.
  /// </summary>
  public static class LocationStatusCodes
  {
    /// <summary>
    /// The location is operational.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/location-status"
    };
    /// <summary>
    /// The location is no longer used.
    /// </summary>
    public static readonly Coding Inactive = new Coding
    {
      Code = "inactive",
      Display = "Inactive",
      System = "http://hl7.org/fhir/location-status"
    };
    /// <summary>
    /// The location is temporarily closed.
    /// </summary>
    public static readonly Coding Suspended = new Coding
    {
      Code = "suspended",
      Display = "Suspended",
      System = "http://hl7.org/fhir/location-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: LocationStatusActive
    /// </summary>
    public const string LiteralLocationStatusActive = "http://hl7.org/fhir/location-status#active";

    /// <summary>
    /// Literal for code: Inactive
    /// </summary>
    public const string LiteralInactive = "inactive";

    /// <summary>
    /// Literal for code: LocationStatusInactive
    /// </summary>
    public const string LiteralLocationStatusInactive = "http://hl7.org/fhir/location-status#inactive";

    /// <summary>
    /// Literal for code: Suspended
    /// </summary>
    public const string LiteralSuspended = "suspended";

    /// <summary>
    /// Literal for code: LocationStatusSuspended
    /// </summary>
    public const string LiteralLocationStatusSuspended = "http://hl7.org/fhir/location-status#suspended";

    /// <summary>
    /// Dictionary for looking up LocationStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/location-status#active", Active }, 
      { "inactive", Inactive }, 
      { "http://hl7.org/fhir/location-status#inactive", Inactive }, 
      { "suspended", Suspended }, 
      { "http://hl7.org/fhir/location-status#suspended", Suspended }, 
    };
  };
}
