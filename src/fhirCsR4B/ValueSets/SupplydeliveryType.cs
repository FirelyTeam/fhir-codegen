// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// This value sets refers to a specific supply item.
  /// </summary>
  public static class SupplydeliveryTypeCodes
  {
    /// <summary>
    /// What is supplied (or requested) is a device.
    /// </summary>
    public static readonly Coding Device = new Coding
    {
      Code = "device",
      Display = "Device",
      System = "http://terminology.hl7.org/CodeSystem/supply-item-type"
    };
    /// <summary>
    /// Supply is a kind of medication.
    /// </summary>
    public static readonly Coding Medication = new Coding
    {
      Code = "medication",
      Display = "Medication",
      System = "http://terminology.hl7.org/CodeSystem/supply-item-type"
    };

    /// <summary>
    /// Literal for code: Device
    /// </summary>
    public const string LiteralDevice = "device";

    /// <summary>
    /// Literal for code: SupplydeliveryTypeDevice
    /// </summary>
    public const string LiteralSupplydeliveryTypeDevice = "http://terminology.hl7.org/CodeSystem/supply-item-type#device";

    /// <summary>
    /// Literal for code: Medication
    /// </summary>
    public const string LiteralMedication = "medication";

    /// <summary>
    /// Literal for code: SupplydeliveryTypeMedication
    /// </summary>
    public const string LiteralSupplydeliveryTypeMedication = "http://terminology.hl7.org/CodeSystem/supply-item-type#medication";

    /// <summary>
    /// Dictionary for looking up SupplydeliveryType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "device", Device }, 
      { "http://terminology.hl7.org/CodeSystem/supply-item-type#device", Device }, 
      { "medication", Medication }, 
      { "http://terminology.hl7.org/CodeSystem/supply-item-type#medication", Medication }, 
    };
  };
}
