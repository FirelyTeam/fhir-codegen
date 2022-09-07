// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Example Set of Location Characteristics
  /// </summary>
  public static class LocationCharacteristicCodes
  {
    /// <summary>
    /// The location is or has an intensive care unit
    /// </summary>
    public static readonly Coding HasICU = new Coding
    {
      Code = "has-icu",
      Display = "has ICU",
      System = "http://hl7.org/fhir/location-characteristic"
    };
    /// <summary>
    /// The location is or has an isolation ward
    /// </summary>
    public static readonly Coding IsolationWard = new Coding
    {
      Code = "has-iso-ward",
      Display = "isolation ward",
      System = "http://hl7.org/fhir/location-characteristic"
    };
    /// <summary>
    /// The location has negative pressure rooms available
    /// </summary>
    public static readonly Coding NegativePressureRoomsAvailable = new Coding
    {
      Code = "has-neg-press",
      Display = "negative pressure rooms available",
      System = "http://hl7.org/fhir/location-characteristic"
    };
    /// <summary>
    /// The location has oxygen and nitrogen services available
    /// </summary>
    public static readonly Coding OxygenNitrogenAvailable = new Coding
    {
      Code = "has-oxy-nitro",
      Display = "oxygen/nitrogen available",
      System = "http://hl7.org/fhir/location-characteristic"
    };
    /// <summary>
    /// The location has translation services available
    /// </summary>
    public static readonly Coding TranslationServicesAvailable = new Coding
    {
      Code = "has-translation",
      Display = "translation services available",
      System = "http://hl7.org/fhir/location-characteristic"
    };
    /// <summary>
    /// The location is accessible to thosre requiring wheelchair access
    /// </summary>
    public static readonly Coding WheelchairAccessible = new Coding
    {
      Code = "wheelchair",
      Display = "Wheelchair accessible",
      System = "http://hl7.org/fhir/location-characteristic"
    };

    /// <summary>
    /// Literal for code: HasICU
    /// </summary>
    public const string LiteralHasICU = "has-icu";

    /// <summary>
    /// Literal for code: LocationCharacteristicHasICU
    /// </summary>
    public const string LiteralLocationCharacteristicHasICU = "http://hl7.org/fhir/location-characteristic#has-icu";

    /// <summary>
    /// Literal for code: IsolationWard
    /// </summary>
    public const string LiteralIsolationWard = "has-iso-ward";

    /// <summary>
    /// Literal for code: LocationCharacteristicIsolationWard
    /// </summary>
    public const string LiteralLocationCharacteristicIsolationWard = "http://hl7.org/fhir/location-characteristic#has-iso-ward";

    /// <summary>
    /// Literal for code: NegativePressureRoomsAvailable
    /// </summary>
    public const string LiteralNegativePressureRoomsAvailable = "has-neg-press";

    /// <summary>
    /// Literal for code: LocationCharacteristicNegativePressureRoomsAvailable
    /// </summary>
    public const string LiteralLocationCharacteristicNegativePressureRoomsAvailable = "http://hl7.org/fhir/location-characteristic#has-neg-press";

    /// <summary>
    /// Literal for code: OxygenNitrogenAvailable
    /// </summary>
    public const string LiteralOxygenNitrogenAvailable = "has-oxy-nitro";

    /// <summary>
    /// Literal for code: LocationCharacteristicOxygenNitrogenAvailable
    /// </summary>
    public const string LiteralLocationCharacteristicOxygenNitrogenAvailable = "http://hl7.org/fhir/location-characteristic#has-oxy-nitro";

    /// <summary>
    /// Literal for code: TranslationServicesAvailable
    /// </summary>
    public const string LiteralTranslationServicesAvailable = "has-translation";

    /// <summary>
    /// Literal for code: LocationCharacteristicTranslationServicesAvailable
    /// </summary>
    public const string LiteralLocationCharacteristicTranslationServicesAvailable = "http://hl7.org/fhir/location-characteristic#has-translation";

    /// <summary>
    /// Literal for code: WheelchairAccessible
    /// </summary>
    public const string LiteralWheelchairAccessible = "wheelchair";

    /// <summary>
    /// Literal for code: LocationCharacteristicWheelchairAccessible
    /// </summary>
    public const string LiteralLocationCharacteristicWheelchairAccessible = "http://hl7.org/fhir/location-characteristic#wheelchair";

    /// <summary>
    /// Dictionary for looking up LocationCharacteristic Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "has-icu", HasICU }, 
      { "http://hl7.org/fhir/location-characteristic#has-icu", HasICU }, 
      { "has-iso-ward", IsolationWard }, 
      { "http://hl7.org/fhir/location-characteristic#has-iso-ward", IsolationWard }, 
      { "has-neg-press", NegativePressureRoomsAvailable }, 
      { "http://hl7.org/fhir/location-characteristic#has-neg-press", NegativePressureRoomsAvailable }, 
      { "has-oxy-nitro", OxygenNitrogenAvailable }, 
      { "http://hl7.org/fhir/location-characteristic#has-oxy-nitro", OxygenNitrogenAvailable }, 
      { "has-translation", TranslationServicesAvailable }, 
      { "http://hl7.org/fhir/location-characteristic#has-translation", TranslationServicesAvailable }, 
      { "wheelchair", WheelchairAccessible }, 
      { "http://hl7.org/fhir/location-characteristic#wheelchair", WheelchairAccessible }, 
    };
  };
}
