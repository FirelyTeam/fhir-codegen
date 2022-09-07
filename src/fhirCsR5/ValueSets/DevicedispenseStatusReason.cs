// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// DeviceDispense Status Reason Codes
  /// </summary>
  public static class DevicedispenseStatusReasonCodes
  {
    /// <summary>
    /// The device was not dispensed because a contraindication was found - for example pregnancy, allergy to a device component...
    /// </summary>
    public static readonly Coding Contraindication = new Coding
    {
      Code = "contraindication",
      Display = "Contraindication",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };
    /// <summary>
    /// The device was not dispensed because an incompatibility has been found with the device or between the device and other devices being used in the same context.
    /// </summary>
    public static readonly Coding IncompatibleDevice = new Coding
    {
      Code = "incompatible-device",
      Display = "Incompatible device",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };
    /// <summary>
    /// The device was not dispensed because it is of-market - for example not authorized, withdrawn or recalled.
    /// </summary>
    public static readonly Coding OffMarket = new Coding
    {
      Code = "off-market",
      Display = "Off market",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };
    /// <summary>
    /// The device was not dispensed because the order has expired or been invalidated.
    /// </summary>
    public static readonly Coding OrderExpired = new Coding
    {
      Code = "order-expired",
      Display = "Order expired",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };
    /// <summary>
    /// The device was not dispensed because it was not available.
    /// </summary>
    public static readonly Coding OutOfStock = new Coding
    {
      Code = "out-of-stock",
      Display = "Out of Stock",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };
    /// <summary>
    /// The device not dispensed because there was a verbal order.
    /// </summary>
    public static readonly Coding VerbalOrder = new Coding
    {
      Code = "verbal-order",
      Display = "Verbal order",
      System = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason"
    };

    /// <summary>
    /// Literal for code: Contraindication
    /// </summary>
    public const string LiteralContraindication = "contraindication";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonContraindication
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonContraindication = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#contraindication";

    /// <summary>
    /// Literal for code: IncompatibleDevice
    /// </summary>
    public const string LiteralIncompatibleDevice = "incompatible-device";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonIncompatibleDevice
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonIncompatibleDevice = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#incompatible-device";

    /// <summary>
    /// Literal for code: OffMarket
    /// </summary>
    public const string LiteralOffMarket = "off-market";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonOffMarket
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonOffMarket = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#off-market";

    /// <summary>
    /// Literal for code: OrderExpired
    /// </summary>
    public const string LiteralOrderExpired = "order-expired";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonOrderExpired
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonOrderExpired = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#order-expired";

    /// <summary>
    /// Literal for code: OutOfStock
    /// </summary>
    public const string LiteralOutOfStock = "out-of-stock";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonOutOfStock
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonOutOfStock = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#out-of-stock";

    /// <summary>
    /// Literal for code: VerbalOrder
    /// </summary>
    public const string LiteralVerbalOrder = "verbal-order";

    /// <summary>
    /// Literal for code: DevicedispenseStatusReasonVerbalOrder
    /// </summary>
    public const string LiteralDevicedispenseStatusReasonVerbalOrder = "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#verbal-order";

    /// <summary>
    /// Dictionary for looking up DevicedispenseStatusReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "contraindication", Contraindication }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#contraindication", Contraindication }, 
      { "incompatible-device", IncompatibleDevice }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#incompatible-device", IncompatibleDevice }, 
      { "off-market", OffMarket }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#off-market", OffMarket }, 
      { "order-expired", OrderExpired }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#order-expired", OrderExpired }, 
      { "out-of-stock", OutOfStock }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#out-of-stock", OutOfStock }, 
      { "verbal-order", VerbalOrder }, 
      { "http://hl7.org/fhir/CodeSystem/devicedispense-status-reason#verbal-order", VerbalOrder }, 
    };
  };
}
