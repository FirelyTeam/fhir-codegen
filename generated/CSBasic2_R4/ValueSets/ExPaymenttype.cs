// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set includes example Payment Type codes.
  /// </summary>
  public static class ExPaymenttypeCodes
  {
    /// <summary>
    /// Complete (final) payment of the benefit under the Claim less any adjustments.
    /// </summary>
    public static readonly Coding Complete = new Coding
    {
      Code = "complete",
      Display = "Complete",
      System = "http://terminology.hl7.org/CodeSystem/ex-paymenttype"
    };
    /// <summary>
    /// Partial payment of the benefit under the Claim less any adjustments.
    /// </summary>
    public static readonly Coding Partial = new Coding
    {
      Code = "partial",
      Display = "Partial",
      System = "http://terminology.hl7.org/CodeSystem/ex-paymenttype"
    };
  };
}