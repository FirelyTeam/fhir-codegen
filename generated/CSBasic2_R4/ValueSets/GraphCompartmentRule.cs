// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// How a compartment must be linked.
  /// </summary>
  public static class GraphCompartmentRuleCodes
  {
    /// <summary>
    /// The compartment rule is defined in the accompanying FHIRPath expression.
    /// </summary>
    public static readonly Coding Custom = new Coding
    {
      Code = "custom",
      Display = "Custom",
      System = "http://hl7.org/fhir/graph-compartment-rule"
    };
    /// <summary>
    /// The compartment must be different.
    /// </summary>
    public static readonly Coding Different = new Coding
    {
      Code = "different",
      Display = "Different",
      System = "http://hl7.org/fhir/graph-compartment-rule"
    };
    /// <summary>
    /// The compartment must be identical (the same literal reference).
    /// </summary>
    public static readonly Coding Identical = new Coding
    {
      Code = "identical",
      Display = "Identical",
      System = "http://hl7.org/fhir/graph-compartment-rule"
    };
    /// <summary>
    /// The compartment must be the same - the record must be about the same patient, but the reference may be different.
    /// </summary>
    public static readonly Coding Matching = new Coding
    {
      Code = "matching",
      Display = "Matching",
      System = "http://hl7.org/fhir/graph-compartment-rule"
    };
  };
}