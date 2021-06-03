// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// FHIR Value set/code system definition for HL7 v2 table 0916 ( Relevant Clincial Information)
  /// </summary>
  public static class V20916Codes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PatientWasFastingPriorToTheProcedure = new Coding
    {
      Code = "F",
      Display = "Patient was fasting prior to the procedure.",
      System = "http://terminology.hl7.org/CodeSystem/v2-0916"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ThePatientIndicatedTheyDidNotFastPriorToTheProcedure = new Coding
    {
      Code = "NF",
      Display = "The patient indicated they did not fast prior to the procedure.",
      System = "http://terminology.hl7.org/CodeSystem/v2-0916"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding NotGivenPatientWasNotAskedAtTheTimeOfTheProcedure = new Coding
    {
      Code = "NG",
      Display = "Not Given - Patient was not asked at the time of the procedure.",
      System = "http://terminology.hl7.org/CodeSystem/v2-0916"
    };
  };
}