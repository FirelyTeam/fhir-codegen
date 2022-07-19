// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// This value set includes sample funds reservation type codes.
  /// </summary>
  public static class FundsreserveCodes
  {
    /// <summary>
    /// The payor is not being requested to reserve any funds for the settlement of future claims.
    /// </summary>
    public static readonly Coding None = new Coding
    {
      Code = "none",
      Display = "None",
      System = "http://terminology.hl7.org/CodeSystem/fundsreserve"
    };
    /// <summary>
    /// The payor is requested to reserve funds for the provision of the named services by any provider for settlement of future claims related to this request.
    /// </summary>
    public static readonly Coding Patient = new Coding
    {
      Code = "patient",
      Display = "Patient",
      System = "http://terminology.hl7.org/CodeSystem/fundsreserve"
    };
    /// <summary>
    /// The payor is requested to reserve funds solely for the named provider for settlement of future claims related to this request.
    /// </summary>
    public static readonly Coding Provider = new Coding
    {
      Code = "provider",
      Display = "Provider",
      System = "http://terminology.hl7.org/CodeSystem/fundsreserve"
    };

    /// <summary>
    /// Literal for code: None
    /// </summary>
    public const string LiteralNone = "none";

    /// <summary>
    /// Literal for code: FundsreserveNone
    /// </summary>
    public const string LiteralFundsreserveNone = "http://terminology.hl7.org/CodeSystem/fundsreserve#none";

    /// <summary>
    /// Literal for code: Patient
    /// </summary>
    public const string LiteralPatient = "patient";

    /// <summary>
    /// Literal for code: FundsreservePatient
    /// </summary>
    public const string LiteralFundsreservePatient = "http://terminology.hl7.org/CodeSystem/fundsreserve#patient";

    /// <summary>
    /// Literal for code: Provider
    /// </summary>
    public const string LiteralProvider = "provider";

    /// <summary>
    /// Literal for code: FundsreserveProvider
    /// </summary>
    public const string LiteralFundsreserveProvider = "http://terminology.hl7.org/CodeSystem/fundsreserve#provider";

    /// <summary>
    /// Dictionary for looking up Fundsreserve Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "none", None }, 
      { "http://terminology.hl7.org/CodeSystem/fundsreserve#none", None }, 
      { "patient", Patient }, 
      { "http://terminology.hl7.org/CodeSystem/fundsreserve#patient", Patient }, 
      { "provider", Provider }, 
      { "http://terminology.hl7.org/CodeSystem/fundsreserve#provider", Provider }, 
    };
  };
}
