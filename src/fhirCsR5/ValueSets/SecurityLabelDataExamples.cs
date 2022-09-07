// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// A sample of security labels from [Healthcare Privacy and Security Classification System](security-labels.html#hcs) used on data (.meta.security) to indicate confidentialityCode classification and maybe sensitivity codes.
  /// </summary>
  public static class SecurityLabelDataExamplesCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ETH = new Coding
    {
      Code = "ETH",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding N = new Coding
    {
      Code = "N",
      System = "http://terminology.hl7.org/CodeSystem/v3-Confidentiality"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding PSY = new Coding
    {
      Code = "PSY",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding R = new Coding
    {
      Code = "R",
      System = "http://terminology.hl7.org/CodeSystem/v3-Confidentiality"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding STD = new Coding
    {
      Code = "STD",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };

    /// <summary>
    /// Literal for code: ETH
    /// </summary>
    public const string LiteralETH = "ETH";

    /// <summary>
    /// Literal for code: V3ActCodeETH
    /// </summary>
    public const string LiteralV3ActCodeETH = "http://terminology.hl7.org/CodeSystem/v3-ActCode#ETH";

    /// <summary>
    /// Literal for code: N
    /// </summary>
    public const string LiteralN = "N";

    /// <summary>
    /// Literal for code: V3ConfidentialityN
    /// </summary>
    public const string LiteralV3ConfidentialityN = "http://terminology.hl7.org/CodeSystem/v3-Confidentiality#N";

    /// <summary>
    /// Literal for code: PSY
    /// </summary>
    public const string LiteralPSY = "PSY";

    /// <summary>
    /// Literal for code: V3ActCodePSY
    /// </summary>
    public const string LiteralV3ActCodePSY = "http://terminology.hl7.org/CodeSystem/v3-ActCode#PSY";

    /// <summary>
    /// Literal for code: R
    /// </summary>
    public const string LiteralR = "R";

    /// <summary>
    /// Literal for code: V3ConfidentialityR
    /// </summary>
    public const string LiteralV3ConfidentialityR = "http://terminology.hl7.org/CodeSystem/v3-Confidentiality#R";

    /// <summary>
    /// Literal for code: STD
    /// </summary>
    public const string LiteralSTD = "STD";

    /// <summary>
    /// Literal for code: V3ActCodeSTD
    /// </summary>
    public const string LiteralV3ActCodeSTD = "http://terminology.hl7.org/CodeSystem/v3-ActCode#STD";

    /// <summary>
    /// Dictionary for looking up SecurityLabelDataExamples Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "ETH", ETH }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#ETH", ETH }, 
      { "N", N }, 
      { "http://terminology.hl7.org/CodeSystem/v3-Confidentiality#N", N }, 
      { "PSY", PSY }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#PSY", PSY }, 
      { "R", R }, 
      { "http://terminology.hl7.org/CodeSystem/v3-Confidentiality#R", R }, 
      { "STD", STD }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActCode#STD", STD }, 
    };
  };
}
