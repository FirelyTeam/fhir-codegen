// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the reason why an administered dose has been assigned a particular status. Often, this reason describes why a dose is considered invalid. This value set is provided as a suggestive example.
  /// </summary>
  public static class ImmunizationEvaluationDoseStatusReasonCodes
  {
    /// <summary>
    /// The product was stored in a manner inconsistent with manufacturer guidelines potentially reducing the effectiveness of the product.
    /// </summary>
    public static readonly Coding AdverseStorageCondition = new Coding
    {
      Code = "advstorage",
      Display = "Adverse storage condition",
      System = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason"
    };
    /// <summary>
    /// The product was stored at a temperature inconsistent with manufacturer guidelines potentially reducing the effectiveness of the product.
    /// </summary>
    public static readonly Coding ColdChainBreak = new Coding
    {
      Code = "coldchbrk",
      Display = "Cold chain break",
      System = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason"
    };
    /// <summary>
    /// The product was administered after the expiration date associated with lot of vaccine.
    /// </summary>
    public static readonly Coding ExpiredLot = new Coding
    {
      Code = "explot",
      Display = "Expired lot",
      System = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason"
    };
    /// <summary>
    /// The product was administered at a time inconsistent with the documented schedule.
    /// </summary>
    public static readonly Coding AdministeredOutsideRecommendedSchedule = new Coding
    {
      Code = "outsidesched",
      Display = "Administered outside recommended schedule",
      System = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason"
    };
    /// <summary>
    /// The product administered has been recalled by the manufacturer.
    /// </summary>
    public static readonly Coding ProductRecall = new Coding
    {
      Code = "prodrecall",
      Display = "Product recall",
      System = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason"
    };

    /// <summary>
    /// Literal for code: AdverseStorageCondition
    /// </summary>
    public const string LiteralAdverseStorageCondition = "advstorage";

    /// <summary>
    /// Literal for code: ImmunizationEvaluationDoseStatusReasonAdverseStorageCondition
    /// </summary>
    public const string LiteralImmunizationEvaluationDoseStatusReasonAdverseStorageCondition = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#advstorage";

    /// <summary>
    /// Literal for code: ColdChainBreak
    /// </summary>
    public const string LiteralColdChainBreak = "coldchbrk";

    /// <summary>
    /// Literal for code: ImmunizationEvaluationDoseStatusReasonColdChainBreak
    /// </summary>
    public const string LiteralImmunizationEvaluationDoseStatusReasonColdChainBreak = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#coldchbrk";

    /// <summary>
    /// Literal for code: ExpiredLot
    /// </summary>
    public const string LiteralExpiredLot = "explot";

    /// <summary>
    /// Literal for code: ImmunizationEvaluationDoseStatusReasonExpiredLot
    /// </summary>
    public const string LiteralImmunizationEvaluationDoseStatusReasonExpiredLot = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#explot";

    /// <summary>
    /// Literal for code: AdministeredOutsideRecommendedSchedule
    /// </summary>
    public const string LiteralAdministeredOutsideRecommendedSchedule = "outsidesched";

    /// <summary>
    /// Literal for code: ImmunizationEvaluationDoseStatusReasonAdministeredOutsideRecommendedSchedule
    /// </summary>
    public const string LiteralImmunizationEvaluationDoseStatusReasonAdministeredOutsideRecommendedSchedule = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#outsidesched";

    /// <summary>
    /// Literal for code: ProductRecall
    /// </summary>
    public const string LiteralProductRecall = "prodrecall";

    /// <summary>
    /// Literal for code: ImmunizationEvaluationDoseStatusReasonProductRecall
    /// </summary>
    public const string LiteralImmunizationEvaluationDoseStatusReasonProductRecall = "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#prodrecall";

    /// <summary>
    /// Dictionary for looking up ImmunizationEvaluationDoseStatusReason Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "advstorage", AdverseStorageCondition }, 
      { "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#advstorage", AdverseStorageCondition }, 
      { "coldchbrk", ColdChainBreak }, 
      { "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#coldchbrk", ColdChainBreak }, 
      { "explot", ExpiredLot }, 
      { "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#explot", ExpiredLot }, 
      { "outsidesched", AdministeredOutsideRecommendedSchedule }, 
      { "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#outsidesched", AdministeredOutsideRecommendedSchedule }, 
      { "prodrecall", ProductRecall }, 
      { "http://terminology.hl7.org/CodeSystem/immunization-evaluation-dose-status-reason#prodrecall", ProductRecall }, 
    };
  };
}
