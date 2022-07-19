// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set defines a set of codes that can be used to where the patient left the hospital.
  /// </summary>
  public static class EncounterDischargeDispositionCodes
  {
    /// <summary>
    /// The patient self discharged against medical advice.
    /// </summary>
    public static readonly Coding LeftAgainstAdvice = new Coding
    {
      Code = "aadvice",
      Display = "Left against advice",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient was discharged and has indicated that they are going to return home afterwards, but not the patient's home - e.g. a family member's home.
    /// </summary>
    public static readonly Coding AlternativeHome = new Coding
    {
      Code = "alt-home",
      Display = "Alternative home",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient has deceased during this encounter.
    /// </summary>
    public static readonly Coding Expired = new Coding
    {
      Code = "exp",
      Display = "Expired",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient was dicharged and has indicated that they are going to return home afterwards.
    /// </summary>
    public static readonly Coding Home = new Coding
    {
      Code = "home",
      Display = "Home",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient has been discharged into palliative care.
    /// </summary>
    public static readonly Coding Hospice = new Coding
    {
      Code = "hosp",
      Display = "Hospice",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient has been discharged into long-term care where is likely to be monitored through an ongoing episode-of-care.
    /// </summary>
    public static readonly Coding LongTermCare = new Coding
    {
      Code = "long",
      Display = "Long-term care",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The discharge disposition has not otherwise defined.
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "oth",
      Display = "Other",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient was transferred to another healthcare facility.
    /// </summary>
    public static readonly Coding OtherHealthcareFacility = new Coding
    {
      Code = "other-hcf",
      Display = "Other healthcare facility",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient has been transferred to a psychiatric facility.
    /// </summary>
    public static readonly Coding PsychiatricHospital = new Coding
    {
      Code = "psy",
      Display = "Psychiatric hospital",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient was discharged and is to receive post acute care rehabilitation services.
    /// </summary>
    public static readonly Coding Rehabilitation = new Coding
    {
      Code = "rehab",
      Display = "Rehabilitation",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };
    /// <summary>
    /// The patient has been discharged to a skilled nursing facility for the patient to receive additional care.
    /// </summary>
    public static readonly Coding SkilledNursingFacility = new Coding
    {
      Code = "snf",
      Display = "Skilled nursing facility",
      System = "http://terminology.hl7.org/CodeSystem/discharge-disposition"
    };

    /// <summary>
    /// Literal for code: LeftAgainstAdvice
    /// </summary>
    public const string LiteralLeftAgainstAdvice = "aadvice";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionLeftAgainstAdvice
    /// </summary>
    public const string LiteralEncounterDischargeDispositionLeftAgainstAdvice = "http://terminology.hl7.org/CodeSystem/discharge-disposition#aadvice";

    /// <summary>
    /// Literal for code: AlternativeHome
    /// </summary>
    public const string LiteralAlternativeHome = "alt-home";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionAlternativeHome
    /// </summary>
    public const string LiteralEncounterDischargeDispositionAlternativeHome = "http://terminology.hl7.org/CodeSystem/discharge-disposition#alt-home";

    /// <summary>
    /// Literal for code: Expired
    /// </summary>
    public const string LiteralExpired = "exp";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionExpired
    /// </summary>
    public const string LiteralEncounterDischargeDispositionExpired = "http://terminology.hl7.org/CodeSystem/discharge-disposition#exp";

    /// <summary>
    /// Literal for code: Home
    /// </summary>
    public const string LiteralHome = "home";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionHome
    /// </summary>
    public const string LiteralEncounterDischargeDispositionHome = "http://terminology.hl7.org/CodeSystem/discharge-disposition#home";

    /// <summary>
    /// Literal for code: Hospice
    /// </summary>
    public const string LiteralHospice = "hosp";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionHospice
    /// </summary>
    public const string LiteralEncounterDischargeDispositionHospice = "http://terminology.hl7.org/CodeSystem/discharge-disposition#hosp";

    /// <summary>
    /// Literal for code: LongTermCare
    /// </summary>
    public const string LiteralLongTermCare = "long";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionLongTermCare
    /// </summary>
    public const string LiteralEncounterDischargeDispositionLongTermCare = "http://terminology.hl7.org/CodeSystem/discharge-disposition#long";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "oth";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionOther
    /// </summary>
    public const string LiteralEncounterDischargeDispositionOther = "http://terminology.hl7.org/CodeSystem/discharge-disposition#oth";

    /// <summary>
    /// Literal for code: OtherHealthcareFacility
    /// </summary>
    public const string LiteralOtherHealthcareFacility = "other-hcf";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionOtherHealthcareFacility
    /// </summary>
    public const string LiteralEncounterDischargeDispositionOtherHealthcareFacility = "http://terminology.hl7.org/CodeSystem/discharge-disposition#other-hcf";

    /// <summary>
    /// Literal for code: PsychiatricHospital
    /// </summary>
    public const string LiteralPsychiatricHospital = "psy";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionPsychiatricHospital
    /// </summary>
    public const string LiteralEncounterDischargeDispositionPsychiatricHospital = "http://terminology.hl7.org/CodeSystem/discharge-disposition#psy";

    /// <summary>
    /// Literal for code: Rehabilitation
    /// </summary>
    public const string LiteralRehabilitation = "rehab";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionRehabilitation
    /// </summary>
    public const string LiteralEncounterDischargeDispositionRehabilitation = "http://terminology.hl7.org/CodeSystem/discharge-disposition#rehab";

    /// <summary>
    /// Literal for code: SkilledNursingFacility
    /// </summary>
    public const string LiteralSkilledNursingFacility = "snf";

    /// <summary>
    /// Literal for code: EncounterDischargeDispositionSkilledNursingFacility
    /// </summary>
    public const string LiteralEncounterDischargeDispositionSkilledNursingFacility = "http://terminology.hl7.org/CodeSystem/discharge-disposition#snf";

    /// <summary>
    /// Dictionary for looking up EncounterDischargeDisposition Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "aadvice", LeftAgainstAdvice }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#aadvice", LeftAgainstAdvice }, 
      { "alt-home", AlternativeHome }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#alt-home", AlternativeHome }, 
      { "exp", Expired }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#exp", Expired }, 
      { "home", Home }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#home", Home }, 
      { "hosp", Hospice }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#hosp", Hospice }, 
      { "long", LongTermCare }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#long", LongTermCare }, 
      { "oth", Other }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#oth", Other }, 
      { "other-hcf", OtherHealthcareFacility }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#other-hcf", OtherHealthcareFacility }, 
      { "psy", PsychiatricHospital }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#psy", PsychiatricHospital }, 
      { "rehab", Rehabilitation }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#rehab", Rehabilitation }, 
      { "snf", SkilledNursingFacility }, 
      { "http://terminology.hl7.org/CodeSystem/discharge-disposition#snf", SkilledNursingFacility }, 
    };
  };
}
