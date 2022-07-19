// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// MedicationRequest Course of Therapy Codes
  /// </summary>
  public static class MedicationrequestCourseOfTherapyCodes
  {
    /// <summary>
    /// A medication which the patient is only expected to consume for the duration of the current order and which is not expected to be renewed.
    /// </summary>
    public static readonly Coding ShortCourseAcuteTherapy = new Coding
    {
      Code = "acute",
      Display = "Short course (acute) therapy",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy"
    };
    /// <summary>
    /// A medication which is expected to be continued beyond the present order and which the patient should be assumed to be taking unless explicitly stopped.
    /// </summary>
    public static readonly Coding ContinuousLongTermTherapy = new Coding
    {
      Code = "continuous",
      Display = "Continuous long term therapy",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy"
    };
    /// <summary>
    /// A medication which is expected to be used on a part time basis at certain times of the year
    /// </summary>
    public static readonly Coding Seasonal = new Coding
    {
      Code = "seasonal",
      Display = "Seasonal",
      System = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy"
    };

    /// <summary>
    /// Literal for code: ShortCourseAcuteTherapy
    /// </summary>
    public const string LiteralShortCourseAcuteTherapy = "acute";

    /// <summary>
    /// Literal for code: MedicationrequestCourseOfTherapyShortCourseAcuteTherapy
    /// </summary>
    public const string LiteralMedicationrequestCourseOfTherapyShortCourseAcuteTherapy = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#acute";

    /// <summary>
    /// Literal for code: ContinuousLongTermTherapy
    /// </summary>
    public const string LiteralContinuousLongTermTherapy = "continuous";

    /// <summary>
    /// Literal for code: MedicationrequestCourseOfTherapyContinuousLongTermTherapy
    /// </summary>
    public const string LiteralMedicationrequestCourseOfTherapyContinuousLongTermTherapy = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#continuous";

    /// <summary>
    /// Literal for code: Seasonal
    /// </summary>
    public const string LiteralSeasonal = "seasonal";

    /// <summary>
    /// Literal for code: MedicationrequestCourseOfTherapySeasonal
    /// </summary>
    public const string LiteralMedicationrequestCourseOfTherapySeasonal = "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#seasonal";

    /// <summary>
    /// Dictionary for looking up MedicationrequestCourseOfTherapy Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "acute", ShortCourseAcuteTherapy }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#acute", ShortCourseAcuteTherapy }, 
      { "continuous", ContinuousLongTermTherapy }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#continuous", ContinuousLongTermTherapy }, 
      { "seasonal", Seasonal }, 
      { "http://terminology.hl7.org/CodeSystem/medicationrequest-course-of-therapy#seasonal", Seasonal }, 
    };
  };
}
