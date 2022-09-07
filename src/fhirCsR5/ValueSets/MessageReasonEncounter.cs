// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Example Message Reasons. These are the set of codes that might be used an updating an encounter using admin-update.
  /// </summary>
  public static class MessageReasonEncounterCodes
  {
    /// <summary>
    /// Absent
    /// </summary>
    public static readonly Coding Absent = new Coding
    {
      Code = "absent",
      Display = "Absent",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };
    /// <summary>
    /// Admit
    /// </summary>
    public static readonly Coding Admit = new Coding
    {
      Code = "admit",
      Display = "Admit",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };
    /// <summary>
    /// Discharge
    /// </summary>
    public static readonly Coding Discharge = new Coding
    {
      Code = "discharge",
      Display = "Discharge",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };
    /// <summary>
    /// Edit
    /// </summary>
    public static readonly Coding Edit = new Coding
    {
      Code = "edit",
      Display = "Edit",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };
    /// <summary>
    /// Moved
    /// </summary>
    public static readonly Coding Moved = new Coding
    {
      Code = "moved",
      Display = "Moved",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };
    /// <summary>
    /// Returned
    /// </summary>
    public static readonly Coding Returned = new Coding
    {
      Code = "return",
      Display = "Returned",
      System = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter"
    };

    /// <summary>
    /// Literal for code: Absent
    /// </summary>
    public const string LiteralAbsent = "absent";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterAbsent
    /// </summary>
    public const string LiteralMessageReasonsEncounterAbsent = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#absent";

    /// <summary>
    /// Literal for code: Admit
    /// </summary>
    public const string LiteralAdmit = "admit";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterAdmit
    /// </summary>
    public const string LiteralMessageReasonsEncounterAdmit = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#admit";

    /// <summary>
    /// Literal for code: Discharge
    /// </summary>
    public const string LiteralDischarge = "discharge";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterDischarge
    /// </summary>
    public const string LiteralMessageReasonsEncounterDischarge = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#discharge";

    /// <summary>
    /// Literal for code: Edit
    /// </summary>
    public const string LiteralEdit = "edit";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterEdit
    /// </summary>
    public const string LiteralMessageReasonsEncounterEdit = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#edit";

    /// <summary>
    /// Literal for code: Moved
    /// </summary>
    public const string LiteralMoved = "moved";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterMoved
    /// </summary>
    public const string LiteralMessageReasonsEncounterMoved = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#moved";

    /// <summary>
    /// Literal for code: Returned
    /// </summary>
    public const string LiteralReturned = "return";

    /// <summary>
    /// Literal for code: MessageReasonsEncounterReturned
    /// </summary>
    public const string LiteralMessageReasonsEncounterReturned = "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#return";

    /// <summary>
    /// Dictionary for looking up MessageReasonEncounter Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "absent", Absent }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#absent", Absent }, 
      { "admit", Admit }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#admit", Admit }, 
      { "discharge", Discharge }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#discharge", Discharge }, 
      { "edit", Edit }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#edit", Edit }, 
      { "moved", Moved }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#moved", Moved }, 
      { "return", Returned }, 
      { "http://terminology.hl7.org/CodeSystem/message-reasons-encounter#return", Returned }, 
    };
  };
}
