// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// The severity of the audit entry.
  /// </summary>
  public static class AuditEventSeverityCodes
  {
    /// <summary>
    /// Notification should be sent to trigger action be taken. e.g., Loss of the primary network connection needing attention.
    /// </summary>
    public static readonly Coding Alert = new Coding
    {
      Code = "alert",
      Display = "Alert",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Critical conditions. e.g., A failure in the system's primary application that will reset automatically.
    /// </summary>
    public static readonly Coding Critical = new Coding
    {
      Code = "critical",
      Display = "Critical",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Debug-level messages. Information useful to developers for debugging the application.
    /// </summary>
    public static readonly Coding Debug = new Coding
    {
      Code = "debug",
      Display = "Debug",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// System is unusable. e.g., This level should only be reported by infrastructure and should not be used by applications.
    /// </summary>
    public static readonly Coding Emergency = new Coding
    {
      Code = "emergency",
      Display = "Emergency",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Error conditions. e.g., An application has exceeded its file storage limit and attempts to write are failing. 
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Normal operational messages that require no action. e.g., An application has started, paused, or ended successfully.
    /// </summary>
    public static readonly Coding Informational = new Coding
    {
      Code = "informational",
      Display = "Informational",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Notice messages. Normal but significant condition. Events that are unusual, but not error conditions.
    /// </summary>
    public static readonly Coding Notice = new Coding
    {
      Code = "notice",
      Display = "Notice",
      System = "http://hl7.org/fhir/audit-event-severity"
    };
    /// <summary>
    /// Warning conditions. May indicate that an error will occur if action is not taken. e.g., A non-root file system has only 2GB remaining.
    /// </summary>
    public static readonly Coding Warning = new Coding
    {
      Code = "warning",
      Display = "Warning",
      System = "http://hl7.org/fhir/audit-event-severity"
    };

    /// <summary>
    /// Literal for code: Alert
    /// </summary>
    public const string LiteralAlert = "alert";

    /// <summary>
    /// Literal for code: AuditEventSeverityAlert
    /// </summary>
    public const string LiteralAuditEventSeverityAlert = "http://hl7.org/fhir/audit-event-severity#alert";

    /// <summary>
    /// Literal for code: Critical
    /// </summary>
    public const string LiteralCritical = "critical";

    /// <summary>
    /// Literal for code: AuditEventSeverityCritical
    /// </summary>
    public const string LiteralAuditEventSeverityCritical = "http://hl7.org/fhir/audit-event-severity#critical";

    /// <summary>
    /// Literal for code: Debug
    /// </summary>
    public const string LiteralDebug = "debug";

    /// <summary>
    /// Literal for code: AuditEventSeverityDebug
    /// </summary>
    public const string LiteralAuditEventSeverityDebug = "http://hl7.org/fhir/audit-event-severity#debug";

    /// <summary>
    /// Literal for code: Emergency
    /// </summary>
    public const string LiteralEmergency = "emergency";

    /// <summary>
    /// Literal for code: AuditEventSeverityEmergency
    /// </summary>
    public const string LiteralAuditEventSeverityEmergency = "http://hl7.org/fhir/audit-event-severity#emergency";

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: AuditEventSeverityError
    /// </summary>
    public const string LiteralAuditEventSeverityError = "http://hl7.org/fhir/audit-event-severity#error";

    /// <summary>
    /// Literal for code: Informational
    /// </summary>
    public const string LiteralInformational = "informational";

    /// <summary>
    /// Literal for code: AuditEventSeverityInformational
    /// </summary>
    public const string LiteralAuditEventSeverityInformational = "http://hl7.org/fhir/audit-event-severity#informational";

    /// <summary>
    /// Literal for code: Notice
    /// </summary>
    public const string LiteralNotice = "notice";

    /// <summary>
    /// Literal for code: AuditEventSeverityNotice
    /// </summary>
    public const string LiteralAuditEventSeverityNotice = "http://hl7.org/fhir/audit-event-severity#notice";

    /// <summary>
    /// Literal for code: Warning
    /// </summary>
    public const string LiteralWarning = "warning";

    /// <summary>
    /// Literal for code: AuditEventSeverityWarning
    /// </summary>
    public const string LiteralAuditEventSeverityWarning = "http://hl7.org/fhir/audit-event-severity#warning";

    /// <summary>
    /// Dictionary for looking up AuditEventSeverity Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "alert", Alert }, 
      { "http://hl7.org/fhir/audit-event-severity#alert", Alert }, 
      { "critical", Critical }, 
      { "http://hl7.org/fhir/audit-event-severity#critical", Critical }, 
      { "debug", Debug }, 
      { "http://hl7.org/fhir/audit-event-severity#debug", Debug }, 
      { "emergency", Emergency }, 
      { "http://hl7.org/fhir/audit-event-severity#emergency", Emergency }, 
      { "error", Error }, 
      { "http://hl7.org/fhir/audit-event-severity#error", Error }, 
      { "informational", Informational }, 
      { "http://hl7.org/fhir/audit-event-severity#informational", Informational }, 
      { "notice", Notice }, 
      { "http://hl7.org/fhir/audit-event-severity#notice", Notice }, 
      { "warning", Warning }, 
      { "http://hl7.org/fhir/audit-event-severity#warning", Warning }, 
    };
  };
}
