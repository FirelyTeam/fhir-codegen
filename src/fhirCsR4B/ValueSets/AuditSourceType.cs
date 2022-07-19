// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The type of process where the audit event originated from.
  /// </summary>
  public static class AuditSourceTypeCodes
  {
    /// <summary>
    /// End-user display device, diagnostic device.
    /// </summary>
    public static readonly Coding UserDevice = new Coding
    {
      Code = "1",
      Display = "User Device",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Data acquisition device or instrument.
    /// </summary>
    public static readonly Coding DataInterface = new Coding
    {
      Code = "2",
      Display = "Data Interface",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Web Server process or thread.
    /// </summary>
    public static readonly Coding WebServer = new Coding
    {
      Code = "3",
      Display = "Web Server",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Application Server process or thread.
    /// </summary>
    public static readonly Coding ApplicationServer = new Coding
    {
      Code = "4",
      Display = "Application Server",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Database Server process or thread.
    /// </summary>
    public static readonly Coding DatabaseServer = new Coding
    {
      Code = "5",
      Display = "Database Server",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Security server, e.g. a domain controller.
    /// </summary>
    public static readonly Coding SecurityServer = new Coding
    {
      Code = "6",
      Display = "Security Server",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// ISO level 1-3 network component.
    /// </summary>
    public static readonly Coding NetworkDevice = new Coding
    {
      Code = "7",
      Display = "Network Device",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// ISO level 4-6 operating software.
    /// </summary>
    public static readonly Coding NetworkRouter = new Coding
    {
      Code = "8",
      Display = "Network Router",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };
    /// <summary>
    /// Other kind of device (defined by DICOM, but some other code/system can be used).
    /// </summary>
    public static readonly Coding Other = new Coding
    {
      Code = "9",
      Display = "Other",
      System = "http://terminology.hl7.org/CodeSystem/security-source-type"
    };

    /// <summary>
    /// Literal for code: UserDevice
    /// </summary>
    public const string LiteralUserDevice = "1";

    /// <summary>
    /// Literal for code: AuditSourceTypeUserDevice
    /// </summary>
    public const string LiteralAuditSourceTypeUserDevice = "http://terminology.hl7.org/CodeSystem/security-source-type#1";

    /// <summary>
    /// Literal for code: DataInterface
    /// </summary>
    public const string LiteralDataInterface = "2";

    /// <summary>
    /// Literal for code: AuditSourceTypeDataInterface
    /// </summary>
    public const string LiteralAuditSourceTypeDataInterface = "http://terminology.hl7.org/CodeSystem/security-source-type#2";

    /// <summary>
    /// Literal for code: WebServer
    /// </summary>
    public const string LiteralWebServer = "3";

    /// <summary>
    /// Literal for code: AuditSourceTypeWebServer
    /// </summary>
    public const string LiteralAuditSourceTypeWebServer = "http://terminology.hl7.org/CodeSystem/security-source-type#3";

    /// <summary>
    /// Literal for code: ApplicationServer
    /// </summary>
    public const string LiteralApplicationServer = "4";

    /// <summary>
    /// Literal for code: AuditSourceTypeApplicationServer
    /// </summary>
    public const string LiteralAuditSourceTypeApplicationServer = "http://terminology.hl7.org/CodeSystem/security-source-type#4";

    /// <summary>
    /// Literal for code: DatabaseServer
    /// </summary>
    public const string LiteralDatabaseServer = "5";

    /// <summary>
    /// Literal for code: AuditSourceTypeDatabaseServer
    /// </summary>
    public const string LiteralAuditSourceTypeDatabaseServer = "http://terminology.hl7.org/CodeSystem/security-source-type#5";

    /// <summary>
    /// Literal for code: SecurityServer
    /// </summary>
    public const string LiteralSecurityServer = "6";

    /// <summary>
    /// Literal for code: AuditSourceTypeSecurityServer
    /// </summary>
    public const string LiteralAuditSourceTypeSecurityServer = "http://terminology.hl7.org/CodeSystem/security-source-type#6";

    /// <summary>
    /// Literal for code: NetworkDevice
    /// </summary>
    public const string LiteralNetworkDevice = "7";

    /// <summary>
    /// Literal for code: AuditSourceTypeNetworkDevice
    /// </summary>
    public const string LiteralAuditSourceTypeNetworkDevice = "http://terminology.hl7.org/CodeSystem/security-source-type#7";

    /// <summary>
    /// Literal for code: NetworkRouter
    /// </summary>
    public const string LiteralNetworkRouter = "8";

    /// <summary>
    /// Literal for code: AuditSourceTypeNetworkRouter
    /// </summary>
    public const string LiteralAuditSourceTypeNetworkRouter = "http://terminology.hl7.org/CodeSystem/security-source-type#8";

    /// <summary>
    /// Literal for code: Other
    /// </summary>
    public const string LiteralOther = "9";

    /// <summary>
    /// Literal for code: AuditSourceTypeOther
    /// </summary>
    public const string LiteralAuditSourceTypeOther = "http://terminology.hl7.org/CodeSystem/security-source-type#9";

    /// <summary>
    /// Dictionary for looking up AuditSourceType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "1", UserDevice }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#1", UserDevice }, 
      { "2", DataInterface }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#2", DataInterface }, 
      { "3", WebServer }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#3", WebServer }, 
      { "4", ApplicationServer }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#4", ApplicationServer }, 
      { "5", DatabaseServer }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#5", DatabaseServer }, 
      { "6", SecurityServer }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#6", SecurityServer }, 
      { "7", NetworkDevice }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#7", NetworkDevice }, 
      { "8", NetworkRouter }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#8", NetworkRouter }, 
      { "9", Other }, 
      { "http://terminology.hl7.org/CodeSystem/security-source-type#9", Other }, 
    };
  };
}
