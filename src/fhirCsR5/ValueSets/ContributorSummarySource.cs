// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Used to code the producer or rule for creating the display string.
  /// </summary>
  public static class ContributorSummarySourceCodes
  {
    /// <summary>
    /// Data copied by human from article text.
    /// </summary>
    public static readonly Coding CopiedFromArticle = new Coding
    {
      Code = "article-copy",
      Display = "Copied from article",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Data copied by machine from citation manager data.
    /// </summary>
    public static readonly Coding ReportedByCitationManager = new Coding
    {
      Code = "citation-manager",
      Display = "Reported by citation manager",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Custom format (may be described in text note).
    /// </summary>
    public static readonly Coding CustomFormat = new Coding
    {
      Code = "custom",
      Display = "custom format",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };
    /// <summary>
    /// Data copied by machine from publisher data.
    /// </summary>
    public static readonly Coding PublisherProvided = new Coding
    {
      Code = "publisher-data",
      Display = "Publisher provided",
      System = "http://terminology.hl7.org/CodeSystem/contributor-summary-source"
    };

    /// <summary>
    /// Literal for code: CopiedFromArticle
    /// </summary>
    public const string LiteralCopiedFromArticle = "article-copy";

    /// <summary>
    /// Literal for code: ContributorSummarySourceCopiedFromArticle
    /// </summary>
    public const string LiteralContributorSummarySourceCopiedFromArticle = "http://terminology.hl7.org/CodeSystem/contributor-summary-source#article-copy";

    /// <summary>
    /// Literal for code: ReportedByCitationManager
    /// </summary>
    public const string LiteralReportedByCitationManager = "citation-manager";

    /// <summary>
    /// Literal for code: ContributorSummarySourceReportedByCitationManager
    /// </summary>
    public const string LiteralContributorSummarySourceReportedByCitationManager = "http://terminology.hl7.org/CodeSystem/contributor-summary-source#citation-manager";

    /// <summary>
    /// Literal for code: CustomFormat
    /// </summary>
    public const string LiteralCustomFormat = "custom";

    /// <summary>
    /// Literal for code: ContributorSummarySourceCustomFormat
    /// </summary>
    public const string LiteralContributorSummarySourceCustomFormat = "http://terminology.hl7.org/CodeSystem/contributor-summary-source#custom";

    /// <summary>
    /// Literal for code: PublisherProvided
    /// </summary>
    public const string LiteralPublisherProvided = "publisher-data";

    /// <summary>
    /// Literal for code: ContributorSummarySourcePublisherProvided
    /// </summary>
    public const string LiteralContributorSummarySourcePublisherProvided = "http://terminology.hl7.org/CodeSystem/contributor-summary-source#publisher-data";

    /// <summary>
    /// Dictionary for looking up ContributorSummarySource Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "article-copy", CopiedFromArticle }, 
      { "http://terminology.hl7.org/CodeSystem/contributor-summary-source#article-copy", CopiedFromArticle }, 
      { "citation-manager", ReportedByCitationManager }, 
      { "http://terminology.hl7.org/CodeSystem/contributor-summary-source#citation-manager", ReportedByCitationManager }, 
      { "custom", CustomFormat }, 
      { "http://terminology.hl7.org/CodeSystem/contributor-summary-source#custom", CustomFormat }, 
      { "publisher-data", PublisherProvided }, 
      { "http://terminology.hl7.org/CodeSystem/contributor-summary-source#publisher-data", PublisherProvided }, 
    };
  };
}
