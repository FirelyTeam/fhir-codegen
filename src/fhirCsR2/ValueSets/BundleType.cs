// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Indicates the purpose of a bundle - how it was intended to be used.
  /// </summary>
  public static class BundleTypeCodes
  {
    /// <summary>
    /// The bundle is a transaction - intended to be processed by a server as a group of actions.
    /// </summary>
    public static readonly Coding Batch = new Coding
    {
      Code = "batch",
      Display = "Batch",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a batch response. Note that as a batch, some responses may indicate failure and others success.
    /// </summary>
    public static readonly Coding BatchResponse = new Coding
    {
      Code = "batch-response",
      Display = "Batch Response",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a set of resources collected into a single document for ease of distribution.
    /// </summary>
    public static readonly Coding Collection = new Coding
    {
      Code = "collection",
      Display = "Collection",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a document. The first resource is a Composition.
    /// </summary>
    public static readonly Coding Document = new Coding
    {
      Code = "document",
      Display = "Document",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a list of resources from a history interaction on a server.
    /// </summary>
    public static readonly Coding HistoryList = new Coding
    {
      Code = "history",
      Display = "History List",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a message. The first resource is a MessageHeader.
    /// </summary>
    public static readonly Coding Message = new Coding
    {
      Code = "message",
      Display = "Message",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a list of resources returned as a result of a search/query interaction, operation, or message.
    /// </summary>
    public static readonly Coding SearchResults = new Coding
    {
      Code = "searchset",
      Display = "Search Results",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a transaction - intended to be processed by a server as an atomic commit.
    /// </summary>
    public static readonly Coding Transaction = new Coding
    {
      Code = "transaction",
      Display = "Transaction",
      System = "http://hl7.org/fhir/bundle-type"
    };
    /// <summary>
    /// The bundle is a transaction response. Because the response is a transaction response, the transactionhas succeeded, and all responses are error free.
    /// </summary>
    public static readonly Coding TransactionResponse = new Coding
    {
      Code = "transaction-response",
      Display = "Transaction Response",
      System = "http://hl7.org/fhir/bundle-type"
    };

    /// <summary>
    /// Literal for code: Batch
    /// </summary>
    public const string LiteralBatch = "batch";

    /// <summary>
    /// Literal for code: BundleTypeBatch
    /// </summary>
    public const string LiteralBundleTypeBatch = "http://hl7.org/fhir/bundle-type#batch";

    /// <summary>
    /// Literal for code: BatchResponse
    /// </summary>
    public const string LiteralBatchResponse = "batch-response";

    /// <summary>
    /// Literal for code: BundleTypeBatchResponse
    /// </summary>
    public const string LiteralBundleTypeBatchResponse = "http://hl7.org/fhir/bundle-type#batch-response";

    /// <summary>
    /// Literal for code: Collection
    /// </summary>
    public const string LiteralCollection = "collection";

    /// <summary>
    /// Literal for code: BundleTypeCollection
    /// </summary>
    public const string LiteralBundleTypeCollection = "http://hl7.org/fhir/bundle-type#collection";

    /// <summary>
    /// Literal for code: Document
    /// </summary>
    public const string LiteralDocument = "document";

    /// <summary>
    /// Literal for code: BundleTypeDocument
    /// </summary>
    public const string LiteralBundleTypeDocument = "http://hl7.org/fhir/bundle-type#document";

    /// <summary>
    /// Literal for code: HistoryList
    /// </summary>
    public const string LiteralHistoryList = "history";

    /// <summary>
    /// Literal for code: BundleTypeHistoryList
    /// </summary>
    public const string LiteralBundleTypeHistoryList = "http://hl7.org/fhir/bundle-type#history";

    /// <summary>
    /// Literal for code: Message
    /// </summary>
    public const string LiteralMessage = "message";

    /// <summary>
    /// Literal for code: BundleTypeMessage
    /// </summary>
    public const string LiteralBundleTypeMessage = "http://hl7.org/fhir/bundle-type#message";

    /// <summary>
    /// Literal for code: SearchResults
    /// </summary>
    public const string LiteralSearchResults = "searchset";

    /// <summary>
    /// Literal for code: BundleTypeSearchResults
    /// </summary>
    public const string LiteralBundleTypeSearchResults = "http://hl7.org/fhir/bundle-type#searchset";

    /// <summary>
    /// Literal for code: Transaction
    /// </summary>
    public const string LiteralTransaction = "transaction";

    /// <summary>
    /// Literal for code: BundleTypeTransaction
    /// </summary>
    public const string LiteralBundleTypeTransaction = "http://hl7.org/fhir/bundle-type#transaction";

    /// <summary>
    /// Literal for code: TransactionResponse
    /// </summary>
    public const string LiteralTransactionResponse = "transaction-response";

    /// <summary>
    /// Literal for code: BundleTypeTransactionResponse
    /// </summary>
    public const string LiteralBundleTypeTransactionResponse = "http://hl7.org/fhir/bundle-type#transaction-response";

    /// <summary>
    /// Dictionary for looking up BundleType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "batch", Batch }, 
      { "http://hl7.org/fhir/bundle-type#batch", Batch }, 
      { "batch-response", BatchResponse }, 
      { "http://hl7.org/fhir/bundle-type#batch-response", BatchResponse }, 
      { "collection", Collection }, 
      { "http://hl7.org/fhir/bundle-type#collection", Collection }, 
      { "document", Document }, 
      { "http://hl7.org/fhir/bundle-type#document", Document }, 
      { "history", HistoryList }, 
      { "http://hl7.org/fhir/bundle-type#history", HistoryList }, 
      { "message", Message }, 
      { "http://hl7.org/fhir/bundle-type#message", Message }, 
      { "searchset", SearchResults }, 
      { "http://hl7.org/fhir/bundle-type#searchset", SearchResults }, 
      { "transaction", Transaction }, 
      { "http://hl7.org/fhir/bundle-type#transaction", Transaction }, 
      { "transaction-response", TransactionResponse }, 
      { "http://hl7.org/fhir/bundle-type#transaction-response", TransactionResponse }, 
    };
  };
}
