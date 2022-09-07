// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// A code that indicates how transactions are supported.
  /// </summary>
  public static class TransactionModeCodes
  {
    /// <summary>
    /// Batches are  supported.
    /// </summary>
    public static readonly Coding BatchesSupported = new Coding
    {
      Code = "batch",
      Display = "Batches supported",
      System = "http://hl7.org/fhir/transaction-mode"
    };
    /// <summary>
    /// Both batches and transactions are supported.
    /// </summary>
    public static readonly Coding BatchesAndTransactions = new Coding
    {
      Code = "both",
      Display = "Batches & Transactions",
      System = "http://hl7.org/fhir/transaction-mode"
    };
    /// <summary>
    /// Neither batch or transaction is supported.
    /// </summary>
    public static readonly Coding None = new Coding
    {
      Code = "not-supported",
      Display = "None",
      System = "http://hl7.org/fhir/transaction-mode"
    };
    /// <summary>
    /// Transactions are supported.
    /// </summary>
    public static readonly Coding TransactionsSupported = new Coding
    {
      Code = "transaction",
      Display = "Transactions Supported",
      System = "http://hl7.org/fhir/transaction-mode"
    };

    /// <summary>
    /// Literal for code: BatchesSupported
    /// </summary>
    public const string LiteralBatchesSupported = "batch";

    /// <summary>
    /// Literal for code: TransactionModeBatchesSupported
    /// </summary>
    public const string LiteralTransactionModeBatchesSupported = "http://hl7.org/fhir/transaction-mode#batch";

    /// <summary>
    /// Literal for code: BatchesAndTransactions
    /// </summary>
    public const string LiteralBatchesAndTransactions = "both";

    /// <summary>
    /// Literal for code: TransactionModeBatchesAndTransactions
    /// </summary>
    public const string LiteralTransactionModeBatchesAndTransactions = "http://hl7.org/fhir/transaction-mode#both";

    /// <summary>
    /// Literal for code: None
    /// </summary>
    public const string LiteralNone = "not-supported";

    /// <summary>
    /// Literal for code: TransactionModeNone
    /// </summary>
    public const string LiteralTransactionModeNone = "http://hl7.org/fhir/transaction-mode#not-supported";

    /// <summary>
    /// Literal for code: TransactionsSupported
    /// </summary>
    public const string LiteralTransactionsSupported = "transaction";

    /// <summary>
    /// Literal for code: TransactionModeTransactionsSupported
    /// </summary>
    public const string LiteralTransactionModeTransactionsSupported = "http://hl7.org/fhir/transaction-mode#transaction";

    /// <summary>
    /// Dictionary for looking up TransactionMode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "batch", BatchesSupported }, 
      { "http://hl7.org/fhir/transaction-mode#batch", BatchesSupported }, 
      { "both", BatchesAndTransactions }, 
      { "http://hl7.org/fhir/transaction-mode#both", BatchesAndTransactions }, 
      { "not-supported", None }, 
      { "http://hl7.org/fhir/transaction-mode#not-supported", None }, 
      { "transaction", TransactionsSupported }, 
      { "http://hl7.org/fhir/transaction-mode#transaction", TransactionsSupported }, 
    };
  };
}