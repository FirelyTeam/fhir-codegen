// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Codes indicating the kind of the price component.
  /// </summary>
  public static class InvoicePriceComponentTypeCodes
  {
    /// <summary>
    /// the amount is the base price used for calculating the total price before applying surcharges, discount or taxes.
    /// </summary>
    public static readonly Coding BasePrice = new Coding
    {
      Code = "base",
      Display = "base price",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };
    /// <summary>
    /// the amount is a deduction applied on the base price.
    /// </summary>
    public static readonly Coding Deduction = new Coding
    {
      Code = "deduction",
      Display = "deduction",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };
    /// <summary>
    /// the amount is a discount applied on the base price.
    /// </summary>
    public static readonly Coding Discount = new Coding
    {
      Code = "discount",
      Display = "discount",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };
    /// <summary>
    /// the amount is of informational character, it has not been applied in the calculation of the total price.
    /// </summary>
    public static readonly Coding Informational = new Coding
    {
      Code = "informational",
      Display = "informational",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };
    /// <summary>
    /// the amount is a surcharge applied on the base price.
    /// </summary>
    public static readonly Coding Surcharge = new Coding
    {
      Code = "surcharge",
      Display = "surcharge",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };
    /// <summary>
    /// the amount is the tax component of the total price.
    /// </summary>
    public static readonly Coding Tax = new Coding
    {
      Code = "tax",
      Display = "tax",
      System = "http://hl7.org/fhir/invoice-priceComponentType"
    };

    /// <summary>
    /// Literal for code: BasePrice
    /// </summary>
    public const string LiteralBasePrice = "base";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeBasePrice
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeBasePrice = "http://hl7.org/fhir/invoice-priceComponentType#base";

    /// <summary>
    /// Literal for code: Deduction
    /// </summary>
    public const string LiteralDeduction = "deduction";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeDeduction
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeDeduction = "http://hl7.org/fhir/invoice-priceComponentType#deduction";

    /// <summary>
    /// Literal for code: Discount
    /// </summary>
    public const string LiteralDiscount = "discount";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeDiscount
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeDiscount = "http://hl7.org/fhir/invoice-priceComponentType#discount";

    /// <summary>
    /// Literal for code: Informational
    /// </summary>
    public const string LiteralInformational = "informational";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeInformational
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeInformational = "http://hl7.org/fhir/invoice-priceComponentType#informational";

    /// <summary>
    /// Literal for code: Surcharge
    /// </summary>
    public const string LiteralSurcharge = "surcharge";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeSurcharge
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeSurcharge = "http://hl7.org/fhir/invoice-priceComponentType#surcharge";

    /// <summary>
    /// Literal for code: Tax
    /// </summary>
    public const string LiteralTax = "tax";

    /// <summary>
    /// Literal for code: InvoicePriceComponentTypeTax
    /// </summary>
    public const string LiteralInvoicePriceComponentTypeTax = "http://hl7.org/fhir/invoice-priceComponentType#tax";

    /// <summary>
    /// Dictionary for looking up InvoicePriceComponentType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "base", BasePrice }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#base", BasePrice }, 
      { "deduction", Deduction }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#deduction", Deduction }, 
      { "discount", Discount }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#discount", Discount }, 
      { "informational", Informational }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#informational", Informational }, 
      { "surcharge", Surcharge }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#surcharge", Surcharge }, 
      { "tax", Tax }, 
      { "http://hl7.org/fhir/invoice-priceComponentType#tax", Tax }, 
    };
  };
}
