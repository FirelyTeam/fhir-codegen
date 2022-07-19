// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  ///  Type of invoice element that is used to assist in describing an Invoice that is either submitted for adjudication or for which is returned on adjudication results. Invoice elements of this type signify a grouping of one or more children (detail) invoice elements.  They do not have intrinsic costing associated with them, but merely reflect the sum of all costing for it's immediate children invoice elements.
  /// </summary>
  public static class V3ActInvoiceGroupCodeCodes
  {
    /// <summary>
    /// Type of invoice element that is used to assist in describing an Invoice that is either submitted for adjudication or for which is returned on adjudication results.
    /// 
    ///                         Invoice elements of this type signify a grouping of one or more children (detail) invoice elements.  They do not have intrinsic costing associated with them, but merely reflect the sum of all costing for it's immediate children invoice elements.
    /// 
    ///                         The domain is only specified for an intermediate invoice element group (non-root or non-top level) for an Invoice.
    /// </summary>
    public static readonly Coding ActInvoiceInterGroupCode = new Coding
    {
      Code = "_ActInvoiceInterGroupCode",
      Display = "ActInvoiceInterGroupCode",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Type of invoice element that is used to assist in describing an Invoice that is either submitted for adjudication or for which is returned on adjudication results.
    /// 
    ///                         Invoice elements of this type signify a grouping of one or more children (detail) invoice elements.  They do not have intrinsic costing associated with them, but merely reflect the sum of all costing for it's immediate children invoice elements.
    /// 
    ///                         Codes from this domain reflect the type of Invoice such as Pharmacy Dispense, Clinical Service and Clinical Product.  The domain is only specified for the root (top level) invoice element group for an Invoice.
    /// </summary>
    public static readonly Coding ActInvoiceRootGroupCode = new Coding
    {
      Code = "_ActInvoiceRootGroupCode",
      Display = "ActInvoiceRootGroupCode",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Clinical product invoice where the Invoice Grouping contains one or more billable item and is supported by clinical product(s).
    /// 
    ///                         For example, a crutch or a wheelchair.
    /// </summary>
    public static readonly Coding ClinicalProductInvoice = new Coding
    {
      Code = "CPINV",
      Display = "clinical product invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element groups and details including the ones specifying the compound ingredients being invoiced. It may also contain generic detail items such as markup.
    /// </summary>
    public static readonly Coding CompoundDrugInvoiceGroup = new Coding
    {
      Code = "CPNDDRGING",
      Display = "compound drug invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element details including the one specifying an ingredient drug being invoiced. It may also contain generic detail items such as tax or markup.
    /// </summary>
    public static readonly Coding CompoundIngredientInvoiceGroup = new Coding
    {
      Code = "CPNDINDING",
      Display = "compound ingredient invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element groups and details including the ones specifying the compound supplies being invoiced. It may also contain generic detail items such as markup.
    /// </summary>
    public static readonly Coding CompoundSupplyInvoiceGroup = new Coding
    {
      Code = "CPNDSUPING",
      Display = "compound supply invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Clinical Services Invoice which can be used to describe a single service, multiple services or repeated services.
    /// 
    ///                         [1] Single Clinical services invoice where the Invoice Grouping contains one billable item and is supported by one clinical service.
    /// 
    ///                         For example, a single service for an office visit or simple clinical procedure (e.g. knee mobilization).
    /// 
    ///                         [2] Multiple Clinical services invoice where the Invoice Grouping contains more than one billable item, supported by one or more clinical services.  The services can be distinct and over multiple dates, but for the same patient. This type of invoice includes a series of treatments which must be adjudicated together.
    /// 
    ///                         For example, an adjustment and ultrasound for a chiropractic session where fees are associated for each of the services and adjudicated (invoiced) together.
    /// 
    ///                         [3] Repeated Clinical services invoice where the Invoice Grouping contains one or more billable item, supported by the same clinical service repeated over a period of time.
    /// 
    ///                         For example, the same Chiropractic adjustment (service or treatment) delivered on 3 separate occasions over a period of time at the discretion of the provider (e.g. month).
    /// </summary>
    public static readonly Coding ClinicalServiceInvoice = new Coding
    {
      Code = "CSINV",
      Display = "clinical service invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A clinical Invoice Grouping consisting of one or more services and one or more product.  Billing for these service(s) and product(s) are supported by multiple clinical billable events (acts).
    /// 
    ///                         All items in the Invoice Grouping must be adjudicated together to be acceptable to the Adjudicator.
    /// 
    ///                         For example , a brace (product) invoiced together with the fitting (service).
    /// </summary>
    public static readonly Coding ClinicalServiceAndProduct = new Coding
    {
      Code = "CSPINV",
      Display = "clinical service and product",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element details including the one specifying the drug being invoiced. It may also contain generic detail items such as markup.
    /// </summary>
    public static readonly Coding DrugInvoiceGroup = new Coding
    {
      Code = "DRUGING",
      Display = "drug invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Invoice Grouping without clinical justification.  These will not require identification of participants and associations from a clinical context such as patient and provider.
    /// 
    ///                         Examples are interest charges and mileage.
    /// </summary>
    public static readonly Coding FinancialInvoice = new Coding
    {
      Code = "FININV",
      Display = "financial invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element details including the ones specifying the frame fee and the frame dispensing cost that are being invoiced.
    /// </summary>
    public static readonly Coding FrameInvoiceGroup = new Coding
    {
      Code = "FRAMEING",
      Display = "frame invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element details including the ones specifying the lens fee and the lens dispensing cost that are being invoiced.
    /// </summary>
    public static readonly Coding LensInvoiceGroup = new Coding
    {
      Code = "LENSING",
      Display = "lens invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A clinical Invoice Grouping consisting of one or more oral health services. Billing for these service(s) are supported by multiple clinical billable events (acts).
    /// 
    ///                         All items in the Invoice Grouping must be adjudicated together to be acceptable to the Adjudicator.
    /// </summary>
    public static readonly Coding OralHealthService = new Coding
    {
      Code = "OHSINV",
      Display = "oral health service",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// HealthCare facility preferred accommodation invoice.
    /// </summary>
    public static readonly Coding PreferredAccommodationInvoice = new Coding
    {
      Code = "PAINV",
      Display = "preferred accommodation invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// A grouping of invoice element details including the one specifying the product (good or supply) being invoiced. It may also contain generic detail items such as tax or discount.
    /// </summary>
    public static readonly Coding ProductInvoiceGroup = new Coding
    {
      Code = "PRDING",
      Display = "product invoice group",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Pharmacy dispense invoice for a compound.
    /// </summary>
    public static readonly Coding RxCompoundInvoice = new Coding
    {
      Code = "RXCINV",
      Display = "Rx compound invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Pharmacy dispense invoice not involving a compound
    /// </summary>
    public static readonly Coding RxDispenseInvoice = new Coding
    {
      Code = "RXDINV",
      Display = "Rx dispense invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Clinical services invoice where the Invoice Group contains one billable item for multiple clinical services in one or more sessions.
    /// </summary>
    public static readonly Coding SessionalOrBlockFeeInvoice = new Coding
    {
      Code = "SBFINV",
      Display = "sessional or block fee invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };
    /// <summary>
    /// Vision dispense invoice for up to 2 lens (left and right), frame and optional discount.  Eye exams are invoiced as a clinical service invoice.
    /// </summary>
    public static readonly Coding VisionDispenseInvoice = new Coding
    {
      Code = "VRXINV",
      Display = "vision dispense invoice",
      System = "http://hl7.org/fhir/v3/ActCode"
    };

    /// <summary>
    /// Literal for code: ActInvoiceInterGroupCode
    /// </summary>
    public const string LiteralActInvoiceInterGroupCode = "_ActInvoiceInterGroupCode";

    /// <summary>
    /// Literal for code: V3ActCodeActInvoiceInterGroupCode
    /// </summary>
    public const string LiteralV3ActCodeActInvoiceInterGroupCode = "http://hl7.org/fhir/v3/ActCode#_ActInvoiceInterGroupCode";

    /// <summary>
    /// Literal for code: ActInvoiceRootGroupCode
    /// </summary>
    public const string LiteralActInvoiceRootGroupCode = "_ActInvoiceRootGroupCode";

    /// <summary>
    /// Literal for code: V3ActCodeActInvoiceRootGroupCode
    /// </summary>
    public const string LiteralV3ActCodeActInvoiceRootGroupCode = "http://hl7.org/fhir/v3/ActCode#_ActInvoiceRootGroupCode";

    /// <summary>
    /// Literal for code: ClinicalProductInvoice
    /// </summary>
    public const string LiteralClinicalProductInvoice = "CPINV";

    /// <summary>
    /// Literal for code: V3ActCodeClinicalProductInvoice
    /// </summary>
    public const string LiteralV3ActCodeClinicalProductInvoice = "http://hl7.org/fhir/v3/ActCode#CPINV";

    /// <summary>
    /// Literal for code: CompoundDrugInvoiceGroup
    /// </summary>
    public const string LiteralCompoundDrugInvoiceGroup = "CPNDDRGING";

    /// <summary>
    /// Literal for code: V3ActCodeCompoundDrugInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeCompoundDrugInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#CPNDDRGING";

    /// <summary>
    /// Literal for code: CompoundIngredientInvoiceGroup
    /// </summary>
    public const string LiteralCompoundIngredientInvoiceGroup = "CPNDINDING";

    /// <summary>
    /// Literal for code: V3ActCodeCompoundIngredientInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeCompoundIngredientInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#CPNDINDING";

    /// <summary>
    /// Literal for code: CompoundSupplyInvoiceGroup
    /// </summary>
    public const string LiteralCompoundSupplyInvoiceGroup = "CPNDSUPING";

    /// <summary>
    /// Literal for code: V3ActCodeCompoundSupplyInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeCompoundSupplyInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#CPNDSUPING";

    /// <summary>
    /// Literal for code: ClinicalServiceInvoice
    /// </summary>
    public const string LiteralClinicalServiceInvoice = "CSINV";

    /// <summary>
    /// Literal for code: V3ActCodeClinicalServiceInvoice
    /// </summary>
    public const string LiteralV3ActCodeClinicalServiceInvoice = "http://hl7.org/fhir/v3/ActCode#CSINV";

    /// <summary>
    /// Literal for code: ClinicalServiceAndProduct
    /// </summary>
    public const string LiteralClinicalServiceAndProduct = "CSPINV";

    /// <summary>
    /// Literal for code: V3ActCodeClinicalServiceAndProduct
    /// </summary>
    public const string LiteralV3ActCodeClinicalServiceAndProduct = "http://hl7.org/fhir/v3/ActCode#CSPINV";

    /// <summary>
    /// Literal for code: DrugInvoiceGroup
    /// </summary>
    public const string LiteralDrugInvoiceGroup = "DRUGING";

    /// <summary>
    /// Literal for code: V3ActCodeDrugInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeDrugInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#DRUGING";

    /// <summary>
    /// Literal for code: FinancialInvoice
    /// </summary>
    public const string LiteralFinancialInvoice = "FININV";

    /// <summary>
    /// Literal for code: V3ActCodeFinancialInvoice
    /// </summary>
    public const string LiteralV3ActCodeFinancialInvoice = "http://hl7.org/fhir/v3/ActCode#FININV";

    /// <summary>
    /// Literal for code: FrameInvoiceGroup
    /// </summary>
    public const string LiteralFrameInvoiceGroup = "FRAMEING";

    /// <summary>
    /// Literal for code: V3ActCodeFrameInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeFrameInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#FRAMEING";

    /// <summary>
    /// Literal for code: LensInvoiceGroup
    /// </summary>
    public const string LiteralLensInvoiceGroup = "LENSING";

    /// <summary>
    /// Literal for code: V3ActCodeLensInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeLensInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#LENSING";

    /// <summary>
    /// Literal for code: OralHealthService
    /// </summary>
    public const string LiteralOralHealthService = "OHSINV";

    /// <summary>
    /// Literal for code: V3ActCodeOralHealthService
    /// </summary>
    public const string LiteralV3ActCodeOralHealthService = "http://hl7.org/fhir/v3/ActCode#OHSINV";

    /// <summary>
    /// Literal for code: PreferredAccommodationInvoice
    /// </summary>
    public const string LiteralPreferredAccommodationInvoice = "PAINV";

    /// <summary>
    /// Literal for code: V3ActCodePreferredAccommodationInvoice
    /// </summary>
    public const string LiteralV3ActCodePreferredAccommodationInvoice = "http://hl7.org/fhir/v3/ActCode#PAINV";

    /// <summary>
    /// Literal for code: ProductInvoiceGroup
    /// </summary>
    public const string LiteralProductInvoiceGroup = "PRDING";

    /// <summary>
    /// Literal for code: V3ActCodeProductInvoiceGroup
    /// </summary>
    public const string LiteralV3ActCodeProductInvoiceGroup = "http://hl7.org/fhir/v3/ActCode#PRDING";

    /// <summary>
    /// Literal for code: RxCompoundInvoice
    /// </summary>
    public const string LiteralRxCompoundInvoice = "RXCINV";

    /// <summary>
    /// Literal for code: V3ActCodeRxCompoundInvoice
    /// </summary>
    public const string LiteralV3ActCodeRxCompoundInvoice = "http://hl7.org/fhir/v3/ActCode#RXCINV";

    /// <summary>
    /// Literal for code: RxDispenseInvoice
    /// </summary>
    public const string LiteralRxDispenseInvoice = "RXDINV";

    /// <summary>
    /// Literal for code: V3ActCodeRxDispenseInvoice
    /// </summary>
    public const string LiteralV3ActCodeRxDispenseInvoice = "http://hl7.org/fhir/v3/ActCode#RXDINV";

    /// <summary>
    /// Literal for code: SessionalOrBlockFeeInvoice
    /// </summary>
    public const string LiteralSessionalOrBlockFeeInvoice = "SBFINV";

    /// <summary>
    /// Literal for code: V3ActCodeSessionalOrBlockFeeInvoice
    /// </summary>
    public const string LiteralV3ActCodeSessionalOrBlockFeeInvoice = "http://hl7.org/fhir/v3/ActCode#SBFINV";

    /// <summary>
    /// Literal for code: VisionDispenseInvoice
    /// </summary>
    public const string LiteralVisionDispenseInvoice = "VRXINV";

    /// <summary>
    /// Literal for code: V3ActCodeVisionDispenseInvoice
    /// </summary>
    public const string LiteralV3ActCodeVisionDispenseInvoice = "http://hl7.org/fhir/v3/ActCode#VRXINV";

    /// <summary>
    /// Dictionary for looking up V3ActInvoiceGroupCode Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "_ActInvoiceInterGroupCode", ActInvoiceInterGroupCode }, 
      { "http://hl7.org/fhir/v3/ActCode#_ActInvoiceInterGroupCode", ActInvoiceInterGroupCode }, 
      { "_ActInvoiceRootGroupCode", ActInvoiceRootGroupCode }, 
      { "http://hl7.org/fhir/v3/ActCode#_ActInvoiceRootGroupCode", ActInvoiceRootGroupCode }, 
      { "CPINV", ClinicalProductInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#CPINV", ClinicalProductInvoice }, 
      { "CPNDDRGING", CompoundDrugInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#CPNDDRGING", CompoundDrugInvoiceGroup }, 
      { "CPNDINDING", CompoundIngredientInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#CPNDINDING", CompoundIngredientInvoiceGroup }, 
      { "CPNDSUPING", CompoundSupplyInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#CPNDSUPING", CompoundSupplyInvoiceGroup }, 
      { "CSINV", ClinicalServiceInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#CSINV", ClinicalServiceInvoice }, 
      { "CSPINV", ClinicalServiceAndProduct }, 
      { "http://hl7.org/fhir/v3/ActCode#CSPINV", ClinicalServiceAndProduct }, 
      { "DRUGING", DrugInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#DRUGING", DrugInvoiceGroup }, 
      { "FININV", FinancialInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#FININV", FinancialInvoice }, 
      { "FRAMEING", FrameInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#FRAMEING", FrameInvoiceGroup }, 
      { "LENSING", LensInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#LENSING", LensInvoiceGroup }, 
      { "OHSINV", OralHealthService }, 
      { "http://hl7.org/fhir/v3/ActCode#OHSINV", OralHealthService }, 
      { "PAINV", PreferredAccommodationInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#PAINV", PreferredAccommodationInvoice }, 
      { "PRDING", ProductInvoiceGroup }, 
      { "http://hl7.org/fhir/v3/ActCode#PRDING", ProductInvoiceGroup }, 
      { "RXCINV", RxCompoundInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#RXCINV", RxCompoundInvoice }, 
      { "RXDINV", RxDispenseInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#RXDINV", RxDispenseInvoice }, 
      { "SBFINV", SessionalOrBlockFeeInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#SBFINV", SessionalOrBlockFeeInvoice }, 
      { "VRXINV", VisionDispenseInvoice }, 
      { "http://hl7.org/fhir/v3/ActCode#VRXINV", VisionDispenseInvoice }, 
    };
  };
}
