// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-ballot
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Xml;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// Distribution of the payment amount for a previously acknowledged payable.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<PaymentReconciliationAllocation>))]
  public class PaymentReconciliationAllocation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The Account to which this payment applies, may be completed by the receiver, used for search.
    /// </summary>
    public Reference Account { get; set; }
    /// <summary>
    /// The monetary amount allocated from the total payment to the payable.
    /// </summary>
    public Money Amount { get; set; }
    /// <summary>
    /// The date from the response resource containing a commitment to pay.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// The Encounter to which this payment applies, may be completed by the receiver, used for search.
    /// </summary>
    public Reference Encounter { get; set; }
    /// <summary>
    /// Unique identifier for the current payment item for the referenced payable.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// The party which is receiving the payment.
    /// </summary>
    public Reference Payee { get; set; }
    /// <summary>
    /// Unique identifier for the prior payment item for the referenced payable.
    /// </summary>
    public Identifier Predecessor { get; set; }
    /// <summary>
    /// A resource, such as a ClaimResponse, which contains a commitment to payment.
    /// </summary>
    public Reference Response { get; set; }
    /// <summary>
    /// A reference to the individual who is responsible for inquiries regarding the response and its payment.
    /// </summary>
    public Reference Responsible { get; set; }
    /// <summary>
    /// The party which submitted the claim or financial transaction.
    /// </summary>
    public Reference Submitter { get; set; }
    /// <summary>
    /// Specific resource to which the payment/adjustment/advance applies.
    /// </summary>
    public Reference Target { get; set; }
    /// <summary>
    ///  Identifies the claim line item, encounter or other sub-element being paid. Note payment may be partial, that is not match the then outstanding balance or amount incurred.
    /// </summary>
    public string TargetItemString { get; set; }
    /// <summary>
    /// Extension container element for TargetItemString
    /// </summary>
    public Element _TargetItemString { get; set; }
    /// <summary>
    ///  Identifies the claim line item, encounter or other sub-element being paid. Note payment may be partial, that is not match the then outstanding balance or amount incurred.
    /// </summary>
    public Identifier TargetItemIdentifier { get; set; }
    /// <summary>
    ///  Identifies the claim line item, encounter or other sub-element being paid. Note payment may be partial, that is not match the then outstanding balance or amount incurred.
    /// </summary>
    public uint? TargetItemPositiveInt { get; set; }
    /// <summary>
    /// For example: payment, adjustment, funds advance, etc.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
      }

      if (Predecessor != null)
      {
        writer.WritePropertyName("predecessor");
        Predecessor.SerializeJson(writer, options);
      }

      if (Target != null)
      {
        writer.WritePropertyName("target");
        Target.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(TargetItemString))
      {
        writer.WriteString("targetItemString", (string)TargetItemString!);
      }

      if (_TargetItemString != null)
      {
        writer.WritePropertyName("_targetItemString");
        _TargetItemString.SerializeJson(writer, options);
      }

      if (TargetItemIdentifier != null)
      {
        writer.WritePropertyName("targetItemIdentifier");
        TargetItemIdentifier.SerializeJson(writer, options);
      }

      if (TargetItemPositiveInt != null)
      {
        writer.WriteNumber("targetItemPositiveInt", (uint)TargetItemPositiveInt!);
      }

      if (Encounter != null)
      {
        writer.WritePropertyName("encounter");
        Encounter.SerializeJson(writer, options);
      }

      if (Account != null)
      {
        writer.WritePropertyName("account");
        Account.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (Submitter != null)
      {
        writer.WritePropertyName("submitter");
        Submitter.SerializeJson(writer, options);
      }

      if (Response != null)
      {
        writer.WritePropertyName("response");
        Response.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (Responsible != null)
      {
        writer.WritePropertyName("responsible");
        Responsible.SerializeJson(writer, options);
      }

      if (Payee != null)
      {
        writer.WritePropertyName("payee");
        Payee.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "account":
          Account = new fhirCsR5.Models.Reference();
          Account.DeserializeJson(ref reader, options);
          break;

        case "amount":
          Amount = new fhirCsR5.Models.Money();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR5.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "encounter":
          Encounter = new fhirCsR5.Models.Reference();
          Encounter.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR5.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "payee":
          Payee = new fhirCsR5.Models.Reference();
          Payee.DeserializeJson(ref reader, options);
          break;

        case "predecessor":
          Predecessor = new fhirCsR5.Models.Identifier();
          Predecessor.DeserializeJson(ref reader, options);
          break;

        case "response":
          Response = new fhirCsR5.Models.Reference();
          Response.DeserializeJson(ref reader, options);
          break;

        case "responsible":
          Responsible = new fhirCsR5.Models.Reference();
          Responsible.DeserializeJson(ref reader, options);
          break;

        case "submitter":
          Submitter = new fhirCsR5.Models.Reference();
          Submitter.DeserializeJson(ref reader, options);
          break;

        case "target":
          Target = new fhirCsR5.Models.Reference();
          Target.DeserializeJson(ref reader, options);
          break;

        case "targetItemString":
          TargetItemString = reader.GetString();
          break;

        case "_targetItemString":
          _TargetItemString = new fhirCsR5.Models.Element();
          _TargetItemString.DeserializeJson(ref reader, options);
          break;

        case "targetItemIdentifier":
          TargetItemIdentifier = new fhirCsR5.Models.Identifier();
          TargetItemIdentifier.DeserializeJson(ref reader, options);
          break;

        case "targetItemPositiveInt":
          TargetItemPositiveInt = reader.GetUInt32();
          break;

        case "type":
          Type = new fhirCsR5.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// A note that describes or explains the processing in a human readable form.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<PaymentReconciliationProcessNote>))]
  public class PaymentReconciliationProcessNote : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The explanation or description associated with the processing.
    /// </summary>
    public string Text { get; set; }
    /// <summary>
    /// Extension container element for Text
    /// </summary>
    public Element _Text { get; set; }
    /// <summary>
    /// The business purpose of the note text.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Text))
      {
        writer.WriteString("text", (string)Text!);
      }

      if (_Text != null)
      {
        writer.WritePropertyName("_text");
        _Text.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "text":
          Text = reader.GetString();
          break;

        case "_text":
          _Text = new fhirCsR5.Models.Element();
          _Text.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR5.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the PaymentReconciliation.processNote.type field
  /// </summary>
  public static class PaymentReconciliationProcessNoteTypeCodes {
    public const string DISPLAY = "display";
    public const string PRINT = "print";
    public const string PRINTOPER = "printoper";
    public static HashSet<string> Values = new HashSet<string>() {
      "display",
      "print",
      "printoper",
    };
  }
  /// <summary>
  /// This resource provides the details including amount of a payment and allocates the payment items being paid.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<PaymentReconciliation>))]
  public class PaymentReconciliation : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "PaymentReconciliation";
    /// <summary>
    /// A portion of the account number, often the last 4 digits, used for verification not charging purposes.
    /// </summary>
    public string AccountNumber { get; set; }
    /// <summary>
    /// Extension container element for AccountNumber
    /// </summary>
    public Element _AccountNumber { get; set; }
    /// <summary>
    /// Distribution of the payment amount for a previously acknowledged payable.
    /// </summary>
    public List<PaymentReconciliationAllocation> Allocation { get; set; }
    /// <summary>
    /// Total payment amount as indicated on the financial instrument.
    /// </summary>
    public Money Amount { get; set; }
    /// <summary>
    /// An alphanumeric issued by the processor to confirm the successful issuance of payment.
    /// </summary>
    public string Authorization { get; set; }
    /// <summary>
    /// Extension container element for Authorization
    /// </summary>
    public Element _Authorization { get; set; }
    /// <summary>
    /// The card brand such as debit, Visa, Amex etc. used if a card is the method of payment.
    /// </summary>
    public string CardBrand { get; set; }
    /// <summary>
    /// Extension container element for CardBrand
    /// </summary>
    public Element _CardBrand { get; set; }
    /// <summary>
    /// The date when the resource was created.
    /// </summary>
    public string Created { get; set; }
    /// <summary>
    /// Extension container element for Created
    /// </summary>
    public Element _Created { get; set; }
    /// <summary>
    /// The date of payment as indicated on the financial instrument.
    /// </summary>
    public string Date { get; set; }
    /// <summary>
    /// Extension container element for Date
    /// </summary>
    public Element _Date { get; set; }
    /// <summary>
    /// A human readable description of the status of the request for the reconciliation.
    /// </summary>
    public string Disposition { get; set; }
    /// <summary>
    /// Extension container element for Disposition
    /// </summary>
    public Element _Disposition { get; set; }
    /// <summary>
    /// Payment enterer if not the actual payment issuer.
    /// </summary>
    public Reference Enterer { get; set; }
    /// <summary>
    /// The year and month (YYYY-MM) when the instrument, typically card, expires.
    /// </summary>
    public string ExpirationDate { get; set; }
    /// <summary>
    /// Extension container element for ExpirationDate
    /// </summary>
    public Element _ExpirationDate { get; set; }
    /// <summary>
    /// May be needed to identify specific jurisdictional forms.
    /// </summary>
    public CodeableConcept FormCode { get; set; }
    /// <summary>
    /// A unique identifier assigned to this payment reconciliation.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The type of the source such as patient or insurance.
    /// </summary>
    public CodeableConcept IssuerType { get; set; }
    /// <summary>
    /// The workflow or activity which gave rise to or during which the payment ocurred such as a kiosk, deposit on account, periodic payment etc.
    /// </summary>
    public CodeableConcept Kind { get; set; }
    /// <summary>
    /// The location of the site or device for electronic transfers or physical location for cash payments.
    /// </summary>
    public Reference Location { get; set; }
    /// <summary>
    /// The means of payment such as check, card cash, or electronic funds transfer.
    /// </summary>
    public CodeableConcept Method { get; set; }
    /// <summary>
    /// The resource may be used to indicate that: the request has been held (queued) for processing; that it has been processed and errors found (error); that no errors were found and that some of the adjudication has been undertaken (partial) or that all of the adjudication has been undertaken (complete).
    /// </summary>
    public string Outcome { get; set; }
    /// <summary>
    /// Extension container element for Outcome
    /// </summary>
    public Element _Outcome { get; set; }
    /// <summary>
    /// For example: EFT number or check number.
    /// </summary>
    public Identifier PaymentIdentifier { get; set; }
    /// <summary>
    /// This party is also responsible for the reconciliation.
    /// </summary>
    public Reference PaymentIssuer { get; set; }
    /// <summary>
    /// The period of time for which payments have been gathered into this bulk payment for settlement.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// A note that describes or explains the processing in a human readable form.
    /// </summary>
    public List<PaymentReconciliationProcessNote> ProcessNote { get; set; }
    /// <summary>
    /// The name of the card processor, etf processor, bank for checks.
    /// </summary>
    public string Processor { get; set; }
    /// <summary>
    /// Extension container element for Processor
    /// </summary>
    public Element _Processor { get; set; }
    /// <summary>
    /// The check number, eft reference, car processor reference.
    /// </summary>
    public string ReferenceNumber { get; set; }
    /// <summary>
    /// Extension container element for ReferenceNumber
    /// </summary>
    public Element _ReferenceNumber { get; set; }
    /// <summary>
    /// Original request resource reference.
    /// </summary>
    public Reference Request { get; set; }
    /// <summary>
    /// The practitioner who is responsible for the services rendered to the patient.
    /// </summary>
    public Reference Requestor { get; set; }
    /// <summary>
    /// The amount returned by the receiver which is excess to the amount payable, often referred to as 'change'.
    /// </summary>
    public Money ReturnedAmount { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The amount offered by the issuer, typically applies to cash when the issuer provides an amount in bank note denominations equal to or excess of the amount actually being paid.
    /// </summary>
    public Money TenderedAmount { get; set; }
    /// <summary>
    /// Code to indicate the nature of the payment such as payment, adjustment.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (Kind != null)
      {
        writer.WritePropertyName("kind");
        Kind.SerializeJson(writer, options);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Created))
      {
        writer.WriteString("created", (string)Created!);
      }

      if (_Created != null)
      {
        writer.WritePropertyName("_created");
        _Created.SerializeJson(writer, options);
      }

      if (Enterer != null)
      {
        writer.WritePropertyName("enterer");
        Enterer.SerializeJson(writer, options);
      }

      if (IssuerType != null)
      {
        writer.WritePropertyName("issuerType");
        IssuerType.SerializeJson(writer, options);
      }

      if (PaymentIssuer != null)
      {
        writer.WritePropertyName("paymentIssuer");
        PaymentIssuer.SerializeJson(writer, options);
      }

      if (Request != null)
      {
        writer.WritePropertyName("request");
        Request.SerializeJson(writer, options);
      }

      if (Requestor != null)
      {
        writer.WritePropertyName("requestor");
        Requestor.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Outcome))
      {
        writer.WriteString("outcome", (string)Outcome!);
      }

      if (_Outcome != null)
      {
        writer.WritePropertyName("_outcome");
        _Outcome.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Disposition))
      {
        writer.WriteString("disposition", (string)Disposition!);
      }

      if (_Disposition != null)
      {
        writer.WritePropertyName("_disposition");
        _Disposition.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Date))
      {
        writer.WriteString("date", (string)Date!);
      }

      if (_Date != null)
      {
        writer.WritePropertyName("_date");
        _Date.SerializeJson(writer, options);
      }

      if (Location != null)
      {
        writer.WritePropertyName("location");
        Location.SerializeJson(writer, options);
      }

      if (Method != null)
      {
        writer.WritePropertyName("method");
        Method.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(CardBrand))
      {
        writer.WriteString("cardBrand", (string)CardBrand!);
      }

      if (_CardBrand != null)
      {
        writer.WritePropertyName("_cardBrand");
        _CardBrand.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AccountNumber))
      {
        writer.WriteString("accountNumber", (string)AccountNumber!);
      }

      if (_AccountNumber != null)
      {
        writer.WritePropertyName("_accountNumber");
        _AccountNumber.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ExpirationDate))
      {
        writer.WriteString("expirationDate", (string)ExpirationDate!);
      }

      if (_ExpirationDate != null)
      {
        writer.WritePropertyName("_expirationDate");
        _ExpirationDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Processor))
      {
        writer.WriteString("processor", (string)Processor!);
      }

      if (_Processor != null)
      {
        writer.WritePropertyName("_processor");
        _Processor.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ReferenceNumber))
      {
        writer.WriteString("referenceNumber", (string)ReferenceNumber!);
      }

      if (_ReferenceNumber != null)
      {
        writer.WritePropertyName("_referenceNumber");
        _ReferenceNumber.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Authorization))
      {
        writer.WriteString("authorization", (string)Authorization!);
      }

      if (_Authorization != null)
      {
        writer.WritePropertyName("_authorization");
        _Authorization.SerializeJson(writer, options);
      }

      if (TenderedAmount != null)
      {
        writer.WritePropertyName("tenderedAmount");
        TenderedAmount.SerializeJson(writer, options);
      }

      if (ReturnedAmount != null)
      {
        writer.WritePropertyName("returnedAmount");
        ReturnedAmount.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
      }

      if (PaymentIdentifier != null)
      {
        writer.WritePropertyName("paymentIdentifier");
        PaymentIdentifier.SerializeJson(writer, options);
      }

      if ((Allocation != null) && (Allocation.Count != 0))
      {
        writer.WritePropertyName("allocation");
        writer.WriteStartArray();

        foreach (PaymentReconciliationAllocation valAllocation in Allocation)
        {
          valAllocation.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (FormCode != null)
      {
        writer.WritePropertyName("formCode");
        FormCode.SerializeJson(writer, options);
      }

      if ((ProcessNote != null) && (ProcessNote.Count != 0))
      {
        writer.WritePropertyName("processNote");
        writer.WriteStartArray();

        foreach (PaymentReconciliationProcessNote valProcessNote in ProcessNote)
        {
          valProcessNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "accountNumber":
          AccountNumber = reader.GetString();
          break;

        case "_accountNumber":
          _AccountNumber = new fhirCsR5.Models.Element();
          _AccountNumber.DeserializeJson(ref reader, options);
          break;

        case "allocation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Allocation = new List<PaymentReconciliationAllocation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.PaymentReconciliationAllocation objAllocation = new fhirCsR5.Models.PaymentReconciliationAllocation();
            objAllocation.DeserializeJson(ref reader, options);
            Allocation.Add(objAllocation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Allocation.Count == 0)
          {
            Allocation = null;
          }

          break;

        case "amount":
          Amount = new fhirCsR5.Models.Money();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "authorization":
          Authorization = reader.GetString();
          break;

        case "_authorization":
          _Authorization = new fhirCsR5.Models.Element();
          _Authorization.DeserializeJson(ref reader, options);
          break;

        case "cardBrand":
          CardBrand = reader.GetString();
          break;

        case "_cardBrand":
          _CardBrand = new fhirCsR5.Models.Element();
          _CardBrand.DeserializeJson(ref reader, options);
          break;

        case "created":
          Created = reader.GetString();
          break;

        case "_created":
          _Created = new fhirCsR5.Models.Element();
          _Created.DeserializeJson(ref reader, options);
          break;

        case "date":
          Date = reader.GetString();
          break;

        case "_date":
          _Date = new fhirCsR5.Models.Element();
          _Date.DeserializeJson(ref reader, options);
          break;

        case "disposition":
          Disposition = reader.GetString();
          break;

        case "_disposition":
          _Disposition = new fhirCsR5.Models.Element();
          _Disposition.DeserializeJson(ref reader, options);
          break;

        case "enterer":
          Enterer = new fhirCsR5.Models.Reference();
          Enterer.DeserializeJson(ref reader, options);
          break;

        case "expirationDate":
          ExpirationDate = reader.GetString();
          break;

        case "_expirationDate":
          _ExpirationDate = new fhirCsR5.Models.Element();
          _ExpirationDate.DeserializeJson(ref reader, options);
          break;

        case "formCode":
          FormCode = new fhirCsR5.Models.CodeableConcept();
          FormCode.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "issuerType":
          IssuerType = new fhirCsR5.Models.CodeableConcept();
          IssuerType.DeserializeJson(ref reader, options);
          break;

        case "kind":
          Kind = new fhirCsR5.Models.CodeableConcept();
          Kind.DeserializeJson(ref reader, options);
          break;

        case "location":
          Location = new fhirCsR5.Models.Reference();
          Location.DeserializeJson(ref reader, options);
          break;

        case "method":
          Method = new fhirCsR5.Models.CodeableConcept();
          Method.DeserializeJson(ref reader, options);
          break;

        case "outcome":
          Outcome = reader.GetString();
          break;

        case "_outcome":
          _Outcome = new fhirCsR5.Models.Element();
          _Outcome.DeserializeJson(ref reader, options);
          break;

        case "paymentIdentifier":
          PaymentIdentifier = new fhirCsR5.Models.Identifier();
          PaymentIdentifier.DeserializeJson(ref reader, options);
          break;

        case "paymentIssuer":
          PaymentIssuer = new fhirCsR5.Models.Reference();
          PaymentIssuer.DeserializeJson(ref reader, options);
          break;

        case "period":
          Period = new fhirCsR5.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "processNote":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ProcessNote = new List<PaymentReconciliationProcessNote>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.PaymentReconciliationProcessNote objProcessNote = new fhirCsR5.Models.PaymentReconciliationProcessNote();
            objProcessNote.DeserializeJson(ref reader, options);
            ProcessNote.Add(objProcessNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ProcessNote.Count == 0)
          {
            ProcessNote = null;
          }

          break;

        case "processor":
          Processor = reader.GetString();
          break;

        case "_processor":
          _Processor = new fhirCsR5.Models.Element();
          _Processor.DeserializeJson(ref reader, options);
          break;

        case "referenceNumber":
          ReferenceNumber = reader.GetString();
          break;

        case "_referenceNumber":
          _ReferenceNumber = new fhirCsR5.Models.Element();
          _ReferenceNumber.DeserializeJson(ref reader, options);
          break;

        case "request":
          Request = new fhirCsR5.Models.Reference();
          Request.DeserializeJson(ref reader, options);
          break;

        case "requestor":
          Requestor = new fhirCsR5.Models.Reference();
          Requestor.DeserializeJson(ref reader, options);
          break;

        case "returnedAmount":
          ReturnedAmount = new fhirCsR5.Models.Money();
          ReturnedAmount.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR5.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "tenderedAmount":
          TenderedAmount = new fhirCsR5.Models.Money();
          TenderedAmount.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR5.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the PaymentReconciliation.outcome field
  /// </summary>
  public static class PaymentReconciliationOutcomeCodes {
    public const string QUEUED = "queued";
    public const string COMPLETE = "complete";
    public const string ERROR = "error";
    public const string PARTIAL = "partial";
    public static HashSet<string> Values = new HashSet<string>() {
      "queued",
      "complete",
      "error",
      "partial",
    };
  }
  /// <summary>
  /// Code Values for the PaymentReconciliation.status field
  /// </summary>
  public static class PaymentReconciliationStatusCodes {
    public const string ACTIVE = "active";
    public const string CANCELLED = "cancelled";
    public const string DRAFT = "draft";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public static HashSet<string> Values = new HashSet<string>() {
      "active",
      "cancelled",
      "draft",
      "entered-in-error",
    };
  }
}
