// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4B.Serialization;

namespace fhirCsR4B.Models
{
  /// <summary>
  /// This resource provides the status of the payment for goods and services rendered, and the request and response resource references.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<PaymentNotice>))]
  public class PaymentNotice : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "PaymentNotice";
    /// <summary>
    /// The amount sent to the payee.
    /// </summary>
    public Money Amount { get; set; }
    /// <summary>
    /// The date when this resource was created.
    /// </summary>
    public string Created { get; set; }
    /// <summary>
    /// Extension container element for Created
    /// </summary>
    public Element _Created { get; set; }
    /// <summary>
    /// A unique identifier assigned to this payment notice.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The party who will receive or has received payment that is the subject of this notification.
    /// </summary>
    public Reference Payee { get; set; }
    /// <summary>
    /// A reference to the payment which is the subject of this notice.
    /// </summary>
    public Reference Payment { get; set; }
    /// <summary>
    /// The date when the above payment action occurred.
    /// </summary>
    public string PaymentDate { get; set; }
    /// <summary>
    /// Extension container element for PaymentDate
    /// </summary>
    public Element _PaymentDate { get; set; }
    /// <summary>
    /// Typically paid: payment sent, cleared: payment received.
    /// </summary>
    public CodeableConcept PaymentStatus { get; set; }
    /// <summary>
    /// The practitioner who is responsible for the services rendered to the patient.
    /// </summary>
    public Reference Provider { get; set; }
    /// <summary>
    /// The party who is notified of the payment status.
    /// </summary>
    public Reference Recipient { get; set; }
    /// <summary>
    /// Reference of resource for which payment is being made.
    /// </summary>
    public Reference Request { get; set; }
    /// <summary>
    /// Reference of response to resource for which payment is being made.
    /// </summary>
    public Reference Response { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
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


      ((fhirCsR4B.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (Request != null)
      {
        writer.WritePropertyName("request");
        Request.SerializeJson(writer, options);
      }

      if (Response != null)
      {
        writer.WritePropertyName("response");
        Response.SerializeJson(writer, options);
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

      if (Provider != null)
      {
        writer.WritePropertyName("provider");
        Provider.SerializeJson(writer, options);
      }

      if (Payment != null)
      {
        writer.WritePropertyName("payment");
        Payment.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(PaymentDate))
      {
        writer.WriteString("paymentDate", (string)PaymentDate!);
      }

      if (_PaymentDate != null)
      {
        writer.WritePropertyName("_paymentDate");
        _PaymentDate.SerializeJson(writer, options);
      }

      if (Payee != null)
      {
        writer.WritePropertyName("payee");
        Payee.SerializeJson(writer, options);
      }

      if (Recipient != null)
      {
        writer.WritePropertyName("recipient");
        Recipient.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
      }

      if (PaymentStatus != null)
      {
        writer.WritePropertyName("paymentStatus");
        PaymentStatus.SerializeJson(writer, options);
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
        case "amount":
          Amount = new fhirCsR4B.Models.Money();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "created":
          Created = reader.GetString();
          break;

        case "_created":
          _Created = new fhirCsR4B.Models.Element();
          _Created.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Identifier objIdentifier = new fhirCsR4B.Models.Identifier();
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

        case "payee":
          Payee = new fhirCsR4B.Models.Reference();
          Payee.DeserializeJson(ref reader, options);
          break;

        case "payment":
          Payment = new fhirCsR4B.Models.Reference();
          Payment.DeserializeJson(ref reader, options);
          break;

        case "paymentDate":
          PaymentDate = reader.GetString();
          break;

        case "_paymentDate":
          _PaymentDate = new fhirCsR4B.Models.Element();
          _PaymentDate.DeserializeJson(ref reader, options);
          break;

        case "paymentStatus":
          PaymentStatus = new fhirCsR4B.Models.CodeableConcept();
          PaymentStatus.DeserializeJson(ref reader, options);
          break;

        case "provider":
          Provider = new fhirCsR4B.Models.Reference();
          Provider.DeserializeJson(ref reader, options);
          break;

        case "recipient":
          Recipient = new fhirCsR4B.Models.Reference();
          Recipient.DeserializeJson(ref reader, options);
          break;

        case "request":
          Request = new fhirCsR4B.Models.Reference();
          Request.DeserializeJson(ref reader, options);
          break;

        case "response":
          Response = new fhirCsR4B.Models.Reference();
          Response.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR4B.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4B.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the PaymentNotice.status field
  /// </summary>
  public static class PaymentNoticeStatusCodes {
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
