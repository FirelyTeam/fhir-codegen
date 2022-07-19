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
  /// Used for example, to point to a substance, or to a device used to administer a medication.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<CatalogEntryRelatedEntry>))]
  public class CatalogEntryRelatedEntry : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The reference to the related item.
    /// </summary>
    public Reference Item { get; set; }
    /// <summary>
    /// The type of relation to the related item: child, parent, packageContent, containerPackage, usedIn, uses, requires, etc.
    /// </summary>
    public string Relationtype { get; set; }
    /// <summary>
    /// Extension container element for Relationtype
    /// </summary>
    public Element _Relationtype { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4B.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Relationtype))
      {
        writer.WriteString("relationtype", (string)Relationtype!);
      }

      if (_Relationtype != null)
      {
        writer.WritePropertyName("_relationtype");
        _Relationtype.SerializeJson(writer, options);
      }

      if (Item != null)
      {
        writer.WritePropertyName("item");
        Item.SerializeJson(writer, options);
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
        case "item":
          Item = new fhirCsR4B.Models.Reference();
          Item.DeserializeJson(ref reader, options);
          break;

        case "relationtype":
          Relationtype = reader.GetString();
          break;

        case "_relationtype":
          _Relationtype = new fhirCsR4B.Models.Element();
          _Relationtype.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4B.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the CatalogEntry.relatedEntry.relationtype field
  /// </summary>
  public static class CatalogEntryRelatedEntryRelationtypeCodes {
    public const string TRIGGERS = "triggers";
    public const string IS_REPLACED_BY = "is-replaced-by";
    public static HashSet<string> Values = new HashSet<string>() {
      "triggers",
      "is-replaced-by",
    };
  }
  /// <summary>
  /// Catalog entries are wrappers that contextualize items included in a catalog.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<CatalogEntry>))]
  public class CatalogEntry : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "CatalogEntry";
    /// <summary>
    /// Used for examplefor Out of Formulary, or any specifics.
    /// </summary>
    public List<CodeableConcept> AdditionalCharacteristic { get; set; }
    /// <summary>
    /// User for example for ATC classification, or.
    /// </summary>
    public List<CodeableConcept> AdditionalClassification { get; set; }
    /// <summary>
    /// Used in supporting related concepts, e.g. NDC to RxNorm.
    /// </summary>
    public List<Identifier> AdditionalIdentifier { get; set; }
    /// <summary>
    /// Classes of devices, or ATC for medication.
    /// </summary>
    public List<CodeableConcept> Classification { get; set; }
    /// <summary>
    /// Used in supporting different identifiers for the same product, e.g. manufacturer code and retailer code.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Perhaps not needed - if we use fhir resource metadata.
    /// </summary>
    public string LastUpdated { get; set; }
    /// <summary>
    /// Extension container element for LastUpdated
    /// </summary>
    public Element _LastUpdated { get; set; }
    /// <summary>
    /// Whether the entry represents an orderable item.
    /// </summary>
    public bool Orderable { get; set; }
    /// <summary>
    /// Extension container element for Orderable
    /// </summary>
    public Element _Orderable { get; set; }
    /// <summary>
    /// The item in a catalog or definition.
    /// </summary>
    public Reference ReferencedItem { get; set; }
    /// <summary>
    /// Used for example, to point to a substance, or to a device used to administer a medication.
    /// </summary>
    public List<CatalogEntryRelatedEntry> RelatedEntry { get; set; }
    /// <summary>
    /// Used to support catalog exchange even for unsupported products, e.g. getting list of medications even if not prescribable.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The type of item - medication, device, service, protocol or other.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// The time period in which this catalog entry is expected to be active.
    /// </summary>
    public Period ValidityPeriod { get; set; }
    /// <summary>
    /// The date until which this catalog entry is expected to be active.
    /// </summary>
    public string ValidTo { get; set; }
    /// <summary>
    /// Extension container element for ValidTo
    /// </summary>
    public Element _ValidTo { get; set; }
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

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      writer.WriteBoolean("orderable", Orderable);

      if (_Orderable != null)
      {
        writer.WritePropertyName("_orderable");
        _Orderable.SerializeJson(writer, options);
      }

      if (ReferencedItem != null)
      {
        writer.WritePropertyName("referencedItem");
        ReferencedItem.SerializeJson(writer, options);
      }

      if ((AdditionalIdentifier != null) && (AdditionalIdentifier.Count != 0))
      {
        writer.WritePropertyName("additionalIdentifier");
        writer.WriteStartArray();

        foreach (Identifier valAdditionalIdentifier in AdditionalIdentifier)
        {
          valAdditionalIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Classification != null) && (Classification.Count != 0))
      {
        writer.WritePropertyName("classification");
        writer.WriteStartArray();

        foreach (CodeableConcept valClassification in Classification)
        {
          valClassification.SerializeJson(writer, options, true);
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

      if (ValidityPeriod != null)
      {
        writer.WritePropertyName("validityPeriod");
        ValidityPeriod.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ValidTo))
      {
        writer.WriteString("validTo", (string)ValidTo!);
      }

      if (_ValidTo != null)
      {
        writer.WritePropertyName("_validTo");
        _ValidTo.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(LastUpdated))
      {
        writer.WriteString("lastUpdated", (string)LastUpdated!);
      }

      if (_LastUpdated != null)
      {
        writer.WritePropertyName("_lastUpdated");
        _LastUpdated.SerializeJson(writer, options);
      }

      if ((AdditionalCharacteristic != null) && (AdditionalCharacteristic.Count != 0))
      {
        writer.WritePropertyName("additionalCharacteristic");
        writer.WriteStartArray();

        foreach (CodeableConcept valAdditionalCharacteristic in AdditionalCharacteristic)
        {
          valAdditionalCharacteristic.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((AdditionalClassification != null) && (AdditionalClassification.Count != 0))
      {
        writer.WritePropertyName("additionalClassification");
        writer.WriteStartArray();

        foreach (CodeableConcept valAdditionalClassification in AdditionalClassification)
        {
          valAdditionalClassification.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((RelatedEntry != null) && (RelatedEntry.Count != 0))
      {
        writer.WritePropertyName("relatedEntry");
        writer.WriteStartArray();

        foreach (CatalogEntryRelatedEntry valRelatedEntry in RelatedEntry)
        {
          valRelatedEntry.SerializeJson(writer, options, true);
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
        case "additionalCharacteristic":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AdditionalCharacteristic = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.CodeableConcept objAdditionalCharacteristic = new fhirCsR4B.Models.CodeableConcept();
            objAdditionalCharacteristic.DeserializeJson(ref reader, options);
            AdditionalCharacteristic.Add(objAdditionalCharacteristic);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AdditionalCharacteristic.Count == 0)
          {
            AdditionalCharacteristic = null;
          }

          break;

        case "additionalClassification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AdditionalClassification = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.CodeableConcept objAdditionalClassification = new fhirCsR4B.Models.CodeableConcept();
            objAdditionalClassification.DeserializeJson(ref reader, options);
            AdditionalClassification.Add(objAdditionalClassification);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AdditionalClassification.Count == 0)
          {
            AdditionalClassification = null;
          }

          break;

        case "additionalIdentifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AdditionalIdentifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.Identifier objAdditionalIdentifier = new fhirCsR4B.Models.Identifier();
            objAdditionalIdentifier.DeserializeJson(ref reader, options);
            AdditionalIdentifier.Add(objAdditionalIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AdditionalIdentifier.Count == 0)
          {
            AdditionalIdentifier = null;
          }

          break;

        case "classification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Classification = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.CodeableConcept objClassification = new fhirCsR4B.Models.CodeableConcept();
            objClassification.DeserializeJson(ref reader, options);
            Classification.Add(objClassification);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Classification.Count == 0)
          {
            Classification = null;
          }

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

        case "lastUpdated":
          LastUpdated = reader.GetString();
          break;

        case "_lastUpdated":
          _LastUpdated = new fhirCsR4B.Models.Element();
          _LastUpdated.DeserializeJson(ref reader, options);
          break;

        case "orderable":
          Orderable = reader.GetBoolean();
          break;

        case "_orderable":
          _Orderable = new fhirCsR4B.Models.Element();
          _Orderable.DeserializeJson(ref reader, options);
          break;

        case "referencedItem":
          ReferencedItem = new fhirCsR4B.Models.Reference();
          ReferencedItem.DeserializeJson(ref reader, options);
          break;

        case "relatedEntry":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RelatedEntry = new List<CatalogEntryRelatedEntry>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4B.Models.CatalogEntryRelatedEntry objRelatedEntry = new fhirCsR4B.Models.CatalogEntryRelatedEntry();
            objRelatedEntry.DeserializeJson(ref reader, options);
            RelatedEntry.Add(objRelatedEntry);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (RelatedEntry.Count == 0)
          {
            RelatedEntry = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR4B.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4B.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "validityPeriod":
          ValidityPeriod = new fhirCsR4B.Models.Period();
          ValidityPeriod.DeserializeJson(ref reader, options);
          break;

        case "validTo":
          ValidTo = reader.GetString();
          break;

        case "_validTo":
          _ValidTo = new fhirCsR4B.Models.Element();
          _ValidTo.DeserializeJson(ref reader, options);
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
  /// Code Values for the CatalogEntry.status field
  /// </summary>
  public static class CatalogEntryStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string RETIRED = "retired";
    public const string UNKNOWN = "unknown";
    public static HashSet<string> Values = new HashSet<string>() {
      "draft",
      "active",
      "retired",
      "unknown",
    };
  }
}
