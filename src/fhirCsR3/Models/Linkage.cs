// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// Identifies one of the records that is considered to refer to the same real-world occurrence as well as how the items hould be evaluated within the collection of linked items.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<LinkageItem>))]
  public class LinkageItem : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The resource instance being linked as part of the group.
    /// </summary>
    public Reference Resource { get; set; }
    /// <summary>
    /// Distinguishes which item is "source of truth" (if any) and which items are no longer considered to be current representations.
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
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
      }

      if (Resource != null)
      {
        writer.WritePropertyName("resource");
        Resource.SerializeJson(writer, options);
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
        case "resource":
          Resource = new fhirCsR3.Models.Reference();
          Resource.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR3.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the Linkage.item.type field
  /// </summary>
  public static class LinkageItemTypeCodes {
    public const string SOURCE = "source";
    public const string ALTERNATE = "alternate";
    public const string HISTORICAL = "historical";
    public static HashSet<string> Values = new HashSet<string>() {
      "source",
      "alternate",
      "historical",
    };
  }
  /// <summary>
  /// Identifies two or more records (resource instances) that are referring to the same real-world "occurrence".
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<Linkage>))]
  public class Linkage : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Linkage";
    /// <summary>
    /// If false, any asserted linkages should not be considered current/relevant/applicable.
    /// </summary>
    public bool? Active { get; set; }
    /// <summary>
    /// Extension container element for Active
    /// </summary>
    public Element _Active { get; set; }
    /// <summary>
    /// Identifies the user or organization responsible for asserting the linkages and who establishes the context for evaluating the nature of each linkage.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// Identifies one of the records that is considered to refer to the same real-world occurrence as well as how the items hould be evaluated within the collection of linked items.
    /// </summary>
    public List<LinkageItem> Item { get; set; }
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


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (Active != null)
      {
        writer.WriteBoolean("active", (bool)Active!);
      }

      if (_Active != null)
      {
        writer.WritePropertyName("_active");
        _Active.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
      }

      if ((Item != null) && (Item.Count != 0))
      {
        writer.WritePropertyName("item");
        writer.WriteStartArray();

        foreach (LinkageItem valItem in Item)
        {
          valItem.SerializeJson(writer, options, true);
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
        case "active":
          Active = reader.GetBoolean();
          break;

        case "_active":
          _Active = new fhirCsR3.Models.Element();
          _Active.DeserializeJson(ref reader, options);
          break;

        case "author":
          Author = new fhirCsR3.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "item":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Item = new List<LinkageItem>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.LinkageItem objItem = new fhirCsR3.Models.LinkageItem();
            objItem.DeserializeJson(ref reader, options);
            Item.Add(objItem);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Item.Count == 0)
          {
            Item = null;
          }

          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
}
