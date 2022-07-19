// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// The case or regulatory procedure for granting or amending a marketing authorization. Note: This area is subject to ongoing review and the workgroup is seeking implementer feedback on its use (see link at bottom of page).
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<RegulatedAuthorizationCase>))]
  public class RegulatedAuthorizationCase : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Applications submitted to obtain a marketing authorization. Steps within the longer running case or procedure.
    /// </summary>
    public List<RegulatedAuthorizationCase> Application { get; set; }
    /// <summary>
    /// Relevant date for this of case.
    /// </summary>
    public Period DatePeriod { get; set; }
    /// <summary>
    /// Relevant date for this of case.
    /// </summary>
    public string DateDateTime { get; set; }
    /// <summary>
    /// Extension container element for DateDateTime
    /// </summary>
    public Element _DateDateTime { get; set; }
    /// <summary>
    /// Identifier by which this case can be referenced.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// The status associated with the case.
    /// </summary>
    public CodeableConcept Status { get; set; }
    /// <summary>
    /// The defining type of case.
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

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (Status != null)
      {
        writer.WritePropertyName("status");
        Status.SerializeJson(writer, options);
      }

      if (DatePeriod != null)
      {
        writer.WritePropertyName("datePeriod");
        DatePeriod.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(DateDateTime))
      {
        writer.WriteString("dateDateTime", (string)DateDateTime!);
      }

      if (_DateDateTime != null)
      {
        writer.WritePropertyName("_dateDateTime");
        _DateDateTime.SerializeJson(writer, options);
      }

      if ((Application != null) && (Application.Count != 0))
      {
        writer.WritePropertyName("application");
        writer.WriteStartArray();

        foreach (RegulatedAuthorizationCase valApplication in Application)
        {
          valApplication.SerializeJson(writer, options, true);
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
        case "application":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Application = new List<RegulatedAuthorizationCase>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.RegulatedAuthorizationCase objApplication = new fhirCsR5.Models.RegulatedAuthorizationCase();
            objApplication.DeserializeJson(ref reader, options);
            Application.Add(objApplication);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Application.Count == 0)
          {
            Application = null;
          }

          break;

        case "datePeriod":
          DatePeriod = new fhirCsR5.Models.Period();
          DatePeriod.DeserializeJson(ref reader, options);
          break;

        case "dateDateTime":
          DateDateTime = reader.GetString();
          break;

        case "_dateDateTime":
          _DateDateTime = new fhirCsR5.Models.Element();
          _DateDateTime.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR5.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = new fhirCsR5.Models.CodeableConcept();
          Status.DeserializeJson(ref reader, options);
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
  /// Regulatory approval, clearance or licencing related to a regulated product, treatment, facility or activity that is cited in a guidance, regulation, rule or legislative act. An example is Market Authorization relating to a Medicinal Product.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<RegulatedAuthorization>))]
  public class RegulatedAuthorization : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "RegulatedAuthorization";
    /// <summary>
    /// Additional information or supporting documentation about the authorization.
    /// </summary>
    public List<Reference> AttachedDocument { get; set; }
    /// <summary>
    /// The legal or regulatory framework against which this authorization is granted, or other reasons for it.
    /// </summary>
    public List<CodeableConcept> Basis { get; set; }
    /// <summary>
    /// The case or regulatory procedure for granting or amending a marketing authorization. Note: This area is subject to ongoing review and the workgroup is seeking implementer feedback on its use (see link at bottom of page).
    /// </summary>
    public RegulatedAuthorizationCase Case { get; set; }
    /// <summary>
    /// General textual supporting information.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// The organization that holds the granted authorization.
    /// </summary>
    public Reference Holder { get; set; }
    /// <summary>
    /// Business identifier for the authorization, typically assigned by the authorizing body.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Condition for which the use of the regulated product applies.
    /// </summary>
    public CodeableReference Indication { get; set; }
    /// <summary>
    /// The intended use of the product, e.g. prevention, treatment.
    /// </summary>
    public CodeableConcept IntendedUse { get; set; }
    /// <summary>
    /// The territory (e.g., country, jurisdiction etc.) in which the authorization has been granted.
    /// </summary>
    public List<CodeableConcept> Region { get; set; }
    /// <summary>
    /// The regulatory authority or authorizing body granting the authorization. For example, European Medicines Agency (EMA), Food and Drug Administration (FDA), Health Canada (HC), etc.
    /// </summary>
    public Reference Regulator { get; set; }
    /// <summary>
    /// The status that is authorised e.g. approved. Intermediate states can be tracked with cases and applications.
    /// </summary>
    public CodeableConcept Status { get; set; }
    /// <summary>
    /// The date at which the current status was assigned.
    /// </summary>
    public string StatusDate { get; set; }
    /// <summary>
    /// Extension container element for StatusDate
    /// </summary>
    public Element _StatusDate { get; set; }
    /// <summary>
    /// The product type, treatment, facility or activity that is being authorized.
    /// </summary>
    public List<Reference> Subject { get; set; }
    /// <summary>
    /// Overall type of this authorization, for example drug marketing approval, orphan drug designation.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// The time period in which the regulatory approval, clearance or licencing is in effect. As an example, a Marketing Authorization includes the date of authorization and/or an expiration date.
    /// </summary>
    public Period ValidityPeriod { get; set; }
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

      if ((Subject != null) && (Subject.Count != 0))
      {
        writer.WritePropertyName("subject");
        writer.WriteStartArray();

        foreach (Reference valSubject in Subject)
        {
          valSubject.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if ((Region != null) && (Region.Count != 0))
      {
        writer.WritePropertyName("region");
        writer.WriteStartArray();

        foreach (CodeableConcept valRegion in Region)
        {
          valRegion.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Status != null)
      {
        writer.WritePropertyName("status");
        Status.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(StatusDate))
      {
        writer.WriteString("statusDate", (string)StatusDate!);
      }

      if (_StatusDate != null)
      {
        writer.WritePropertyName("_statusDate");
        _StatusDate.SerializeJson(writer, options);
      }

      if (ValidityPeriod != null)
      {
        writer.WritePropertyName("validityPeriod");
        ValidityPeriod.SerializeJson(writer, options);
      }

      if (Indication != null)
      {
        writer.WritePropertyName("indication");
        Indication.SerializeJson(writer, options);
      }

      if (IntendedUse != null)
      {
        writer.WritePropertyName("intendedUse");
        IntendedUse.SerializeJson(writer, options);
      }

      if ((Basis != null) && (Basis.Count != 0))
      {
        writer.WritePropertyName("basis");
        writer.WriteStartArray();

        foreach (CodeableConcept valBasis in Basis)
        {
          valBasis.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Holder != null)
      {
        writer.WritePropertyName("holder");
        Holder.SerializeJson(writer, options);
      }

      if (Regulator != null)
      {
        writer.WritePropertyName("regulator");
        Regulator.SerializeJson(writer, options);
      }

      if ((AttachedDocument != null) && (AttachedDocument.Count != 0))
      {
        writer.WritePropertyName("attachedDocument");
        writer.WriteStartArray();

        foreach (Reference valAttachedDocument in AttachedDocument)
        {
          valAttachedDocument.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Case != null)
      {
        writer.WritePropertyName("case");
        Case.SerializeJson(writer, options);
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
        case "attachedDocument":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          AttachedDocument = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Reference objAttachedDocument = new fhirCsR5.Models.Reference();
            objAttachedDocument.DeserializeJson(ref reader, options);
            AttachedDocument.Add(objAttachedDocument);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (AttachedDocument.Count == 0)
          {
            AttachedDocument = null;
          }

          break;

        case "basis":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Basis = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objBasis = new fhirCsR5.Models.CodeableConcept();
            objBasis.DeserializeJson(ref reader, options);
            Basis.Add(objBasis);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Basis.Count == 0)
          {
            Basis = null;
          }

          break;

        case "case":
          Case = new fhirCsR5.Models.RegulatedAuthorizationCase();
          Case.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR5.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "holder":
          Holder = new fhirCsR5.Models.Reference();
          Holder.DeserializeJson(ref reader, options);
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

        case "indication":
          Indication = new fhirCsR5.Models.CodeableReference();
          Indication.DeserializeJson(ref reader, options);
          break;

        case "intendedUse":
          IntendedUse = new fhirCsR5.Models.CodeableConcept();
          IntendedUse.DeserializeJson(ref reader, options);
          break;

        case "region":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Region = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objRegion = new fhirCsR5.Models.CodeableConcept();
            objRegion.DeserializeJson(ref reader, options);
            Region.Add(objRegion);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Region.Count == 0)
          {
            Region = null;
          }

          break;

        case "regulator":
          Regulator = new fhirCsR5.Models.Reference();
          Regulator.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = new fhirCsR5.Models.CodeableConcept();
          Status.DeserializeJson(ref reader, options);
          break;

        case "statusDate":
          StatusDate = reader.GetString();
          break;

        case "_statusDate":
          _StatusDate = new fhirCsR5.Models.Element();
          _StatusDate.DeserializeJson(ref reader, options);
          break;

        case "subject":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Subject = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Reference objSubject = new fhirCsR5.Models.Reference();
            objSubject.DeserializeJson(ref reader, options);
            Subject.Add(objSubject);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Subject.Count == 0)
          {
            Subject = null;
          }

          break;

        case "type":
          Type = new fhirCsR5.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "validityPeriod":
          ValidityPeriod = new fhirCsR5.Models.Period();
          ValidityPeriod.DeserializeJson(ref reader, options);
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
}
