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
  /// Describes an expected sequence of events for one of the participants of a study.  E.g. Exposure to drug A, wash-out, exposure to drug B, wash-out, follow-up.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ResearchStudyArm>))]
  public class ResearchStudyArm : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Categorization of study arm, e.g. experimental, active comparator, placebo comparater.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// A succinct description of the path through the study that would be followed by a subject adhering to this arm.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Unique, human-readable label for this arm of the study.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
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

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
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
        case "code":
          Code = new fhirCsR3.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR3.Models.Element();
          _Name.DeserializeJson(ref reader, options);
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
  /// A process where a researcher or organization plans and then executes a series of steps intended to increase the field of healthcare-related knowledge.  This includes studies of safety, efficacy, comparative effectiveness and other information about medications, devices, therapies and other interventional and investigative techniques.  A ResearchStudy involves the gathering of information about human or animal subjects.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ResearchStudy>))]
  public class ResearchStudy : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ResearchStudy";
    /// <summary>
    /// Describes an expected sequence of events for one of the participants of a study.  E.g. Exposure to drug A, wash-out, exposure to drug B, wash-out, follow-up.
    /// </summary>
    public List<ResearchStudyArm> Arm { get; set; }
    /// <summary>
    /// Codes categorizing the type of study such as investigational vs. observational, type of blinding, type of randomization, safety vs. efficacy, etc.
    /// </summary>
    public List<CodeableConcept> Category { get; set; }
    /// <summary>
    /// Contact details to assist a user in learning more about or engaging with the study.
    /// </summary>
    public List<ContactDetail> Contact { get; set; }
    /// <summary>
    /// A full description of how the study is being conducted.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// The Group referenced should not generally enumerate specific subjects.  Subjects will be linked to the study using the ResearchSubject resource.
    /// </summary>
    public List<Reference> Enrollment { get; set; }
    /// <summary>
    /// The condition(s), medication(s), food(s), therapy(ies), device(s) or other concerns or interventions that the study is seeking to gain more information about.
    /// </summary>
    public List<CodeableConcept> Focus { get; set; }
    /// <summary>
    /// Identifiers assigned to this research study by the sponsor or other systems.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Indicates a country, state or other region where the study is taking place.
    /// </summary>
    public List<CodeableConcept> Jurisdiction { get; set; }
    /// <summary>
    /// Key terms to aid in searching for or filtering the study.
    /// </summary>
    public List<CodeableConcept> Keyword { get; set; }
    /// <summary>
    /// Comments made about the event by the performer, subject or other participants.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// A larger research study of which this particular study is a component or step.
    /// </summary>
    public List<Reference> PartOf { get; set; }
    /// <summary>
    /// Identifies the start date and the expected (or actual, depending on status) end date for the study.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Indicates the individual who has primary oversite of the execution of the study.
    /// </summary>
    public Reference PrincipalInvestigator { get; set; }
    /// <summary>
    /// The set of steps expected to be performed as part of the execution of the study.
    /// </summary>
    public List<Reference> Protocol { get; set; }
    /// <summary>
    /// A description and/or code explaining the premature termination of the study.
    /// </summary>
    public CodeableConcept ReasonStopped { get; set; }
    /// <summary>
    /// Citations, references and other related documents.
    /// </summary>
    public List<RelatedArtifact> RelatedArtifact { get; set; }
    /// <summary>
    /// Clinic, hospital or other healthcare location that is participating in the study.
    /// </summary>
    public List<Reference> Site { get; set; }
    /// <summary>
    /// The organization responsible for the execution of the study.
    /// </summary>
    public Reference Sponsor { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// A short, descriptive user-friendly label for the study.
    /// </summary>
    public string Title { get; set; }
    /// <summary>
    /// Extension container element for Title
    /// </summary>
    public Element _Title { get; set; }
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

      if (!string.IsNullOrEmpty(Title))
      {
        writer.WriteString("title", (string)Title!);
      }

      if (_Title != null)
      {
        writer.WritePropertyName("_title");
        _Title.SerializeJson(writer, options);
      }

      if ((Protocol != null) && (Protocol.Count != 0))
      {
        writer.WritePropertyName("protocol");
        writer.WriteStartArray();

        foreach (Reference valProtocol in Protocol)
        {
          valProtocol.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((PartOf != null) && (PartOf.Count != 0))
      {
        writer.WritePropertyName("partOf");
        writer.WriteStartArray();

        foreach (Reference valPartOf in PartOf)
        {
          valPartOf.SerializeJson(writer, options, true);
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

      if ((Category != null) && (Category.Count != 0))
      {
        writer.WritePropertyName("category");
        writer.WriteStartArray();

        foreach (CodeableConcept valCategory in Category)
        {
          valCategory.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Focus != null) && (Focus.Count != 0))
      {
        writer.WritePropertyName("focus");
        writer.WriteStartArray();

        foreach (CodeableConcept valFocus in Focus)
        {
          valFocus.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (ContactDetail valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((RelatedArtifact != null) && (RelatedArtifact.Count != 0))
      {
        writer.WritePropertyName("relatedArtifact");
        writer.WriteStartArray();

        foreach (RelatedArtifact valRelatedArtifact in RelatedArtifact)
        {
          valRelatedArtifact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Keyword != null) && (Keyword.Count != 0))
      {
        writer.WritePropertyName("keyword");
        writer.WriteStartArray();

        foreach (CodeableConcept valKeyword in Keyword)
        {
          valKeyword.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Jurisdiction != null) && (Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();

        foreach (CodeableConcept valJurisdiction in Jurisdiction)
        {
          valJurisdiction.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if ((Enrollment != null) && (Enrollment.Count != 0))
      {
        writer.WritePropertyName("enrollment");
        writer.WriteStartArray();

        foreach (Reference valEnrollment in Enrollment)
        {
          valEnrollment.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
      }

      if (Sponsor != null)
      {
        writer.WritePropertyName("sponsor");
        Sponsor.SerializeJson(writer, options);
      }

      if (PrincipalInvestigator != null)
      {
        writer.WritePropertyName("principalInvestigator");
        PrincipalInvestigator.SerializeJson(writer, options);
      }

      if ((Site != null) && (Site.Count != 0))
      {
        writer.WritePropertyName("site");
        writer.WriteStartArray();

        foreach (Reference valSite in Site)
        {
          valSite.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (ReasonStopped != null)
      {
        writer.WritePropertyName("reasonStopped");
        ReasonStopped.SerializeJson(writer, options);
      }

      if ((Note != null) && (Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();

        foreach (Annotation valNote in Note)
        {
          valNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Arm != null) && (Arm.Count != 0))
      {
        writer.WritePropertyName("arm");
        writer.WriteStartArray();

        foreach (ResearchStudyArm valArm in Arm)
        {
          valArm.SerializeJson(writer, options, true);
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
        case "arm":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Arm = new List<ResearchStudyArm>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ResearchStudyArm objArm = new fhirCsR3.Models.ResearchStudyArm();
            objArm.DeserializeJson(ref reader, options);
            Arm.Add(objArm);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Arm.Count == 0)
          {
            Arm = null;
          }

          break;

        case "category":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Category = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objCategory = new fhirCsR3.Models.CodeableConcept();
            objCategory.DeserializeJson(ref reader, options);
            Category.Add(objCategory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Category.Count == 0)
          {
            Category = null;
          }

          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactDetail>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ContactDetail objContact = new fhirCsR3.Models.ContactDetail();
            objContact.DeserializeJson(ref reader, options);
            Contact.Add(objContact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Contact.Count == 0)
          {
            Contact = null;
          }

          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "enrollment":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Enrollment = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objEnrollment = new fhirCsR3.Models.Reference();
            objEnrollment.DeserializeJson(ref reader, options);
            Enrollment.Add(objEnrollment);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Enrollment.Count == 0)
          {
            Enrollment = null;
          }

          break;

        case "focus":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Focus = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objFocus = new fhirCsR3.Models.CodeableConcept();
            objFocus.DeserializeJson(ref reader, options);
            Focus.Add(objFocus);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Focus.Count == 0)
          {
            Focus = null;
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
            fhirCsR3.Models.Identifier objIdentifier = new fhirCsR3.Models.Identifier();
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

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objJurisdiction = new fhirCsR3.Models.CodeableConcept();
            objJurisdiction.DeserializeJson(ref reader, options);
            Jurisdiction.Add(objJurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Jurisdiction.Count == 0)
          {
            Jurisdiction = null;
          }

          break;

        case "keyword":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Keyword = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objKeyword = new fhirCsR3.Models.CodeableConcept();
            objKeyword.DeserializeJson(ref reader, options);
            Keyword.Add(objKeyword);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Keyword.Count == 0)
          {
            Keyword = null;
          }

          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Annotation objNote = new fhirCsR3.Models.Annotation();
            objNote.DeserializeJson(ref reader, options);
            Note.Add(objNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Note.Count == 0)
          {
            Note = null;
          }

          break;

        case "partOf":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          PartOf = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objPartOf = new fhirCsR3.Models.Reference();
            objPartOf.DeserializeJson(ref reader, options);
            PartOf.Add(objPartOf);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (PartOf.Count == 0)
          {
            PartOf = null;
          }

          break;

        case "period":
          Period = new fhirCsR3.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "principalInvestigator":
          PrincipalInvestigator = new fhirCsR3.Models.Reference();
          PrincipalInvestigator.DeserializeJson(ref reader, options);
          break;

        case "protocol":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Protocol = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objProtocol = new fhirCsR3.Models.Reference();
            objProtocol.DeserializeJson(ref reader, options);
            Protocol.Add(objProtocol);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Protocol.Count == 0)
          {
            Protocol = null;
          }

          break;

        case "reasonStopped":
          ReasonStopped = new fhirCsR3.Models.CodeableConcept();
          ReasonStopped.DeserializeJson(ref reader, options);
          break;

        case "relatedArtifact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RelatedArtifact = new List<RelatedArtifact>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.RelatedArtifact objRelatedArtifact = new fhirCsR3.Models.RelatedArtifact();
            objRelatedArtifact.DeserializeJson(ref reader, options);
            RelatedArtifact.Add(objRelatedArtifact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (RelatedArtifact.Count == 0)
          {
            RelatedArtifact = null;
          }

          break;

        case "site":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Site = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objSite = new fhirCsR3.Models.Reference();
            objSite.DeserializeJson(ref reader, options);
            Site.Add(objSite);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Site.Count == 0)
          {
            Site = null;
          }

          break;

        case "sponsor":
          Sponsor = new fhirCsR3.Models.Reference();
          Sponsor.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "title":
          Title = reader.GetString();
          break;

        case "_title":
          _Title = new fhirCsR3.Models.Element();
          _Title.DeserializeJson(ref reader, options);
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
  /// <summary>
  /// Code Values for the ResearchStudy.status field
  /// </summary>
  public static class ResearchStudyStatusCodes {
    public const string DRAFT = "draft";
    public const string IN_PROGRESS = "in-progress";
    public const string SUSPENDED = "suspended";
    public const string STOPPED = "stopped";
    public const string COMPLETED = "completed";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public static HashSet<string> Values = new HashSet<string>() {
      "draft",
      "in-progress",
      "suspended",
      "stopped",
      "completed",
      "entered-in-error",
    };
  }
}
