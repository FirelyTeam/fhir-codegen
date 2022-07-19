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
  /// A record of a medication that is being consumed by a patient.   A MedicationStatement may indicate that the patient may be taking the medication now, or has taken the medication in the past or will be taking the medication in the future.  The source of this information can be the patient, significant other (such as a family member or spouse), or a clinician.  A common scenario where this information is captured is during the history taking process during a patient visit or stay.   The medication information may come from sources such as the patient's memory, from a prescription bottle,  or from a list of medications the patient, clinician or other party maintains 
  /// The primary difference between a medication statement and a medication administration is that the medication administration has complete administration information and is based on actual administration information from the person who administered the medication.  A medication statement is often, if not always, less specific.  There is no required date/time when the medication was administered, in fact we only know that a source has reported the patient is taking this medication, where details such as time, quantity, or rate or even medication product may be incomplete or missing or less precise.  As stated earlier, the medication statement information may come from the patient's memory, from a prescription bottle or from a list of medications the patient, clinician or other party maintains.  Medication administration is more formal and is not missing detailed information.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<MedicationStatement>))]
  public class MedicationStatement : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "MedicationStatement";
    /// <summary>
    /// A plan, proposal or order that is fulfilled in whole or in part by this event.
    /// </summary>
    public List<Reference> BasedOn { get; set; }
    /// <summary>
    /// Indicates where type of medication statement and where the medication is expected to be consumed or administered.
    /// </summary>
    public CodeableConcept Category { get; set; }
    /// <summary>
    /// The encounter or episode of care that establishes the context for this MedicationStatement.
    /// </summary>
    public Reference Context { get; set; }
    /// <summary>
    /// The date when the medication statement was asserted by the information source.
    /// </summary>
    public string DateAsserted { get; set; }
    /// <summary>
    /// Extension container element for DateAsserted
    /// </summary>
    public Element _DateAsserted { get; set; }
    /// <summary>
    /// Likely references would be to MedicationRequest, MedicationDispense, Claim, Observation or QuestionnaireAnswers.  The most common use cases for deriving a MedicationStatement comes from creating a MedicationStatement from a MedicationRequest or from a lab observation or a claim.  it should be noted that the amount of information that is available varies from the type resource that you derive the MedicationStatement from.
    /// </summary>
    public List<Reference> DerivedFrom { get; set; }
    /// <summary>
    /// The dates included in the dosage on a Medication Statement reflect the dates for a given dose.  For example, "from November 1, 2016 to November 3, 2016, take one tablet daily and from November 4, 2016 to November 7, 2016, take two tablets daily."  It is expected that this specificity may only be populated where the patient brings in their labeled container or where the Medication Statement is derived from a MedicationRequest.
    /// </summary>
    public List<Dosage> Dosage { get; set; }
    /// <summary>
    /// This attribute reflects the period over which the patient consumed the medication and is expected to be populated on the majority of Medication Statements. If the medication is still being taken at the time the statement is recorded, the "end" date will be omitted.
    /// </summary>
    public string EffectiveDateTime { get; set; }
    /// <summary>
    /// Extension container element for EffectiveDateTime
    /// </summary>
    public Element _EffectiveDateTime { get; set; }
    /// <summary>
    /// This attribute reflects the period over which the patient consumed the medication and is expected to be populated on the majority of Medication Statements. If the medication is still being taken at the time the statement is recorded, the "end" date will be omitted.
    /// </summary>
    public Period EffectivePeriod { get; set; }
    /// <summary>
    /// External identifier - FHIR will generate its own internal identifiers (probably URLs) which do not need to be explicitly managed by the resource.  The identifier here is one that would be used by another non-FHIR system - for example an automated medication pump would provide a record each time it operated; an administration while the patient was off the ward might be made with a different system and entered after the event.  Particularly important if these records have to be updated.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The person or organization that provided the information about the taking of this medication. Note: Use derivedFrom when a MedicationStatement is derived from other resources, e.g Claim or MedicationRequest.
    /// </summary>
    public Reference InformationSource { get; set; }
    /// <summary>
    /// If only a code is specified, then it needs to be a code for a specific product. If more information is required, then the use of the medication resource is recommended.  For example if you require form or lot number, then you must reference the Medication resource. .
    /// </summary>
    public CodeableConcept MedicationCodeableConcept { get; set; }
    /// <summary>
    /// If only a code is specified, then it needs to be a code for a specific product. If more information is required, then the use of the medication resource is recommended.  For example if you require form or lot number, then you must reference the Medication resource. .
    /// </summary>
    public Reference MedicationReference { get; set; }
    /// <summary>
    /// Provides extra information about the medication statement that is not conveyed by the other attributes.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// A larger event of which this particular event is a component or step.
    /// </summary>
    public List<Reference> PartOf { get; set; }
    /// <summary>
    /// This could be a diagnosis code. If a full condition record exists or additional detail is needed, use reasonForUseReference.
    /// </summary>
    public List<CodeableConcept> ReasonCode { get; set; }
    /// <summary>
    /// A code indicating why the medication was not taken.
    /// </summary>
    public List<CodeableConcept> ReasonNotTaken { get; set; }
    /// <summary>
    /// This is a reference to a condition that is the reason why the medication is being/was taken.  If only a code exists, use reasonForUseCode.
    /// </summary>
    public List<Reference> ReasonReference { get; set; }
    /// <summary>
    /// MedicationStatement is a statement at a point in time.  The status is only representative at the point when it was asserted.  The value set for MedicationStatement.status contains codes that assert the status of the use of the medication by the patient (for example, stopped or on hold) as well as codes that assert the status of the medication statement itself (for example, entered in error).
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The person, animal or group who is/was taking the medication.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because it indicates that the medication was not taken.
    /// </summary>
    public string Taken { get; set; }
    /// <summary>
    /// Extension container element for Taken
    /// </summary>
    public Element _Taken { get; set; }
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

      if ((BasedOn != null) && (BasedOn.Count != 0))
      {
        writer.WritePropertyName("basedOn");
        writer.WriteStartArray();

        foreach (Reference valBasedOn in BasedOn)
        {
          valBasedOn.SerializeJson(writer, options, true);
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

      if (Context != null)
      {
        writer.WritePropertyName("context");
        Context.SerializeJson(writer, options);
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

      if (Category != null)
      {
        writer.WritePropertyName("category");
        Category.SerializeJson(writer, options);
      }

      if (MedicationCodeableConcept != null)
      {
        writer.WritePropertyName("medicationCodeableConcept");
        MedicationCodeableConcept.SerializeJson(writer, options);
      }

      if (MedicationReference != null)
      {
        writer.WritePropertyName("medicationReference");
        MedicationReference.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(EffectiveDateTime))
      {
        writer.WriteString("effectiveDateTime", (string)EffectiveDateTime!);
      }

      if (_EffectiveDateTime != null)
      {
        writer.WritePropertyName("_effectiveDateTime");
        _EffectiveDateTime.SerializeJson(writer, options);
      }

      if (EffectivePeriod != null)
      {
        writer.WritePropertyName("effectivePeriod");
        EffectivePeriod.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(DateAsserted))
      {
        writer.WriteString("dateAsserted", (string)DateAsserted!);
      }

      if (_DateAsserted != null)
      {
        writer.WritePropertyName("_dateAsserted");
        _DateAsserted.SerializeJson(writer, options);
      }

      if (InformationSource != null)
      {
        writer.WritePropertyName("informationSource");
        InformationSource.SerializeJson(writer, options);
      }

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
      }

      if ((DerivedFrom != null) && (DerivedFrom.Count != 0))
      {
        writer.WritePropertyName("derivedFrom");
        writer.WriteStartArray();

        foreach (Reference valDerivedFrom in DerivedFrom)
        {
          valDerivedFrom.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Taken))
      {
        writer.WriteString("taken", (string)Taken!);
      }

      if (_Taken != null)
      {
        writer.WritePropertyName("_taken");
        _Taken.SerializeJson(writer, options);
      }

      if ((ReasonNotTaken != null) && (ReasonNotTaken.Count != 0))
      {
        writer.WritePropertyName("reasonNotTaken");
        writer.WriteStartArray();

        foreach (CodeableConcept valReasonNotTaken in ReasonNotTaken)
        {
          valReasonNotTaken.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((ReasonCode != null) && (ReasonCode.Count != 0))
      {
        writer.WritePropertyName("reasonCode");
        writer.WriteStartArray();

        foreach (CodeableConcept valReasonCode in ReasonCode)
        {
          valReasonCode.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((ReasonReference != null) && (ReasonReference.Count != 0))
      {
        writer.WritePropertyName("reasonReference");
        writer.WriteStartArray();

        foreach (Reference valReasonReference in ReasonReference)
        {
          valReasonReference.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if ((Dosage != null) && (Dosage.Count != 0))
      {
        writer.WritePropertyName("dosage");
        writer.WriteStartArray();

        foreach (Dosage valDosage in Dosage)
        {
          valDosage.SerializeJson(writer, options, true);
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
        case "basedOn":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          BasedOn = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objBasedOn = new fhirCsR3.Models.Reference();
            objBasedOn.DeserializeJson(ref reader, options);
            BasedOn.Add(objBasedOn);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (BasedOn.Count == 0)
          {
            BasedOn = null;
          }

          break;

        case "category":
          Category = new fhirCsR3.Models.CodeableConcept();
          Category.DeserializeJson(ref reader, options);
          break;

        case "context":
          Context = new fhirCsR3.Models.Reference();
          Context.DeserializeJson(ref reader, options);
          break;

        case "dateAsserted":
          DateAsserted = reader.GetString();
          break;

        case "_dateAsserted":
          _DateAsserted = new fhirCsR3.Models.Element();
          _DateAsserted.DeserializeJson(ref reader, options);
          break;

        case "derivedFrom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DerivedFrom = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objDerivedFrom = new fhirCsR3.Models.Reference();
            objDerivedFrom.DeserializeJson(ref reader, options);
            DerivedFrom.Add(objDerivedFrom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DerivedFrom.Count == 0)
          {
            DerivedFrom = null;
          }

          break;

        case "dosage":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Dosage = new List<Dosage>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Dosage objDosage = new fhirCsR3.Models.Dosage();
            objDosage.DeserializeJson(ref reader, options);
            Dosage.Add(objDosage);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Dosage.Count == 0)
          {
            Dosage = null;
          }

          break;

        case "effectiveDateTime":
          EffectiveDateTime = reader.GetString();
          break;

        case "_effectiveDateTime":
          _EffectiveDateTime = new fhirCsR3.Models.Element();
          _EffectiveDateTime.DeserializeJson(ref reader, options);
          break;

        case "effectivePeriod":
          EffectivePeriod = new fhirCsR3.Models.Period();
          EffectivePeriod.DeserializeJson(ref reader, options);
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

        case "informationSource":
          InformationSource = new fhirCsR3.Models.Reference();
          InformationSource.DeserializeJson(ref reader, options);
          break;

        case "medicationCodeableConcept":
          MedicationCodeableConcept = new fhirCsR3.Models.CodeableConcept();
          MedicationCodeableConcept.DeserializeJson(ref reader, options);
          break;

        case "medicationReference":
          MedicationReference = new fhirCsR3.Models.Reference();
          MedicationReference.DeserializeJson(ref reader, options);
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

        case "reasonCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objReasonCode = new fhirCsR3.Models.CodeableConcept();
            objReasonCode.DeserializeJson(ref reader, options);
            ReasonCode.Add(objReasonCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ReasonCode.Count == 0)
          {
            ReasonCode = null;
          }

          break;

        case "reasonNotTaken":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonNotTaken = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objReasonNotTaken = new fhirCsR3.Models.CodeableConcept();
            objReasonNotTaken.DeserializeJson(ref reader, options);
            ReasonNotTaken.Add(objReasonNotTaken);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ReasonNotTaken.Count == 0)
          {
            ReasonNotTaken = null;
          }

          break;

        case "reasonReference":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonReference = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objReasonReference = new fhirCsR3.Models.Reference();
            objReasonReference.DeserializeJson(ref reader, options);
            ReasonReference.Add(objReasonReference);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ReasonReference.Count == 0)
          {
            ReasonReference = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new fhirCsR3.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        case "taken":
          Taken = reader.GetString();
          break;

        case "_taken":
          _Taken = new fhirCsR3.Models.Element();
          _Taken.DeserializeJson(ref reader, options);
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
  /// Code Values for the MedicationStatement.status field
  /// </summary>
  public static class MedicationStatementStatusCodes {
    public const string ACTIVE = "active";
    public const string COMPLETED = "completed";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string INTENDED = "intended";
    public const string STOPPED = "stopped";
    public const string ON_HOLD = "on-hold";
    public static HashSet<string> Values = new HashSet<string>() {
      "active",
      "completed",
      "entered-in-error",
      "intended",
      "stopped",
      "on-hold",
    };
  }
  /// <summary>
  /// Code Values for the MedicationStatement.taken field
  /// </summary>
  public static class MedicationStatementTakenCodes {
    public const string Y = "y";
    public const string N = "n";
    public const string UNK = "unk";
    public const string NA = "na";
    public static HashSet<string> Values = new HashSet<string>() {
      "y",
      "n",
      "unk",
      "na",
    };
  }
}
