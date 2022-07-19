// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4.Serialization;

namespace fhirCsR4.Models
{
  /// <summary>
  /// The linkages between sugar residues will also be captured.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstanceNucleicAcidSubunitLinkage>))]
  public class SubstanceNucleicAcidSubunitLinkage : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The entity that links the sugar residues together should also be captured for nearly all naturally occurring nucleic acid the linkage is a phosphate group. For many synthetic oligonucleotides phosphorothioate linkages are often seen. Linkage connectivity is assumed to be 3’-5’. If the linkage is either 3’-3’ or 5’-5’ this should be specified.
    /// </summary>
    public string Connectivity { get; set; }
    /// <summary>
    /// Extension container element for Connectivity
    /// </summary>
    public Element _Connectivity { get; set; }
    /// <summary>
    /// Each linkage will be registered as a fragment and have an ID.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// Each linkage will be registered as a fragment and have at least one name. A single name shall be assigned to each linkage.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Residues shall be captured as described in 5.3.6.8.3.
    /// </summary>
    public string ResidueSite { get; set; }
    /// <summary>
    /// Extension container element for ResidueSite
    /// </summary>
    public Element _ResidueSite { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Connectivity))
      {
        writer.WriteString("connectivity", (string)Connectivity!);
      }

      if (_Connectivity != null)
      {
        writer.WritePropertyName("_connectivity");
        _Connectivity.SerializeJson(writer, options);
      }

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ResidueSite))
      {
        writer.WriteString("residueSite", (string)ResidueSite!);
      }

      if (_ResidueSite != null)
      {
        writer.WritePropertyName("_residueSite");
        _ResidueSite.SerializeJson(writer, options);
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
        case "connectivity":
          Connectivity = reader.GetString();
          break;

        case "_connectivity":
          _Connectivity = new fhirCsR4.Models.Element();
          _Connectivity.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR4.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR4.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "residueSite":
          ResidueSite = reader.GetString();
          break;

        case "_residueSite":
          _ResidueSite = new fhirCsR4.Models.Element();
          _ResidueSite.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// 5.3.6.8.1 Sugar ID (Mandatory).
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstanceNucleicAcidSubunitSugar>))]
  public class SubstanceNucleicAcidSubunitSugar : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The Substance ID of the sugar or sugar-like component that make up the nucleotide.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// The name of the sugar or sugar-like component that make up the nucleotide.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// The residues that contain a given sugar will be captured. The order of given residues will be captured in the 5‘-3‘direction consistent with the base sequences listed above.
    /// </summary>
    public string ResidueSite { get; set; }
    /// <summary>
    /// Extension container element for ResidueSite
    /// </summary>
    public Element _ResidueSite { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ResidueSite))
      {
        writer.WriteString("residueSite", (string)ResidueSite!);
      }

      if (_ResidueSite != null)
      {
        writer.WritePropertyName("_residueSite");
        _ResidueSite.SerializeJson(writer, options);
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
        case "identifier":
          Identifier = new fhirCsR4.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR4.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "residueSite":
          ResidueSite = reader.GetString();
          break;

        case "_residueSite":
          _ResidueSite = new fhirCsR4.Models.Element();
          _ResidueSite.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Subunits are listed in order of decreasing length; sequences of the same length will be ordered by molecular weight; subunits that have identical sequences will be repeated multiple times.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstanceNucleicAcidSubunit>))]
  public class SubstanceNucleicAcidSubunit : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The nucleotide present at the 5’ terminal shall be specified based on a controlled vocabulary. Since the sequence is represented from the 5' to the 3' end, the 5’ prime nucleotide is the letter at the first position in the sequence. A separate representation would be redundant.
    /// </summary>
    public CodeableConcept FivePrime { get; set; }
    /// <summary>
    /// The length of the sequence shall be captured.
    /// </summary>
    public int? Length { get; set; }
    /// <summary>
    /// Extension container element for Length
    /// </summary>
    public Element _Length { get; set; }
    /// <summary>
    /// The linkages between sugar residues will also be captured.
    /// </summary>
    public List<SubstanceNucleicAcidSubunitLinkage> Linkage { get; set; }
    /// <summary>
    /// Actual nucleotide sequence notation from 5' to 3' end using standard single letter codes. In addition to the base sequence, sugar and type of phosphate or non-phosphate linkage should also be captured.
    /// </summary>
    public string Sequence { get; set; }
    /// <summary>
    /// Extension container element for Sequence
    /// </summary>
    public Element _Sequence { get; set; }
    /// <summary>
    /// (TBC).
    /// </summary>
    public Attachment SequenceAttachment { get; set; }
    /// <summary>
    /// Index of linear sequences of nucleic acids in order of decreasing length. Sequences of the same length will be ordered by molecular weight. Subunits that have identical sequences will be repeated and have sequential subscripts.
    /// </summary>
    public int? Subunit { get; set; }
    /// <summary>
    /// Extension container element for Subunit
    /// </summary>
    public Element _Subunit { get; set; }
    /// <summary>
    /// 5.3.6.8.1 Sugar ID (Mandatory).
    /// </summary>
    public List<SubstanceNucleicAcidSubunitSugar> Sugar { get; set; }
    /// <summary>
    /// The nucleotide present at the 3’ terminal shall be specified based on a controlled vocabulary. Since the sequence is represented from the 5' to the 3' end, the 5’ prime nucleotide is the letter at the last position in the sequence. A separate representation would be redundant.
    /// </summary>
    public CodeableConcept ThreePrime { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Subunit != null)
      {
        writer.WriteNumber("subunit", (int)Subunit!);
      }

      if (_Subunit != null)
      {
        writer.WritePropertyName("_subunit");
        _Subunit.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Sequence))
      {
        writer.WriteString("sequence", (string)Sequence!);
      }

      if (_Sequence != null)
      {
        writer.WritePropertyName("_sequence");
        _Sequence.SerializeJson(writer, options);
      }

      if (Length != null)
      {
        writer.WriteNumber("length", (int)Length!);
      }

      if (_Length != null)
      {
        writer.WritePropertyName("_length");
        _Length.SerializeJson(writer, options);
      }

      if (SequenceAttachment != null)
      {
        writer.WritePropertyName("sequenceAttachment");
        SequenceAttachment.SerializeJson(writer, options);
      }

      if (FivePrime != null)
      {
        writer.WritePropertyName("fivePrime");
        FivePrime.SerializeJson(writer, options);
      }

      if (ThreePrime != null)
      {
        writer.WritePropertyName("threePrime");
        ThreePrime.SerializeJson(writer, options);
      }

      if ((Linkage != null) && (Linkage.Count != 0))
      {
        writer.WritePropertyName("linkage");
        writer.WriteStartArray();

        foreach (SubstanceNucleicAcidSubunitLinkage valLinkage in Linkage)
        {
          valLinkage.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Sugar != null) && (Sugar.Count != 0))
      {
        writer.WritePropertyName("sugar");
        writer.WriteStartArray();

        foreach (SubstanceNucleicAcidSubunitSugar valSugar in Sugar)
        {
          valSugar.SerializeJson(writer, options, true);
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
        case "fivePrime":
          FivePrime = new fhirCsR4.Models.CodeableConcept();
          FivePrime.DeserializeJson(ref reader, options);
          break;

        case "length":
          Length = reader.GetInt32();
          break;

        case "_length":
          _Length = new fhirCsR4.Models.Element();
          _Length.DeserializeJson(ref reader, options);
          break;

        case "linkage":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Linkage = new List<SubstanceNucleicAcidSubunitLinkage>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstanceNucleicAcidSubunitLinkage objLinkage = new fhirCsR4.Models.SubstanceNucleicAcidSubunitLinkage();
            objLinkage.DeserializeJson(ref reader, options);
            Linkage.Add(objLinkage);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Linkage.Count == 0)
          {
            Linkage = null;
          }

          break;

        case "sequence":
          Sequence = reader.GetString();
          break;

        case "_sequence":
          _Sequence = new fhirCsR4.Models.Element();
          _Sequence.DeserializeJson(ref reader, options);
          break;

        case "sequenceAttachment":
          SequenceAttachment = new fhirCsR4.Models.Attachment();
          SequenceAttachment.DeserializeJson(ref reader, options);
          break;

        case "subunit":
          Subunit = reader.GetInt32();
          break;

        case "_subunit":
          _Subunit = new fhirCsR4.Models.Element();
          _Subunit.DeserializeJson(ref reader, options);
          break;

        case "sugar":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Sugar = new List<SubstanceNucleicAcidSubunitSugar>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstanceNucleicAcidSubunitSugar objSugar = new fhirCsR4.Models.SubstanceNucleicAcidSubunitSugar();
            objSugar.DeserializeJson(ref reader, options);
            Sugar.Add(objSugar);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Sugar.Count == 0)
          {
            Sugar = null;
          }

          break;

        case "threePrime":
          ThreePrime = new fhirCsR4.Models.CodeableConcept();
          ThreePrime.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Nucleic acids are defined by three distinct elements: the base, sugar and linkage. Individual substance/moiety IDs will be created for each of these elements. The nucleotide sequence will be always entered in the 5’-3’ direction.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstanceNucleicAcid>))]
  public class SubstanceNucleicAcid : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "SubstanceNucleicAcid";
    /// <summary>
    /// The area of hybridisation shall be described if applicable for double stranded RNA or DNA. The number associated with the subunit followed by the number associated to the residue shall be specified in increasing order. The underscore “” shall be used as separator as follows: “Subunitnumber Residue”.
    /// </summary>
    public string AreaOfHybridisation { get; set; }
    /// <summary>
    /// Extension container element for AreaOfHybridisation
    /// </summary>
    public Element _AreaOfHybridisation { get; set; }
    /// <summary>
    /// The number of linear sequences of nucleotides linked through phosphodiester bonds shall be described. Subunits would be strands of nucleic acids that are tightly associated typically through Watson-Crick base pairing. NOTE: If not specified in the reference source, the assumption is that there is 1 subunit.
    /// </summary>
    public int? NumberOfSubunits { get; set; }
    /// <summary>
    /// Extension container element for NumberOfSubunits
    /// </summary>
    public Element _NumberOfSubunits { get; set; }
    /// <summary>
    /// (TBC).
    /// </summary>
    public CodeableConcept OligoNucleotideType { get; set; }
    /// <summary>
    /// The type of the sequence shall be specified based on a controlled vocabulary.
    /// </summary>
    public CodeableConcept SequenceType { get; set; }
    /// <summary>
    /// Subunits are listed in order of decreasing length; sequences of the same length will be ordered by molecular weight; subunits that have identical sequences will be repeated multiple times.
    /// </summary>
    public List<SubstanceNucleicAcidSubunit> Subunit { get; set; }
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


      ((fhirCsR4.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (SequenceType != null)
      {
        writer.WritePropertyName("sequenceType");
        SequenceType.SerializeJson(writer, options);
      }

      if (NumberOfSubunits != null)
      {
        writer.WriteNumber("numberOfSubunits", (int)NumberOfSubunits!);
      }

      if (_NumberOfSubunits != null)
      {
        writer.WritePropertyName("_numberOfSubunits");
        _NumberOfSubunits.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AreaOfHybridisation))
      {
        writer.WriteString("areaOfHybridisation", (string)AreaOfHybridisation!);
      }

      if (_AreaOfHybridisation != null)
      {
        writer.WritePropertyName("_areaOfHybridisation");
        _AreaOfHybridisation.SerializeJson(writer, options);
      }

      if (OligoNucleotideType != null)
      {
        writer.WritePropertyName("oligoNucleotideType");
        OligoNucleotideType.SerializeJson(writer, options);
      }

      if ((Subunit != null) && (Subunit.Count != 0))
      {
        writer.WritePropertyName("subunit");
        writer.WriteStartArray();

        foreach (SubstanceNucleicAcidSubunit valSubunit in Subunit)
        {
          valSubunit.SerializeJson(writer, options, true);
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
        case "areaOfHybridisation":
          AreaOfHybridisation = reader.GetString();
          break;

        case "_areaOfHybridisation":
          _AreaOfHybridisation = new fhirCsR4.Models.Element();
          _AreaOfHybridisation.DeserializeJson(ref reader, options);
          break;

        case "numberOfSubunits":
          NumberOfSubunits = reader.GetInt32();
          break;

        case "_numberOfSubunits":
          _NumberOfSubunits = new fhirCsR4.Models.Element();
          _NumberOfSubunits.DeserializeJson(ref reader, options);
          break;

        case "oligoNucleotideType":
          OligoNucleotideType = new fhirCsR4.Models.CodeableConcept();
          OligoNucleotideType.DeserializeJson(ref reader, options);
          break;

        case "sequenceType":
          SequenceType = new fhirCsR4.Models.CodeableConcept();
          SequenceType.DeserializeJson(ref reader, options);
          break;

        case "subunit":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Subunit = new List<SubstanceNucleicAcidSubunit>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstanceNucleicAcidSubunit objSubunit = new fhirCsR4.Models.SubstanceNucleicAcidSubunit();
            objSubunit.DeserializeJson(ref reader, options);
            Subunit.Add(objSubunit);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Subunit.Count == 0)
          {
            Subunit = null;
          }

          break;

        default:
          ((fhirCsR4.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
