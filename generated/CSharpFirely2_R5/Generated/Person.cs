// <auto-generated/>
// Contents of: hl7.fhir.r5.core version: 5.0.0-ballot

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using Hl7.Fhir.Introspection;
using Hl7.Fhir.Serialization;
using Hl7.Fhir.Specification;
using Hl7.Fhir.Utility;
using Hl7.Fhir.Validation;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model
{
  /// <summary>
  /// A generic person record
  /// </summary>
  [Serializable]
  [DataContract]
  [FhirType("Person","http://hl7.org/fhir/StructureDefinition/Person", IsResource=true)]
  public partial class Person : Hl7.Fhir.Model.DomainResource
  {
    /// <summary>
    /// FHIR Type Name
    /// </summary>
    public override string TypeName { get { return "Person"; } }

    /// <summary>
    /// The level of confidence that this link represents the same actual person, based on NIST Authentication Levels.
    /// (url: http://hl7.org/fhir/ValueSet/identity-assuranceLevel)
    /// (system: http://hl7.org/fhir/identity-assuranceLevel)
    /// </summary>
    [FhirEnumeration("IdentityAssuranceLevel")]
    public enum IdentityAssuranceLevel
    {
      /// <summary>
      /// Little or no confidence in the asserted identity's accuracy.
      /// (system: http://hl7.org/fhir/identity-assuranceLevel)
      /// </summary>
      [EnumLiteral("level1", "http://hl7.org/fhir/identity-assuranceLevel"), Description("Level 1")]
      Level1,
      /// <summary>
      /// Some confidence in the asserted identity's accuracy.
      /// (system: http://hl7.org/fhir/identity-assuranceLevel)
      /// </summary>
      [EnumLiteral("level2", "http://hl7.org/fhir/identity-assuranceLevel"), Description("Level 2")]
      Level2,
      /// <summary>
      /// High confidence in the asserted identity's accuracy.
      /// (system: http://hl7.org/fhir/identity-assuranceLevel)
      /// </summary>
      [EnumLiteral("level3", "http://hl7.org/fhir/identity-assuranceLevel"), Description("Level 3")]
      Level3,
      /// <summary>
      /// Very high confidence in the asserted identity's accuracy.
      /// (system: http://hl7.org/fhir/identity-assuranceLevel)
      /// </summary>
      [EnumLiteral("level4", "http://hl7.org/fhir/identity-assuranceLevel"), Description("Level 4")]
      Level4,
    }

    /// <summary>
    /// A language which may be used to communicate with the person about his or her health
    /// </summary>
    [Serializable]
    [DataContract]
    [FhirType("Person#Communication", IsNestedType=true)]
    public partial class CommunicationComponent : Hl7.Fhir.Model.BackboneElement
    {
      /// <summary>
      /// FHIR Type Name
      /// </summary>
      public override string TypeName { get { return "Person#Communication"; } }

      /// <summary>
      /// The language which can be used to communicate with the person about his or her health
      /// </summary>
      [FhirElement("language", Order=40)]
      [Cardinality(Min=1,Max=1)]
      [DataMember]
      public Hl7.Fhir.Model.CodeableConcept Language
      {
        get { return _Language; }
        set { _Language = value; OnPropertyChanged("Language"); }
      }

      private Hl7.Fhir.Model.CodeableConcept _Language;

      /// <summary>
      /// Language preference indicator
      /// </summary>
      [FhirElement("preferred", Order=50)]
      [DataMember]
      public Hl7.Fhir.Model.FhirBoolean PreferredElement
      {
        get { return _PreferredElement; }
        set { _PreferredElement = value; OnPropertyChanged("PreferredElement"); }
      }

      private Hl7.Fhir.Model.FhirBoolean _PreferredElement;

      /// <summary>
      /// Language preference indicator
      /// </summary>
      /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
      [IgnoreDataMember]
      public bool? Preferred
      {
        get { return PreferredElement != null ? PreferredElement.Value : null; }
        set
        {
          if (value == null)
            PreferredElement = null;
          else
            PreferredElement = new Hl7.Fhir.Model.FhirBoolean(value);
          OnPropertyChanged("Preferred");
        }
      }

      public override IDeepCopyable CopyTo(IDeepCopyable other)
      {
        var dest = other as CommunicationComponent;

        if (dest == null)
        {
          throw new ArgumentException("Can only copy to an object of the same type", "other");
        }

        base.CopyTo(dest);
        if(Language != null) dest.Language = (Hl7.Fhir.Model.CodeableConcept)Language.DeepCopy();
        if(PreferredElement != null) dest.PreferredElement = (Hl7.Fhir.Model.FhirBoolean)PreferredElement.DeepCopy();
        return dest;
      }

      public override IDeepCopyable DeepCopy()
      {
        return CopyTo(new CommunicationComponent());
      }

      ///<inheritdoc />
      public override bool Matches(IDeepComparable other)
      {
        var otherT = other as CommunicationComponent;
        if(otherT == null) return false;

        if(!base.Matches(otherT)) return false;
        if( !DeepComparable.Matches(Language, otherT.Language)) return false;
        if( !DeepComparable.Matches(PreferredElement, otherT.PreferredElement)) return false;

        return true;
      }

      public override bool IsExactly(IDeepComparable other)
      {
        var otherT = other as CommunicationComponent;
        if(otherT == null) return false;

        if(!base.IsExactly(otherT)) return false;
        if( !DeepComparable.IsExactly(Language, otherT.Language)) return false;
        if( !DeepComparable.IsExactly(PreferredElement, otherT.PreferredElement)) return false;

        return true;
      }

      [IgnoreDataMember]
      public override IEnumerable<Base> Children
      {
        get
        {
          foreach (var item in base.Children) yield return item;
          if (Language != null) yield return Language;
          if (PreferredElement != null) yield return PreferredElement;
        }
      }

      [IgnoreDataMember]
      public override IEnumerable<ElementValue> NamedChildren
      {
        get
        {
          foreach (var item in base.NamedChildren) yield return item;
          if (Language != null) yield return new ElementValue("language", Language);
          if (PreferredElement != null) yield return new ElementValue("preferred", PreferredElement);
        }
      }

      protected override bool TryGetValue(string key, out object value)
      {
        switch (key)
        {
          case "language":
            value = Language;
            return Language is not null;
          case "preferred":
            value = PreferredElement;
            return PreferredElement is not null;
          default:
            return base.TryGetValue(key, out value);
        };

      }

      protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
      {
        foreach (var kvp in base.GetElementPairs()) yield return kvp;
        if (Language is not null) yield return new KeyValuePair<string,object>("language",Language);
        if (PreferredElement is not null) yield return new KeyValuePair<string,object>("preferred",PreferredElement);
      }

    }

    /// <summary>
    /// Link to a resource that concerns the same actual person
    /// </summary>
    [Serializable]
    [DataContract]
    [FhirType("Person#Link", IsNestedType=true)]
    public partial class LinkComponent : Hl7.Fhir.Model.BackboneElement
    {
      /// <summary>
      /// FHIR Type Name
      /// </summary>
      public override string TypeName { get { return "Person#Link"; } }

      /// <summary>
      /// The resource to which this actual person is associated
      /// </summary>
      [FhirElement("target", Order=40)]
      [CLSCompliant(false)]
      [References("Patient","Practitioner","RelatedPerson","Person")]
      [Cardinality(Min=1,Max=1)]
      [DataMember]
      public Hl7.Fhir.Model.ResourceReference Target
      {
        get { return _Target; }
        set { _Target = value; OnPropertyChanged("Target"); }
      }

      private Hl7.Fhir.Model.ResourceReference _Target;

      /// <summary>
      /// level1 | level2 | level3 | level4
      /// </summary>
      [FhirElement("assurance", Order=50)]
      [DeclaredType(Type = typeof(Code))]
      [DataMember]
      public Code<Hl7.Fhir.Model.Person.IdentityAssuranceLevel> AssuranceElement
      {
        get { return _AssuranceElement; }
        set { _AssuranceElement = value; OnPropertyChanged("AssuranceElement"); }
      }

      private Code<Hl7.Fhir.Model.Person.IdentityAssuranceLevel> _AssuranceElement;

      /// <summary>
      /// level1 | level2 | level3 | level4
      /// </summary>
      /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
      [IgnoreDataMember]
      public Hl7.Fhir.Model.Person.IdentityAssuranceLevel? Assurance
      {
        get { return AssuranceElement != null ? AssuranceElement.Value : null; }
        set
        {
          if (value == null)
            AssuranceElement = null;
          else
            AssuranceElement = new Code<Hl7.Fhir.Model.Person.IdentityAssuranceLevel>(value);
          OnPropertyChanged("Assurance");
        }
      }

      public override IDeepCopyable CopyTo(IDeepCopyable other)
      {
        var dest = other as LinkComponent;

        if (dest == null)
        {
          throw new ArgumentException("Can only copy to an object of the same type", "other");
        }

        base.CopyTo(dest);
        if(Target != null) dest.Target = (Hl7.Fhir.Model.ResourceReference)Target.DeepCopy();
        if(AssuranceElement != null) dest.AssuranceElement = (Code<Hl7.Fhir.Model.Person.IdentityAssuranceLevel>)AssuranceElement.DeepCopy();
        return dest;
      }

      public override IDeepCopyable DeepCopy()
      {
        return CopyTo(new LinkComponent());
      }

      ///<inheritdoc />
      public override bool Matches(IDeepComparable other)
      {
        var otherT = other as LinkComponent;
        if(otherT == null) return false;

        if(!base.Matches(otherT)) return false;
        if( !DeepComparable.Matches(Target, otherT.Target)) return false;
        if( !DeepComparable.Matches(AssuranceElement, otherT.AssuranceElement)) return false;

        return true;
      }

      public override bool IsExactly(IDeepComparable other)
      {
        var otherT = other as LinkComponent;
        if(otherT == null) return false;

        if(!base.IsExactly(otherT)) return false;
        if( !DeepComparable.IsExactly(Target, otherT.Target)) return false;
        if( !DeepComparable.IsExactly(AssuranceElement, otherT.AssuranceElement)) return false;

        return true;
      }

      [IgnoreDataMember]
      public override IEnumerable<Base> Children
      {
        get
        {
          foreach (var item in base.Children) yield return item;
          if (Target != null) yield return Target;
          if (AssuranceElement != null) yield return AssuranceElement;
        }
      }

      [IgnoreDataMember]
      public override IEnumerable<ElementValue> NamedChildren
      {
        get
        {
          foreach (var item in base.NamedChildren) yield return item;
          if (Target != null) yield return new ElementValue("target", Target);
          if (AssuranceElement != null) yield return new ElementValue("assurance", AssuranceElement);
        }
      }

      protected override bool TryGetValue(string key, out object value)
      {
        switch (key)
        {
          case "target":
            value = Target;
            return Target is not null;
          case "assurance":
            value = AssuranceElement;
            return AssuranceElement is not null;
          default:
            return base.TryGetValue(key, out value);
        };

      }

      protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
      {
        foreach (var kvp in base.GetElementPairs()) yield return kvp;
        if (Target is not null) yield return new KeyValuePair<string,object>("target",Target);
        if (AssuranceElement is not null) yield return new KeyValuePair<string,object>("assurance",AssuranceElement);
      }

    }

    /// <summary>
    /// A human identifier for this person
    /// </summary>
    [FhirElement("identifier", InSummary=true, Order=90, FiveWs="FiveWs.identifier")]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Identifier> Identifier
    {
      get { if(_Identifier==null) _Identifier = new List<Hl7.Fhir.Model.Identifier>(); return _Identifier; }
      set { _Identifier = value; OnPropertyChanged("Identifier"); }
    }

    private List<Hl7.Fhir.Model.Identifier> _Identifier;

    /// <summary>
    /// This person's record is in active use
    /// </summary>
    [FhirElement("active", InSummary=true, IsModifier=true, Order=100, FiveWs="FiveWs.status")]
    [DataMember]
    public Hl7.Fhir.Model.FhirBoolean ActiveElement
    {
      get { return _ActiveElement; }
      set { _ActiveElement = value; OnPropertyChanged("ActiveElement"); }
    }

    private Hl7.Fhir.Model.FhirBoolean _ActiveElement;

    /// <summary>
    /// This person's record is in active use
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public bool? Active
    {
      get { return ActiveElement != null ? ActiveElement.Value : null; }
      set
      {
        if (value == null)
          ActiveElement = null;
        else
          ActiveElement = new Hl7.Fhir.Model.FhirBoolean(value);
        OnPropertyChanged("Active");
      }
    }

    /// <summary>
    /// A name associated with the person
    /// </summary>
    [FhirElement("name", InSummary=true, Order=110)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.HumanName> Name
    {
      get { if(_Name==null) _Name = new List<Hl7.Fhir.Model.HumanName>(); return _Name; }
      set { _Name = value; OnPropertyChanged("Name"); }
    }

    private List<Hl7.Fhir.Model.HumanName> _Name;

    /// <summary>
    /// A contact detail for the person
    /// </summary>
    [FhirElement("telecom", InSummary=true, Order=120)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.ContactPoint> Telecom
    {
      get { if(_Telecom==null) _Telecom = new List<Hl7.Fhir.Model.ContactPoint>(); return _Telecom; }
      set { _Telecom = value; OnPropertyChanged("Telecom"); }
    }

    private List<Hl7.Fhir.Model.ContactPoint> _Telecom;

    /// <summary>
    /// male | female | other | unknown
    /// </summary>
    [FhirElement("gender", InSummary=true, Order=130)]
    [DeclaredType(Type = typeof(Code))]
    [DataMember]
    public Code<Hl7.Fhir.Model.AdministrativeGender> GenderElement
    {
      get { return _GenderElement; }
      set { _GenderElement = value; OnPropertyChanged("GenderElement"); }
    }

    private Code<Hl7.Fhir.Model.AdministrativeGender> _GenderElement;

    /// <summary>
    /// male | female | other | unknown
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public Hl7.Fhir.Model.AdministrativeGender? Gender
    {
      get { return GenderElement != null ? GenderElement.Value : null; }
      set
      {
        if (value == null)
          GenderElement = null;
        else
          GenderElement = new Code<Hl7.Fhir.Model.AdministrativeGender>(value);
        OnPropertyChanged("Gender");
      }
    }

    /// <summary>
    /// The date on which the person was born
    /// </summary>
    [FhirElement("birthDate", InSummary=true, Order=140)]
    [DataMember]
    public Hl7.Fhir.Model.Date BirthDateElement
    {
      get { return _BirthDateElement; }
      set { _BirthDateElement = value; OnPropertyChanged("BirthDateElement"); }
    }

    private Hl7.Fhir.Model.Date _BirthDateElement;

    /// <summary>
    /// The date on which the person was born
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public string BirthDate
    {
      get { return BirthDateElement != null ? BirthDateElement.Value : null; }
      set
      {
        if (value == null)
          BirthDateElement = null;
        else
          BirthDateElement = new Hl7.Fhir.Model.Date(value);
        OnPropertyChanged("BirthDate");
      }
    }

    /// <summary>
    /// Indicates if the individual is deceased or not
    /// </summary>
    [FhirElement("deceased", InSummary=true, Order=150, Choice=ChoiceType.DatatypeChoice)]
    [CLSCompliant(false)]
    [AllowedTypes(typeof(Hl7.Fhir.Model.FhirBoolean),typeof(Hl7.Fhir.Model.FhirDateTime))]
    [DataMember]
    public Hl7.Fhir.Model.DataType Deceased
    {
      get { return _Deceased; }
      set { _Deceased = value; OnPropertyChanged("Deceased"); }
    }

    private Hl7.Fhir.Model.DataType _Deceased;

    /// <summary>
    /// One or more addresses for the person
    /// </summary>
    [FhirElement("address", InSummary=true, Order=160)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Address> Address
    {
      get { if(_Address==null) _Address = new List<Hl7.Fhir.Model.Address>(); return _Address; }
      set { _Address = value; OnPropertyChanged("Address"); }
    }

    private List<Hl7.Fhir.Model.Address> _Address;

    /// <summary>
    /// Marital (civil) status of a person
    /// </summary>
    [FhirElement("maritalStatus", Order=170)]
    [DataMember]
    public Hl7.Fhir.Model.CodeableConcept MaritalStatus
    {
      get { return _MaritalStatus; }
      set { _MaritalStatus = value; OnPropertyChanged("MaritalStatus"); }
    }

    private Hl7.Fhir.Model.CodeableConcept _MaritalStatus;

    /// <summary>
    /// Image of the person
    /// </summary>
    [FhirElement("photo", Order=180)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Attachment> Photo
    {
      get { if(_Photo==null) _Photo = new List<Hl7.Fhir.Model.Attachment>(); return _Photo; }
      set { _Photo = value; OnPropertyChanged("Photo"); }
    }

    private List<Hl7.Fhir.Model.Attachment> _Photo;

    /// <summary>
    /// A language which may be used to communicate with the person about his or her health
    /// </summary>
    [FhirElement("communication", Order=190)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Person.CommunicationComponent> Communication
    {
      get { if(_Communication==null) _Communication = new List<Hl7.Fhir.Model.Person.CommunicationComponent>(); return _Communication; }
      set { _Communication = value; OnPropertyChanged("Communication"); }
    }

    private List<Hl7.Fhir.Model.Person.CommunicationComponent> _Communication;

    /// <summary>
    /// The organization that is the custodian of the person record
    /// </summary>
    [FhirElement("managingOrganization", InSummary=true, Order=200)]
    [CLSCompliant(false)]
    [References("Organization")]
    [DataMember]
    public Hl7.Fhir.Model.ResourceReference ManagingOrganization
    {
      get { return _ManagingOrganization; }
      set { _ManagingOrganization = value; OnPropertyChanged("ManagingOrganization"); }
    }

    private Hl7.Fhir.Model.ResourceReference _ManagingOrganization;

    /// <summary>
    /// Link to a resource that concerns the same actual person
    /// </summary>
    [FhirElement("link", Order=210)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Person.LinkComponent> Link
    {
      get { if(_Link==null) _Link = new List<Hl7.Fhir.Model.Person.LinkComponent>(); return _Link; }
      set { _Link = value; OnPropertyChanged("Link"); }
    }

    private List<Hl7.Fhir.Model.Person.LinkComponent> _Link;

    public override IDeepCopyable CopyTo(IDeepCopyable other)
    {
      var dest = other as Person;

      if (dest == null)
      {
        throw new ArgumentException("Can only copy to an object of the same type", "other");
      }

      base.CopyTo(dest);
      if(Identifier != null) dest.Identifier = new List<Hl7.Fhir.Model.Identifier>(Identifier.DeepCopy());
      if(ActiveElement != null) dest.ActiveElement = (Hl7.Fhir.Model.FhirBoolean)ActiveElement.DeepCopy();
      if(Name != null) dest.Name = new List<Hl7.Fhir.Model.HumanName>(Name.DeepCopy());
      if(Telecom != null) dest.Telecom = new List<Hl7.Fhir.Model.ContactPoint>(Telecom.DeepCopy());
      if(GenderElement != null) dest.GenderElement = (Code<Hl7.Fhir.Model.AdministrativeGender>)GenderElement.DeepCopy();
      if(BirthDateElement != null) dest.BirthDateElement = (Hl7.Fhir.Model.Date)BirthDateElement.DeepCopy();
      if(Deceased != null) dest.Deceased = (Hl7.Fhir.Model.DataType)Deceased.DeepCopy();
      if(Address != null) dest.Address = new List<Hl7.Fhir.Model.Address>(Address.DeepCopy());
      if(MaritalStatus != null) dest.MaritalStatus = (Hl7.Fhir.Model.CodeableConcept)MaritalStatus.DeepCopy();
      if(Photo != null) dest.Photo = new List<Hl7.Fhir.Model.Attachment>(Photo.DeepCopy());
      if(Communication != null) dest.Communication = new List<Hl7.Fhir.Model.Person.CommunicationComponent>(Communication.DeepCopy());
      if(ManagingOrganization != null) dest.ManagingOrganization = (Hl7.Fhir.Model.ResourceReference)ManagingOrganization.DeepCopy();
      if(Link != null) dest.Link = new List<Hl7.Fhir.Model.Person.LinkComponent>(Link.DeepCopy());
      return dest;
    }

    public override IDeepCopyable DeepCopy()
    {
      return CopyTo(new Person());
    }

    ///<inheritdoc />
    public override bool Matches(IDeepComparable other)
    {
      var otherT = other as Person;
      if(otherT == null) return false;

      if(!base.Matches(otherT)) return false;
      if( !DeepComparable.Matches(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.Matches(ActiveElement, otherT.ActiveElement)) return false;
      if( !DeepComparable.Matches(Name, otherT.Name)) return false;
      if( !DeepComparable.Matches(Telecom, otherT.Telecom)) return false;
      if( !DeepComparable.Matches(GenderElement, otherT.GenderElement)) return false;
      if( !DeepComparable.Matches(BirthDateElement, otherT.BirthDateElement)) return false;
      if( !DeepComparable.Matches(Deceased, otherT.Deceased)) return false;
      if( !DeepComparable.Matches(Address, otherT.Address)) return false;
      if( !DeepComparable.Matches(MaritalStatus, otherT.MaritalStatus)) return false;
      if( !DeepComparable.Matches(Photo, otherT.Photo)) return false;
      if( !DeepComparable.Matches(Communication, otherT.Communication)) return false;
      if( !DeepComparable.Matches(ManagingOrganization, otherT.ManagingOrganization)) return false;
      if( !DeepComparable.Matches(Link, otherT.Link)) return false;

      return true;
    }

    public override bool IsExactly(IDeepComparable other)
    {
      var otherT = other as Person;
      if(otherT == null) return false;

      if(!base.IsExactly(otherT)) return false;
      if( !DeepComparable.IsExactly(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.IsExactly(ActiveElement, otherT.ActiveElement)) return false;
      if( !DeepComparable.IsExactly(Name, otherT.Name)) return false;
      if( !DeepComparable.IsExactly(Telecom, otherT.Telecom)) return false;
      if( !DeepComparable.IsExactly(GenderElement, otherT.GenderElement)) return false;
      if( !DeepComparable.IsExactly(BirthDateElement, otherT.BirthDateElement)) return false;
      if( !DeepComparable.IsExactly(Deceased, otherT.Deceased)) return false;
      if( !DeepComparable.IsExactly(Address, otherT.Address)) return false;
      if( !DeepComparable.IsExactly(MaritalStatus, otherT.MaritalStatus)) return false;
      if( !DeepComparable.IsExactly(Photo, otherT.Photo)) return false;
      if( !DeepComparable.IsExactly(Communication, otherT.Communication)) return false;
      if( !DeepComparable.IsExactly(ManagingOrganization, otherT.ManagingOrganization)) return false;
      if( !DeepComparable.IsExactly(Link, otherT.Link)) return false;

      return true;
    }

    [IgnoreDataMember]
    public override IEnumerable<Base> Children
    {
      get
      {
        foreach (var item in base.Children) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return elem; }
        if (ActiveElement != null) yield return ActiveElement;
        foreach (var elem in Name) { if (elem != null) yield return elem; }
        foreach (var elem in Telecom) { if (elem != null) yield return elem; }
        if (GenderElement != null) yield return GenderElement;
        if (BirthDateElement != null) yield return BirthDateElement;
        if (Deceased != null) yield return Deceased;
        foreach (var elem in Address) { if (elem != null) yield return elem; }
        if (MaritalStatus != null) yield return MaritalStatus;
        foreach (var elem in Photo) { if (elem != null) yield return elem; }
        foreach (var elem in Communication) { if (elem != null) yield return elem; }
        if (ManagingOrganization != null) yield return ManagingOrganization;
        foreach (var elem in Link) { if (elem != null) yield return elem; }
      }
    }

    [IgnoreDataMember]
    public override IEnumerable<ElementValue> NamedChildren
    {
      get
      {
        foreach (var item in base.NamedChildren) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return new ElementValue("identifier", elem); }
        if (ActiveElement != null) yield return new ElementValue("active", ActiveElement);
        foreach (var elem in Name) { if (elem != null) yield return new ElementValue("name", elem); }
        foreach (var elem in Telecom) { if (elem != null) yield return new ElementValue("telecom", elem); }
        if (GenderElement != null) yield return new ElementValue("gender", GenderElement);
        if (BirthDateElement != null) yield return new ElementValue("birthDate", BirthDateElement);
        if (Deceased != null) yield return new ElementValue("deceased", Deceased);
        foreach (var elem in Address) { if (elem != null) yield return new ElementValue("address", elem); }
        if (MaritalStatus != null) yield return new ElementValue("maritalStatus", MaritalStatus);
        foreach (var elem in Photo) { if (elem != null) yield return new ElementValue("photo", elem); }
        foreach (var elem in Communication) { if (elem != null) yield return new ElementValue("communication", elem); }
        if (ManagingOrganization != null) yield return new ElementValue("managingOrganization", ManagingOrganization);
        foreach (var elem in Link) { if (elem != null) yield return new ElementValue("link", elem); }
      }
    }

    protected override bool TryGetValue(string key, out object value)
    {
      switch (key)
      {
        case "identifier":
          value = Identifier;
          return Identifier?.Any() == true;
        case "active":
          value = ActiveElement;
          return ActiveElement is not null;
        case "name":
          value = Name;
          return Name?.Any() == true;
        case "telecom":
          value = Telecom;
          return Telecom?.Any() == true;
        case "gender":
          value = GenderElement;
          return GenderElement is not null;
        case "birthDate":
          value = BirthDateElement;
          return BirthDateElement is not null;
        case "deceased":
          value = Deceased;
          return Deceased is not null;
        case "address":
          value = Address;
          return Address?.Any() == true;
        case "maritalStatus":
          value = MaritalStatus;
          return MaritalStatus is not null;
        case "photo":
          value = Photo;
          return Photo?.Any() == true;
        case "communication":
          value = Communication;
          return Communication?.Any() == true;
        case "managingOrganization":
          value = ManagingOrganization;
          return ManagingOrganization is not null;
        case "link":
          value = Link;
          return Link?.Any() == true;
        default:
          return base.TryGetValue(key, out value);
      };

    }

    protected override IEnumerable<KeyValuePair<string, object>> GetElementPairs()
    {
      foreach (var kvp in base.GetElementPairs()) yield return kvp;
      if (Identifier?.Any() == true) yield return new KeyValuePair<string,object>("identifier",Identifier);
      if (ActiveElement is not null) yield return new KeyValuePair<string,object>("active",ActiveElement);
      if (Name?.Any() == true) yield return new KeyValuePair<string,object>("name",Name);
      if (Telecom?.Any() == true) yield return new KeyValuePair<string,object>("telecom",Telecom);
      if (GenderElement is not null) yield return new KeyValuePair<string,object>("gender",GenderElement);
      if (BirthDateElement is not null) yield return new KeyValuePair<string,object>("birthDate",BirthDateElement);
      if (Deceased is not null) yield return new KeyValuePair<string,object>("deceased",Deceased);
      if (Address?.Any() == true) yield return new KeyValuePair<string,object>("address",Address);
      if (MaritalStatus is not null) yield return new KeyValuePair<string,object>("maritalStatus",MaritalStatus);
      if (Photo?.Any() == true) yield return new KeyValuePair<string,object>("photo",Photo);
      if (Communication?.Any() == true) yield return new KeyValuePair<string,object>("communication",Communication);
      if (ManagingOrganization is not null) yield return new KeyValuePair<string,object>("managingOrganization",ManagingOrganization);
      if (Link?.Any() == true) yield return new KeyValuePair<string,object>("link",Link);
    }

  }

}

// end of file
