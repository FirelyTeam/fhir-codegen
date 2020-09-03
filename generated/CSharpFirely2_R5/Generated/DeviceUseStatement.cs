// <auto-generated/>
// Contents of: hl7.fhir.r5.core version: 4.4.0

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
  /// Record of use of a device
  /// </summary>
#if !NETSTANDARD1_1
  [Serializable]
#endif
  [FhirType("DeviceUseStatement", IsResource=true)]
  [DataContract]
  public partial class DeviceUseStatement : Hl7.Fhir.Model.DomainResource
  {
    /// <summary>
    /// FHIR Type Name
    /// </summary>
    public override string TypeName { get { return "DeviceUseStatement"; } }

    /// <summary>
    /// A coded concept indicating the current status of the Device Usage.
    /// (url: http://hl7.org/fhir/ValueSet/device-statement-status)
    /// (system: http://hl7.org/fhir/device-statement-status)
    /// </summary>
    [FhirEnumeration("DeviceUseStatementStatus")]
    public enum DeviceUseStatementStatus
    {
      /// <summary>
      /// The device is still being used.
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("active", "http://hl7.org/fhir/device-statement-status"), Description("Active")]
      Active,
      /// <summary>
      /// The device is no longer being used.
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("completed", "http://hl7.org/fhir/device-statement-status"), Description("Completed")]
      Completed,
      /// <summary>
      /// The statement was recorded incorrectly.
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("entered-in-error", "http://hl7.org/fhir/device-statement-status"), Description("Entered in Error")]
      EnteredInError,
      /// <summary>
      /// The device may be used at some time in the future.
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("intended", "http://hl7.org/fhir/device-statement-status"), Description("Intended")]
      Intended,
      /// <summary>
      /// Actions implied by the statement have been permanently halted, before all of them occurred.
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("stopped", "http://hl7.org/fhir/device-statement-status"), Description("Stopped")]
      Stopped,
      /// <summary>
      /// Actions implied by the statement have been temporarily halted, but are expected to continue later. May also be called "suspended".
      /// (system: http://hl7.org/fhir/device-statement-status)
      /// </summary>
      [EnumLiteral("on-hold", "http://hl7.org/fhir/device-statement-status"), Description("On Hold")]
      OnHold,
    }

    /// <summary>
    /// External identifier for this record
    /// </summary>
    [FhirElement("identifier", InSummary=true, Order=90)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Identifier> Identifier
    {
      get { if(_Identifier==null) _Identifier = new List<Hl7.Fhir.Model.Identifier>(); return _Identifier; }
      set { _Identifier = value; OnPropertyChanged("Identifier"); }
    }

    private List<Hl7.Fhir.Model.Identifier> _Identifier;

    /// <summary>
    /// Fulfills plan, proposal or order
    /// </summary>
    [FhirElement("basedOn", InSummary=true, Order=100)]
    [CLSCompliant(false)]
    [References("ServiceRequest")]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.ResourceReference> BasedOn
    {
      get { if(_BasedOn==null) _BasedOn = new List<Hl7.Fhir.Model.ResourceReference>(); return _BasedOn; }
      set { _BasedOn = value; OnPropertyChanged("BasedOn"); }
    }

    private List<Hl7.Fhir.Model.ResourceReference> _BasedOn;

    /// <summary>
    /// active | completed | entered-in-error +
    /// </summary>
    [FhirElement("status", InSummary=true, Order=110)]
    [Cardinality(Min=1,Max=1)]
    [DataMember]
    public Code<Hl7.Fhir.Model.DeviceUseStatement.DeviceUseStatementStatus> StatusElement
    {
      get { return _StatusElement; }
      set { _StatusElement = value; OnPropertyChanged("StatusElement"); }
    }

    private Code<Hl7.Fhir.Model.DeviceUseStatement.DeviceUseStatementStatus> _StatusElement;

    /// <summary>
    /// active | completed | entered-in-error +
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public Hl7.Fhir.Model.DeviceUseStatement.DeviceUseStatementStatus? Status
    {
      get { return StatusElement != null ? StatusElement.Value : null; }
      set
      {
        if (value == null)
          StatusElement = null;
        else
          StatusElement = new Code<Hl7.Fhir.Model.DeviceUseStatement.DeviceUseStatementStatus>(value);
        OnPropertyChanged("Status");
      }
    }

    /// <summary>
    /// The category of the statement - classifying how the statement is made
    /// </summary>
    [FhirElement("category", Order=120)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.CodeableConcept> Category
    {
      get { if(_Category==null) _Category = new List<Hl7.Fhir.Model.CodeableConcept>(); return _Category; }
      set { _Category = value; OnPropertyChanged("Category"); }
    }

    private List<Hl7.Fhir.Model.CodeableConcept> _Category;

    /// <summary>
    /// Patient using device
    /// </summary>
    [FhirElement("subject", InSummary=true, Order=130)]
    [CLSCompliant(false)]
    [References("Patient","Group")]
    [Cardinality(Min=1,Max=1)]
    [DataMember]
    public Hl7.Fhir.Model.ResourceReference Subject
    {
      get { return _Subject; }
      set { _Subject = value; OnPropertyChanged("Subject"); }
    }

    private Hl7.Fhir.Model.ResourceReference _Subject;

    /// <summary>
    /// Supporting information
    /// </summary>
    [FhirElement("derivedFrom", InSummary=true, Order=140)]
    [CLSCompliant(false)]
    [References("ServiceRequest","Procedure","Claim","Observation","QuestionnaireResponse","DocumentReference")]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.ResourceReference> DerivedFrom
    {
      get { if(_DerivedFrom==null) _DerivedFrom = new List<Hl7.Fhir.Model.ResourceReference>(); return _DerivedFrom; }
      set { _DerivedFrom = value; OnPropertyChanged("DerivedFrom"); }
    }

    private List<Hl7.Fhir.Model.ResourceReference> _DerivedFrom;

    /// <summary>
    /// The encounter or episode of care that establishes the context for this device use statement
    /// </summary>
    [FhirElement("context", InSummary=true, Order=150)]
    [CLSCompliant(false)]
    [References("Encounter","EpisodeOfCare")]
    [DataMember]
    public Hl7.Fhir.Model.ResourceReference Context
    {
      get { return _Context; }
      set { _Context = value; OnPropertyChanged("Context"); }
    }

    private Hl7.Fhir.Model.ResourceReference _Context;

    /// <summary>
    /// How often  the device was used
    /// </summary>
    [FhirElement("timing", InSummary=true, Order=160, Choice=ChoiceType.DatatypeChoice)]
    [CLSCompliant(false)]
    [AllowedTypes(typeof(Hl7.Fhir.Model.Timing),typeof(Hl7.Fhir.Model.Period),typeof(Hl7.Fhir.Model.FhirDateTime))]
    [DataMember]
    public Hl7.Fhir.Model.DataType Timing
    {
      get { return _Timing; }
      set { _Timing = value; OnPropertyChanged("Timing"); }
    }

    private Hl7.Fhir.Model.DataType _Timing;

    /// <summary>
    /// When the statement was made (and recorded)
    /// </summary>
    [FhirElement("dateAsserted", InSummary=true, Order=170)]
    [DataMember]
    public Hl7.Fhir.Model.FhirDateTime DateAssertedElement
    {
      get { return _DateAssertedElement; }
      set { _DateAssertedElement = value; OnPropertyChanged("DateAssertedElement"); }
    }

    private Hl7.Fhir.Model.FhirDateTime _DateAssertedElement;

    /// <summary>
    /// When the statement was made (and recorded)
    /// </summary>
    /// <remarks>This uses the native .NET datatype, rather than the FHIR equivalent</remarks>
    [IgnoreDataMember]
    public string DateAsserted
    {
      get { return DateAssertedElement != null ? DateAssertedElement.Value : null; }
      set
      {
        if (value == null)
          DateAssertedElement = null;
        else
          DateAssertedElement = new Hl7.Fhir.Model.FhirDateTime(value);
        OnPropertyChanged("DateAsserted");
      }
    }

    /// <summary>
    /// The status of the device usage, for example always, sometimes, never. This is not the same as the status of the statement
    /// </summary>
    [FhirElement("usageStatus", Order=180)]
    [DataMember]
    public Hl7.Fhir.Model.CodeableConcept UsageStatus
    {
      get { return _UsageStatus; }
      set { _UsageStatus = value; OnPropertyChanged("UsageStatus"); }
    }

    private Hl7.Fhir.Model.CodeableConcept _UsageStatus;

    /// <summary>
    /// The reason for asserting the usage status - for example forgot, lost, stolen, broken
    /// </summary>
    [FhirElement("usageReason", Order=190)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.CodeableConcept> UsageReason
    {
      get { if(_UsageReason==null) _UsageReason = new List<Hl7.Fhir.Model.CodeableConcept>(); return _UsageReason; }
      set { _UsageReason = value; OnPropertyChanged("UsageReason"); }
    }

    private List<Hl7.Fhir.Model.CodeableConcept> _UsageReason;

    /// <summary>
    /// Who made the statement
    /// </summary>
    [FhirElement("informationSource", InSummary=true, Order=200)]
    [CLSCompliant(false)]
    [References("Patient","Practitioner","PractitionerRole","RelatedPerson","Organization")]
    [DataMember]
    public Hl7.Fhir.Model.ResourceReference InformationSource
    {
      get { return _InformationSource; }
      set { _InformationSource = value; OnPropertyChanged("InformationSource"); }
    }

    private Hl7.Fhir.Model.ResourceReference _InformationSource;

    /// <summary>
    /// Code or Reference to device used
    /// </summary>
    [FhirElement("device", InSummary=true, Order=210)]
    [Cardinality(Min=1,Max=1)]
    [DataMember]
    public Hl7.Fhir.Model.CodeableReference Device
    {
      get { return _Device; }
      set { _Device = value; OnPropertyChanged("Device"); }
    }

    private Hl7.Fhir.Model.CodeableReference _Device;

    /// <summary>
    /// Why device was used
    /// </summary>
    [FhirElement("reason", InSummary=true, Order=220)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.CodeableReference> Reason
    {
      get { if(_Reason==null) _Reason = new List<Hl7.Fhir.Model.CodeableReference>(); return _Reason; }
      set { _Reason = value; OnPropertyChanged("Reason"); }
    }

    private List<Hl7.Fhir.Model.CodeableReference> _Reason;

    /// <summary>
    /// Target body site
    /// </summary>
    [FhirElement("bodySite", InSummary=true, Order=230)]
    [DataMember]
    public Hl7.Fhir.Model.CodeableReference BodySite
    {
      get { return _BodySite; }
      set { _BodySite = value; OnPropertyChanged("BodySite"); }
    }

    private Hl7.Fhir.Model.CodeableReference _BodySite;

    /// <summary>
    /// Addition details (comments, instructions)
    /// </summary>
    [FhirElement("note", Order=240)]
    [Cardinality(Min=0,Max=-1)]
    [DataMember]
    public List<Hl7.Fhir.Model.Annotation> Note
    {
      get { if(_Note==null) _Note = new List<Hl7.Fhir.Model.Annotation>(); return _Note; }
      set { _Note = value; OnPropertyChanged("Note"); }
    }

    private List<Hl7.Fhir.Model.Annotation> _Note;

    public override IDeepCopyable CopyTo(IDeepCopyable other)
    {
      var dest = other as DeviceUseStatement;

      if (dest == null)
      {
        throw new ArgumentException("Can only copy to an object of the same type", "other");
      }

      base.CopyTo(dest);
      if(Identifier != null) dest.Identifier = new List<Hl7.Fhir.Model.Identifier>(Identifier.DeepCopy());
      if(BasedOn != null) dest.BasedOn = new List<Hl7.Fhir.Model.ResourceReference>(BasedOn.DeepCopy());
      if(StatusElement != null) dest.StatusElement = (Code<Hl7.Fhir.Model.DeviceUseStatement.DeviceUseStatementStatus>)StatusElement.DeepCopy();
      if(Category != null) dest.Category = new List<Hl7.Fhir.Model.CodeableConcept>(Category.DeepCopy());
      if(Subject != null) dest.Subject = (Hl7.Fhir.Model.ResourceReference)Subject.DeepCopy();
      if(DerivedFrom != null) dest.DerivedFrom = new List<Hl7.Fhir.Model.ResourceReference>(DerivedFrom.DeepCopy());
      if(Context != null) dest.Context = (Hl7.Fhir.Model.ResourceReference)Context.DeepCopy();
      if(Timing != null) dest.Timing = (Hl7.Fhir.Model.DataType)Timing.DeepCopy();
      if(DateAssertedElement != null) dest.DateAssertedElement = (Hl7.Fhir.Model.FhirDateTime)DateAssertedElement.DeepCopy();
      if(UsageStatus != null) dest.UsageStatus = (Hl7.Fhir.Model.CodeableConcept)UsageStatus.DeepCopy();
      if(UsageReason != null) dest.UsageReason = new List<Hl7.Fhir.Model.CodeableConcept>(UsageReason.DeepCopy());
      if(InformationSource != null) dest.InformationSource = (Hl7.Fhir.Model.ResourceReference)InformationSource.DeepCopy();
      if(Device != null) dest.Device = (Hl7.Fhir.Model.CodeableReference)Device.DeepCopy();
      if(Reason != null) dest.Reason = new List<Hl7.Fhir.Model.CodeableReference>(Reason.DeepCopy());
      if(BodySite != null) dest.BodySite = (Hl7.Fhir.Model.CodeableReference)BodySite.DeepCopy();
      if(Note != null) dest.Note = new List<Hl7.Fhir.Model.Annotation>(Note.DeepCopy());
      return dest;
    }

    public override IDeepCopyable DeepCopy()
    {
      return CopyTo(new DeviceUseStatement());
    }

    public override bool Matches(IDeepComparable other)
    {
      var otherT = other as DeviceUseStatement;
      if(otherT == null) return false;

      if(!base.Matches(otherT)) return false;
      if( !DeepComparable.Matches(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.Matches(BasedOn, otherT.BasedOn)) return false;
      if( !DeepComparable.Matches(StatusElement, otherT.StatusElement)) return false;
      if( !DeepComparable.Matches(Category, otherT.Category)) return false;
      if( !DeepComparable.Matches(Subject, otherT.Subject)) return false;
      if( !DeepComparable.Matches(DerivedFrom, otherT.DerivedFrom)) return false;
      if( !DeepComparable.Matches(Context, otherT.Context)) return false;
      if( !DeepComparable.Matches(Timing, otherT.Timing)) return false;
      if( !DeepComparable.Matches(DateAssertedElement, otherT.DateAssertedElement)) return false;
      if( !DeepComparable.Matches(UsageStatus, otherT.UsageStatus)) return false;
      if( !DeepComparable.Matches(UsageReason, otherT.UsageReason)) return false;
      if( !DeepComparable.Matches(InformationSource, otherT.InformationSource)) return false;
      if( !DeepComparable.Matches(Device, otherT.Device)) return false;
      if( !DeepComparable.Matches(Reason, otherT.Reason)) return false;
      if( !DeepComparable.Matches(BodySite, otherT.BodySite)) return false;
      if( !DeepComparable.Matches(Note, otherT.Note)) return false;

      return true;
    }

    public override bool IsExactly(IDeepComparable other)
    {
      var otherT = other as DeviceUseStatement;
      if(otherT == null) return false;

      if(!base.IsExactly(otherT)) return false;
      if( !DeepComparable.IsExactly(Identifier, otherT.Identifier)) return false;
      if( !DeepComparable.IsExactly(BasedOn, otherT.BasedOn)) return false;
      if( !DeepComparable.IsExactly(StatusElement, otherT.StatusElement)) return false;
      if( !DeepComparable.IsExactly(Category, otherT.Category)) return false;
      if( !DeepComparable.IsExactly(Subject, otherT.Subject)) return false;
      if( !DeepComparable.IsExactly(DerivedFrom, otherT.DerivedFrom)) return false;
      if( !DeepComparable.IsExactly(Context, otherT.Context)) return false;
      if( !DeepComparable.IsExactly(Timing, otherT.Timing)) return false;
      if( !DeepComparable.IsExactly(DateAssertedElement, otherT.DateAssertedElement)) return false;
      if( !DeepComparable.IsExactly(UsageStatus, otherT.UsageStatus)) return false;
      if( !DeepComparable.IsExactly(UsageReason, otherT.UsageReason)) return false;
      if( !DeepComparable.IsExactly(InformationSource, otherT.InformationSource)) return false;
      if( !DeepComparable.IsExactly(Device, otherT.Device)) return false;
      if( !DeepComparable.IsExactly(Reason, otherT.Reason)) return false;
      if( !DeepComparable.IsExactly(BodySite, otherT.BodySite)) return false;
      if( !DeepComparable.IsExactly(Note, otherT.Note)) return false;

      return true;
    }

    [IgnoreDataMember]
    public override IEnumerable<Base> Children
    {
      get
      {
        foreach (var item in base.Children) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return elem; }
        foreach (var elem in BasedOn) { if (elem != null) yield return elem; }
        if (StatusElement != null) yield return StatusElement;
        foreach (var elem in Category) { if (elem != null) yield return elem; }
        if (Subject != null) yield return Subject;
        foreach (var elem in DerivedFrom) { if (elem != null) yield return elem; }
        if (Context != null) yield return Context;
        if (Timing != null) yield return Timing;
        if (DateAssertedElement != null) yield return DateAssertedElement;
        if (UsageStatus != null) yield return UsageStatus;
        foreach (var elem in UsageReason) { if (elem != null) yield return elem; }
        if (InformationSource != null) yield return InformationSource;
        if (Device != null) yield return Device;
        foreach (var elem in Reason) { if (elem != null) yield return elem; }
        if (BodySite != null) yield return BodySite;
        foreach (var elem in Note) { if (elem != null) yield return elem; }
      }
    }

    [IgnoreDataMember]
    public override IEnumerable<ElementValue> NamedChildren
    {
      get
      {
        foreach (var item in base.NamedChildren) yield return item;
        foreach (var elem in Identifier) { if (elem != null) yield return new ElementValue("identifier", elem); }
        foreach (var elem in BasedOn) { if (elem != null) yield return new ElementValue("basedOn", elem); }
        if (StatusElement != null) yield return new ElementValue("status", StatusElement);
        foreach (var elem in Category) { if (elem != null) yield return new ElementValue("category", elem); }
        if (Subject != null) yield return new ElementValue("subject", Subject);
        foreach (var elem in DerivedFrom) { if (elem != null) yield return new ElementValue("derivedFrom", elem); }
        if (Context != null) yield return new ElementValue("context", Context);
        if (Timing != null) yield return new ElementValue("timing", Timing);
        if (DateAssertedElement != null) yield return new ElementValue("dateAsserted", DateAssertedElement);
        if (UsageStatus != null) yield return new ElementValue("usageStatus", UsageStatus);
        foreach (var elem in UsageReason) { if (elem != null) yield return new ElementValue("usageReason", elem); }
        if (InformationSource != null) yield return new ElementValue("informationSource", InformationSource);
        if (Device != null) yield return new ElementValue("device", Device);
        foreach (var elem in Reason) { if (elem != null) yield return new ElementValue("reason", elem); }
        if (BodySite != null) yield return new ElementValue("bodySite", BodySite);
        foreach (var elem in Note) { if (elem != null) yield return new ElementValue("note", elem); }
      }
    }

  }

}

// end of file
