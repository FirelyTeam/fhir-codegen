// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Whether the media is a photo, video, or audio
  /// </summary>
  public static class DigitalMediaTypeCodes
  {
    /// <summary>
    /// The media consists of a sound recording
    /// </summary>
    public static readonly Coding Audio = new Coding
    {
      Code = "audio",
      Display = "Audio",
      System = "http://hl7.org/fhir/digital-media-type"
    };
    /// <summary>
    /// The media consists of one or more unmoving images, including photographs, computer-generated graphs and charts, and scanned documents
    /// </summary>
    public static readonly Coding Photo = new Coding
    {
      Code = "photo",
      Display = "Photo",
      System = "http://hl7.org/fhir/digital-media-type"
    };
    /// <summary>
    /// The media consists of a series of frames that capture a moving image
    /// </summary>
    public static readonly Coding Video = new Coding
    {
      Code = "video",
      Display = "Video",
      System = "http://hl7.org/fhir/digital-media-type"
    };

    /// <summary>
    /// Literal for code: Audio
    /// </summary>
    public const string LiteralAudio = "audio";

    /// <summary>
    /// Literal for code: DigitalMediaTypeAudio
    /// </summary>
    public const string LiteralDigitalMediaTypeAudio = "http://hl7.org/fhir/digital-media-type#audio";

    /// <summary>
    /// Literal for code: Photo
    /// </summary>
    public const string LiteralPhoto = "photo";

    /// <summary>
    /// Literal for code: DigitalMediaTypePhoto
    /// </summary>
    public const string LiteralDigitalMediaTypePhoto = "http://hl7.org/fhir/digital-media-type#photo";

    /// <summary>
    /// Literal for code: Video
    /// </summary>
    public const string LiteralVideo = "video";

    /// <summary>
    /// Literal for code: DigitalMediaTypeVideo
    /// </summary>
    public const string LiteralDigitalMediaTypeVideo = "http://hl7.org/fhir/digital-media-type#video";

    /// <summary>
    /// Dictionary for looking up DigitalMediaType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "audio", Audio }, 
      { "http://hl7.org/fhir/digital-media-type#audio", Audio }, 
      { "photo", Photo }, 
      { "http://hl7.org/fhir/digital-media-type#photo", Photo }, 
      { "video", Video }, 
      { "http://hl7.org/fhir/digital-media-type#video", Video }, 
    };
  };
}
