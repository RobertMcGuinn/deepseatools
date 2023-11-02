##### Header #####
## author: Robert McGuinn
## startdate: 20231024
## purpose: darwin core event core centric tables

## Event table
c(eventID,
  parentEventID,
  eventType,
  ## [initiative: cruise: survey: subsurvey: video: stillimage: sample: subsample]
  ## only EventType = c(video, stillimage, sample, subsample) can produce artifacts
  ## The occurrence table is just a
  eventDate,
  eventTime,
  eventRemarks,
  samplingProtocol,
  institutionID)

## Location table
  c(
    locationID, ## H3
    footprintWKT,## should be polygons
    footprintSRS,
    coordinateUncertaintyInMeters,
    minimumDepthInMeters,
    maximumDepthInMeters)

## Occurrence table
c(eventID,
  ## eventType MUST result in an artifact (image or sample)
  ## we are inside of an event that led to the artifact
  occurrenceID,
  ## occurrenceID must be carefully defined and bounded
  ## example: In the case of a still image we define a single 'occurrence'
  ## as all of individuals or occurrences of a single taxon within the
  ## viewing frame.This observational framing is key.
  ## The rules are different for video, depending on whether
  ## you break the video into discrete segments, that you then
  ## annotate in turn, or you only analyze a subset of stills
  ## taken from the video.  In the latter case you are dealing with
  ## a particular artifact from the EventType [video]. Video can
  ## involve large swaths of space and time.  It may also be very
  ## difficult to compute the view frame of the video without
  ## specialized positional information from the video platform (ROV/TowCam/Lander).
  ## The footPrint WKT polygon for a video segment may be hard to get accurate,
  ## depending on how the survey was conducted.

  taxonID,
  scientificName,
  basisOfRecord,
  individualCount,
  ## sample related
  materialEntityID,
  preparations,
  disposition,
  verbatimLabel,
  associatedSequences,
  materialEntityRemarks,
  )

## MeasurementOrFact table
c(measurementID, # unique
  occurrenceID,
  measurementType,
  measurementValue,
  measurementAccuracy,
  mesasurementUnit,
  measurementDeterminedBy,
  measurementDeterminedDate,
  measurementMethod,
  measurementRemarks)










