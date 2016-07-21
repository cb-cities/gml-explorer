module RoadLinkRCI
  ( root
  ) where

import Data.Aeson ((.=), ToJSON, toJSON)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Text.XML.Expat.SAX (SAXEvent(..))
import qualified Data.Aeson as J

import Attributes
import MealyMachine
import Toolkit


data RoadLinkRCI = RLR
    { rlrIndex        :: Int
    , rlrTOID         :: Text
    , rlrTerm         :: Maybe Text
    , rlrNature       :: Maybe Text
    , rlrPolyline     :: Maybe [Double]
    , rlrNegativeNode :: Maybe Text
    , rlrPositiveNode :: Maybe Text

    --Added for RCI
    , rlrRoadID       :: Maybe Int
    , rlrStCha        :: Maybe Double
    , rlrEndCh        :: Maybe Double
    , rlrRCI          :: Maybe Double
    , rlrLV3          :: Maybe Double
    , rlrLV10         :: Maybe Double
    , rlrLLRT         :: Maybe Double
    , rlrLRRT         :: Maybe Double
    , rlrLTRC         :: Maybe Double
    , rlrLLTX         :: Maybe Double
    , rlrLCRV         :: Maybe Double
    , rlrLLRD         :: Maybe Double
    , rlrLRRD         :: Maybe Double
    }
  deriving (Eq, Ord, Show)


instance ToJSON RoadLinkRCI where
  toJSON RLR{..} =
      J.object
        [ "index"        .= rlrIndex
        , "toid"         .= rlrTOID
        , "term"         .= rlrTerm
        , "nature"       .= rlrNature
        , "polyline"     .= rlrPolyline
        , "negativeNode" .= rlrNegativeNode
        , "positiveNode" .= rlrPositiveNode

        --Below are RCI related outputs
        , "roadid"       .= rlrRoadID
        , "stcha"        .= rlrStCha
        , "endch"        .= rlrEndCh
        , "rci"          .= rlrRCI
        , "lv3"          .= rlrLV3
        , "lv10"         .= rlrLV10
        , "llrt"         .= rlrLLRT
        , "lrrt"         .= rlrLRRT
        , "ltrc"         .= rlrLTRC
        , "lltx"         .= rlrLLTX
        , "lcrv"         .= rlrLCRV
        , "llrd"         .= rlrLLRD
        , "lrrd"         .= rlrLRRD
        ]


newRLR :: Int -> Text -> RoadLinkRCI
newRLR index toid = RLR
    { rlrIndex        = index
    , rlrTOID         = toid
    , rlrTerm         = Nothing
    , rlrNature       = Nothing
    , rlrPolyline     = Nothing
    , rlrNegativeNode = Nothing
    , rlrPositiveNode = Nothing

    --Below are RCI related information
    , rlrRoadID       = Nothing
    , rlrStCha        = Nothing
    , rlrEndCh        = Nothing
    , rlrRCI          = Nothing
    , rlrLV3          = Nothing
    , rlrLV10         = Nothing
    , rlrLLRT         = Nothing
    , rlrLRRT         = Nothing
    , rlrLTRC         = Nothing
    , rlrLLTX         = Nothing
    , rlrLCRV         = Nothing
    , rlrLLRD         = Nothing
    , rlrLRRD         = Nothing
    }


validRLR :: RoadLinkRCI -> Bool
validRLR RLR{..} =
       rlrTerm         /= Nothing
    && rlrNature       /= Nothing
    && rlrPolyline     /= Nothing && l >= 4 && l `mod` 2 == 0
    && rlrNegativeNode /= Nothing
    && rlrPositiveNode /= Nothing
  where
    l = length (fromJust rlrPolyline)


root :: Int -> Transition
root index (StartElement "osgb:networkMember" _) =
    await (networkMember index)
    -- returns the order number of the <osgb:networkMember> element
root index _ =
    await (root index)
-- No change


networkMember :: Int -> Transition
networkMember index (EndElement "osgb:networkMember") =
    await (root index)
networkMember index (StartElement "osgb:RoadLink" attrs) =
    await (roadLink (newRLR index (getTOID attrs)))
networkMember index _ =
    await (networkMember index)


roadLink :: RoadLinkRCI -> Transition
roadLink rlr (EndElement "osgb:RoadLink")
  | validRLR rlr =
        yield rlr (networkMember (rlrIndex rlr + 1))
  | otherwise =
        error ("roadLink: invalid osgb:RoadLink: " ++ show rlr)
roadLink rlr (StartElement "osgb:descriptiveTerm" _)
  | rlrTerm rlr == Nothing =
        await (term none rlr)
  | otherwise = --means rlrTerm rlr is not Nothing, e.g., there are two <osgb:descriptive Term> elements
        error "roadLink: expected 1 osgb:descriptiveTerm" --other than more than 1
roadLink rlr (StartElement "osgb:natureOfRoad" _)
  | rlrNature rlr == Nothing =
        await (nature none rlr)
  | otherwise =
        error "roadLink: expected 1 osgb:natureOfRoad"
roadLink rlr (StartElement "osgb:polyline" _)
  | rlrPolyline rlr == Nothing =
        await (polyline rlr)
  | otherwise =
        error "roadLink: expected 1 osgb:polyline"
roadLink rlr (StartElement "osgb:directedNode" attrs) =
    case (rlrNegativeNode rlr, rlrPositiveNode rlr, getDirectedNode attrs) of
      (Nothing, _, Left nn) ->
        await (roadLink rlr {rlrNegativeNode = Just nn})
      (_, Nothing, Right pn) ->
        await (roadLink rlr {rlrPositiveNode = Just pn})
      _ ->
        error "roadLink: expected 2 osgb:directedNode"

--Below are for parsing RCI related elements
roadLink rlr (StartElement "ogr:road_id" _)
  | rlrRoadID rlr == Nothing =
        await (roadid none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:road_id for this link in total"
roadLink rlr (StartElement "ogr:st_cha" _)
  | rlrStCha rlr == Nothing =
        await (stcha none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:st_cha for this link in total"
roadLink rlr (StartElement "ogr:end_ch" _)
  | rlrEndCh rlr == Nothing =
        await (endch none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:end_ch for this link in total"
roadLink rlr (StartElement "ogr:rci" _)
  | rlrRCI rlr == Nothing =
        await (rci none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:rci for this link in total"
roadLink rlr (StartElement "ogr:lv3" _)
  | rlrLV3 rlr == Nothing =
        await (lv3 none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lv3 for this link in total"
roadLink rlr (StartElement "ogr:lv10" _)
  | rlrLV10 rlr == Nothing =
        await (lv10 none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lv10 for this link in total"
roadLink rlr (StartElement "ogr:llrt" _)
  | rlrLLRT rlr == Nothing =
        await (llrt none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:llrt for this link in total"
roadLink rlr (StartElement "ogr:lrrt" _)
  | rlrLRRT rlr == Nothing =
        await (lrrt none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lrrt for this link in total"
roadLink rlr (StartElement "ogr:ltrc" _)
  | rlrLTRC rlr == Nothing =
        await (ltrc none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:ltrc for this link in total"
roadLink rlr (StartElement "ogr:lltx" _)
  | rlrLLTX rlr == Nothing =
        await (lltx none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lltx for this link in total"
roadLink rlr (StartElement "ogr:lcrv" _)
  | rlrLCRV rlr == Nothing =
        await (lcrv none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lcrv for this link in total"
roadLink rlr (StartElement "ogr:llrd" _)
  | rlrLLRD rlr == Nothing =
        await (llrd none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:llrd for this link in total"
roadLink rlr (StartElement "ogr:lrrd" _)
  | rlrLRRD rlr == Nothing =
        await (lrrd none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:lrrd for this link in total"
roadLink rlr _ =
    await (roadLink rlr)


term :: Builder -> RoadLinkRCI -> Transition
term parts rlr (EndElement "osgb:descriptiveTerm") =
    await (roadLink rlr {rlrTerm = Just (build parts)})
term parts rlr (CharacterData part) =
    await (term (parts <> part) rlr)
term parts rlr _ =
    await (term parts rlr)


nature :: Builder -> RoadLinkRCI -> Transition
nature parts rlr (EndElement "osgb:natureOfRoad") =
    await (roadLink rlr {rlrNature = Just (build parts)})
nature parts rlr (CharacterData part) =
    await (nature (parts <> part) rlr)
nature parts rlr _ =
    await (nature parts rlr)


polyline :: RoadLinkRCI -> Transition
polyline rlr (EndElement "osgb:polyline") =
    await (roadLink rlr)
polyline rlr (StartElement "gml:LineString" _)
  | rlrPolyline rlr == Nothing =
        await (lineString rlr)
  | otherwise =
        error "polyline: expected 1 gml:LineString"
polyline rlr _ =
    await (polyline rlr)


lineString :: RoadLinkRCI -> Transition
lineString rlr (EndElement "gml:LineString") =
    await (polyline rlr)
lineString rlr (StartElement "gml:coordinates" _)
  | rlrPolyline rlr == Nothing =
        await (coordinates none rlr)
  | otherwise =
        error "lineString: expected 1 gml:coordinates"
lineString rlr _ =
    await (lineString rlr)


coordinates :: Builder -> RoadLinkRCI -> Transition
coordinates parts rlr (EndElement "gml:coordinates") =
    await (lineString rlr {rlrPolyline = Just (decodePolyline (build parts))})
coordinates parts rlr (CharacterData part) =
    await (coordinates (parts <> part) rlr)
coordinates parts rlr _ =
    await (coordinates parts rlr)


--Below are RCI related functions
roadid :: Builder -> RoadLinkRCI -> Transition
roadid parts rlr (EndElement "ogr:road_id") =
  await (roadLink rlr {rlrRoadID = Just (decodeInt (build parts))})
roadid parts rlr (CharacterData part) =
  await (roadid (parts <> part) rlr)
roadid parts rlr _ =
  await (roadid parts rlr)

stcha :: Builder -> RoadLinkRCI -> Transition
stcha parts rlr (EndElement "ogr:st_cha") =
  await (roadLink rlr {rlrStCha = Just (decodeDouble (build parts))})
stcha parts rlr (CharacterData part) =
  await (stcha (parts <> part) rlr)
stcha parts rlr _ =
  await (stcha parts rlr)

end_ch :: Builder -> RoadLinkRCI -> Transition
end_ch parts rlr (EndElement "ogr:end_ch") =
  await (roadLink rlr {rlrEndCh = Just (decodeDouble (build parts))})
end_ch parts rlr (CharacterData part) =
  await (end_ch (parts <> part) rlr)
end_ch parts rlr _ =
  await (end_ch parts rlr)

rci :: Builder -> RoadLinkRCI -> Transition
rci parts rlr (EndElement "ogr:rci") =
  await (roadLink rlr {rlrRCI = Just (decodeDouble (build parts))})
rci parts rlr (CharacterData part) =
  await (rci (parts <> part) rlr)
rci parts rlr _ =
  await (rci parts rlr)

lv3 :: Builder -> RoadLinkRCI -> Transition
lv3 parts rlr (EndElement "ogr:lv3") =
  await (roadLink rlr {rlrLV3 = Just (decodeDouble (build parts))})
lv3 parts rlr (CharacterData part) =
  await (lv3 (parts <> part) rlr)
lv3 parts rlr _ =
  await (lv3 parts rlr)


lv10 :: Builder -> RoadLinkRCI -> Transition
lv10 parts rlr (EndElement "ogr:lv10") =
  await (roadLink rlr {rlrLV10 = Just (decodeDouble (build parts))})
lv10 parts rlr (CharacterData part) =
  await (lv10 (parts <> part) rlr)
lv10 parts rlr _ =
  await (lv10 parts rlr)


llrt :: Builder -> RoadLinkRCI -> Transition
llrt parts rlr (EndElement "ogr:llrt") =
  await (roadLink rlr {rlrLLRT = Just (decodeDouble (build parts))})
llrt parts rlr (CharacterData part) =
  await (llrt (parts <> part) rlr)
llrt parts rlr _ =
  await (llrt parts rlr)


lrrt :: Builder -> RoadLinkRCI -> Transition
lrrt parts rlr (EndElement "ogr:lrrt") =
  await (roadLink rlr {rlrLRRT = Just (decodeDouble (build parts))})
lrrt parts rlr (CharacterData part) =
  await (lrrt (parts <> part) rlr)
lrrt parts rlr _ =
  await (lrrt parts rlr)


ltrc :: Builder -> RoadLinkRCI -> Transition
ltrc parts rlr (EndElement "ogr:ltrc") =
  await (roadLink rlr {rlrLTRC = Just (decodeDouble (build parts))})
ltrc parts rlr (CharacterData part) =
  await (ltrc (parts <> part) rlr)
ltrc parts rlr _ =
  await (ltrc parts rlr)


lltx :: Builder -> RoadLinkRCI -> Transition
lltx parts rlr (EndElement "ogr:lltx") =
  await (roadLink rlr {rlrLLTX = Just (decodeDouble (build parts))})
lltx parts rlr (CharacterData part) =
  await (lltx (parts <> part) rlr)
lltx parts rlr _ =
  await (lltx parts rlr)

lcrv :: Builder -> RoadLinkRCI -> Transition
lcrv parts rlr (EndElement "ogr:lcrv") =
  await (roadLink rlr {rlrLCRV = Just (decodeDouble (build parts))})
lcrv parts rlr (CharacterData part) =
  await (lcrv (parts <> part) rlr)
lcrv parts rlr _ =
  await (lcrv parts rlr)


llrd :: Builder -> RoadLinkRCI -> Transition
llrd parts rlr (EndElement "ogr:llrd") =
  await (roadLink rlr {rlrLLRD = Just (decodeDouble (build parts))})
llrd parts rlr (CharacterData part) =
  await (llrd (parts <> part) rlr)
llrd parts rlr _ =
  await (llrd parts rlr)


lrrd :: Builder -> RoadLinkRCI -> Transition
lrrd parts rlr (EndElement "ogr:lrrd") =
  await (roadLink rlr {rlrLRRD = Just (decodeDouble (build parts))})
lrrd parts rlr (CharacterData part) =
  await (lrrd (parts <> part) rlr)
lrrd parts rlr _ =
  await (lrrd parts rlr)