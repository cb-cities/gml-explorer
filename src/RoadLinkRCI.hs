--module RoadLinks
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


--data RoadLink = RL
--    { rlIndex        :: Int
--    , rlTOID         :: Text
--    , rlTerm         :: Maybe Text
--    , rlNature       :: Maybe Text
--    , rlPolyline     :: Maybe [Double]
--    , rlNegativeNode :: Maybe Text
--    , rlPositiveNode :: Maybe Text
--    }
--  deriving (Eq, Ord, Show)

data RoadLinkRCI = RLR
    { rlrIndex        :: Int
    , rlrTOID         :: Text
    , rlrTerm         :: Maybe Text
    , rlrNature       :: Maybe Text
    , rlrPolyline     :: Maybe [Double]
    , rlrNegativeNode :: Maybe Text
    , rlrPositiveNode :: Maybe Text
    , rlrGID          :: Maybe Text
    , rlrLV3          :: Maybe [Double]
    }
  deriving (Eq, Ord, Show)


--instance ToJSON RoadLink where
--  toJSON RL{..} =
--      J.object
--        [ "index"        .= rlIndex
--        , "toid"         .= rlTOID
--        , "term"         .= rlTerm
--        , "nature"       .= rlNature
--        , "polyline"     .= rlPolyline
--        , "negativeNode" .= rlNegativeNode
--        , "positiveNode" .= rlPositiveNode
--        ]

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
        , "gid"          .= rlrGID
        , "lv3"          .= rlrLV3
        ]


--newRL :: Int -> Text -> RoadLink
--newRL index toid = RL
--    { rlIndex        = index
--    , rlTOID         = toid
--    , rlTerm         = Nothing
--    , rlNature       = Nothing
--    , rlPolyline     = Nothing
--    , rlNegativeNode = Nothing
--    , rlPositiveNode = Nothing
--    }

newRLR :: Int -> Text -> RoadLinkRCI
newRLR index toid = RLR
    { rlrIndex        = index
    , rlrTOID         = toid
    , rlrTerm         = Nothing
    , rlrNature       = Nothing
    , rlrPolyline     = Nothing
    , rlrNegativeNode = Nothing
    , rlrPositiveNode = Nothing
    , rlrGID          = Nothing
    , rlrLV3          = Nothing
    }


--validRL :: RoadLink -> Bool
--validRL RL{..} =
--       rlTerm         /= Nothing
--    && rlNature       /= Nothing
--    && rlPolyline     /= Nothing && l >= 4 && l `mod` 2 == 0
--    && rlNegativeNode /= Nothing
--    && rlPositiveNode /= Nothing
--  where
--    l = length (fromJust rlPolyline)

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
root index _ =
    await (root index)
-- No change


--networkMember :: Int -> Transition
--networkMember index (EndElement "osgb:networkMember") =
--    await (root index)
--networkMember index (StartElement "osgb:RoadLink" attrs) =
--    await (roadLink (newRL index (getTOID attrs)))
--networkMember index _ =
--    await (networkMember index)

networkMember :: Int -> Transition
networkMember index (EndElement "osgb:networkMember") =
    await (root index)
networkMember index (StartElement "osgb:RoadLink" attrs) =
    await (roadLink (newRLR index (getTOID attrs)))
networkMember index _ =
    await (networkMember index)


--roadLink :: RoadLink -> Transition
--roadLink rl (EndElement "osgb:RoadLink")
--  | validRL rl =
--        yield rl (networkMember (rlIndex rl + 1))
--  | otherwise =
--        error ("roadLink: invalid osgb:RoadLink: " ++ show rl)
--roadLink rl (StartElement "osgb:descriptiveTerm" _)
--  | rlTerm rl == Nothing =
--        await (term none rl)
--  | otherwise =
--        error "roadLink: expected 1 osgb:descriptiveTerm"
--roadLink rl (StartElement "osgb:natureOfRoad" _)
--  | rlNature rl == Nothing =
--        await (nature none rl)
--  | otherwise =
--        error "roadLink: expected 1 osgb:natureOfRoad"
--roadLink rl (StartElement "osgb:polyline" _)
--  | rlPolyline rl == Nothing =
--        await (polyline rl)
--  | otherwise =
--        error "roadLink: expected 1 osgb:polyline"
--roadLink rl (StartElement "osgb:directedNode" attrs) =
--    case (rlNegativeNode rl, rlPositiveNode rl, getDirectedNode attrs) of
--      (Nothing, _, Left nn) ->
--        await (roadLink rl {rlNegativeNode = Just nn})
--      (_, Nothing, Right pn) ->
--        await (roadLink rl {rlPositiveNode = Just pn})
--      _ ->
--        error "roadLink: expected 2 osgb:directedNode"
--roadLink rl _ =
--    await (roadLink rl)

roadLink :: RoadLinkRCI -> Transition
roadLink rlr (EndElement "osgb:RoadLink")
  | validRLR rlr =
        yield rlr (networkMember (rlrIndex rlr + 1))
  | otherwise =
        error ("roadLink: invalid osgb:RoadLink: " ++ show rlr)
roadLink rlr (StartElement "osgb:descriptiveTerm" _)
  | rlrTerm rlr == Nothing =
        await (term none rlr)
  | otherwise =
        error "roadLink: expected 1 osgb:descriptiveTerm"
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
roadLink rlr (StartElement "ogr:gid" _)
  | rlrGID rlr == Nothing =
        await (gid none rlr)
  | otherwise =
        error "roadLink: expect 1 ogr:gid"
roadLink rlr _ =
    await (roadLink rlr)

gid :: Builder -> RoadLinkRCI -> Transition
gid parts rlr (EndElement "ogr:gid") =
  await (roadLink rlr {rlrGID = Just (build parts)})
gid parts rlr (CharacterData part) =
  await (gid (parts <> part) rlr)
gid parts rlr _ =
  await (gid parts rlr)

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
