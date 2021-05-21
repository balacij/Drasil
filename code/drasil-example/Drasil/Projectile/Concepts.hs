module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed, motion, distance, iSpeed, time,
  rectilinear, velocity)

concepts :: [IdeaDict]
concepts = nw projMotion : map nw [landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC,
  rectVel] ++ map nw defs

durationNC, flightDurNC, landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC,
  rectVel :: NamedChunk
durationNC   = nc "duration" (nounPhraseSP "duration")
launchNC     = nc "launch"   (nounPhraseSP "launch")
offsetNC     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phrase targetPosNC `S.andThe` phrase landingPosNC)

flightDurNC   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) durationNC
landingPosNC  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngleNC = compoundNC launchNC angle
launchSpeedNC = compoundNC launchNC speed
targetPosNC   = compoundNC target position
rectVel       = compoundNC rectilinear velocity

projMotion :: NamedChunk
projMotion = compoundNC projectile motion
---

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

launcher, projectile, target :: ConceptChunk
launcher   = dcc "launcher"   (nounPhraseSP "launcher")  ("where the projectile is launched from " ++
                                                          "and the device that does the launching")
projectile = dcc "projectile" (nounPhraseSP "projectile") "the object to be launched at the target"
target     = dcc "target"     (nounPhraseSP "target")     "where the projectile should be launched to"

landPos, launAngle, launSpeed, offset, targPos, flightDur :: ConceptChunk
landPos = cc' landingPosNC
  (foldlSent_ [phraseNP (the distance) `S.fromThe` phrase launcher, S "to",
            (S "final" +:+ phrase position) `S.the_ofThe` phrase projectile])

launAngle = cc' launchAngleNC
  (foldlSent_ [phraseNP (the angle), S "between the", phrase launcher `S.and_` S "a straight line"
             `S.fromThe` phrase launcher `S.toThe` phrase target])

launSpeed = cc' launchSpeedNC (phrase iSpeed `S.the_ofThe` phrase projectile +:+ S "when launched")
offset = cc' offsetNC (S "the offset between the" +:+ phrase targetPosNC `S.andThe` phrase landingPosNC)
targPos = cc' targetPosNC (phraseNP (the distance) `S.fromThe` phrase launcher `S.toThe` phrase target)
flightDur = cc' flightDurNC (foldlSent_ [phraseNP (the time), S "when the", phrase projectile, S "lands"])

