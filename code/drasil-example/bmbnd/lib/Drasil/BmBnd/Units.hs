module Drasil.BmBnd.Units where

import Language.Drasil
import Data.Drasil.SI_Units

units :: [UnitDefn]
units = [forcePerMetreSqd, forcePerMetreCubed, forcePerMetreQtc]

forcePerMetreSqd :: UnitDefn
forcePerMetreSqd = newUnit "force per metre squared" $ newton /: m_2

forcePerMetreCubed :: UnitDefn
forcePerMetreCubed = newUnit "force per metre cubed" $ newton /: m_3

forcePerMetreQtc :: UnitDefn
forcePerMetreQtc = newUnit "force per quartic metre" $ newton /: m_4
