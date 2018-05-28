% Neutropenia
% Dominic Steinitz
% 21st May 2018

Whatever
========

We describe the drug absorption with the following differential equations:

$$ \frac{dy_{\mathrm{gut}}}{dt} = -k_a y_{\mathrm{gut}} \\
\frac{dy_{\mathrm{central}}}{dt} = k_a y_{\mathrm{gut}} - (\frac{CL}{V_{\mathrm{central}}} + \frac{Q}{V_{\mathrm{central}}}) y_{\mathrm{central}} +  \frac{Q}{V_{\mathrm{peripheral}}} y_{\mathrm{peripheral}} \\
\frac{dy_{\mathrm{peripheral}}}{dt} = \frac{Q}{V_{\mathrm{central}}} y_{\mathrm{central}} - \frac{Q}{V_{\mathrm{peripheral}}} y_{\mathrm{peripheral}} $$

with
$y_{\mathrm{gut}}$ : the drug amount in the gut (mg)
$y_{\mathrm{central}}$ : the drug amount in the central compartment (mg)
$y_{\mathrm{peripheral}}$ : the drug amount in the peripheral compartment (mg)
$k_a$ : the rate constant at which the drug flows from the gut to the central compartment ($h^{-1}$)
$Q$ : the clearance at which the drug flows back and forth between the central and the peripheral compartment (L/h)
$CL$ : the clearance at which the drug is cleared from the central compartment (L/h)
$V_{\mathrm{central}}$ : the volume of the central compartment (L)
$V_{\mathrm{peripheral}}$ : the volume of the peripheral compartment (L)

The data we fit our model to is the drug concentration in the blood, which our model treats as the concentration in the central compartment, and is given by:

$$ c = \frac{y_{\mathrm{central}}}{V_{\mathrm{central}}} $$

and the parameters we wish to estimate are $k_a$, $Q$, $CL$, $V_{\mathrm{central}}$, and $V_{\mathrm{peripheral}}$.


> {-# OPTIONS_GHC -Wall          #-}
> {-# LANGUAGE TypeFamilies      #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE DataKinds         #-}
> {-# LANGUAGE TypeOperators     #-}
> {-# LANGUAGE QuasiQuotes       #-}

> module Neutropenia where

> import qualified Prelude as P


> import qualified Language.R as R
> import Language.R (R)
> import Language.R.QQ

> import Numeric.Units.Dimensional.Prelude hiding (Unit)
> import Numeric.Units.Dimensional

> import Numeric.LinearAlgebra hiding (R)
> import Numeric.Integration.TanhSinh

> import Control.Monad.Writer
> import Control.Monad.Loops

> neutObs :: R s Double
> neutObs = R.dynSEXP <$> [r| nData$neutObs |]


> -- foo :: IO ()
> foo = do
>   es <- R.runRegion $ do
>     [r| library(rstan) |]
>     nData <- [r| read_rdump("data/neutropenia.data.R") |]
>     let eRs :: R s [Double]
>         eRs = R.dynSEXP <$> [r| nData_hs$neutObs |]
>     eRs
>   return es

> mttR :: IO (Time Double)
> mttR = do
>   es <- R.runRegion $ do
>     [r| library(rstan) |]
>     nData <- [r| read_rdump("data/neutropenia.data.R") |]
>     let eRs :: R s Double
>         eRs = R.dynSEXP <$> [r| nData_hs$mttPrior |]
>     eRs
>   return (es *~ hour)

$Q$ : the clearance at which the drug flows back and forth between the
central and the peripheral compartment (L/h)

> qR :: IO (Quantity (DVolume / DTime) Double)
> qR = do
>   es <- R.runRegion $ do
>     [r| library(rstan) |]
>     nData <- [r| read_rdump("data/neutropenia.data.R") |]
>     let eRs :: R s Double
>         eRs = R.dynSEXP <$> [r| nData_hs$QPrior |]
>     eRs
>   return (es *~ (litre / hour))

$CL$ : the clearance at which the drug is cleared from the central
compartment (L/h)

> clR :: IO (Quantity (DVolume / DTime) Double)
> clR = do
>   es <- R.runRegion $ do
>     [r| library(rstan) |]
>     nData <- [r| read_rdump("data/neutropenia.data.R") |]
>     let eRs :: R s Double
>         eRs = R.dynSEXP <$> [r| nData_hs$CLPrior |]
>     eRs
>   return (es *~ (litre / hour))

$k_a$ : the rate constant at which the drug flows from the gut to the
central compartment ($h^{-1}$)

> data Parms = Parms { ka :: Frequency Double
>                    , cl :: Quantity (DVolume / DTime) Double
>                    , v1 :: Volume Double
>                    , q  :: Quantity (DVolume / DTime) Double
>                    , v2 :: Volume Double
>                    }

> parms :: IO Parms
> parms = do
>   ps <- R.runRegion $ do
>      qs <- [r| set.seed(42)
>                list(CL = exp(rnorm(1, log(10), 0.2)),
>                     Q = exp(rnorm(1, log(20), 0.2)),
>                     V1 = exp(rnorm(1, log(70), 0.2)),
>                     V2 = exp(rnorm(1, log(70), 0.2)),
>                     ka = exp(rnorm(1, log(1), 0.2)),
>                     sigma = runif(1, 0.5, 2),
>                     alpha = exp(rnorm(1, log(2E-3), 0.2)),
>                     mtt = exp(rnorm(1, log(125), 0.2)),
>                     circ0 = exp(rnorm(1, 5, 0.2)),
>                     gamma = exp(rnorm(1, 0.17, 0.2)),
>                     sigmaNeut = runif(1, 0.5, 2)) |]
>      let eRs :: R s [Double]
>          eRs = R.dynSEXP <$> [r| qs_hs |]
>      eRs
>   return $ Parms { ka = (ps!!4) *~ (one / hour)
>                  , cl = (ps!!0) *~ (litre / hour)
>                  , v1 = (ps!!2) *~ litre
>                  , q  = (ps!!1) *~ (litre / hour)
>                  , v2 = (ps!!3) *~ litre
>                  }

> dxdt :: Parms -> Double -> Vector Double -> Vector Double
> dxdt parms t x = undefined
>   where
>     foo :: [Double]
>     foo = [ -ka' P.* (x!0)
>           ,  ka' P.* (x!0) P.- (k10 P.+ k12) P.* (x!1) P.+ k21 P.* (x!2)
>           ]
>     ka', k10, k12 :: Double
>     ka' = ka parms /~ (one / hour)
>     k10 = (cl parms / v1 parms) /~ (one / hour)
>     k12 = (q parms / v1 parms) /~ (one / hour)
>     k21 = (q parms / v2 parms) /~ (one / hour)

References
==========
