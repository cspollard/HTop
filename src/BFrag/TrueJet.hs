{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module BFrag.TrueJet
  ( TrueParticle(..), BHadron(..), TrueJet(..)
  , HasPID(..)
  , neutral, charged, threeCharge
  , tjPVFiducialConstits, tjBHadrons
  , bhTP, trueProbeJet
  , readTrueJets, bhFiducialChildren
  , trueBJet, bMesonH, bBaryonH
  ) where

import           Atlas
import           BFrag.BFrag
import           BFrag.PtEtaPhiE
import           Control.Applicative (ZipList (..))
import           Control.Lens
import           Data.HEP.PID
import           Data.TTree
import qualified Data.Vector         as V
import           GHC.Float
import           GHC.Generics        (Generic)


data TrueParticle =
  TrueParticle
  { _tpPID       :: PID
  , _tp3Q        :: Int
  , _tpPtEtaPhiE :: PtEtaPhiE
  } deriving  (Generic, Show)

neutral :: TrueParticle -> Bool
neutral = (==0) . view threeCharge

charged :: TrueParticle -> Bool
charged = not . neutral

threeCharge :: Lens' TrueParticle Int
threeCharge = lens _tp3Q $ \tp x -> tp { _tp3Q = x }

instance HasPID TrueParticle where
  pid = lens _tpPID $ \tp x -> tp { _tpPID = x }

instance HasLorentzVector TrueParticle where
  toPtEtaPhiE = lens _tpPtEtaPhiE $ \tp x -> tp { _tpPtEtaPhiE = x }


data TrueJet =
  TrueJet
  { _tjPtEtaPhiE  :: PtEtaPhiE
  , _tjPVFiducialConstits :: [TrueParticle]
  , _tjBHadrons   :: [BHadron]
  } deriving (Generic, Show)

instance HasLorentzVector TrueJet where
  toPtEtaPhiE = lens _tjPtEtaPhiE $ \tj x -> tj { _tjPtEtaPhiE = x }

data BHadron =
  BHadron
  { _bhTP       :: TrueParticle
  , _bhFiducialChildren :: [TrueParticle]
  } deriving (Generic, Show)


instance HasPID BHadron where
  pid = bhTP . pid

instance HasLorentzVector BHadron where
  toPtEtaPhiE = bhTP . toPtEtaPhiE

instance HasSVConstits TrueJet where
  svChargedConstits = pure . toListOf (tjBHadrons.traverse.bhFiducialChildren.traverse.toPtEtaPhiE)

instance HasPVConstits TrueJet where
  pvChargedConstits = pure . toListOf (tjPVFiducialConstits.traverse.toPtEtaPhiE)


fiducialParticle :: TrueParticle -> Bool
fiducialParticle p = view lvPt p > 0.5 && charged p


readBHadrons :: (MonadIO m, MonadThrow m) => TreeRead m [BHadron]
readBHadrons = do
  tlvs <-
    lvsFromTTreeF
      "bhad_pt"
      "bhad_eta"
      "bhad_phi"
      "bhad_e"

  chs <- fmap cintToInt <$> readBranch "bhad_3q"
  pids <- fmap (toEnum . cintToInt) <$> readBranch "bhad_pdgid"

  childs <- fmap (filter fiducialParticle) <$> vecVecTP "bhad_child_"

  let tps = TrueParticle <$> pids <*> chs <*> tlvs
      bhs = getZipList $ BHadron <$> tps <*> childs

  return bhs



readTrueJets :: (MonadIO m, MonadThrow m) => TreeRead m [TrueJet]
readTrueJets = do
  tlvs <- lvsFromTTreeF "jet_pt" "jet_eta" "jet_phi" "jet_e"
  pvconstits <- fmap (filter fiducialParticle) <$> vecVecTP "jet_constit_"

  let tmp = V.fromList . getZipList $ TrueJet <$> tlvs <*> pvconstits <*> pure []

  bhads <- filter ((> 5) . view lvPt) <$> readBHadrons

  return . V.toList $ foldr matchBTJ tmp bhads


trueBJet :: TrueJet -> Bool
trueBJet j = lengthOf (tjBHadrons.traverse) j == 1 && view lvPt j > 30


trueProbeJet :: TrueJet -> Bool
trueProbeJet j = trueBJet j && hasGoodSV j
  where
    hasGoodSV = (>= 3) . lengthOf (tjBHadrons.traverse.bhFiducialChildren.traverse)


vecVecTP
  :: (MonadIO m, MonadThrow m)
  => String -> TreeRead m (ZipList [TrueParticle])
vecVecTP prefix = do
  tlvs <- vecVecTLV
    (prefix ++ "pt")
    (prefix ++ "eta")
    (prefix ++ "phi")
    (prefix ++ "e")

  chs <-
    ZipList . V.toList . fmap (V.toList . fmap cintToInt) . fromVVector
    <$> readBranch (prefix ++ "3q")

  pids <-
    ZipList . V.toList . fmap (V.toList . fmap (toEnum . cintToInt)) . fromVVector
    <$> readBranch (prefix ++ "pdgid")

  return $ zipWith3 TrueParticle <$> pids <*> chs <*> tlvs


cintToInt :: CInt -> Int
cintToInt = fromEnum


vecVecTLV
  :: (MonadIO m, MonadThrow m)
  => String -> String -> String -> String -> TreeRead m (ZipList [PtEtaPhiE])
vecVecTLV spt seta sphi se = do
    partpts <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch spt
    partetas <- (fmap.fmap) float2Double . fromVVector <$> readBranch seta
    partphis <- (fmap.fmap) float2Double . fromVVector <$> readBranch sphi
    partes <- (fmap.fmap) ((/1e3) . float2Double) . fromVVector <$> readBranch se

    let ps = V.zipWith4
            ( \pts etas phis es ->
                V.toList $ V.zipWith4 PtEtaPhiE pts etas phis es
            ) partpts partetas partphis partes

    return . ZipList $ V.toList ps


matchBTJ :: BHadron -> V.Vector TrueJet -> V.Vector TrueJet
matchBTJ bh tjs =
  if V.length tjs == 0
    then tjs
    else
      let mi = V.minIndex $ lvDREta bh <$> tjs
      in over (ix mi) (g bh) tjs

  where
    g b j =
      if lvDREta b j < 0.3
        then over tjBHadrons ((:) b) j
        else j


bMesonH :: VarFills BHadron
bMesonH = singleton "/bmesonpid" <$> h =$<< pure . fromIntegral . view abspid
  where
    h =
      hist1DDef
        (binD 500 100 600)
        "B meson PDGID"
        (dsigdXpbY "\\mathrm{PDGID}" "1")

bBaryonH :: VarFills BHadron
bBaryonH = singleton "/bbaryonpid" <$> h =$<< pure . fromIntegral . view abspid
  where
    h =
      hist1DDef
        (binD 5000 1000 6000)
        "B baryon PDGID"
        (dsigdXpbY "\\mathrm{PDGID}" "1")



tjPVFiducialConstits :: Lens' TrueJet [TrueParticle]
tjPVFiducialConstits = lens _tjPVFiducialConstits $ \tj x -> tj { _tjPVFiducialConstits = x }

tjBHadrons :: Lens' TrueJet [BHadron]
tjBHadrons = lens _tjBHadrons $ \tj x -> tj { _tjBHadrons = x }

bhTP :: Lens' BHadron TrueParticle
bhTP = lens _bhTP $ \bh x -> bh { _bhTP = x }

bhFiducialChildren :: Lens' BHadron [TrueParticle]
bhFiducialChildren = lens _bhFiducialChildren $ \b x -> b { _bhFiducialChildren = x }
