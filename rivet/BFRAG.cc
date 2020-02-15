// -*- C++ -*-
#include "Rivet/Analysis.hh"
#include "Rivet/Projections/FinalState.hh"
#include "Rivet/Projections/ChargedFinalState.hh"
#include "Rivet/Projections/IdentifiedFinalState.hh"
#include "Rivet/Projections/PromptFinalState.hh"
#include "Rivet/Projections/VetoedFinalState.hh"
#include "Rivet/Projections/DressedLeptons.hh"
#include "Rivet/Projections/FastJets.hh"
#include "Rivet/Projections/PartonicTops.hh"
#include "Rivet/Projections/HeavyHadrons.hh"

namespace Rivet {


  /// @brief Add a short analysis description here
  class BFRAG : public Analysis {
    public:

      /// Constructor
      DEFAULT_RIVET_ANALYSIS_CTOR(BFRAG);

      /// @name Analysis methods
      //@{

      /// Book histograms and initialise projections before the run
      void init() {

        /////////////////////////////////////////////////

        Cut eta_full =  (Cuts::abseta < 5.0);
        Cut eta_neutrino = (Cuts::open() );
        Cut lep_cuts = (Cuts::abseta < 2.5) && (Cuts::pT > 25*GeV); //increasing cut of the lepton


        addProjection(HeavyHadrons(), "HeavyHadrons");

        // All final state particles
        FinalState fs(eta_full);
        FinalState fs_neutrino(eta_neutrino);
        // Get photons to dress leptons
        IdentifiedFinalState all_photons(fs);
        all_photons.acceptIdPair(PID::PHOTON);   //previous definition of photons

        IdentifiedFinalState ph_id(fs);
        ph_id.acceptIdPair(PID::PHOTON);

        PromptFinalState photons(ph_id);
        photons.acceptTauDecays(false);
        addProjection(photons, "photons");

        // Projection to find the electrons
        IdentifiedFinalState el_id(fs);
        el_id.acceptIdPair(PID::ELECTRON);

        PromptFinalState electrons(el_id);
        electrons.acceptTauDecays(true);
        addProjection(electrons, "electrons");

        //PromptFinalState electrons2(el_id);
        //electrons2.acceptTauDecays(false);
        //addProjection(electrons2, "electrons2");

        // Switching to new definition with the last boolean considered deprecated
        //DressedLeptons dressedelectrons(photons, electrons, 0.1, lep_cuts, true, true);
        DressedLeptons dressedelectrons(photons, electrons, 0.1, lep_cuts, true);
        addProjection(dressedelectrons, "dressedelectrons");

        //DressedLeptons ewdressedelectrons(all_photons, electrons, 0.1, eta_full, true, true);
        DressedLeptons ewdressedelectrons(all_photons, electrons, 0.1, eta_full, true);
        addProjection(ewdressedelectrons, "ewdressedelectrons");

        // Projection to find the muons
        IdentifiedFinalState mu_id(fs);
        mu_id.acceptIdPair(PID::MUON);

        PromptFinalState muons(mu_id);
        muons.acceptTauDecays(true);
        addProjection(muons, "muons");

        //PromptFinalState muons2(mu_id);
        //muons2.acceptTauDecays(false);
        //addProjection(muons2, "muons2");


        //DressedLeptons dressedmuons(photons, muons, 0.1, lep_cuts, true, true);
        DressedLeptons dressedmuons(photons, muons, 0.1, lep_cuts, true);
        addProjection(dressedmuons, "dressedmuons");

        //DressedLeptons ewdressedmuons(all_photons, muons, 0.1, eta_full, true, true);
        DressedLeptons ewdressedmuons(all_photons, muons, 0.1, eta_full, true);
        addProjection(ewdressedmuons, "ewdressedmuons");

        // Projection to find neutrinos and produce MET
        IdentifiedFinalState nu_id(fs_neutrino);
        nu_id.acceptNeutrinos();
        addProjection(nu_id, "neutrinos");
        PromptFinalState neutrinos(nu_id);
        neutrinos.acceptTauDecays(true);

        // Jet clustering.
        VetoedFinalState vfs(fs);
        vfs.addVetoOnThisFinalState(dressedelectrons);
        vfs.addVetoOnThisFinalState(dressedmuons);
        vfs.addVetoOnThisFinalState(neutrinos);
        FastJets jets(vfs, FastJets::ANTIKT, 0.4);
        jets.useInvisibles(true);
        addProjection(jets, "jets");

        VetoedFinalState vfs2(fs);
        vfs2.addVetoOnThisFinalState(ewdressedelectrons);
        vfs2.addVetoOnThisFinalState(ewdressedmuons);
        vfs2.addVetoOnThisFinalState(neutrinos);
        FastJets jets2(vfs2, FastJets::ANTIKT, 0.4);
        jets2.useInvisibles(true);
        addProjection(jets2, "jets2");


        //VetoedFinalState vfs2(fs);
        //vfs2.addVetoOnThisFinalState(ewdressedelectrons);
        //vfs2.addVetoOnThisFinalState(ewdressedmuons);
        //vfs2.addVetoOnThisFinalState(neutrinos);
        //FastJets jets2(vfs2, FastJets::ANTIKT, 0.4);
        //      jets2.useInvisibles(true);
        //addProjection(jets2, "jets2");


        ChargedFinalState cfs(Cuts::pT > 500*MeV && Cuts::abseta < 2.5);
        declare(cfs, "Tracks");

        // define asymmetric bin-edges
        std::vector< double > binedges_zB;
        binedges_zB.push_back(0.00); binedges_zB.push_back(0.40); binedges_zB.push_back(0.50); binedges_zB.push_back(0.60); 
        binedges_zB.push_back(0.70); binedges_zB.push_back(0.80); binedges_zB.push_back(0.90); binedges_zB.push_back(1.00);     

        std::vector< double > binedges_zBrel;
        binedges_zBrel.push_back(0.000); binedges_zBrel.push_back(0.005);  binedges_zBrel.push_back(0.010);  binedges_zBrel.push_back(0.015);
        binedges_zBrel.push_back(0.020); binedges_zBrel.push_back(0.0275); binedges_zBrel.push_back(0.050);

        std::vector< double > binedges_n;
        binedges_n.push_back(3);       binedges_n.push_back(4);      binedges_n.push_back(5);      binedges_n.push_back(6);
        binedges_n.push_back(7);       binedges_n.push_back(9);      binedges_n.push_back(11);

        // /htop/elmujjtrue/truejets/zbtc
        // /htop/elmujjtrue/truejets/zblc
        // /htop/elmujjtrue/truejets/nsvtrk

        // Book histograms
        _h_xB       = bookHisto1D("zbtc", binedges_zB);
        _h_xBlong   = bookHisto1D("zblc", binedges_zB);
        _h_nsvtrk   = bookHisto1D("nsvtrk",   binedges_n);      
        _h_xBperp   = bookHisto1D("zbrelcnorm", binedges_zBrel);                 // logspace(50, 1e-3, 0.1));
        _h_cutflow  = bookHisto1D("cutflow",   8,  -0.5,  7.5);
        _h_bjet_eta = bookHisto1D("bjet_eta", 26,  -2.60, 2.60);
        _h_bjet_pt  = bookHisto1D("bjet_pt",  33, 20.0, 350.0);
        _h_weight   = bookHisto1D("weight",  80,  -20.0, 20.0);
        _h_weight_zoom = bookHisto1D("weight_zoom",   80,  -5.0, 5.0);

        counter_event = -1;

      }


      /// Perform the per-event analysis
      void analyze(const Event& event) {

        counter_event += 1;

        double weight = event.weight();

        // Cut on tagging b-hadrons
        const Cut TAGCUT = Cuts::pT > 5*GeV;

        // Get jets and dressed leptons
        //      const Jets jets   = apply<FastJets>(event, "CaloJets").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5);
        //const Jets jets2  = apply<FastJets>(event, "CaloJets2").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5);
        //const Jets jets3  = apply<FastJets>(event, "CaloJets3").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5);
        //const Jets jets4  = apply<FastJets>(event, "CaloJets4").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5);

        //      const vector<DressedLepton>& _dressedelectrons = apply<DressedLeptons>(event, "DressedElectrons").dressedLeptons();
        //const vector<DressedLepton>& _dressedmuons = apply<DressedLeptons>(event, "DressedMuons").dressedLeptons();

        //      const vector<DressedLepton>& _ewdressedelectrons = apply<DressedLeptons>(event, "ewDressedElectrons").dressedLeptons();
        // const vector<DressedLepton>& _ewdressedmuons = apply<DressedLeptons>(event, "ewDressedMuons").dressedLeptons();

        _dressedelectrons     = applyProjection<DressedLeptons>(  event, "dressedelectrons").dressedLeptons();         
        _dressedmuons         = applyProjection<DressedLeptons>(  event, "dressedmuons").dressedLeptons();         
        _neutrinos            = applyProjection<IdentifiedFinalState>(event, "neutrinos").particlesByPt();         
        const Jets& all_jets  = applyProjection<FastJets>(        event, "jets").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5);         
        const Jets& all_jets2 = applyProjection<FastJets>(        event, "jets2").jetsByPt(Cuts::pT > 25*GeV && Cuts::abseta < 2.5); 

        vector<DressedLepton> es;         
        vector<DressedLepton> mus;

        //      std::cout << "Event number before = " << counter_event << "\t" <<  all_jets.size() << "\t" << _dressedelectrons.size() << "\t" <<  std::endl;

        if(_dressedelectrons.size() == 0){

          foreach (const Jet& jet, all_jets) {

            //  std::cout << "In loop no lepton " << "\t" << jet.pT() << "\t" << jet.eta() << std::endl;

          }


        }

        // OVERLAP REMOVAL         
        // Jets and muons: remove muons with dR < 0.4 from any of the jets         
        foreach (const DressedLepton& mu, _dressedmuons) {            
          bool keep = true;            
          foreach (const Jet& jet, all_jets2) {               
            keep &= deltaR(jet, mu) >= 0.4;            
          }            
          if (keep && mu.pT() > 25.0)   mus += mu;         
        }         

        // Jets and electrons: remove electrons with dR < 0.4 from any of the jets         
        foreach (const DressedLepton& el, _dressedelectrons) {            
          bool keep = true;            

          foreach (const Jet& jet, all_jets2) {               
            keep &= deltaR(jet, el) >= 0.4;            
          }          

          if (keep && el.pT() > 25.0)   es += el;         

        }


        const Jets& isojets = all_jets;

        Jets bjets;
        Particles assoc_bhad;

        // electroweakly-decaying b-hadrons
        const Particles& bhadrons =
          apply<HeavyHadrons>(event, "HeavyHadrons").bHadrons(Cuts :: pT > 5*GeV);

        // find fiducial b-jets and associated b-hadrons
        for (const Jet& j : isojets) {
          if (j.pT() < 30.0) continue;

          Particles bhads;

          for (const Particle& bhad : bhadrons) {
            if (deltaR(j, bhad) < 0.3)
              bhads.push_back(bhad);
          }

          // a b-jet can only have one associated electroweakly-decaying
          // b-hadron associated to it.
          if (bhads.size() == 1) {
            bjets += j;
            assoc_bhad.push_back(bhads[0]);
          }
        }


        // Require exactly one of each lepton flavour and exactly 2 well-separated central jets, with b-tags
        _h_cutflow->fill(0, 1.0);
        if (es.size() != 1) vetoEvent;

        _h_cutflow->fill(1, 1.0);
        if (mus.size() != 1) vetoEvent;

        _h_cutflow->fill(2, 1.0);
        if (isojets.size() < 1) vetoEvent;

        _h_cutflow->fill(3, 1.0);
        if (isojets.size() < 2) vetoEvent;

        _h_cutflow->fill(4, 1.0);
        if (isojets.size() != 2) vetoEvent;

        _h_cutflow->fill(5, 1.0);
        if (bjets.size() != 2) vetoEvent;

        _h_cutflow->fill(7, 1.0);
        if (deltaR(bjets[0], bjets[1]) < 0.5) vetoEvent;

        _h_cutflow->fill(8, 1.0);

        Cut fidtrk = Cuts::charge != 0 && Cuts::pT > 500*MeV;
        for (unsigned int iJet = 0; iJet < bjets.size(); iJet++) {
          const Jet& bjet = bjets[iJet];
          const Particles jettrks = bjet.constituents(fidtrk);

          const Particle& bhad = assoc_bhad[iJet];
          const Particles btrks = bhad.stableDescendants(fidtrk);

          // require >= 3 fiducial b-hadron decay products
          if (btrks.size() < 3)
            continue;


          Particles alltrks = jettrks;

          // if the b-hadron child is not a jet constituent, add it to alltrks
          // because SV finding in the detector does not use ghost-association
          // this ensures momentum fractions fall between 0 and 1
          for (const Particle& btrk : btrks) {

            bool already_associated = false;
            for (const Particle& trk : jettrks) {
              if (isSame(trk, btrk)) {
                already_associated = true;
                break;
              }
            }

            if (!already_associated)
              alltrks.push_back(btrk);
          }

          const FourMomentum alltrkmom = sum(alltrks, mom, FourMomentum());
          const FourMomentum btrkmom = sum(btrks, mom, FourMomentum());

          const double xB     = btrkmom.pT() / alltrkmom.pT();
          const double xBlong = dot(btrkmom.p3(), alltrkmom.p3()) / alltrkmom.p2();
          const double xBperp = cross(btrkmom.p3(), alltrkmom.p3()).mod() / alltrkmom.p2();

          _h_xB       -> fill(xB != 1.0 ? xB : 0.9999, weight);
          _h_xBlong   -> fill(xBlong != 1.0 ? xBlong : 0.9999, weight);
          _h_nsvtrk   -> fill(btrks.size(), weight);
          _h_xBperp   -> fill(xBperp, weight);
          _h_bjet_eta -> fill(bjet.eta(),    weight);
          _h_bjet_pt  -> fill(bjet.pT()/GeV, weight);
          _h_weight   -> fill(weight);
          _h_weight_zoom -> fill(weight);

        }

      }


      /// Normalise histograms etc., after the run
      void finalize() {
        normalize({_h_xB, _h_xBlong, _h_xBperp, _h_cutflow, _h_bjet_eta, _h_bjet_pt,_h_weight,_h_weight_zoom,_h_nsvtrk});
      }

      //@}


    private:

      /// @name Histograms
      //@{
      Histo1DPtr _h_xB, _h_xBlong, _h_nsvtrk, _h_xBperp, _h_cutflow, _h_bjet_eta, _h_bjet_pt,_h_weight,_h_weight_zoom;
      //@}

      float counter_event;

      /// @name Objects that are used by the event selection decisions       
      vector<DressedLepton> _dressedelectrons, _dressedmuons;       
      Particles _neutrinos;

  };



  // The hook for the plugin system
  DECLARE_RIVET_PLUGIN(BFRAG);


}
