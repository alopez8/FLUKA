// Read data (MuonPb simulations) from an ascii file and create a root file with a Tree.
// Author: Andrew Lopez
   
void fluka2rootMuPb()
{
   TFile file("MuOnPbNeutrons.root","RECREATE");

   Double_t	   Initial_Mu_KE = 0; //initial Muon kinetic energy [GeV]
   Double_t	   Neutron_E = 0; //total neutron energy after exiting Pb [GeV]
   Double_t	   Neutron_KE = 0;//neutorn kinetic energy after exiting Pb [GeV]
   Int_t 	   Num_Source_Particles = 0; //number of simulated source muons
   Long64_t nlines = 0; //number of lines in file

   // Create a ROOT Tree

   TTree *tree = new TTree("MuonPb_GeV","Data from Mu onto Pb simulations in GeV");

   tree->Branch("Initial_Mu_KE_GeV", &Initial_Mu_KE);
   tree->Branch("Neutron_E_GeV", &Neutron_E);
   tree->Branch("Neutron_KE_GeV",&Neutron_KE);
   tree->Branch("Num_Source_Particles",&Num_Source_Particles);

   //readfiles
/*
USING:
	tree->ReadFile(...);
	tree->Fill();
ADDS AN EXTRA EVENT SOMEHOW
*/

   nlines = tree->ReadFile("MuonPb30G001_MuPb.txt","Initial_Mu_KE:Neutron_E:Neutron_KE:Num_Source_Particles/i");
   printf ("MuonPb30G001_MuPb.txt: found %lld points\n",nlines);
   nlines = tree->ReadFile("MuonPb30G002_MuPb.txt");
   printf ("MuonPb30G002_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G003_MuPb.txt");
   printf ("MuonPb30G003_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G004_MuPb.txt");
   printf ("MuonPb30G004_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G005_MuPb.txt");
   printf ("MuonPb30G005_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G006_MuPb.txt");
   printf ("MuonPb30G006_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G007_MuPb.txt");
   printf ("MuonPb30G007_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G008_MuPb.txt");
   printf ("MuonPb30G008_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G009_MuPb.txt");
   printf ("MuonPb30G009_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb30G010_MuPb.txt");
   printf ("MuonPb30G010_MuPb.txt: found %lld points\n",nlines);   

   nlines = tree->ReadFile("MuonPb100G001_MuPb.txt");
   printf ("MuonPb100G001_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G002_MuPb.txt");
   printf ("MuonPb100G002_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G003_MuPb.txt");
   printf ("MuonPb100G003_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G004_MuPb.txt");
   printf ("MuonPb100G004_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G005_MuPb.txt");
   printf ("MuonPb100G005_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G006_MuPb.txt");
   printf ("MuonPb100G006_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G007_MuPb.txt");
   printf ("MuonPb100G007_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G008_MuPb.txt");
   printf ("MuonPb100G008_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G009_MuPb.txt");
   printf ("MuonPb100G009_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb100G010_MuPb.txt");
   printf ("MuonPb100G010_MuPb.txt: found %lld points\n",nlines);

 
   nlines = tree->ReadFile("MuonPb300G001_MuPb.txt");
   printf ("MuonPb300G001_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G002_MuPb.txt");
   printf ("MuonPb300G002_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G003_MuPb.txt");
   printf ("MuonPb300G003_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G004_MuPb.txt");
   printf ("MuonPb300G004_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G005_MuPb.txt");
   printf ("MuonPb300G005_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G006_MuPb.txt");
   printf ("MuonPb300G006_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G007_MuPb.txt");
   printf ("MuonPb300G007_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G008_MuPb.txt");
   printf ("MuonPb300G008_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G009_MuPb.txt");
   printf ("MuonPb300G009_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb300G010_MuPb.txt");
   printf ("MuonPb300G010_MuPb.txt: found %lld points\n",nlines);

   nlines = tree->ReadFile("MuonPb1000G001_MuPb.txt");
   printf ("MuonPb1000G001_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G002_MuPb.txt");
   printf ("MuonPb1000G002_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G003_MuPb.txt");
   printf ("MuonPb1000G003_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G004_MuPb.txt");
   printf ("MuonPb1000G004_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G005_MuPb.txt");
   printf ("MuonPb1000G005_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G006_MuPb.txt");
   printf ("MuonPb1000G006_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G007_MuPb.txt");
   printf ("MuonPb1000G007_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G008_MuPb.txt");
   printf ("MuonPb1000G008_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G009_MuPb.txt");
   printf ("MuonPb1000G009_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb1000G010_MuPb.txt");
   printf ("MuonPb1000G010_MuPb.txt: found %lld points\n",nlines);

   nlines = tree->ReadFile("MuonPb3000G001_MuPb.txt");
   printf ("MuonPb3000G001_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G002_MuPb.txt");
   printf ("MuonPb3000G002_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G003_MuPb.txt");
   printf ("MuonPb3000G003_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G004_MuPb.txt");
   printf ("MuonPb3000G004_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G005_MuPb.txt");
   printf ("MuonPb3000G005_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G006_MuPb.txt");
   printf ("MuonPb3000G006_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G007_MuPb.txt");
   printf ("MuonPb3000G007_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G008_MuPb.txt");
   printf ("MuonPb3000G008_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G009_MuPb.txt");
   printf ("MuonPb3000G009_MuPb.txt: found %lld points\n",nlines);   
   nlines = tree->ReadFile("MuonPb3000G010_MuPb.txt");
   printf ("MuonPb3000G010_MuPb.txt: found %lld points\n",nlines);

   file.Write();
}
