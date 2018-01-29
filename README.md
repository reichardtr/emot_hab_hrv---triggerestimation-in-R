# emot_hab_hrv---triggerestimation-in-R

Data analysis scripts for the study of Simor & Blaskovich - BioTrace data for HRV analysis

The light trigger of NEXUS(TM) failed during the data acquisition phase of the study, thus we need to estimate picture presentation times from the OpenSesame output.

About the script ("read_data_insert_trig.R")

If the subject does have OS output, but no Nexus (BioTrace) output, the script will stop (I don't have BT data for subj 56 yet)
There may be some incorrect outputs (subj 47), because the data from BT is exported in a different way than usual a) i could get a normal BT output b) i could insert more conditions in the script
In some cases the time difference between the OS and BT outputs is suspicious a) the experimenter started Nexus way before the OS program in these early days b) the local time of the two computers differed markedly
