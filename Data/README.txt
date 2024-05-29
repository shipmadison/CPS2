README: 2024 CPS2 data cleaning

Arctic Lady (pot vessel)
	FTP data changes:
		- Haul 8, Haul_ID 22, Specimen_ID 190, changed 6.82mm to 68.2mm based on on-board communication
		- Haul 9, Haul_ID 23, Specimen_ID 214, changed 13.0mm to 130.0mm based on on-board communication
		- *FLAG: Haul 217, 3/31, 107.51mm female, EC5, clutch size 4?

	Data modifications based on decksheets:
		- SPN 24 longitude minutes 5.72 --> 5.712
		- SPN 31 set time 8:34 --> 8:35
		- SPN 82 longitude minutes 37.099 --> 38.099 (based on sequence, judgement call)
		- SPN 131 latitude minutes 19.25 --> 19.125
		- SPN 131 time haul 14:43 --> 16:43 (based on sequence, judgement call)
		- SPN 143 latitude minutes 23.585 --> 25.585
		- SPN 164 longitude minutes 37.708 --> 35.708 (based on sequence, judgement call)
		- SPN 193 longitude minutes 27.730 --> 27.720
		- SPN 224 logger 238 --> 248
		- SPN 263 longitude minutes 24.716 --> 24.766
		- SPN 302 time set 5:57 --> 6:57
		- SPN 328 depth 28.1 --> 28.3

	Script modifications:
		- POT_ID G35 was accidentally sampled twice (SPN 1, SPN 215) -- a decision was made to use data from the first sampling date. This change has been incorporated into the pot processing script.
		- Prior to 3/24/24, the Arctic Lady was likely misclassifying some of the SC3 males as SC2. Males with light scratching and some barnacles were still called SC2. Starting on 3/24/24 these crab will be classified as SC3. This change has been incorporated into the pot processing script by adding 1 to all males shell condition 2+ before March 24th for the Arctic Lady.


Seabrooke (pot vessel)
	FTP data changes:
		- SPN 64, changed a SC3 female EC1 --> EC2

	Data modifications based on decksheets:
		- SPN 45 latitude minutes 25.45 --> 24.45
		- SPN 181 longitude minutes 20.55 --> 20.35 (based on sequence, judgement call)
		- SPN 196 time set 1:45 --> 11:45
		- SPN 206 latitude minutes 03.34 --> 03.43
		- SPN 217 longitude minutes 20.36 --> 20.46 (based on sequence, judgement call)
		- SPN 224 longitude minutes 20.41 --> 20.40
		- SPN 226 dat haul 3/28 --> 3/27
		- SPN 255 latitude minutes 46.39 --> 46.34
		- SPN 312 haul time 18:23 --> 18:13
		- SPN 317 pot ID CAMERA TEST --> CAM2
		- SPN 327 latitude minutes 21.9 --> 21.98
		- SPN 333 longitude degrees 163 --> 164
		- SPN 347 longitude minutes 38.25 --> 48.25
		- SPN 349 pot ID A5 --> A1
		- SPN 350 pot ID A4 --> A2
		- SPN 352 pot ID A2 --> A4
		- SPN 353 pot ID A1 --> A5; A5 --> CAM3 (second time A5 was sampled, CAM test)
	

Vesteraalen (trawl vessel)
	FTP data changes:
		- Haul 9, Station V3, removed an erroneous 4mm SC4 RKC male
		- Haul 46, Station S6, changed a SC3 female EC1 --> EC2
		- Haul 103, Station O4, changed Hanasaki King Crab observation to Hair Crab, based on on-board communication
		- Haul 127, Station N16, changed Hanasaki King Crab observation to Hair Crab, based on on-board communication