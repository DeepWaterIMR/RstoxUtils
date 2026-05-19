# Codes and definitions

## Introduction

The Norwegian Directorate of Fisheries (FDir) and Institute of Marine
Research (IMR) use abbreviations, codes and definitions in their data.
This document contains tables of definition files within the RstoxUtils
package.

## Norwegian Directorate of Fisheries

The codes have been extracted from the [Norwegian Directorate of
Fisheries
website](https://www.fiskeridir.no/Yrkesfiske/Rapportering-ved-landing/Kodeliste).

### Species codes

| idNS | idFAO | norwegian | english | latin |
|---:|:---|:---|:---|:---|
| 1 | NA | FERSKVANNSFISK | NA | OSTEICHTHYES |
| 101 | LAS | Niøye, uspes. | Lampreys nei | Petromyzontidae |
| 102 | FPI | Gjedde | Northern pike | Esox lucius |
| 103 | FBR | Ferskvannsbrasme, uspes. | Freshwater breams nei | Abramis spp |
| 104 | FCP | Karpe | Common carp | Cyprinus carpio |
| 105 | FTE | Suter | Tench | Tinca tinca |
| 106 | FCC | Karuss | Crucian carp | Carassius carassius |
| 107 | FRO | Mort | Roach | Rutilus rutilus |
| 108 | FBU | Lake | Burbot | Lota lota |
| 109 | FPE | Abbor | European perch | Perca fluviatilis |
| 111 | FPP | Gjørs | Pike perch | Stizostedion lucioperca |
| 112 | STU | Stør,uspes | Sturgeons | Acipenseridae |
| 113 | SHD | Maisild, stamsild, uspes. | Allis and twaite shads | Alosa spp |
| 114 | FID | Vederbuk | Orfe(Ide) | Leuciscus idus |
| 115 | SME | Krøkle | European smelt | Osmerus eperlanus |
| 116 | FBM | Vanlig Ferskvannsbrasme | Freshwater bream | Abramis brama |
| 117 | APU | Stør | Sturgeon | Acipenser Sturio |
| 11801 | APB | Sibirsk stør (oppdrett) | Siberian sturgeon | Acipenser baerii |
| 11901 | APG | Russisk stør (oppdrett) | Danube sturgeon (=Osetr) | Acipenser gueldenstaedtii |
| 12001 | APR | Sterlet stør (oppdrett) | Sterlet sturgeon | Acipenser ruthenus |
| 121 | SRE | Sørv | Rudd | Scardinius erythrophthalmus |
| 199 | FRF | Andre ferskvannsfisker | Freshwater fishes | Osteichthyes |
| 2 | NA | SILDEHAIER | NA | LAMNOIDEI |
| 21 | NA | Håbrannfamilien | NA | Lamnidae |
| 211 | POR | Håbrann | Porbeagle | Lamna nasus |
| 212 | BSK | Brugde | Basking shark | Cetorhinus maximus |
| 213 | SMA | Makrellhai | Shortfin mako | Isurus oxyrinchus |
| 22 | NA | NA | Sand tigers | Odontaspididae |
| 221 | CCT | Sand tiger shark | Sand tiger shark | Carcharias taurus |
| 23 | NA | Revehaifamilien | Tresher sharks | Alopiidae |
| 231 | ALV | Revehai | Tresher | Alopias vulpinus |
| 3 | NA | RØDHAIER | NA | SCYLIORHINOIDEI |
| 311 | BSH | Blåhai | Blueshark | Prionace glauca |
| 312 | SHO | Hågjel | Blackmouth catshark | Galeus melastomus |
| 313 | SYC | Småflekket rødhai | Small-spotted catshark | Scyliorhinus canicula |
| 314 | API | Deep-water catsharks | Deep-water catsharks | Apristurus |
| 315 | PTM | False catshark | False catshark | Pseudotriakis microdon |
| 316 | GAU | Crest-tail catsharks nei | Crest-tail catsharks nei | Galeus spp |
| 317 | RHT | Atlantic sharpnose shark | Atlantic sharpnose shark | Rhizipeinodon terraenovae |
| 318 | GAM | Mouse catshark | Mouse catshark | Galeus murinus |
| 319 | SCL | Catcharks, nursehounds nei | Catcharks, nursehounds nei | Scyliorhinus spp |
| 321 | GAG | Gråhai | Tope shark | Galeorhinus galeus |
| 322 | DUS | Dusky shark | Dusky shark | Cacharinus obscurus |
| 323 | FAL | Silkehai | Silky shark | Cacharhinus falciformis |
| 4 | NA | HÅER | NA | SQUALIFORMES |
| 411 | GSK | Håkjerring | Greenland shark | Somniosus microcephalus |
| 412 | DGS | Pigghå | Picked dogfish | Squalus acanthias |
| 413 | DGX | Håer, uspes | Dogfish sharks nei | Squalidae |
| 414 | DGH | Annen hå | Dogfishes and hounds | Squalidae Scyliorhinidae |
| 415 | CFB | Islandshå | Black dogfish | Centroscyllium fabricii |
| 416 | SOR | Little sleeper shark | Little sleeper shark | Somnius rostratus |
| 417 | SCK | Spansk håkjerring | Kitefin shark | Dalatias licha |
| 418 | SHB | Tagghai | Bramble shark | Echinorhinus brucus |
| 419 | DGX | Dogfishes nei | Dogfishes nei | Squalidae |
| 420 | GUP | Gulper shark | Gulper shark | Centrophorus granulosus |
| 430 | GUQ | Brunhå | Shark gulper, leafscale | Centrophorus squamosus |
| 431 | CYO | Dypvannshå | Portuguese dogfish | Centroscymus coelolepis |
| 432 | DCA | Gråhå | Birdbeak dogfish | Deania calceus |
| 433 | CYP | Bunnhå | Longnose velvet dogfish | Centroscymnus crepidater |
| 434 | SHL | Svarthå, uspes. | Lanternsharks nei | Etmopterus spp |
| 435 | ETX | Svarthå | Velvet belly | Etmopterus spinax |
| 436 | OXN | Tornhå | Sailfin roughshark | Oxynotus paradoxus |
| 437 | SYR | Kortpigget hå | Knifetooth dogfish | Scymnodon ringens |
| 438 | GUP | Gulper shark | Gulper shark | Centrophorus granulosus |
| 42 | NA | Havengelfamilien | NA | Squatinidae |
| 421 | ASK | Havengler, uspes. | Angelsharks, sand devils nei | Squatinidae |
| 499 | SKH | Annen hai | Various sharks nei | Selachimorpha (Pleurotremata) |
| 5 | NA | SKATER OG ROKKER | NA | RAJIFORMES |
| 51 | NA | Skatefamilien | NA | Rajidae |
| 511 | RJB | Storskate | Blue skate | Raja batis |
| 512 | RJC | Piggskate | Thornback ray | Raja clavata |
| 513 | RJM | Flekkskate | Spotted ray | Raja montagui |
| 514 | RJF | Nebbskate | Shagreen ray | Raja fullonica |
| 515 | RJN | Gjøkskate | Cuckoo ray | Raja naevus |
| 516 | RJO | Spisskate | Longnosed skate | Raja oxyrinchus |
| 517 | RJD | Little skate | Little skate | Raja erinacea |
| 518 | RJL | Barndoor skate | Barndoor skate | Raja laevis |
| 519 | RJT | Winter skate | Winter skate | Raja ocellata |
| 520 | RJR | Kloskate | Starry ray | Raja radiata |
| 521 | RJS | Smooth skate | Smooth skate | Malacoraja senta |
| 522 | RJQ | Gråskate | Spinetail ray | Bathyraja spinicauda |
| 523 | RJH | Blonde ray | Blonde ray | Raja brachyura |
| 524 | RJI | Sandskate | Sandy ray | Raja circularis |
| 525 | RJY | Rundskate | Round ray | Raja fyllae |
| 526 | RJE | Småøyet skate | Small-eyed ray | Raja microocellata |
| 527 | RJU | Bølgeskate | Undulate ray | Raja undulata |
| 528 | RJA | Burton-skate | White skate | Raja alba |
| 529 | SKA | Skate, uspesifisert | Raja rays nei | Raja spp |
| 53 | NA | Ørneskatefamilien | NA | Myliobatidae |
| 531 | EAG | Ørneskate, uspes. | Eagle rays nei | Myliobatidae |
| 54 | NA | Elrokkefamilien | NA | Torpedinidae |
| 541 | TOE | Elrokke | Torpedo rays | Torpedo spp |
| 55 | NA | Skatefamilien | NA | Rajidae |
| 551 | JAD | Svartskate | Norwegian skate | Raja nidarosiensis |
| 552 | RJG | Isskate | Arctic skate | Raja hyperborea |
| 553 | RJK | Hvitskate | Sailray | Raja lintea |
| 559 | RAJ | Skater uspes. | Rays and skates nei | Rajidae |
| 599 | SRX | Annen skate og rokke | Rays, stingrays, mantas nei | Rajiformes |
| 6 | NA | SILDEFISKER | NA | CLUPEIFORMES |
| 61 | NA | Sildefamilien | NA | Clupeidae |
| 611 | HER | Sild | Atlantic herring | Clupea harengus |
| 61101 | HER | Norsk vårgytende sild | Atlantic herring | Clupea harengus |
| 61102 | HER | Trondheimsfjordsild | Atlantic herring | Clupea harengus |
| 61103 | HER | Mussa | Atlantic herring | Clupea harengus |
| 61104 | HER | Nordsjøsild | Atlantic herring | Clupea harengus |
| 61105 | HER | Skagerraksild | Atlantic herring | Clupea harengus |
| 61106 | HER | Sild vest av 4 graden | Atlantic herring | Clupea harengus |
| 61107 | HER | Fjordsild | Atlantic herring | Clupea harengus |
| 612 | SIX | Annen sardin | Sardinellas | Sardinella spp |
| 613 | MHA | Atlantic menhaden | Atlantic menhaden | Brevoortia tyrannus |
| 614 | PIL | Sardin | European pilchard | Sardina pilchardus |
| 615 | SPR | Brisling | European sprat | Sprattus sprattus |
| 61501 | SPR | Havbrisling | European sprat | Sprattus sprattus |
| 61502 | SPR | Kystbrisling | European sprat | Sprattus sprattus |
| 616 | ALE | Alewife | Alewife | Alosa pseudoharengus |
| 62 | NA | Ansjosfamilien | NA | Engraulidae |
| 621 | ANE | Ansjos | European anchovy | Engraulis encrasicolus |
| 62101 | ANE | Ansjos (oppdrett) | European anchovy | Engraulis encrasicolus |
| 63 | NA | NA | NA | Alepocephalidae |
| 631 | ALC | Baird’s slickhead | Baird’s slickhead | Alepocephalus bairdii |
| 632 | PHO | Risso’s smooth-head | Risso’s smooth-head | Alepocephalus rostratus |
| 64 | HAF | Perlemorfiskfamilien | Hatchetfishes | Sternoptychidae |
| 641 | MAV | Laksesild | Silvery lightfish | Maurolicus muelleri |
| 699 | CLU | Annen sildefisk | Clupeoids | Clupeoidei |
| 7 | NA | LAKSEFISKER | NA | SALMONIFORMES |
| 711 | SAL | Laks | Atlantic salmon | Salmo salar |
| 71101 | SAL | Laks (oppdrett) | Atlantic salmon | Salmo salar |
| 712 | COH | Coho laks | Coho salmon | Oncorhynchus kisutch |
| 713 | TRS | Ørret | Sea trout | Salmo trutta |
| 71301 | TRS | Ørret (oppdrett) | Sea trout | Salmo trutta |
| 714 | TRR | Regnbueørret | Rainbow trout | Oncorhynchus mykiss/ Salmo gairdneri |
| 71401 | TRR | Regnbueørret (oppdrett) | Rainbow trout | Oncorhynchus mykiss/ Salmo gairdneri |
| 715 | TRO | Annen ørret | Trouts | Salmo spp |
| 716 | ACH | Røye | Arctic char | Salvelinus alpinus |
| 71601 | ACH | Røye (oppdrett) | Arctic char | Salvelinus alpinus |
| 717 | CHR | Annen røye | Chars | Salvelinus spp |
| 718 | TLA | Artic grayling | Artic grayling | Thymallus arcticus |
| 719 | TLV | Harr | Grayling | Thymallus thymallus |
| 72 | NA | NA | NA | Coregonidae |
| 721 | FVE | Lagesild | European whitefish | Coregonus albula |
| 722 | PLN | Sik | Pollan | Coregonus lavaretus |
| 723 | HOU | Nebbsik | Houting | Coregonus oxyrinchus |
| 724 | WHF | Annen sik | Whitefish | Coregonus spp |
| 73 | NA | NA | NA | NA |
| 731 | SVF | Kanadisk bekkerøye | Brook trout | Salvelinus fontinalis |
| 732 | PIN | Pukkellaks | Pink(=Humpback) salmon | Oncorhynchus gorbuscha |
| 73201 | PIN | Pukkellaks (oppdrett) | Pink(=Humpback) salmon | Oncorhynchus gorbuscha |
| 74 | NA | Vassildfamilien | NA | Argentinidae |
| 741 | ARG | Strømsild/Vassild | Argentines | Argentina spp |
| 742 | ARU | Vassild | Greater argentine | Argentina silus |
| 743 | ARY | Strømsild | Argentine | Argentina sphyraena |
| 75 | NA | Loddefamilien | NA | Osmeridae |
| 751 | CAP | Lodde | Capelin | Mallotus villosus |
| 75101 | CAP | Barentshavslodde | Capelin | Mallotus villosus |
| 75102 | CAP | Lodde - Island/Ø Grønl./Jan M | Capelin | Mallotus villosus |
| 799 | SLX | Annen laksefisk | Salmonoids | Salmonoidei |
| 8 | NA | ÅLEFISKER | NA | ANGUILLIFORMES |
| 81 | NA | Ålefamilien | NA | Anguillidae |
| 811 | ELE | Ål | European eel | Anguilla anguilla |
| 81101 | ELE | Ål (oppdrett) | European eel | Anguilla anguilla |
| 82 | NA | Havålfamilien | NA | Congridae |
| 821 | COE | Havål | European conger | Conger conger |
| 899 | COX | Havål, uspes. | Congers eels, etc. nei | Congridae |
| 9 | NA | MARULKER | NA | BELONIFORMES |
| 91 | NA | Horngjelfamilien | NA | Belonidae |
| 911 | GAR | Horngjel | Garfish | Belone belone |
| 92 | NA | Makrellgjeddefamilien | NA | Scomberesocidae |
| 921 | SAU | Makrellgjedde | Atlantic saury | Scomberesox saurus |
| 93 | NA | NA | NA | Pomatomidae |
| 931 | BLU | Bluefish | Bluefish | Pomatomus saltratix |
| 999 | BEN | Horngjel, uspes. | Needlefishes, etc nei | Belonidae |
| 10 | NA | TORSKEFISKER | NA | GADIFORMES |
| 1011 | MOR | Mora, uspes. | Moras nei | Moridae |
| 1012 | ANT | Blå antimora | Blue antimora | Antimora rostrata |
| 1013 | RIB | Mora | Common mora | Mora moro |
| 1021 | USK | Brosme | Tusk(= Cusk) | Brosme brosme |
| 1022 | COD | Torsk | Atlantic cod | Gadus morhua |
| 102201 | COD | Skrei | Atlantic cod | Gadus morhua |
| 102202 | COD | Nordøstarktisk torsk | Atlantic cod | Gadus morhua |
| 102203 | COD | Kysttorsk | Atlantic cod | Gadus morhua |
| 102204 | COD | Annen torsk | Atlantic cod | Gadus morhua |
| 102205 | COD | Torsk (oppdrett) | Atlantic cod | Gadus morhua |
| 1023 | LIN | Lange | Ling | Molva molva |
| 1024 | BLI | Blålange | Blue ling | Molva dypterygia |
| 1025 | GFB | Skjellbrosme | Greater forkbeard | Phycis blennoides |
| 1026 | FOR | Forkbeard | Forkbeard | Phycis phycis |
| 1027 | HAD | Hyse | Haddock | Melanogrammus aeglefinus |
| 102701 | HAD | Nordøstarktisk hyse | Haddock | Melanogrammus aeglefinus |
| 102702 | HAD | Kysthyse | Haddock | Melanogrammus aeglefinus |
| 102703 | HAD | Nordsjøhyse | Haddock | Melanogrammus aeglefinus |
| 102704 | HAD | Annen hyse | Haddock | Melanogrammus aeglefinus |
| 102705 | HAD | Hyse (oppdrett) | Haddock | Melanogrammus aeglefinus |
| 1028 | HKR | Rød lysing | Red hake | Urophycis chuss |
| 1029 | HKW | Hvit lysing | White hake | Urophycis tenuis |
| 1030 | POD | Sypike | Poor cod | Trisopterus minutus |
| 1031 | COW | Navagotorsk | Navaga(=Wachna cod) | Eleginus nawaga |
| 1032 | POK | Sei | Saithe(= Pollock) | Pollachius virens |
| 103201 | POK | Sei (oppdrett) | Saithe(= Pollock) | Pollachius virens |
| 1034 | POL | Lyr | Pollack | Pollachius pollachius |
| 1035 | POC | Polartorsk | Polar cod | Boreogadus saida |
| 1036 | NOP | Øyepål | Norway pout | Trisopterus esmarkii |
| 1037 | BIB | Skjeggtorsk | Pouting | Trisopterus luscus |
| 1038 | WHB | Kolmule | Blue whiting | Micromesistius poutassou |
| 1039 | WHG | Hvitting | Whiting | Merlangius merlangus |
| 1040 | GDG | Sølvtorsk | Silvery pout | Gadiculus argenteus |
| 1041 | RCR | Paddetorsk | Tadpole fish | Raniceps raninus |
| 1042 | GRC | Greenland cod | Greenland cod | Gadus ogac |
| 1043 | ATG | Istorsk | Arctic Cod | Arctogadus glacialis |
| 1051 | HKE | Lysing | European hake | Merluccius merluccius |
| 1052 | HKS | Silver hake | Silver hake | Merluccius bilinearis |
| 1061 | RHG | Isgalt | Roughhead grenadier | Macrourus berglax |
| 1062 | RNG | Skolest | Roundnose grenadier | Coryphaenoides rupestris |
| 1063 | CQL | Spiritist | Hollowsnout grenadier | Caelorinchus caelorhincus |
| 1099 | GAD | Torskefisk, uspes. | Gadiformes nei | Gadiformes |
| 11 | NA | STINGSILDFISKER | NA | GASTEROSTEIFORMES |
| 1111 | SKB | Stingsild | Sticklebacks | Gasterosteus spp |
| 1112 | GTA | Trepigget stingsild | Three-spined stickleback | Gasterosteus aculatus |
| 1115 | GPT | Nipigget stingsild | Ninespine stickleback | Pungitius pungitius |
| 12 | NA | NÅLEFISKER | NA | SYGNATHIFORMES |
| 1211 | SNS | Trompetfisk | Slender snipefish | Macroramphosus scolopax |
| 122 | NA | Nålefiskfamilien | Pipefishes and seahorses | Syngnathidae |
| 1221 | SGQ | Stor kantnål | Greater pipefish | Syngnathus acus |
| 1222 | SFR | Liten kantnål | Nilsson’s pipefish | Syngnathus rostellatus |
| 1225 | HPI | Sjøhest | Long-snouted seahorse | Hippocampus guttulatus |
| 1226 | STQ | Tangsnelle | Broadnosed pipefish | Syngnathus typhle |
| 13 | NA | BERYXFISKER | NA | BERYCIFORMES |
| 131 | NA | Beryxfamilien | NA | Berycidae |
| 1311 | ALF | Alfonsinos nei | Alfonsinos nei | Beryx |
| 1312 | HPR | Mediterranean slimehead | Mediterranean slimehead | Hoplostethus mediterraneus |
| 132 | NA | NA | NA | Trachichthyidae |
| 1321 | ORY | Orange roughy | Orange roughy | Hoplostethus atlanticus |
| 14 | NA | ST. PETERS FISKER | NA | ZEIFORMES |
| 141 | NA | Sanktpetersfiskfamilien | NA | Zeidae |
| 1411 | JOD | Sanktpetersfisk | John dory | Zeus faber |
| 142 | NA | Villsvinfiskfamilien | NA | NA |
| 1421 | BOC | Villsvinfisk | Boarfish | Capros aper |
| 15 | NA | MULTEFISKER | NA | MUGILOIDEI |
| 151 | NA | Multefamilien | NA | Mugilidae |
| 1511 | MUL | Annen multe | Mullets | Mugilidae |
| 1512 | MLR | Tykkleppet multe | Thicklip grey mullet | Chelon labrosus |
| 152 | NA | Stripefiskfamilien | NA | Atherinidae |
| 1521 | SIL | Stripefisker | Silverside smelts | Atherinidae |
| 161 | NA | Hestmakrellfamilien | NA | Carangidae |
| 1611 | HOM | Hestmakrell | Atlantic horse mackerel | Trachurus trachurus |
| 161101 | HOM | Hestmakrell (oppdrett) | Atlantic horse mackerel | Trachurus trachurus |
| 1612 | LEE | Leerfish | Leerfish | Lichia amia |
| 1619 | JAX | Annen hestmakrell | Jack & horsemackerel | Trachurus spp |
| 162 | NA | Havabborfamilien | NA | Serranidae |
| 1621 | GPD | Dusky grouper | Dusky grouper | Epinephelus marginatus |
| 1622 | WRF | Vrakfisk | Wreckfish | Polyprion americanus |
| 1623 | BSS | Havabbor | European seabass | Dicentrarchus labrax |
| 1624 | STB | Stripet havabbor | Striped bass | Morone saxatilis |
| 1625 | GRX | Grunt | Grunts, sweetlips, etc. | Haemulidae(=Pomadasyidae) |
| 1628 | EPI | Dyphavsabbor | Black cardinal fish | Epigonus telescopus |
| 163 | NA | Ørnefiskfamilien | NA | Sciaenidae |
| 1631 | MGR | Ørnefisk | Meagre | Argyrosomus regius |
| 164 | NA | Havkarussfamilien | NA | Sparidae |
| 1641 | SBR | Flekkpagell | Red seabream | Pagellus bogaraveo |
| 1642 | PAC | Rødpagell | Common pandora | Pagellus erythrinus |
| 1643 | SBA | Axillary seabream | Axillary seabream | Pagellus acarne |
| 1644 | DEL | Largeeye dentex | Largeeye dentex | Dentex macrophthalmus |
| 1645 | DEC | Common dentex | Common dentex | Dentex dentex |
| 1646 | DEX | Dentex | Dentex | Dentex spp |
| 1647 | SBG | Dorade | Gilthead seabream | Sparus aurata |
| 1648 | BOG | Oksøyefisk | Bogue | Boops boops |
| 1649 | SBX | Annen havkaruss | Porgies, seabreams etc. | Sparidae |
| 165 | NA | Mullefamilien | NA | Mullidae |
| 1651 | MUR | Mulle | Surmullet | Mullus surmuletus |
| 1652 | MUT | Red mullet | Red mullet | Mullus barbatus |
| 166 | NA | Fjesingfamilien | NA | Trachinidae |
| 1661 | WEG | Fjesing | Greater weever | Trachinus draco |
| 167 | NA | Havbrasmefamilien | NA | Bramidae |
| 1671 | POA | Havbrasme | Atlantic pomfret | Brama brama |
| 1672 | TAS | Høyfinnet havbrasme | Rough pomfret | Taractes asper |
| 1673 | BTB | Sølvbrasme | Atlantic fanfish | Pterycombus brama |
| 168 | NA | NA | NA | Centracanthidae |
| 1681 | PIC | Picarels | Picarels | Spicara spp |
| 169 | NA | Leppefiskfamilien | Wrasses | Labridae |
| 1691 | USB | Berggylt | Ballan wrasse | Labrus bergylta |
| 169101 | USB | Berggylt (oppdrett) | Ballan wrasse | Labrus bergylta |
| 1692 | WRA | Annen leppefisk | Wrasses, hogfishes, etc.nei | Labridae |
| 1693 | TBR | Bergnebb | Gold-sinny wrasse | Ctenolabrus rupestris |
| 169301 | TBR | Bergnebb (oppdrett) | Gold-sinny wrasse | Ctenolabrus rupestris |
| 1694 | YFM | Grøngylt | Corkwing wrasse | Symphodus melops |
| 169401 | YFM | Grøngylt (oppdrett) | Corkwing wrasse | Symphodus melops |
| 1695 | USI | Blåstål/ Rødnebb | Cuckoo wrasse | Labrus (bimaculatus) mixtus |
| 169501 | USI | Blåstål/ Rødnebb (oppdrett) | Cuckoo wrasse | Labrus (bimaculatus) mixtus |
| 169510 | USI | Blåstål | Cuckoo wrasse | Labrus (bimaculatus) mixtus |
| 169520 | USI | Rødnebb | Cuckoo wrasse | Labrus (bimaculatus) mixtus |
| 1696 | ENX | Gressgylt | Rock cook | Centrolabrus exoletus |
| 169601 | ENX | Gressgylt (oppdrett) | Rock cook | Centrolabrus exoletus |
| 1697 | AKL | Brungylt | Scale-rayed wrasse | Acantholabrus palloni |
| 169701 | AKL | Brungylt (oppdrett) | Scale-rayed wrasse | Acantholabrus palloni |
| 1699 | MZZ | Piggfinnefisk, uspes. | Demersal percomorphs nei | Perciformes |
| 169901 | NA | Rensefisk (oppdrett) | Cleanerfish | NA |
| 341 | NA | NA | Butterfishes | Stromateidae |
| 3411 | BUT | Atlantic (American) butterfish | Atlantic (American) butterfish | Peprilus triacanthus |
| 342 | NA | Fløyfiskfamilien | Dragonets | Callionymidae |
| 3421 | LYY | Vanlig fløyfisk | Dragonet | Callionymus lyra |
| 17 | NA | SLIMFISKER | NA | BLENNOIDEI |
| 171 | NA | Steinbitfamilien | NA | Anarhichadidae |
| 1711 | CAA | Gråsteinbit | Atlantic wolffish (= Catfish) | Anarhichas lupus |
| 171101 | CAA | Gråsteinbit (oppdrett) | Atlantic wolffish (= Catfish) | Anarhichas lupus |
| 1712 | CAS | Flekksteinbit | Spotted wolffish (= Catfish) | Anarhichas minor |
| 171201 | CAS | Flekksteinbit (oppdrett) | Spotted wolffish (= Catfish) | Anarhichas minor |
| 1713 | CAB | Blåsteinbit | Northern wolffish | Anarhichas denticulatus |
| 1719 | CAT | Steinbiter | Wolffishes (= Catfishes) nei | Anarhichas spp |
| 172 | NA | Ålekvabbefamilien | NA | Zoarcidae |
| 1721 | ELP | Ålekvabbe | Eelpout | Zoearces viviparus |
| 1722 | ELZ | Ulvefisk | Greater eelpout | Lycodes esmarkii |
| 1723 | OPT | Ocean pout | Ocean pout | Macrozoarces (Zoarces) americanus |
| 1799 | MZZ | Annen slimfisk | Blennoidei | Blennoidei |
| 18 | NA | AMMODYTOIDEI | NA | AMMODYTOIDEI |
| 181 | NA | Silfamilien | NA | Ammodytidae |
| 1811 | SAN | Tobis og annen sil | Sandeels (= Sandlances) nei | Ammodytes |
| 1812 | ABZ | Småsil | Small sandeel | Ammodytes tobianus |
| 1813 | SAN | Havsil | Lesser sandeel | Ammodytes marinus |
| 1815 | YEZ | Storsil | Great sandeel | Hyperoplus lanceolatus |
| 19 | NA | TRICHIUROIDEI | NA | TRICHIUROIDEI |
| 191 | NA | Trådstjertfamilien | NA | Trichiuridae |
| 1911 | SFS | Slirefisk | Silver scabbardfish | Lepidopus caudatus |
| 1912 | BSF | Dolkfisk/trådstjert | Black scabbardfish | Aphanopus carbo |
| 20 | NA | MAKRELLFISKER | NA | SCOMBROIDEI |
| 2011 | BON | Stripet pelamide | Atlantic bonito | Sarda sarda |
| 2012 | MAS | Spansk makrell | Chub mackerel | Scomber japonicus |
| 2013 | MAC | Makrell | Atlantic mackerel | Scomber scombrus |
| 2014 | SSM | Atl. spansk makrell | Atlantic spanish mackerel | Scomberomorus maculatus |
| 2015 | MAX | Annen makrell | Mackerels | Scombridae |
| 2016 | FRZ | Auxid | Frigate & bullet tunas | Auxis thazard,A. rochei |
| 2017 | LTA | Tunnin | Atlantic black skipjack | Euthynnus alletteratus |
| 2018 | SKJ | Bukstripet pelamide | Skipjack tuna | Katsuwonus pelamis |
| 2019 | BFT | Makrellstørje | Atlantic bluefin tuna | Thunnus thynnus |
| 2021 | ALB | Albakor | Albacore | Thunnus alalunga |
| 2022 | YFT | Yellowfin tuna | Yellowfin tuna | Thunnus albacares |
| 2023 | BET | Bigeye tuna | Bigeye tuna | Thunnus obesus |
| 203 | NA | NA | NA | Istiophoridae |
| 2031 | SAI | Seilfisk | Atlantic sailfish | Istiophorus albicans |
| 2032 | BUM | Atlantic blue marlin | Atlantic blue marlin | Makaira nigricans |
| 2033 | WHM | Atlantic white marlin | Atlantic white marlin | Tetrapturus albidus |
| 205 | NA | Sverdfiskfamilien | NA | Xiphiidae |
| 2051 | SWO | Sverdfisk | Swordfish | Xiphias gladius |
| 2099 | TUN | Annen tunfisk | Tunas | Thunnini |
| 210 | NA | Kutlingfamilien | NA | Gobiidae |
| 2101 | GPA | Kutling | Gobies | Gobiidae |
| 2102 | GBF | Tangkutling | Two-spotted goby | Gobiusculus flavescens |
| 2105 | GBN | Svartkutling | black goby | Gobius niger |
| 2110 | OBD | Leirkutling | Common goby | Pomatoschistus microps |
| 2111 | OBZ | Sandkutling | Sand goby | Pomatoschistus minutus |
| 2112 | UFB | Benguela-kutling | Pelagic goby | Sufflogobius bibarbatus |
| 220 | NA | Uerfamilien | NA | Scorpaenidae |
| 2201 | RED | Uer uspes. | Atlantic redfishes | Sebastes |
| 2202 | REG | Uer (vanlig) | Golden redfish | Sebastes norvegicus |
| 2203 | REB | Snabeluer | Beaked redfish | Sebastes mentella |
| 220301 | REB | Snabeluer (Irmingerhavet) | Beaked redfish | Sebastes mentella |
| 2204 | SFV | Lusuer | Norway redfish | Sebastes viviparus |
| 2205 | BRF | Blåkjeft | Bluemouth | Helicolenus dactylopterus |
| 2206 | TJX | Atlantic thornyhead (Spiny scorpionfish) | Atlantic thornyhead (Spiny scorpionfish) | Trachyscorpia cristulata echinata |
| 2211 | GUX | Knurr uspes. | Gurnards, searobins | Triglidae |
| 2212 | GUG | Knurr | Grey gurnard | Eutrigla gurnardus |
| 2213 | GUR | Tverrstripet knurr | Red gurnard | Chelidonichthys cuculus |
| 2214 | GUU | Rødknurr | Tub gurnard | Chelidonichthys lucernus (lucerna) |
| 2215 | SRA | Atlantic searobins | Atlantic searobins | Prionotus spp |
| 222 | NA | Rognkjeksfamilien | NA | Cyclopteridae |
| 2221 | LUM | Rognkjeks (felles) | Lumpfish (=Lumpsucker) | Cyclopterus lumpus |
| 222110 | LUM | Rognkall (han) | Lumpfish (= Lumpsucker) | Cyclopterus lumpus |
| 222120 | LUM | Rognkjeks (hun) | Lumpfish (= Lumpsucker) | Cyclopterus lumpus |
| 222130 | LUM | Rognkjeks (oppdrett) | Lumpfish (= Lumpsucker) | Cyclopterus lumpus |
| 223 | NA | Ulkefamilien | Sculpins | Cottidae |
| 2231 | MXV | Vanlig ulke | Shorthorn sculpin | Myoxocephalus scorpius |
| 2232 | IBI | Tornulke | Twohorn sculpin | Icelus bicornis |
| 2233 | TGM | Nordlig knurrulke | Moustache sculpin | Triglops murrayi |
| 2234 | ZTG | Arktisk knurrulke | Ribbed sculpin | Tripglops pingelii |
| 2235 | GWY | Glattulke | Arctic staghorn sculpin | Gymnocanthus tricuspis |
| 2236 | ZAA | Krokulke | Atlantic hookear sculpin | Artediellus atlanticus |
| 2237 | XTA | Dvergulke | Longspined bullhead | Taurulus bubalis |
| 223701 | XTA | Dvergulke (oppdrett) | Longspined bullhead | Taurulus bubalis |
| 2299 | NA | Annen ulkefisk | Scorpian fishes, gurnards nei | Scorpaeniformes |
| 2311 | HAL | Kveite | Atlantic halibut | Hippoglossus hippoglossus |
| 231101 | HAL | Kveite (oppdrett) | Atlantic halibut | Hippoglossus hippoglossus |
| 2312 | PLE | Rødspette | European plaice | Pleuronectes platessa |
| 2313 | GHL | Blåkveite | Greenland halibut | Reinhardtius hippoglossoides |
| 2314 | WIT | Smørflyndre | Witch flounder | Glyptocephalus cynoglossus |
| 2315 | PLA | Gapeflyndre | Amer. Plaice(=Long rough dab) | Hippoglossoides platessoides |
| 2316 | YEL | Yellowtail flounder | Yellowtail flounder | Limanda ferruginea |
| 2317 | DAB | Sandflyndre | Common dab | Limanda limanda |
| 2318 | LEM | Lomre | Lemon sole | Microstomus kitt |
| 2319 | FLE | Skrubbe | European flounder | Platichthys flesus |
| 2321 | FLW | Winter flounder | Winter flounder | Pseudopleuronects americanus |
| 2329 | PLZ | Annen flyndre | Right eye flounders | Pleuronectidae |
| 234 | NA | Tungefamilien | NA | Soleidae |
| 2341 | SOL | Tunge | Common sole | Solea solea |
| 2342 | SOS | Sandtunge | Sand sole | Solea lascaris |
| 2343 | CET | Wedge sole (Senegal) | Wedge sole (Senegal) | Dicologlossa cuneata |
| 2344 | OAL | Tunge (Senegal) | Senegalese sole | Solea senegalensis |
| 2349 | SOX | Annen tunge | Soles | Soleidae |
| 235 | NA | Varfamilien | NA | Scophthalmidae |
| 2351 | MEG | Glassvar | Megrim | Lepidorhombus whiffiagonis |
| 2352 | BLL | Slettvar | Brill | Scophthalmus rhombus |
| 2353 | FLD | Windowpane flounder | Windowpane flounder | Scophthalmus aquosus |
| 2354 | TUR | Piggvar | Turbot | Scophthalmus maximus |
| 235401 | TUR | Piggvar (oppdrett) | Turbot | Scophthalmus maximus |
| 2355 | FLS | Summer flounder | Summer flounder | Paralichthys dentatus |
| 2356 | MSF | Tungevar | Mediterranean scaldfish | Arnoglossus laterna |
| 2357 | LEZ | Megrim nei | Megrim nei | Lepidorhombus |
| 2359 | LEF | Annen var | Left eye flounders | Bothidae |
| 2399 | FLX | Annen flatfisk | Flatfishes | Pleuronectiformes |
| 24 | NA | BREIFLABBER | NA | LOPHIIFORMES |
| 241 | NA | Breiflabbfamilien | NA | Lophiidae |
| 2411 | MON | Breiflabb | Angler (= Monk) | Lophius piscatorius |
| 2412 | ANG | American angler | American angler | Lophius americanus |
| 2499 | ANF | Andre av breiflabbfamilien | Anglerfishes | Lophiidae |
| 29 | NA | ANNEN FISK | NA | NA |
| 2919 | MZZ | Annen marin fisk | Marine fishes | Osteichthyes |
| 2929 | GRO | Groundfishes nei | Groundfishes nei | Osteichthyes |
| 2939 | PEL | Pelagic fishes nei | Pelagic fishes nei | Osteichthyes |
| 2949 | FIN | Finfishes nei | Finfishes nei | Osteichthyes |
| 2999 | MZZ | Uspesifisert fisk | Marine fishes nei | Indeterminus |
| 30 | NA | SLIMÅLER | HAGFISHES | MYXINIFORMES |
| 301 | MYX | Slimålfamilien | Hagfishes | Myxinidae |
| 3011 | MYG | Slimål | Hagfish | Myxine glutinosa |
| 31 | NA | KIMÆRER | NA | CHIMAERIFORMES |
| 3111 | CMO | Havmus | Rabbit fish | Chimaera monstrosa |
| 3112 | HYD | Ratfishes nei | Ratfishes nei | Hydrolagus spp |
| 3113 | RHC | Knife-nosed chimaeras | Knife-nosed chimaeras | Rhinochimaera spp |
| 3114 | HAR | Longnose chimaeras | Longnose chimaeras | Harriotta spp |
| 3115 | CYA | Brun havmus | Smalleyed ratfish | Hydrolagus affinis |
| 3116 | CYH | Blåvinget havmus | Large-eyed rabbitfish | Hydrolagus mirabilis |
| 3117 | HOL | Havmus uspes. | Chimaeras etc.nei | Chimaeriformes |
| 3118 | RCT | Straighnose rabbitfish | Straighnose rabbitfish | Rhinochimaera atlantica |
| 32 | NA | BÅNDFISKER | NA | SYNGNATHIFORMES |
| 3211 | LAG | Laksestørje | Opah | Lampris guttatus |
| 33 | HXW | SEKS- OG SYVGJELLETE HAIER | FRILL AND COW SHARKS | HEXANCHIFORMES |
| 331 | NA | Kamtannhaifamiliien | Cow sharks | Hexanchidae |
| 3311 | SBL | Kamtannhai | Bluntnose sixgill shark | Hexanchus griseus |
| 332 | NA | Kragehaifamilien | Frilled sharks | Chlamydoselachidae |
| 3321 | HXC | Kragehai | Frilled shark | Chlamydoselachus anguineus |
| 35 | NA | FASTKJEVEDE FISKER | PUFFERS AND FILEFISHES | TETRAODONTOFORMES |
| 351 | NA | Månefiskfamilien | Molas or Ocean sunfishes | Molidae |
| 3511 | MOX | Månefisk | Ocean sunfish | Mola mola |
| 36 | NA | LYSPRIKKFISKER | LANTERNFISHES | MYCTOPHIFORMES |
| 361 | NA | Lysprikkfiskfamilien | Lanternfishes | Myctophidae |
| 3611 | BHG | Nordlig lysprikkfisk | Glacier lanternfish | Benthosema glaciale |
| 361101 | BHG | Nordlig lysprikkfisk (oppdrett) | Glacier lanternfish | Benthosema glaciale |
| 3612 | MTP | Liten lysprikkfisk | Spotted lanternfish | Myctophum punctatum |
| 3613 | OWK | Stor lysprikkfisk | Kroyer’s lanternfish | Notoscopelus kroeyeri |
| 37 | NA | STORKJEFTFISKER | LIGHTFISHES AND DRAGON- | STOMIIFORMES |
| 371 | NA | Storkjeftfamilien | Barbeled dragonfishes | Stomiidae |
| 3711 | SBB | Storkjeft | Boa dragonfish | Stomias boa |
| 39 | NA | NESLEDYR | CORALS AND JELLYFISH | CNIDARIA |
| 391 | NA | NA | NA | NA |
| 3911 | AJH | Koralldyr | Corals | Anthozoa |
| 3912 | MVI | Siksakkorall | Madrepora coral | Madrepora oculata |
| 3913 | BFU | Sjøtre | Bubble gum coral | Paragorgia arborea |
| 3914 | QOE | Risengrynkorall | Red trees | Primnoa resedaeformis |
| 3915 | LWS | Øyekorall | Lophelia pertusa | Lophelia pertusa |
| 3916 | PZL | Sjøbusk | Paramuricea spp | Paramuricea spp |
| 40 | NA | SVAMPER | SPONGES | PORIFERA |
| 409 | NA | NA | NA | NA |
| 4099 | PFR | Svamper | Sponges | Porifera |
| 25 | NA | KREPSDYR | NA | CRUSTACEA |
| 2510 | NA | Krill | Euphausiacea | Euphausiacea |
| 251001 | NA | Krill (oppdrett) | Euphausiacea | Euphausiacea |
| 2511 | NKR | Norsk storkrill | Norwegian krill | Meganyctiphanes norvegica |
| 2512 | JCM | Raudåte | Calanus finmarchicus | Calanus finmarchicus |
| 251201 | JCM | Raudåte (oppdrett) | Calanus finmarchicus | Calanus finmarchicus |
| 2513 | KRI | Antarktisk krill | Antarctic krill | Euphausia superba |
| 251401 | JCA | Acartia tonsa (oppdrett) | Acartia tonsa | Acartia tonsa |
| 2515 | WKT | Arctic sea ice amphipod | Arctic sea ice amphipod | Gammarus wilkitzkii |
| 2516 | WSE | Gammarus setosus | Gammarus setosus | Gammarus setosus |
| 2517 | QLT | Onisimus litoralis | Onisimus litoralis | Onisimus litoralis |
| 2518 | PNQ | Stripet strandreke | Rockpool prawn | Palaemon elegans |
| 2519 | AES | Blomsterreke | Aesop shrimp | Pandalus montagui |
| 2520 | HVO | Sjøgressreke | Chameleon prawn | Hippolyte varians |
| 2521 | UJP | Dvergreke | Doll eualid | Eualus pusiolus |
| 2522 | PEN | Reke av Penaeusslekten | Penaeus shrimps nei | Penaeus spp |
| 2523 | PAN | Reke av Pandalusslekten | Pandalus shrimps nei | Pandalus |
| 2524 | PRA | Dypvannsreke | Northern prawn | Pandalus borealis |
| 2525 | PAL | Reke av Palaemonidaeslekten | Palaemonid shrimps nei | Palaemonidae |
| 2526 | CPR | Common prawn | Common prawn | Palaemon serratus |
| 2527 | CRN | Reke av Crangonidaeslekten | Crangonid shrimps nei | Crangonidae |
| 2528 | CSH | Hestereke | Common shrimp | Crangon crangon |
| 2529 | IRI | Kamuflasjereke | Friendly blade shrimp | Spirontocaris liljeborgi |
| 2530 | PAA | Strandreke | Baltic prawn | Palaemon adspersus |
| 253001 | PAA | Strandreke (oppdrett) | Baltic prawn | Palaemon adspersus |
| 2531 | CRW | Langust, upes. | Palinurid spiny lobsters nei | Palinurus spp |
| 2532 | CRE | Taskekrabbe | Edible crab | Cancer pagurus |
| 253210 | CRE | Taskekrabbe, han- | Edible crab | Cancer pagurus |
| 253220 | CRE | Taskekrabbe, hun- | Edible crab | Cancer pagurus |
| 2533 | CRS | Svømmekrabbe, uspes. | Portunus swimcrabs nei | Portunus spp |
| 2534 | KCD | Kongekrabbe | Red king crab | Paralithodes camtschaticus |
| 253410 | KCD | Kongekrabbe, han- | Red king crab | Paralithodes camtschaticus |
| 253420 | KCD | Kongekrabbe, hun- | Red king crab | Paralithodes camtschaticus |
| 2535 | KCT | Trollkrabbe | Stone king crab | Lithodes maja |
| 2536 | CRQ | Snøkrabbe | Queen crab | Chionoecetes opilio |
| 2537 | CRG | Strandkrabbe | Green crab | Carcinus maenas |
| 2539 | CRA | Annen krabbe | Marine crabs | Reptantia |
| 2541 | NEP | Sjøkreps | Norway lobster | Nephrops norvegicus |
| 2542 | LBE | Hummer | European lobster | Homarus gammarus |
| 2543 | LBA | Amerikansk hummer | American lobster | Homarus americanus |
| 2544 | AAS | Edelkreps (ferskvann) | Noble crayfish | Astacus astacus |
| 2545 | KEF | Deep-sea red crab | Deep-sea red crab | Chaceon affinis |
| 2546 | LOQ | Krinakrabbe | Galathea strigosa | Galathea strigosa |
| 2547 | UEX | Muddertrollkreps, uspes. | Munida spp | Munida spp |
| 2548 | LOQ | Trollhummer | Craylets, squat lobsters nei | Galatheidae |
| 2549 | UEM | Muddertrollkreps | Munida sarsi | Munida sarsi |
| 2550 | PZW | Bernakereremittkreps | Common hermit crab | Pagurus bernhardus |
| 255001 | PZW | Bernakereremittkreps (oppdrett) | Common hermit crab | Pagurus bernhardus |
| 2551 | GXB | Dverghummer | Galathea nexa | Galathea nexa |
| 255201 | NA | Calanus helgolandicus (oppdrett) | Calanus helgolandicus | Calanus helgolandicus |
| 255301 | NA | Acartia longiremis (oppdrett) | Acartia longiremis | Acartia longiremis |
| 255401 | NA | Centropages hamatus (oppdrett) | Centropages hamatus | Centropages hamatus |
| 255501 | NA | Pseudocalanus elongatus (oppdrett) | Pseudocalanus elongatus | Pseudocalanus elongatus |
| 255601 | NA | Calanus glacialis | Calanus glacialis | Calanus glacialis |
| 255701 | NA | Calanus hyperboreus | Calanus hyperboreus | Calanus hyperboreus |
| 255901 | AMS | Saltsjøkreps (oppdrett) | Brine shrimp | Artemia salina |
| 2560 | BXL | Fjærerur | Semibalanus balanoides | Semibalanus balanoides |
| 256101 | NA | Vannloppe, upes.(oppdrett) | Podon spp | Podon spp |
| 256201 | NA | Skipsrur (oppdrett) | Balanus crenatus | Balanus crenatus |
| 256301 | NA | Acartia clausi (oppdrett) | Acartia clausi | Acartia clausi |
| 256401 | NA | Eurytemora spp (oppdrett) | Eurytemora spp | Eurytemora spp |
| 256501 | NA | Metridia longa (oppdrett) | Metridia longa | Metridia longa |
| 256601 | NA | Paraeuchaeta barbata (oppdrett) | Paraeuchaeta barbata | Paraeuchaeta barbata |
| 256701 | NA | Pseudocalanus minutus (oppdrett) | Pseudocalanus minutus | Pseudocalanus minutus |
| 256801 | NA | Pseudocalanus acuspes (oppdrett) | Pseudocalanus acuspes | Pseudocalanus acuspes |
| 256901 | NA | Pseudocalanus moultoni (oppdrett) | Pseudocalanus moultoni | Pseudocalanus moultoni |
| 257001 | NA | Temora longicornis (oppdrett) | Temora longicornis | Temora longicornis |
| 257101 | NA | Oithona similis (oppdrett) | Oithona similis | Oithona similis |
| 257201 | NA | Apherusa glacialis (oppdrett) | Apherusa glacialis | Apherusa glacialis |
| 257301 | NA | Eusirus holmii (oppdrett) | Eusirus holmii | Eusirus holmii |
| 257401 | NA | Trollistidskreps (oppdrett) | Gammaracanthus lacustris | Gammaracanthus lacustris |
| 257501 | NA | Onisimus glacialis (oppdrett) | Onisimus glacialis | Onisimus glacialis |
| 257601 | NA | Themisto libellula (oppdrett) | Themisto libellula | Themisto libellula |
| 257701 | NA | Vanlig tangloppe (oppdrett) | Gammarus locusta | Gammarus locusta |
| 2578 | FAC | Rødglassreke | Crimson pasiphaeid | Pasiphaea tarda |
| 257801 | FAC | Rødglassreke (oppdrett) | Crimson pasiphaeid | Pasiphaea tarda |
| 257901 | IOD | Vanlig svømmekrabbe (oppdrett) | Blue-leg swim crab | Liocarcinus depurator |
| 258001 | NA | Anemoneeremittkreps (oppdrett) | Pagurus prideaux | Pagurus prideaux |
| 2582 | NA | Dvergsvømmekrabbe | Liocarcinus pusillus | Liocarcinus pusillus |
| 2583 | LQA | Rettsnutet svømmekrabbe | Arched swimming crab | Liocarcinus arcuatus |
| 2584 | XPL | Marmorkrabbe | Risso’s crab | Xantho pilipes |
| 2585 | MVD | Sandpyntekrabbe | Atlantic lyre crab | Hyas araneus |
| 2586 | MVH | Gitarpyntekrabbe | Arctic lyre crab | Hyas coarctatus |
| 2587 | IFO | Langfotkrabbe | Scorpion spider crab | Inachus dorsettensis |
| 2588 | IFS | Stankelbenkrabbe | Long-legged spider crab | Macropodia rostrata |
| 2589 | LXT | Porselenskrabbe | Long-clawed porcelain crab | Pisidia longicornis |
| 2590 | JET | Steinkrabbe | Bryer’s nut crab | Ebalia tumefacta |
| 259201 | NA | Rhithropanopeus harrissi (oppdrett) | Rhithropanopeus harrissi | Rhithropanopeus harrissi |
| 259301 | NA | Brachynotus sexdentatus (oppdrett) | Brachynotus sexdentatus | Brachynotus sexdentatus |
| 259401 | NA | Lakselus (oppdrett) | Lepeophtheirus salmonis | Lepeophtheirus salmonis |
| 259501 | NA | Caligus elongatus | Caligus elongatus | Caligus elongatus |
| 2599 | CRU | Andre krepsdyr | Marine crustaceans | Crustacea |
| 26 | NA | BLØTDYR | NA | MOLLUSCA |
| 2611 | OYF | Østers | European flat oyster | Ostrea edulis |
| 261101 | OYF | Østers (oppdrett) | European flat oyster | Ostrea edulis |
| 2612 | OYC | Stillehavsøsters | Cupped oysters | Crassostrea spp |
| 2613 | CTG | Rutet teppeskjell | Grooved carpet shell | Ruditapes decussatus |
| 2614 | CLJ | Asiatisk teppeskjell (Manilaskjell) | Japanese carpet shell | Ruditapes philippinarum |
| 2615 | SVE | Stripet teppeskjell | Striped venus | Chamelea gallina |
| 2616 | QRG | Greenland smoothcockle | Greenland smoothcockle | Serripes groenlandicus |
| 2617 | ZCZ | Hairy cockle | Hairy cockle | Ciliatocardium ciliatum |
| 2618 | VSC | Urskjell | Variegated scallop | Chlamys varia |
| 2619 | CLQ | Kuskjell | Ocean quahog | Cyprina islandica |
| 2620 | CLS | Sandskjell | Sand gaper | Mya arenaria |
| 2621 | SCE | Stor kamskjell (Kamskjell) | Common scallop | Pecten maximus |
| 262101 | SCE | Stort kamskjell (oppdrett) | Great Atlantic scallop | Pecten maximus |
| 2622 | QSC | Harpeskjell | Queen scallop | Chlamys opercularis |
| 2623 | MUS | Blåskjell | Blue mussel | Mytilus edulis |
| 262301 | MUS | Blåskjell (oppdrett) | Blue mussel | Mytilus edulis |
| 2624 | DJO | O-skjell | Northern horse mussel | Modiolus modiolus |
| 2625 | CLB | Atlantic surf clam | Atlantic surf clam | Spisula solidissima |
| 2626 | ISC | Haneskjell | Islandic scallop | Chlamys islandica |
| 2627 | COC | Saueskjell, uspes. | Common edible cockle | Cerastoderma edule |
| 2628 | LPZ | Albuskjell | Limpets nei | Patella spp |
| 2629 | SCX | Annen kammusling | Scallops | Pectinidae |
| 2630 | EOI | Vanlig åttearmet blekksprut | Horned octopus | Eledone cirrhosa |
| 2631 | SQC | Vanlig ti-armet blekksprut uspes. | Common squids nei | Lolio spp |
| 2632 | SQL | Langfinnet vanlig ti-armet blekksprut | Longfinned squid | Lolio pealei |
| 2633 | SQI | Nordlig kortfinnet ti-armet blekksprut | Northern shortfin squid | Illex illecebrosus |
| 2634 | SQE | Akkar | European flying squid | Todarodes sagittatus |
| 2635 | OCT | Åtte-armet blekksprut uspes. | Octopuses | Octopodidae |
| 2636 | SQU | Annen vanlig ti-armet blekksprut | Squids | Loliginidae, Ommastrephidae |
| 2637 | CTL | Annen ti-armet blekksprut | Cuttlefishes | Sepiidae, Sepiolidae |
| 2638 | CEP | Blekksprut uspes. | Cephalopods | Cephalopoda |
| 2639 | SQZ | Annen blekksprut | Inshore squids nei | Loliginidae |
| 2640 | URC | Kråkebolle, uspes. | Sea urchins | Echinoidea |
| 2642 | KHG | Brunpølse | Pudding | Cucumaria frondosa |
| 2643 | TVK | Rødpølse | Red sea cucumber | Parastichopus tremulus |
| 264301 | TVK | Rødpølse (oppdrett) | Red sea cucumber | Parastichopus tremulus |
| 2644 | STH | Vanlig korstroll | Red starfish | Asterias rubens |
| 2645 | ECH | Andre pigghuder | Echinoderms | Echinodermata |
| 2646 | VUC | Teppeskjell | Corrugated venus/Pullet carpet shell | Venerupis corrugata |
| 264601 | VUC | Teppeskjell (oppdrett) | Corrugated venus/Pullet carpet shell | Venerupis corrugata |
| 264701 | VNR | Banded carpet shell (oppdrett) | Banded carpet shell | Polititapes virgineus (Venerupis rhomboides) |
| 2650 | PER | Strandsnegl, uspes. | Periwinkles | Littorinidae |
| 2651 | WHE | Kongsnegl (Kongesnegl) | Whelk | Buccinum undatum |
| 265201 | NA | Pusillina tumidula (Pseudosetia griegi) (oppdrett) | Pusillina tumidula (Pseudosetia griegi) | Pusillina tumidula (Pseudosetia griegi) |
| 265301 | NA | Skenea profunda (oppdrett) | Skenea profunda | Skenea profunda |
| 265901 | HLT | Tuberculate abalone (oppdrett) | Tuberculate abalone | Haliotis tuberculata |
| 2660 | UYD | Drøbaksjøpiggsvin | Strongylocentrotus droebachiensis | Strongylocentrotus droebachiensis |
| 2661 | USD | Langpiggsjøpiggsvin | Echinus acutus(Gracilechinus acutus) | Echinus acutus(Gracilechinus acutus) |
| 2662 | URS | Rød kråkebolle | European edible sea urchin | Echinus esculentus |
| 2663 | KIT | Grønnsjøpiggsvin | Psammechinus miliaris | Psammechinus miliaris |
| 266401 | NA | Leptochiton asellus (oppdrett) | Leptochiton asellus | Leptochiton asellus |
| 266501 | NA | Sjuarmsjøstjerne (oppdrett) | Luidia ciliaris | Luidia ciliaris |
| 266601 | NA | Fem-armet sjøstjerne (oppdrett) | Luidia sarsii | Luidia sarsii |
| 266701 | NA | Piggsolstjerne (oppdrett) | Crossaster papposus | Crossaster papposus |
| 266801 | NA | Piggkorstroll (oppdrett) | Marthasterias glacialis | Marthasterias glacialis |
| 267001 | NA | Limaria hians (oppdrett) | Limaria hians | Limaria hians |
| 2671 | YTK | Butt sandskjell | Blunt gaper | Mya truncata |
| 267201 | NA | Østersjøskjell (oppdrett) | Macoma balthica | Macoma balthica |
| 2673 | ZMC | Chalky macoma | Chalky macoma | Macoma calcarea |
| 2675 | ZHA | Steinboreskjell | Wrinkled rock borer | Hiatella arctica |
| 2676 | SOI | Knivskjell | Razor clams, knife clams nei | Solenidae |
| 2677 | COZ | Hjerteskjell | Cockles nei | Cardiidae |
| 2680 | SSX | Sekkdyr, uspes. | Sea squirts nei | Ascidiacea |
| 2681 | KOJ | Grønnsekkdyr | Ciona | Ciona intestinalis |
| 268201 | NA | Oikopleura (Coecaria) fusiformis (oppdrett) | Oikopleura (Coecaria) fusiformis | Oikopleura (Coecaria) fusiformis |
| 268301 | NA | Oikopleura (Vexillaria) gorskyi (oppdrett) | Oikopleura (Vexillaria) gorskyi | Oikopleura (Vexillaria) gorskyi |
| 268401 | NA | Oikopleura (Vexillaria) labradoriensis (oppdrett) | Oikopleura (Vexillaria) labradoriensis | Oikopleura (Vexillaria) labradoriensis |
| 268501 | NA | Oikopleura (Vexillaria) parva (oppdrett) | Oikopleura (Vexillaria) parva | Oikopleura (Vexillaria) parva |
| 268601 | NA | Oikopleura (Vexillaria) villafrancae (oppdrett) | Oikopleura (Vexillaria) villafrancae | Oikopleura (Vexillaria) villafrancae |
| 268701 | NA | Oikopleura (Vexillaria) dioica (oppdrett) | Oikopleura (Vexillaria) dioica | Oikopleura (Vexillaria) dioica |
| 268801 | NA | Oikopleura (Vexillaria) longocauda (oppdrett) | Oikopleura (Vexillaria) longocauda | Oikopleura (Vexillaria) longocauda |
| 268901 | NA | Oikopleura (Vexillaria) vanhoeffeni (oppdrett) | Oikopleura (Vexillaria) vanhoeffeni | Oikopleura (Vexillaria) vanhoeffeni |
| 269001 | NA | Fallossekkdyr(oppdrett) | Ascidia mentula | Ascidia mentula |
| 269101 | NA | Langhalssekkdyr(oppdrett) | Clavelina lepadiformis | Clavelina lepadiformis |
| 269201 | NA | Ciona edwardsi (oppdrett) | Ciona edwardsi | Ciona edwardsi |
| 269301 | NA | Ciona fascilularis (oppdrett) | Ciona fascilularis | Ciona fascilularis |
| 269401 | NA | Ciona gelatinosa (oppdrett) | Ciona gelatinosa | Ciona gelatinosa |
| 269501 | NA | Ciona imperfecta (oppdrett) | Ciona imperfecta | Ciona imperfecta |
| 269601 | NA | Oransjesekkdyr(oppdrett) | Polyclinum aurantium | Polyclinum aurantium |
| 2697 | GAS | Annen snegl | Gastropods nei | Gastropoda |
| 2698 | CLX | Annen skjell | Clams, etc, nei | Bivalvia |
| 2699 | MOL | Annet bløtdyr | Marine molluscs | Mollusca |
| 27 | NA | PATTEDYR | NA | MAMMALIA |
| 2711 | SEH | Grønlandssel | Harp seal | Pagophilus groenlandicus |
| 2712 | SEC | Steinkobbe | Harbour seal | Phoca vitulina |
| 2713 | SER | Ringsel | Ringed seal | Phoca hispida |
| 2714 | SEZ | Klappmyss | Hooded seal | Cystophora cristata |
| 2715 | SEG | Havert | Grey seal | Halichoerus grypus |
| 2716 | SEB | Blåsel | Bearded seal | Erignatus barbatus |
| 2719 | SXX | Annen sel | Seals and sea lions nei | Otariidae, Phocidae |
| 2720 | PHR | Nise | Harbour porpoise | Phocoena phocoena |
| 2721 | BOW | Tumler | Northern bottlenose whale | Hyperoodon ampullatus |
| 2722 | BEW | Nebbhval | Beaked whale | Berardius bairdii |
| 2723 | SPW | Spermhval | Sperm whale | Physeter macrocephalus |
| 2724 | DCO | Delfin | Common dolphin | Delphinus delphis |
| 2725 | PIW | Grindhval | Longfin pilot whale | Globicephala melas |
| 2726 | SHW | Shortfin pilot whale | Shortfin pilot whale | Globicephala macrorhynchus |
| 2727 | KIW | Spekkhogger | Killer whale | Orcinus orca |
| 2728 | BEL | Hvithval | Beluga(= White whale) | Delphinapterus leucas |
| 2729 | MIW | Vågehval | Minke whale | Balaenoptera acutorostrata |
| 2731 | BRW | Brydehval | Bryde’s whale | Balaenoptera edeni |
| 2732 | SIW | Seihval | Sei whale | Balaenoptera borealis |
| 2733 | BLW | Blåhval | Blue whale | Balaenoptera musculus |
| 2734 | FIW | Finnhval | Fin whale | Balaenoptera physalus |
| 2735 | CPM | Dvergretthval | Pigmy whale | Caperea marginata |
| 2736 | BWD | Kvitnos | White beaked dolphin | Lagenorhynchus albirostris |
| 2737 | BWW | Spisshval | Sowerby’s whale | Mesoplodon bidens |
| 2738 | WAL | Hvalross | Walrus | Odobenus rosmarus |
| 2739 | ODN | Annen tannhval | Toothed whales | Odontoceti |
| 2749 | MYS | Annen bardehval | Baleen whales | Mysticeti |
| 2759 | MAM | Annen hval | Whales | Cetacea |
| 2799 | MAM | Andre sjøpattedyr | Aquatic mammals nei, | Mammalia |
| 28 | NA | ALGER | NA | ALGAE |
| 2811 | SWB | Andre brunalger | Brown seaweed | Phaeophyceae |
| 2812 | LQX | Sukkertare | Sea belt | Saccharina latissima |
| 2813 | LAH | Stortare | North European kelp | Laminaria hyperborea |
| 2814 | LQD | Fingertare | Tangle | Laminaria digitata |
| 2815 | LAZ | Tare uspes. | Kelps nei | Laminariaceae |
| 2816 | AJC | Butare | Babberlocks | Alaria esculenta |
| 2817 | FUV | Blæretang | Bladder wrack | Fucus vesiculosus |
| 2818 | FUU | Sagtang | Toothed wrack | Fucus serratus |
| 2819 | UCU | Tang uspes. | Fucus spp | Fucus spp |
| 2820 | ASN | Grisetang | North Atlantic rockweed | Ascophyllum nodosum |
| 2821 | WAP | Grønlandsbutare | Alaria pylaiei | Alaria pylaiei |
| 2822 | ZSD | Bladtare | Saccorhiza dermatodea | Saccorhiza dermatodea |
| 2823 | ZSY | Draugtare | Furbellow | Saccorhiza polyschides |
| 2824 | ZFC | Martaum | Bootlace weed | Chorda filum |
| 2825 | IMS | Krusflik | Mousse perle | Chondrus crispus |
| 2826 | HLZ | Knapptang | Sea thong | Himanthalia elongata |
| 2827 | FUP | Sauetang | Channel wrack | Pelvetia canaliculata |
| 2828 | FDS | Kaurtang | Spiral wrack | Fucus spiralis |
| 282901 | NA | Japansk drivtang (oppdrett) | Sargassum muticum | Sargassum muticum |
| 2830 | SWR | Andre rødalger | Red seaweeds | Rhodophyceae |
| 2831 | FYS | Fjærehinne uspes | Nori nei | Porphyra spp |
| 2832 | RHP | Søl | Dulse | Palmaria palmata |
| 2833 | OFH | Vanlig fjærehinne | Pink laver | Porphyra umbilicalis |
| 2834 | MVT | Vorteflik | False Irish moss | Mastocarpus stellatus |
| 2835 | OFN | Smal fjærehinne | Ribboned nori | Porphyra linearis |
| 2836 | OFQ | Purpurfjærehinne | Purple laver | Porphyra purpurea |
| 2837 | GJP | Smal agaralge | Dwarf gelidium | Gelidium pusillum |
| 2838 | NLO | Rødsleipe | Sea spaghetti | Nemalion helminthoides |
| 2839 | OFK | Porphyra dioica | Porphyra dioica | Porphyra dioica |
| 2840 | SWG | Andre grønnalger | Green seaweeds | Chlorophyceae |
| 2841 | UVU | Havsalat | Sea lettuce | Ulva lactuca |
| 2860 | HFH | Sjøris | Landlady’s Wig | Ahnfeltia plicata |
| 2861 | SWQ | Fagerving | Red delesseria | Delesseria sanguinea |
| 2862 | FKU | Svartkluft | Red forkweed | Furcellaria lumbricalis |
| 2863 | GZG | Pollris | Slender wart weed | Gracilaria gracilis |
| 286401 | NA | Vanlig rosenrør (oppdrett) | Lomentaria clavellosa | Lomentaria clavellosa |
| 2870 | KMY | Vanlig grønndusk | Common green branched weed | Cladophora rupestris |
| 2871 | UVI | Tarmgrønske | Gut weed | Ulva intestinalis |
| 2872 | KII | Pollpryd | Fragile codium | Codium fragile |
| 2890 | VLA | Grisetangdokke | Vertebrata lanosa | Vertebrata lanosa |
| 2899 | APL | Annen tang og tare | Aquatic plants | Plantae Aquaticae |
| 50 | NA | FERSKVANNSFISK | NA | OSTEICHTHYES |
| 501 | NA | NA | NA | NA |
| 5011 | CGO | Gullfisk | Goldfish | Carassius auratus |
| 5012 | DAI | Sebrafisk | Zebra danio | Danio rerio |
| 5013 | YCK | Fårehodetannkarpe | Sheepshead minnow | Cyprinodon variegatus |
| 5014 | PFL | Guppy | Guppy | Poecilia reticulata |
| 5015 | TLN | Nilmunnruger | Nile tilapia | Oreochromis niloticus |
| 5016 | OZS | Molly | Molly | Poecilia sphenops |
| 5017 | OWJ | Medaka | Japanese rice fish | Oryzias lapides |
| 6111 | SEM | Common (blue) warehou | Common (blue) warehou | Seriolella brama |
| 6112 | SEP | Silver warehou | Silver warehou | Seriollella punctata |
| 6121 | STZ | Giant stargazer, monkfish | Giant stargazer, monkfish | Kathetostoma giganteum |
| 6131 | SFS | Slirefisk | Silver scabbardfish | Lepidopus caudatus |
| 6141 | YTC | Kingfish, yellowtail | Kingfish, yellowtail | Seriola lalandi |
| 6151 | TOP | Patagonsk tannfisk | Patagonian toothfish | Dissostichus eleginoides |
| 6152 | TOA | Antarktisk tannfisk | Antartic toothfish | Dissostichus mawsoni |
| 6153 | TOT | Antarktisk tannfisk,uspes. | Antartic toothfish, nei | Dissostichus spp |
| 6154 | ANS | Antarctic silverfish | Antarctic silverfish | Pleuragramma antarcticum |
| 6155 | NOG | Humped rockcod | Humped rockcod | Notothenia gibberifrons |
| 6156 | NOK | Striped-eyed rockcod | Striped-eyed rockcod | Notothenia kempi |
| 6157 | NOR | Marbled rockcod | Marbled rockcod | Notothenia rossii |
| 6158 | NOS | Grey rockcod | Grey rockcod | Lepidonotothen (Notothenia) squamifrons |
| 6159 | NOX | Tannfisk uspes. | Antarctic rockcods, noties nei | Nototheniidae |
| 6171 | NOT | Patagonian rockcod | Patagonian rockcod | Patagonotothen brevicauda |
| 6172 | TRH | Striped rockcod | Striped rockcod | Pagothenia hansoni |
| 6173 | TRL | Blunt scalyhead | Blunt scalyhead | Trematomus eulepidotus |
| 6161 | ANI | Mackerel icefish | Mackerel icefish | Champsocephalus gunnari |
| 6162 | KIF | Ocellated icefish | Ocellated icefish | Chionodraco rastrospinosus |
| 6163 | TIC | Chionodraco hamatus | Chionodraco hamatus | Chionodraco hamatus |
| 6164 | WIC | Spiny icefish | Spiny icefish | Cheanodraco wilsnoni |
| 6165 | SGI | South Georgia icefish | South Georgia icefish | Pseudochaenichthys georgianus |
| 6166 | SSI | Blackfin icefish | Blackfin icefish | Chaenocephalus aceratus |
| 6169 | ICX | Isfisk uspes. | Crocodile icefishes nei | Channichthyidae |
| 6211 | BOE | Black oreo | Black oreo | Allocyttus niger |
| 6212 | SSO | Smooth oreo dory | Smooth oreo dory | Pseudocyttus maculatus |
| 6299 | ORD | Oreo dories nei | Oreo dories nei | Oreosomatidae |
| 6311 | GRN | Blue grenadier/ Hoki | Blue grenadier/ Hoki | Macruronus novaezelandiae |
| 6312 | HKN | Southern hake | Southern hake | Merluccius australis |
| 6399 | HKX | Hakes nei | Hakes nei | Merluccius spp |
| 6329 | MRL | Patagonsk torsk uspes. | Moray cods nei | Muraenolepis spp |
| 633 | NA | Skolestfamilien | Grenadiers (rattails) | Macrouridae |
| 6331 | WGR | Sørlig skolest | Whitson’s grenadier | Macrourus whitsoni |
| 6332 | GRV | Grenadiers nei | Grenadiers nei | Macrourus spp |
| 634 | NA | Torskefamilien | Gadids | Gadiade |
| 6341 | POS | Southern blue whiting | Southern blue whiting | Micromesistius australis |
| 6411 | CUS | Pink cusk-eel | Pink cusk-eel | Genypterus blacodes |
| 65 | NA | SKATER OG ROKKER | NA | RAJIFORMES |
| 651 | NA | Skatefamilien | NA | Rajidae |
| 6511 | SRR | Antarctic starry skate | Antarctic starry skate | Raja georgiana |
| 66 | NA | PRIKKFISKER | LANTERNFISHES | MYCTOPHIFORMES |
| 661 | NA | Lysprikkfiskfamilien | Lanternfishes | Myctophidae |
| 6611 | ELC | Electron subantarctic | Electron subantarctic | Electrona carlsbergi |
| 6619 | LXX | Lanternfishes nei | Lanternfishes nei | Myctophidae |
| 70 | NA | NA | NA | NA |
| 701 | NA | NA | NA | NA |
| 7011 | PNV | Stillehavsreke | Whiteleg shrimp | Penaeus vannamei |
| 7014 | KCV | Antarctic stone crab | Antarctic stone crab | Paralomis spinosissima |
| 7015 | KCX | King crabs, stone crabs nei | King crabs, stone crabs nei | Lithodidae |
| 71 | NA | BLØTDYR | MARINE MOLLUSCS | MOLLUSCA |
| 7111 | SQS | Sevenstar flying squid | Sevenstar flying squid | Martialia hyadesi |
| 716101 | NA | Dolioletta spp (oppdrett) | Dolioletta spp | Dolioletta spp |
| 716201 | NA | Salpa spp (oppdrett) | Salpa spp | Salpa spp |
| 716301 | NA | Ciona mollis (oppdrett) | Ciona mollis | Ciona mollis |
| 716401 | NA | Ciona savignyi (oppdrett) | Ciona savignyi | Ciona savignyi |

Table 1: List of Directorate of Fisheries species codes. Use idNS or
idFAO codes as species argument in the extractLogbook function. {.table
.table .table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

### Gear codes

| idGear | gearName | gearCategory | Hovedgruppe | Subgruppe |
|---:|:---|:---|:---|:---|
| 10 | Udefinert not | Seines | Not | Not |
| 11 | Snurpenot/ringnot | Seines | Not | Not |
| 12 | Landnot | Seines | Not | Not |
| 14 | Snurpenot med lys | Seines | Not | Not |
| 15 | Landnot med lys | Seines | Not | Not |
| 20 | Udefinert garn | Gillnets | Konvensjonelle | Garn |
| 21 | Drivgarn | Gillnets | Konvensjonelle | Garn |
| 22 | Settegarn | Gillnets | Konvensjonelle | Garn |
| 30 | Udefinert krokredskap | Hook gears | Konvensjonelle | Krokredskap |
| 31 | Flyteline | Hook gears | Konvensjonelle | Krokredskap |
| 32 | Andre liner | Hook gears | Konvensjonelle | Krokredskap |
| 33 | Juksa/pilk | Hook gears | Konvensjonelle | Krokredskap |
| 34 | Dorg/harp/snik | Hook gears | Konvensjonelle | Krokredskap |
| 35 | Autoline | Hook gears | Konvensjonelle | Krokredskap |
| 40 | Udefinert bur og ruser | Traps and fykes | Konvensjonelle | Bur og ruser |
| 41 | Ruser | Traps and fykes | Konvensjonelle | Bur og ruser |
| 42 | Teiner | Traps and fykes | Konvensjonelle | Bur og ruser |
| 43 | Kilenot | Traps and fykes | Konvensjonelle | Bur og ruser |
| 44 | Havteiner | Traps and fykes | Konvensjonelle | Bur og ruser |
| 45 | Krokgarn | Gillnets | Konvensjonelle | Garn |
| 50 | Udefinert trål | Bottom trawls | Traal | Traal |
| 51 | Bunntrål | Bottom trawls | Traal | Traal |
| 52 | Bunntrål par | Bottom trawls | Traal | Traal |
| 53 | Flytetrål | Pelagic trawls | Traal | Traal |
| 54 | Flytetrål par | Pelagic trawls | Traal | Traal |
| 55 | Reketrål | Bottom trawls | Traal | Traal |
| 56 | Bomtrål | Bottom trawls | Traal | Traal |
| 57 | Krepsetrål | Bottom trawls | Traal | Traal |
| 58 | Dobbeltrål | Bottom trawls | Traal | Traal |
| 59 | Trippeltrål | Bottom trawls | Traal | Traal |
| 61 | Snurrevad | Bottom trawls | Konvensjonelle | Snurrevad |
| 70 | Harpun og lignende uspesifiserte typer. | Other | Annet | Harpun/kanon |
| 71 | Brugde /hvalkanon | Other | Annet | Harpun/kanon |
| 72 | Størjeharpun | Other | Annet | Harpun/kanon |
| 73 | Rifle | Other | Annet | Harpun/kanon |
| 80 | Annet | Other | Annet | Andre redskap |
| 81 | Skjellskrape | Other | Annet | Andre redskap |
| 82 | Håv | Other | Annet | Andre redskap |
| 83 | Taretrål | Other | Annet | Andre redskap |
| 84 | Tangkutter (-skjærer) | Other | Annet | Andre redskap |
| 85 | Håndplukking | Other | Annet | Andre redskap |
| 90 | Oppdrett | Other | Annet | Oppdrett/uspesifisert |
| 99 | Uspesifisert | Other | Annet | Andre redskap |

Table 2: List of Directorate of Fisheries gear codes. {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

## Institute of Marine Research

### Cruise series list

The `cruiseSeriesList` can be used to connect cruises in the Biotic data
with their respective cruise series. The [BioticExplorerServer
package](https://github.com/DeepWaterIMR/BioticExplorerServer) does this
automatically allowing database search using cruise series. You can
update the list with the
[`prepareCruiseSeriesList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareCruiseSeriesList.md)
function while connected to IMR intranet.

``` r

cruiseSeriesList
#>       cruiseseriescode     cruise    platformname startyear
#>                  <num>     <char>          <char>     <num>
#>    1:                1    2004204     Johan Hjort      2004
#>    2:                1    2005105        G.O.Sars      2005
#>    3:                1    2006104        G.O.Sars      2006
#>    4:                1    2007845            Eros      2007
#>    5:                1    2008809          Gardar      2008
#>   ---                                                      
#> 1217:                9    2015101        G.O.Sars      2015
#> 1218:                9    2016101        G.O.Sars      2016
#> 1219:                9    2017850 Cefas Endeavour      2017
#> 1220:                9    2018102        G.O.Sars      2018
#> 1221:                9 2024001002        G.O.Sars      2024
#>                                                                                  name
#>                                                                                <char>
#>    1: Atlantic Ocean West of British Isles INT blue whiting spawning survey in spring
#>    2: Atlantic Ocean West of British Isles INT blue whiting spawning survey in spring
#>    3: Atlantic Ocean West of British Isles INT blue whiting spawning survey in spring
#>    4: Atlantic Ocean West of British Isles INT blue whiting spawning survey in spring
#>    5: Atlantic Ocean West of British Isles INT blue whiting spawning survey in spring
#>   ---                                                                                
#> 1217:                                       North Sea International IBTS cruise in Q1
#> 1218:                                       North Sea International IBTS cruise in Q1
#> 1219:                                       North Sea International IBTS cruise in Q1
#> 1220:                                       North Sea International IBTS cruise in Q1
#> 1221:                                       North Sea International IBTS cruise in Q1
```

### Gear list

The `gearList` provides gear codes used in the IMR Biotic database:

``` r

gearList
#>        code            gearname   gearcategory
#>      <char>              <char>         <char>
#>   1:      0       Visuelle obs.          Other
#>   2:    100          Sikteskive          Other
#>   3:   1000 Innsamling av vann. Water samplers
#>   4:   1100   Vannhenter uspes. Water samplers
#>   5:   1105   Rutler Vannhenter Water samplers
#>  ---                                          
#> 703:   8410      Scanmar uspes.          Other
#> 704:   8411     Dekksenhet 0404          Other
#> 705:   8412     Dekksenhet 4004          Other
#> 706:   8413     Dekksenhet 4016          Other
#> 707:   9000      Redskapsforsøk          Other
#>                                                                                                                 description
#>                                                                                                                      <char>
#>   1:                                                                                                 Visuelle observasjoner
#>   2:                                                                                                          Uspesifisert.
#>   3:                                                                                       Redskaper for innsamling av vann
#>   4:                                                                                                          Uspesifisert.
#>   5:                                                                                             2.5 liter plastvannhenter.
#>  ---                                                                                                                       
#> 703:                                                                                                          Uspesifisert.
#> 704:                                                           Dekksenhet  404. Sensorer: dybde, avstand, hoyde, hastighet.
#> 705:                                          Dekksenhet 4004. Sensorer: dybde, avstand, hoyde, hastighet, minitransponder.
#> 706:                                          Dekksenhet 4016. Sensorer: dybde, avstand, hoyde, hastighet, minitransponder.
#> 707: Redskapsforsøk. Har kode 8000 i SPD-formatet. Kodene 9000-9999 blir reservert for redskapsforsøk. Listen blir utvidet.
```

You can update the list with the
[`prepareGearList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareGearList.md)
function while connected to IMR intranet.

### Taxa list

The `taxaList` provides all taxa used in the IMR Biotic database:

``` r

taxaList
#>           tsn aphiaid       nodc pgnapes   language                 name
#>        <char>  <char>     <char>  <char>     <fctr>               <char>
#>     1: 180541    <NA>       <NA>    <NA> scientific                Ursus
#>     2: 180540    <NA>       <NA>    <NA> scientific              Ursidae
#>     3: 659776  264959       <NA>    <NA> scientific          Aulacoctena
#>     4: 999080    <NA> 8831090248    <NA> scientific   Careproctus (kort)
#>     5: 555716  153035     570725    <NA> scientific      Walvisteuthidae
#>    ---                                                                  
#> 17101: 180473  137117 9218021801     PHR    russian      morskaya svinya
#> 17102: 172905  158885 8857041504     FLW    english      winter flounder
#> 17103: 168588  126822 8835280103     HOM  norwegian       spansk makrell
#> 17104: 168588  126822 8835280103     HOM  norwegian          taggmakrell
#> 17105: 165368  158950 8794010403    <NA> scientific Coelorinchus labitus
```

You can update the list with the
[`prepareTaxaList()`](https://deepwaterimr.github.io/RstoxUtils/reference/prepareTaxaList.md)
function while connected to IMR intranet.
