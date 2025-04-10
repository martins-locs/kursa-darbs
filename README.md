
# Materiāli "IESKATS PASĪVĀ AKUSTISKĀ MONITORINGA IESPĒJĀS UZLABOT STRUKTURĒTO INFORMĀCIJU PAR PUTNU POPULĀCIJĀM LATVIJĀ" reproducēšanai

Šis repozitorijs satur komandu rindas un daļu no ievades datiem mana kursa darba gaitas un rezultātu reproducēšanai. Tā kā daļa no datiem ir sensitīvi, piemēram, precīzas katras sugas putnu novērojumu atrašanās vietas, vai ierobežotas pieejamības, piemēram, zemes seguma/lietojuma veids šajā izšķirtspējā, tie nav šeit ievietoti. Toties repozitorijs satur visas darbā izmantotās komandu rindas un to rezultātā radītos produktus.

## Repozitorija saturs

### R skripti

- **`0_datu_apstrade.R`**  
  Importē ainavu klases un apstrādā putnu novērojumu datus, kas nepieciešami tālākai analīzei. Datus filtrē un strukturē, novēršot kļūdas un dublikātus. Tiek apstrādāti putnu novērojumu dati no XML un Excel formāta, datnes tiek konvertētas uz datu rāmjiem, veikta sugu nosaukumu saskaņošana, kā arī sagatavots datu apvienojums ar sugu pazīmēm.

- **`1_tabulas_izveide.R`**  
  Veido apvienotu un sakārtotu datu tabulu ar būtiskākajiem mainīgajiem no dažādiem avotiem. Tiek savienoti putnu novērojumu dati ar sugu pazīmēm un dziedāšanas īpašībām, pārveidoti mainīgie modeļiem piemērotā formā, kā arī izveidoti papildu indikatori par akustiskām un vizuālām sugām.

- **`2_dziedosie_putni.R`**  
  Atlasa dziedošās putnu sugas, analizē to sadalījumu un saistību ar dažādām ainavu un sugu pazīmēm. Veido dažādas vizualizācijas, piemēram, sugu izplatību dažādos biotopos. Iekļauta arī statistiskā modeļu veidošana, lai identificētu būtiskus mainīgos, kas ietekmē sugu akustisko noteikšanu.

- **`3_ainavu_preference.R`**  
  Analizē putnu sugu ainavu preferences, apvienojot sugu datus ar ekspertu vērtējumiem. Tiek veidoti loģistiskās regresijas modeļi, analizēta ainavu tipu ietekme uz sugu klātbūtni un dziedāšanu. Rezultāti tiek vizualizēti, lai izceltu būtiskās ainavu struktūras, kas ietekmē sugu izvēli.

### ievades dati/
Šī mape satur dažādu veidu ievades datus, kas izmantoti datu apstrādei un analīzei:
- `LV_birds_reports_2019.xlsx` un `LV_birds_reports_20191030-151740.xml` — putnu novērojumu dati no 2019. gada.
- `preferences.xlsx` — dati par ekspertu vērtētajām ainavu preferencēm dažādām sugām.
- `putni.xlsx` un `putni_SkaitiPazimes_kops2016.xlsx` — strukturētie dati par ligzdojošo putnu sugām Latvijā un to pazīmēm.

### Rezultāti/
Vizualizācijas un attēli, kas ilustrē galvenos darba rezultātus:
- `Putnu_populacija_monitorings.jpg` — Putnu sugu populāciju lielumu sadalījums atkarībā no populācijas lieluma datu kvalitātes klases un populāciju aprēķina veida.
- `Putnu_sugu_ainavu_preference.jpg` — Preference ainavas klasēm divos aprēķinu veidos un atsevišķi izdalītiem uz ligzdošanu attiecināmajiem novērojumiem katrā no tiem.
- `Putnu_sugu_akustiska_ainava.jpg` — Ligzdojošo putnu sugu D-B pazīmju attiecība atkarībā no preferences ainavas klasēm (attēla daļās), norādot putnu sugas populāciju datu kvalitātes klasi.
- `Putnu_sugu_akustiska_kvalitate.jpg` — Akustisko signālu nozīmes sugu novērojumos (D-B attiecība) saistībā ar populācijas lieluma informācijas kvalitāti un aprēķinu veidu Putnu Direktīvas 12. panta ziņojumā par 2013.-2018. gadu periodu.
- `prognoze.jpg` — Prognozētā varbūtība uzlabot informācijas par putnu populācijām kvalitāti, atkarībā no sugas akustiskuma un preferences izmantotajām ainavas klasēm.

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2025-04-10 pieejamajā versijā.
