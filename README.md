
# Materiāli "IESKATS PASĪVĀ AKUSTISKĀ MONITORINGA IESPĒJĀS UZLABOT STRUKTURĒTO INFORMĀCIJU PAR PUTNU POPULĀCIJĀM LATVIJĀ" reproducēšanai

Šis repozitorijs satur komandu rindas un daļu no ievades datiem mana kursa darba gaitas un rezultātu reproducēšanai. Tā kā daļa no datiem ir sensitīvi, piemēram, precīzas katras sugas putnu novērojumu atrašanās vietas, vai ierobežotas pieejamības, piemēram, zemes seguma/lietojuma veids (LULC) šajā izšķirtspējā, tie nav šeit ievietoti. Toties repozitorijs satur visas darbā izmantotās komandu rindas un to rezultātā radītos produktus.

## Repozitorija saturs

### R skripti

- **`0_datu_apstrade.R`**  
  Importē ainavu klases un apstrādā putnu novērojumu datus, kas nepieciešami tālākai analīzei. Datus filtrē un strukturē, novēršot kļūdas un dublikātus. Aprēķina taksonu ainavu preferences un rezultāti tiek vizualizēti.

- **`1_tabulas_izveide.R`**  
  Veido apvienotu un sakārtotu datu tabulu ar būtiskākajiem mainīgajiem no dažādiem avotiem. Tiek apstrādāti putnu novērojumu dati no XML un Excel formāta, datnes tiek konvertētas uz datu rāmjiem, veikta sugu nosaukumu saskaņošana, kā arī sagatavots datu apvienojums ar sugu pazīmēm.

- **`2_dziedosie_putni.R`**  
  Strādā ar putnu akustiskajiem rādītājiem un to saistībām ar īstermiņa populāciju izmaiņu datu kvalitātēm un to aprēķina veidu.

- **`3_ainavu_preference.R`**  
  Tiek veidoti loģistiskās regresijas modeļi, analizēta ainavu tipu ietekme uz sugu klātbūtni un dziedāšanu. Rezultāti tiek vizualizēti, lai izceltu būtiskās ainavu struktūras, kas ietekmē sugu izvēli.

### [IevadesDati/](./IevadesDati/)
Šī mape satur dažādu veidu ievades datus, kas izmantoti datu apstrādei un analīzei:
- `LV_birds_reports_2019.xlsx` un `LV_birds_reports_20191030-151740.xml` — putnu novērojumu dati no 2019. gada.
- `preferences.xlsx` — dati par ekspertu vērtētajām ainavu preferencēm dažādām sugām.
- `putni.xlsx` un `putni_SkaitiPazimes_kops2016.xlsx` — strukturētie dati par ligzdojošo putnu sugām Latvijā un to pazīmēm.

### [Rezultati/](./Rezultati/)
Šī mapes satur attēlus, kas ilustrē galvenos darba rezultātus:
- `Putnu_populacija_monitorings.jpg` — Putnu sugu populāciju lielumu sadalījums saistībā ar populācijas populācijas īstermiņa pārmaiņu datu kvalitāti un populācijas lieluma aprēķinu veidu Putnu Direktīvas 12. panta ziņojumā par 2013.-2018. gadu periodu.
- `Putnu_sugu_ainavu_preference.jpg` — Preference ainavas klasēm divos aprēķinu veidos un atsevišķi izdalītiem uz ligzdošanu attiecināmajiem novērojumiem katrā no tiem.
- `Putnu_sugu_akustiska_ainava.jpg` — Ligzdojošo putnu sugu D-B pazīmju attiecība atkarībā no preferences ainavas klasēm, norādot putnu sugas populāciju datu kvalitātes klasi.
- `Putnu_sugu_akustiska_kvalitate.jpg` — Akustisko signālu nozīmes sugu novērojumos (D-B attiecība) saistībā ar populācijas īstermiņa pārmaiņu datu kvalitāti un populācijas lieluma aprēķinu veidu Putnu Direktīvas 12. panta ziņojumā par 2013.-2018. gadu periodu.
- `prognoze.jpg` — Prognozētā varbūtība uzlabot informācijas par putnu populācijām kvalitāti, atkarībā no sugas akustiskuma un preferences izmantotajām ainavas klasēm.

### [Atteli/](./Atteli/)
Šī mapes satur 280 putnu sugu ainavu klašu preferences vizualizācijas

##

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2025-04-13 pieejamajā versijā.
