# Strukturanalyse und Visualisierung von Kollaborationsnetzwerken in EU-Forschungsförderprogrammen

### Institution: Ludwig-Maximilians-Universität München

### Veranstaltung: Abschlussmodul (P17; "Bachelorarbeit und Disputation")

- Student: Robin Billinger
- Betreuer: Prof. Dr. Göran Kauermann
- Abgabedatum: 16. September 2026 (Sommersemester 2026)

## Kurzbeschreibung

Im Rahmen meiner Bachelorthesis wird anhand von CORDIS Datensätzen der Europäischen Union die Struktur der Framework Programmes Horizon 2020 (2014-2020) und Horizon Europe (2021-2027) untersucht und visualisiert. Dieses Repository dokumentiert die für dieses Ziel erforderliche Datenakquise, Datenaufbereitung, Analyse und Auswertung.

## Hauptdokument

Die vollständige Bachelorthesis befindet sich in: Bachelorthesis.pdf (nicht öffentlich-zugänglich!)

## Ordnerstruktur

Die Arbeit verwendet die folgende Ordnerstruktur. Der Ordner "Data" und dessen Struktur sind als leere Platzhalter im Repository hinterlegt, beschreiben jedoch den entstandenen Aufbau, sobald die Programme auf die genutzten Daten ausgeführt werden.

``` txt
Repository Root
│── README.md                        # Projektübersicht und Reproduzierbarkeit
│── Bachelorthesis.pdf               # Finale Abschlussarbeit
│── Disputation.qmd                  # Endpräsentation zur Verteidigung der Bachelorthesis
│── customstyle.css                  # Stilvorlagen
│── LMU.svg                          # Logo Ludwig-Maximilians-Universität München für Disputation
│── references.bib                   # Quellenverweise der Arbeit
├── chicago-authot-date-de.csl       # Stilvorlage für Literaturverzeichnis der Disputation
│
│── envir_setup.R                    # Globale Einstellungen und Vorbereitung der Arbeitsumgebung
│── functions.R                      # Verfasste und zur Wiederverwendung ausgelagerte Funktionen
│── plot_styling.R                   # Standardisiertes Plot-Design
│── pipeline.R                       # Vorgefertigte Verarbeitungskette der Programme zur Reproduktion
│
├── Data/                            # Datensätze
│   ├── Raw/                         # Rohdaten
│   └── Intermediate/                # Weiter bearbeitete Datensätze
│
├── Programs/                        # Skripte zur Datenaufbereitung, Analyse und Visualisierung
│   ├── download_and_merge_data.R 
│   ├── manage_country_information.R
│   ├── build_networks.R
│   ├── visualise_basic_networks.R
│   ├── network_degrees.R
│   ├── network_centrality.R
│   ├── network_cohesion.R
│   ├── network_roles.R
│   ├── network_countries.R
│   ├── network_mds.R
│   └── mds_feasibility_check.R
│
├── Images/                          # Externe Abbildungen für Disputation
└── Plots/                           # Finale Abbildungen für Thesis/Disputation aus den Daten
```

## Anleitung zur Reproduzierbarkeit

>[!IMPORTANT]
> Zu Beginn jeglicher Arbeit mit diesem Repository wird stets die Ausführung von `envir_setup.R` empfohlen, das globale Einstellungen vornimmt und die erforderlichen R-Pakete installiert. Wenn die vorgefertigte Verarbeitungskette (siehe unten) ausgeführt wird, wird ebenjenes Programm zu Beginn automatisch durchlaufen.

Zur Reproduzierbarkeit der durchgeführten Analyse durchläuft das Programm eine vorgefertigte Verarbeitungskette in `pipeline.R`.

⚠️ **Beachte**: Während auf MacOS das Herunterladen (und ggf. Überschreiben) der Daten ohne Komplikationen funktioniert hat, traten auf Computern mit Windows-Betriebssystem vermehrt Fehler aufgrund fehlenden Zugriffs zum Ändern der Ordnereigenschaften während des Versuchs, die Daten herunterzuladen (unabhängig der gewählten Möglichkeit), auf. Zur Vermeidung etwaiger Komplikationen wird daher empfohlen, vor dem Durchlauf von `pipeline.R` (oder individuell `download_and_merge_data.R`) den Ordner `Data/Raw` immer zunächst zu entleeren (bis auf die versteckte Datei `.gitkeep`). ⚠️

- **Schritt 1**: `envir_setup.R` nimmt globale Einstellungen vor und installiert die erforderlichen R-Pakete.

- **Schritt 2**: `download_and_merge_data.R` lädt die erforderlichen Daten herunter und führt sie zusammen.

>[!TIP]
>Für das Herunterladen der Daten sind zwei Möglichkeiten im Programm hinterlegt, aus denen Nutzende im Programm interaktiv wählen können (während `download_and_merge_data.R` durchlaufen wird):
>1. Verwendung der monatlich aktualisierten, aktuellen Daten direkt aus dem CORDIS Data Portal der Europäischen Union.
>2. Verwendung des eingefrorenen Datensatzes, der im Zuge dieser Arbeit tatsächlich verwendet wurde.
>
> Um auf den eingefrorenen Datensatz, der in einem privaten LRZ Sync&Share Ordner abgelegt ist, zugreifen zu können, wird die zufällig generierte Sync&Share Link-ID des Ordners benötigt, die vom Programm nach Auswahl von Möglichkeit 2 angefordert wird. Bei Bedarf kann diese auf Anfrage ausgehändigt werden.

- **Schritt 3**: `manage_country_information.R` bereinigt fehlende Werte in den Daten bezüglich der Länderinformationen der Organisationen.

- **Schritt 4**: `build_networks.R` konstruiert die während der Analyse benötigten Netzwerkvarianten auf Organisationsebene.

- **Schritt 5**: `visualise_basic_networks.R` veranschaulicht Netzwerke auf Organisationsebene graphisch.

- **Schritt 6**: `network_degrees.R`, `network_centrality.R`, `network_cohesion.R`, `network_roles.R`, `network_countries.R`, `network_mds.R` werden zur Analyse und Visualisierung der Netzwerke durchlaufen.

Ein weiteres Programm, das nicht Teil der `pipeline.R` ist, sondern zur einmaligen Prüfung des Speicher- und Laufzeitaufwands durchgeführt wurde und zu Zwecken der Nachvollziehbarkeit erhalten wurde, findet sich unter dem Namen `mds_feasibility_check.R`.

Im Laufe der Arbeit kommt es zudem an mehreren Stellen (genauer gesagt in `network_centrality.R` und `network_cohesion.R`) zu sehr rechenaufwändigen Berechnungen. Infolgedessen werden die Ergebnisse dieser Berechnungen als *Checkpoints* im zunächst ohne Inhalt angelegten Ordner `Data/Intermediate` in entsprechenden Unterordnern `Data/Intermediate/H2020` und `Data/Intermediate/HORIZON` zwischengespeichert. Hierbei sind folgende Punkte zu beachten:

- Bei *erstmaliger* Durchführung sind **keine Checkpoint-Daten** hinterlegt, das heißt, das Programm wird den vollen Laufzeit- und Speicheraufwand benötigen.

- Sobald Checkpoint-Dateien im Ordner hinterlegt sind, wird *jede* dieser Dateien auch verwendet!

- Um eine neuerliche Berechnung zu erzwingen, müssen entweder

  A)  diejenigen Dateien, die neu berechnet werden sollen, aus den entsprechenden Ordnern gelöscht werden, oder

  B)  die zugehörigen Schaltervariablen in den Programmen umgelegt werden, das heißt, der in beiden Programmen zu Beginn standardmäßig auf
  ```r
  recompute <- FALSE
  ```
  stehende Schalter muss *in beiden Programmen separat* zu
  ```r
  recompute <- TRUE
  ```
  geändert werden. Um eine wiederholte Berechnungen in zukünftigen Durchläufen auszuschließen, müssen die Schalter anschließend wieder auf ihren Default-Fall zurückgesetzt werden.

## Datenschutz/Lizenz

Aus datenschutz- und lizenzrechtlichen Gründen wurde darauf verzichtet, die verwendeten Daten in diesem Repository zu speichern. Stattdessen greift das Programm `download_and_merge_data.R` direkt auf die erforderlichen Daten gemäß ihrer Verfügbarkeit nach Lizenz zu. Für den genauen Ablauf, siehe 'Anleitung zur Reproduzierbarkeit'.

## Software

R 4.6.1, RStudio 2026.07.1+147 "Pacific Dogwood"

Abschließende Liste der verwendeten (neben standardmäßig installierten) R-Pakete mit zum Verfassungszeitpunkt geltenden Versionen:

``` r
Package      Version
checkmate      2.3.4
countrycode    1.8.0
data.table    1.18.4
EconGeo          2.1
ggh4x          0.3.1
ggplot2        4.0.3
ggraph         2.2.2
ggrepel        0.9.8
httr           1.4.8
igraph         2.3.3
maps           3.4.3
patchwork      1.3.2
RColorBrewer   1.1-3
readxl         1.5.0
tidygraph      1.3.1
wpp2024        1.1-3
```

## KI- und Softwareunterstützung
