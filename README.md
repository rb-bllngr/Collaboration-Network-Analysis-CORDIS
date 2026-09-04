# Strukturanalyse und Visualisierung von Kollaborationsnetzwerken in EU-Forschungsförderprogrammen

### Veranstaltung: Abschlussmodul (P17; "Bachelorarbeit und Disputation")

### Institution: Ludwig-Maximilians-Universität München

- Student: Robin Billinger
- Betreuer: Prof. Dr. Göran Kauermann
- Abgabedatum: 16. September 2026 (SoSe 2026)

## Kurzbeschreibung

Im Rahmen meiner Bachelorthesis wird anhand von CORDIS Datensätzen der Europäischen Union 
die Struktur der Framework Programmes Horizon 2020 (2014-2020) und Horizon Europe (2021-2027)
untersucht und visualisiert. Dieses Repository dokumentiert die für dieses Ziel erforderliche
Datenakquise, Datenaufbereitung, Analyse und Auswertung.

## Hauptdokument

Die vollständige Bachelorthesis befindet sich in: Bachelorthesis.pdf

## Ordnerstruktur

Das Projekt verwendet die folgende Ordnerstruktur. Der Ordner "Data" und dessen Struktur sind
als leere Platzhalter im Repository hinterlegt, beschreiben jedoch den entstandenen Aufbau,
sobald die Programme auf die genutzten Daten ausgeführt werden.

``` txt
Repository Root
│── README.md                    # Projektübersicht und Reproduzierbarkeit
│── Bachelorthesis.pdf           # Finale Abschlussarbeit
│── Disputation.qmd              # Endpräsentation zur Disputation
│── customstyle.css              # Stilvorlagen
│
│── envir_setup.R                # Globale Einstellungen und Vorbereitung der Arbeitsumgebung
│── functions.R                  # Verfasste und zur Wiederverwendung ausgelagerte Funktionen
│── plot_styling.R               # Standardisiertes Plot-Design
│── pipeline.R                   # Vorgefertigte Verarbeitungskette der Programme zur Reproduktion
│
├── Data/                        # Datensätze
│   ├── Raw/                     # Rohdaten
│   └── Intermediate/            # Weiter bearbeitete Datensätze
│
├── Programs/                    # Skripte zur Datenaufbereitung, Analyse und Visualisierung
│   ├── ...
│   └── ...
│
└── Plots/                       # Finale Abbildungen für Thesis/Disputation aus den Daten
```

## Anleitung zur Reproduzierbarkeit

Zu Beginn jeglicher Arbeit an diesem Projekt wird die Ausführung von ```txt envir_setup.R```
empfohlen, das globale Umgebungseinstellungen vornimmt und die erforderlichen R-Pakete installiert.

Zur anschließenden Reproduzierbarkeit der durchgeführten Analyse durchläuft das Programm 
eine vorgefertigte Verarbeitungskette 

! Beachte: Während auf MacOS das Herunterladen (und ggf. Überschreiben) der Daten ohne 
Komplikationen funktioniert hat, traten auf Computern mit Windows-Betriebssystem Fehler 
aufgrund fehlenden Zugriffs zum Ändern der Ordner auf. Zur Vermeidung etwaiger 
Komplikationen wird daher empfohlen, vor dem Durchlauf von ```txt pipeline.R``` (oder
spezifisch ```txt download_and_merge_data.R ```) den Ordner ```r Data/Raw``` zu entleeren
(bis auf die versteckte Datei ```txt .gitkeep```).

## Datenschutz/Lizenz

Aus datenschutz- und lizenzrechtlichen Gründen wurde darauf verzichtet, die verwendeten Daten
in diesem Repository zu speichern. Stattdessen greift das Programm ```txt download_and_merge_data.R```
direkt auf die erforderlichen Daten gemäß ihrer Verfügbarkeit nach Lizenz zu. Für den genauen
Ablauf, siehe 'Anleitung zur Reproduzierbarkeit'.

## Software

R 4.6.1, 2026.07.1+147 "Pacific Dogwood"

## KI- und Softwareunterstützung
