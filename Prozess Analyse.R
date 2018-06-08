########## PROZESS ANALYSE
# Ziel: Nützliche Informationen aus (Business-)Prozess / Events-Daten extrahieren
# Prozess/Event-Daten:
#  1. Warum: Prozess-Instanz muss etwas machen - Instanzen des Prozess (auch: cases)
#            => Patient
#  2. Was: Aktivitäten die passieren
#            => Dinge die im Krankenhaus passieren 
#  3. Wer: Wer ist dafür verantwortlich & kann ausführen für das Event - Resourcen
#            => z.B. Arzt
# Event ist ein aufgzeichnete Akivität(Was),
#               für eine spezfische Instanz (Warum),
#               durch eine spezifische Resource (Wer)

# Process analysis workflow
# 1. Extraction: transformation von roh-daten in event-daten
# 2. Processing: anreichern und filtern der daten
# 3. Analysis

# Perspektiven
# 1. Organisation: fokussiert auf die akteure des prozesses, Rollen, und wie arbeiten sie Zusammenhang
# 2. Control-Flow: fokussiert auf flow und struktur des Prozesses
# 3. Performance:  fokkussiert auf zeit und effizienz - wie schnell geht der prozess
# => + zusammenspielt: z.B. wie hängen die rollen mit der performance zusammen

# Beispiel Event-log
# Wieviele cases sind drin?How many cases are described?
# Wieviel verschiedene Aktivitäten? 
# Wieviele Events wurd aufgezeichnet?
# In welcher Zeitperiode aufgezeichnet?
library(bupaR)
library(edeaR)
library(eventdataR)

#### Basis Prozess Informationen
summary(patients)
# Anzahl verschiedener Cases
n_cases(patients)
n_events(patients)          
# Case betrachten
slice(patients,1) 

#### Aktivitäten betrachten
n_activities(patients)       # Anzahl verschiedener Aktivitäten
activities_labels(patients)  # Namen der Aktivitäten
activities(patients)         # Frequenz

#### Traces: 
# Trace: Jeder case ist beschrieben durch eine Sequenz von Aktivitäten - ein Trace
## Happy path # => häufige Traces
traces(patients)     # Verschiene Traces - frequency table
n_traces(patients)   # Wieviele traces gibt es
## Visualieren
trace_explorer(patients)
trace_explorer(patients,coverage = 1) # alle patienten

#### Process Map
process_map(patients)


### Komponenten von Prozess-Daten
# Activity instance = Auftreten einer activity
# Event-log: Case-Id, Activity Label, Activity Instance Id, 
#            Timestamp, Lifecyle Status, Resoruce

### Erzeugen von Event-Daten
library(readr)
claims <- read_csv("~/Documents/R/data/claims.csv")
## Mapping der Spalten und transformieren
claims_log <- eventlog(claims,
                       case_id = "id",
                       activity_id = "action_type",
                       activity_instance_id = "action",
                       lifecycle_id = "status",
                       timestamp = "date",
                       resource_id = "originator")
## Informationen anzeigen
claims_log
summary(claims_log)
activity_labels(claims_log)


###### ANALYSE

### Organisations Perspektive & Resourcen
# Wer führt die Arbeit aus?
# Wer ist spezialisiert auf einen Task?
# Ist Risko von Brain drain?
# Wer transferiert Arbeit an wen?
# => wichtig um Effizienz zu steigern und für Knowledge Management


### Rollen und Spezialisierugen erkennen
# => Wer ist spezialisiert auf einen Task?
# Resource-activity Matrix
# Spezialisierung: eine Person führt nur eine einzige Aktivität aus#
# Brain-Drain: eine Aktivtät wird ausschliesslich von einer Person ausgeführt
# => Problem: Knowledge-Retention, wenn Person Firma verlässt
#
# Metriken:  log, case, trace, activity, resource, and resource-activity
# Wer führt die Arbeit aus?
resources(patients)   # Resource Frequencies
patients %>% resource_frequency(level = "resource")
patients %>% resource_frequency(level = "resource-activity")
# Annzal der Resource pro Aktivität => brain-drain? nein.
patients %>% resource_frequency(level = "resource-activity") %>% plot()
sepsis %>% resource_frequency(level = "resource-activity") %>% plot()
# => Resourcen J,K,P,W haben selben Task "Admission IC"

### 6 Prozess Metriken

## Wer arbeitet woran?
# Nicht alle Resourcen gleich an einem Prozess beteiligt
# => resource_involvement metrik misst die Anzahl der cases in denen Resource involviert
# 3 verschiedene Metriken, welche relevant für Resourcen:
# frequency, specialization and involvement
sepsis %>% resource_involvement(level = "resource")
sepsis %>% resource_involvement(level = "resource") %>% plot
sepsis %>% resource_frequency(level = "resource")
sepsis %>% resource_specialisation(level = "resource")

## Wer transferiert Arbeit an wen?
resource_map(patients)
resource_map(patients, level = "resource")


#### Structuredness des Prozesses
# Zu verstehen als Control-Flow
# Metriken: Entry and exit points, Length of cases, Presence of activities, Rework
# Visualisieren: Process map, Trace explorer, Precedence matrix

## Prozess Varianten
# => Prozess Stuktur untersuchen
sepsis %>% trace_explorer(coverage = 0.10)  # Traces betracten
sepsis %>% trace_length()                   # Durchschnittliche Trace-Länge
sepsis %>% activity_presence() %>% plot     # Activity Presence überprüfen

## Entry & Exit Points
sepsis %>%
  start_activities("activity") %>%
  plot()

sepsis %>%
  end_activities("activity") %>%
  plot()

### Rework
# Wenn Aktivitäten mehrmals für denselben case gemacht werden müssen
# Repetitions & Self-Loops  
# => manchmal Anzeichen von Ineffienz

# Repetitions
sepsis %>% number_of_repetitions(level = "log")                # Min, max und durchschnittliche Anzahl von Repetitions
sepsis %>% number_of_repetitions(level = "activity") %>% plot  # Häufigst wiederholten Aktivitäten
sepsis %>% number_of_repetitions(level = "resource")           # Akteure die am häufigsten wiederholende Aktivitäten haben 
 
sepsis %>% precedence_matrix(type = "absolute") %>% plot  # oder relative
