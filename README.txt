README LISP
Composizione del gruppo: -829584 Davide Pozzoli
			 -830209 Massimo Junior Toselli

Utilizzando la funzione open-file due volte all'interno della funzione lmc-load, riusciamo a creare due liste: 
la prima, in input alla funzione create-mem, contenente tutte le istruzioni che verranno poi codificate per andare a creare la memoria, 
la seconda in input alla funzione assert-label, che contiene le etichette affiancate dal numero con il quale verranno sostituite.  

README PROLOG
Composizione del gruppo: -829584 Davide Pozzoli
			 -830209 Massimo Junior Toselli

Per la gestione delle etichette sfruttiamo il database interno del sistema prolog affermando le label col predicato assert/1.
Prima dell'esecuzione del predicato lmc_load/2 e dopo il predicato executionloop/2 (all'interno del lmc_run/3) usufruiamo del predicato retractall/1 per svuotare il database dalle label
affermate, così si evitano conflitti con l'esecuzione di altri test.
