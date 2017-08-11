### IMPORTA SIMULATED RAW DATA FROM BESA SIMULATOR

# apro la connessione
to.read=file("/Applications/Besa Simulator/BESASimulator/prova.dat", "rb")

# carico come binario. Nota che per prove ed errori ho capito che il size è 4.
mydata.vec=readBin(to.read, double(), n= 10^10, size=4)

# trasformo il vettore in una matrice. Il numero di righe è il numero totale di samples (le colonne saranno di conseguenza gli elettrodi.)
# recuperi l'informazione su NSample dal file .generic che viene creato nella generazione dei dati simulati.

mydata.mat=matrix(mydata.vec, nrow=41600, byrow=T)


plot(mydata.mat[1:100o, 1]) #così plotto i primi 1000 punti del primo elettrodo.


