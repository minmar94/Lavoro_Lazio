
#!/bin/bash
# this is from the NCO program http://nco.sourceforge.net/#RTFM

# immagina di voler ripetere la stessa operazione su tutti i file nella cartella 
# CARTELLA, i file si chiamano 
#PHY_006_009_CUR.nc
#PHY_006_009_SALI.nc
#PHY_006_009_TEMPE.nc
# tu vuoi chiamare i nuovi file creati da nco
#PHY_006_009_CUR_new.nc
#PHY_006_009_SALI_new.nc
#PHY_006_009_TEMPE_new.nc

for f_in in Dati/*; do
	echo $f_in; # questo scrive semplicemente il nome del file su cui sta lavorando
			    # non e' necessario ma magari e' utile!
	f_out = "${f_in/.png/_new.png}";
	echo $f_out; #neache questo e' necessario ma utile per vedere che hai creato 
				 #il nome giusto 
    	
    ncra --mro -d time,,,3,3  f_in.nc f_out.nc
done

