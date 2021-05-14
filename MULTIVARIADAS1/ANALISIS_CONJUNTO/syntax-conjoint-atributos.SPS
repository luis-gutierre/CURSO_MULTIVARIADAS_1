SET SEED 10.
ORTHOPLAN
      /FACTOR=VERBO 'VERBO' (1 'SI TIENE VERBO' 2 'NO TIENE VERBO') CARITA 'CARITA' (1 'SI TIENE CARITA' 2 'NO TIENE CARITA') 
      DINERO 'DINERO' (1 'CON DINERO' 2 'SIN DINERO')
      /OUTFILE=C:\Tecnicas Multivariadas I\Clases\7. Conjoint\diseño-conjoint-atributos2.sav'.

PLANCARDS
     /FACTOR=VERBO CARITA DINERO
     /FORMAT BOTH
     /TITLE 'Lista de Tarjetas' .

CONJOINT
      PLAN='C:\Tecnicas Multivariadas I\Clases\7. Conjoint\diseño-conjoint-atributos.sav'
      /DATA='C:\Tecnicas Multivariadas I\Clases\7. Conjoint\data-conjoint-atributos.sav'
      /FACTOR=VERBO 'VERBO' (1 'SI TIENE VERBO' 2 'NO TIENE VERBO') CARITA 'CARITA' (1 'SI TIENE CARITA' 2 'NO TIENE CARITA') DINERO 'DINERO' (1 'CON DINERO' 2 'SIN DINERO')
      /SEQUENCE=PREF1 to PREF8
      /SUBJECT=ID
      /PRINT=ALL
      /PLOT ALL
      /UTILITY='C:\Tecnicas Multivariadas I\Clases\7. Conjoint\satutil-conjoint-atributos.sav'.


