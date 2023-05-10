# NFA_modularized

Automates finis non déterministes et Automates finis non déterministes généraux : <br>

En principe, un automate Aut = <A,Q,D,F,T> est défini comme suit :   
-Un alphabet A dit alphabet d'entrée   
-Un ensemble fini Q qui décrit les états de l'automate  
-Un ensemble fini D inclus dans Q qui décrit les états de départ de l'automate  
-Un ensemble fini F inclus dans Q qui décrit les états acceptants (ou de fins) de l'automate  
-Un ensemble T inclus dans Q x A x Q qui décrit les transitions de l'automate  

Suivant les implémentations, on permettra ou non à ε d'appartenir à l'alphabet d'entrée (ici c'est le cas, le neutre est décrit par MonoidalType.e mais il faut garder à l'esprit que ε est un mot vide, et non pas une lettre vide). Cependant, si on cherche à implémenter une élimination d'états sur notre automate on va se heurter à un problème : certaines transitions seront décrites par des langages entiers, et non des lettres ou des mots (ce qui aurait été gérable dans une implémentation qui ne gère que l'alphabet). Prenons un exemple :  
On considère l'automate suivant 
![image](https://user-images.githubusercontent.com/102584320/236619967-721ef7cd-4432-45c1-b756-91afd501b140.png)  
Et on essaie d'éliminer son état 3. On peut utiliser un algorithme naïf, on prend toutes les "3 transitions" dont l'état 3 est le "milieu" et on remplace dans l'ensemble des transitions en conséquence (Pour plus de détail, voir https://fr.wikipedia.org/wiki/Minimisation_d%27un_automate_fini_d%C3%A9terministe#cite_note-4) et on obtient l'"automate" suivant  
![image](https://user-images.githubusercontent.com/102584320/236620289-f90eef19-291d-4fcb-8d66-eb3152c50238.png)  
On observe que la transition 1-1 est étiquetée par (xy)\*, qui n'est pas une lettre mais bien un langage entier. La fonction d'élimination d'un état d'un automate n'est pas de type aut -> etat -> aut mais de type aut -> etat -> superaut, où superaut est une généralisation des automates dans laquelle les transitions ne sont pas étiquetées par des lettres mais par des langages entiers. En remontant ce problème, on remarque qu'il est plus logique d'implémenter les "superaut", i.e des automates dont les transitions sont étiquetées par des langages et de dire que les automates usuels sont des "suepraut" dont les transitions sont étiquetées par des langages  particuliers qui sont des singletons ne contenant qu'un seul mot particulier en cela qu'il n'est composé" que d'une seule lettre. Pour savoir si un automate implémenté dans le programme est un automate usuel, on pourra utiliser la fonction est_usuel du module Aut. 
Pour cette raison, on décrira toujours les transitions d'un état à un autre par un UNIQUE langage, si la transition est décrite par plusieurs mots par exemple une union de ces mots sera suffisante.  

Le programme implémente :  
-Automate fini (non) déterministe ✅  
-Automate fini (non) déterministe dont les transitions sont décrites par des langages ✅  
-Algorithme d'élimination d'un état ✅  
-Algorithme pour trouver le langage accepté par un automate ✅  
-Générateur d'automate minimal à partir d'un langage donné ❌  
-Parser/Wrapper pour générer plus facilement des automates ❌   





