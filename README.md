# NFA_modularized

Automates finis non déterministes et Automates finis non déterministes généraux : <br>

En principe, un automate Aut = <A,Q,D,F,T> est défini comme suit :   
-Un alphabet A dit alphabet d'entrée   
-Un ensemble fini Q qui décrit les états de l'automate  
-Un ensemble fini D inclus dans Q qui décrit les états de départ de l'automate  
-Un ensemble fini F inclus dans Q qui décrit les états acceptants (ou de fins) de l'automate  
-Un ensemble T inclus dans Q x A x Q qui décrit les transitions de l'automate  

Suivant les implémentations, on permettra ou non à \(\epsilon\) d'appartenir à l'alphabet d'entrée (ici c'est le cas, le neutre est décrit par MonoidalType.e). Cependant, si on cherche à implémenter une élimination d'états sur notre automate on va se heurter à un problème : certaines transitions seront décrites par des langages entiers, et non des lettres ou des mots (ce qui aurait été gérable dans une implémentation qui ne gère que l'alphabet). Prenons un exemple :

