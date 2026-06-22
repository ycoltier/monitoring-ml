# Outil de monitoring sous R

## Onglet Administration

### Import des données

Les données à importer pour le monitoring doivent être en format parquet ou csv.\
S'ils sont en format csv, alors il faut bien préciser le séparateur et les quotes
 employés au moyen du panneau situé à gauche.
 
Les données à importer doivent avoir au minimum : 
- une variable contenant la cible (code annotée)
- une variable contenant la meilleure prédiction
- une variable contenant le score de confiance associé à la meilleure prédiction
- le poids associé à chaque observation (si pas présente, laisser _weight_, poids de 1 par défaut)

:warning: le nommage n'est pas important car il faut associer chaque variable du 
jeu de données à la variable correspondante pour le monitoring.

Une fois le fichier importé à l'aide du bouton **Browse**, cliquer sur 
**Voir les données**.\
Il faut ensuite faire correspondre les variables avec l'attendu.\
:warning: si la variable weight n'est pas présente dans le fichier importé, 
celle-ci est générée automatiquement et mise à "1".

Une fois la correspondance réalisée, cliquer sur **Traiter les données**.

Si le résultat convient, alors modifier le sigle pour mettre le sigle associé à 
l'enquête puis **Sauvegarder localement**.

### Suppression d'une enquête

Quand vous avez terminé le traitement d'une enquête, vous pouvez la supprimer
au moyen de cette fonctionnalité

### Nettoyer le cache

Pour améliorer le passage d'un onglet à l'autre, certains calculs sont mis en 
cache. Il est parfois utile de supprimer le cache, surtout si on supprime
l'enquête et qu'on la réimporte modifée avec le même sigle.

## Onglet Monitoring

### Sélection des paramètres
- **Sélectionner l'enquête** :
  Utilisez le menu déroulant **"Sélectionner l'enquête"** pour choisir l'enquête ou le jeu de données à analyser.

- **Niveau de seuil de confiance** :
  Ajustez le curseur **"Sélectionnez un niveau de seuil de confiance"** (ex: `0.5`) pour définir le seuil de confiance souhaité.

- **Niveau de nomenclature** :
  Sélectionnez un niveau de nomenclature (ex: `1` à `4`) via le curseur dédié.

### Sous-onglet "Performances"
- **Qualité** :
  - **Graphique** : Affiche le **"Taux de codages automatiques corrects"** en fonction de la **"Proportion de codages automatiques"**.
  - Interprétez la courbe pour évaluer la précision du modèle en fonction du taux de codage automatique.

- **Distribution** :
  - **Histogramme** : Visualise la distribution des codes (ex: `Codages initiaux` en bleu, `Prédictions FastText` en rouge).
  - **Tableau des écarts de prédictions** :
    - Nomenclature : Niveau de nomenclature analysé.
    - Taux bonnes prédictions : Pourcentage de prédictions correctes.
    - Nombre d'observations : Nombre d'observations associées.

### Sous-onglet "Seuil de confiance"
- **Distribution score**
    - **Graphique** : Histogramme de densité présentant la distribution des scores de confiance (fréquence normalisée)
    - **Seuil par défaut** : Ligne verticale à `X.XX`.
    - **Interprétation**:\
     Cela permet de rapidement voir si le modèle a tendance à être sûr de lui ou s'il doute sur beaucoup de prédictions.\
     Cela permet également de voir s'il est capable de bien discréminer ses prédictions.\
     En général, on s'attend à avoir un pic au début et un pic vers la fin.

- **Distribution score selon valeur seuil**
    - **Graphique** : Histogramme de densité présentant la distribution des scores de confiance avec une distinction entre les prédictions correctes (en rouge) et incorrectes (en vert).
    - **Seuil par défaut** : Ligne verticale à `X.XX`.
    - **Interprétation**:\
     Cela permet de rapidement voir si le modèle est confiant sur les prédictions correctes et en doute sur les prédictions incorrectes.
     Pour un modèle qui discrimine bien, on s'attend à avoir pour les prédictions correctes un pic en fin de graphique et pour les prédictions incorrectes, un pic en début de graphique.

- **Taux d'accuracy en fonction du seuil choisi**
    - **Graphique** : Courbe montrant l'**accuracy** (en %) en fonction du **seuil de score choisi** (entre `0.00` et `1.00`).
    - **Seuil par défaut** : Ligne verticale à `X.XX`.
    - **Interprétation** :\
      À un seuil de `X.XX`, l'accuracy peut être de `YY %`. On considère qu'en dessous du seuil de confiance, alors les données sont reprises manuellement et la reprise est 100 % correcte\
      Plus le seuil est élevé, meilleur sera le taux d'accuracy

- **Proportion de codage automatique en fonction du seuil choisi**
    - **Graphique** : Courbe montrant la **proportion de codages automatiques** (en %) en fonction du **seuil de score choisi**.
    - **Seuil par défaut** : Ligne verticale à **0.5**.
    - **Interprétation** :
        - À un seuil de `X.XX`, `YY %` des codages sont automatiques.

### Sous-onglet "Matrice de confusion"
- **Matrice de confusion**
    - **Présentation**\
      Une matrice de confusion est un tableau utilisé en apprentissage supervisé (classification) pour évaluer les performances d'un modèle de machine learning. Elle permet de visualiser les résultats réels (Truth) en fonction des prédictions du modèle, en montrant :\
      Les bonnes prédictions (vrais positifs, vrais négatifs).\
      Les erreurs (faux positifs, faux négatifs).
     - **Interprétation**\
      Les valeurs sur la diagonale représentent le taux de bonnes prédictions pour chaque classe.\
      Les valeurs hors diagonale indiquent des erreurs de classification.
- **Top 10 des confusions remarquables au niveau 2**\
Tableau permettant de repérer rapidement les confusions (erreurs dans la prédiction) les plus remarquables.

## Onglet Aide choix seuil
### Sélection des paramètres
- **Sélectionner l'enquête** :
  Utiliser le menu déroulant **"Sélectionner l'enquête"** pour choisir l'enquête ou le jeu de données à analyser.

- **Niveau de seuil de confiance** :
  Ajuster le curseur **"Sélectionnez un niveau de seuil de confiance"** (ex: `0.5`) pour définir le seuil de confiance souhaité.

- **Niveau de nomenclature** :
  Sélectionner un niveau de nomenclature (ex: `1` à `4`) via le curseur dédié.

- **Volume de l'enquête** :
  Indiquer le volume total de l'enquête pour laquelle on souhaite estimer des métriques.\
  Par défaut, c'est le nombre d'observations importés.

- **Taux recours à la liste** :
  Ajuster le curseur pour déterminer quel sera le taux de recours aux listes de libellés.\
  On estime alors le % du volume de l'enquête qui sera traité automatiquement par Cobra (donc légèrement sous estimé).\
  Par défaut, c'est 0 %.

- **Taux erreur humaine** :
  Ajuster le curseur pour déterminer quel sera le taux d'erreur humaine lors de la reprise manuelle.\
  Cela permet d'ajuster le calcul des métriques en prenant en compte cet incertitude.\
  Par défaut, c'est 0 %.

### Graphiques
- **Métriques en fonction du seuil de confiance**
  - **Hypothèse** : En dessous du seuil de confiance, les données sont reprises manuellement et le code est considéré comme correct sauf pour X % des cas, X étant le taux d'erreur humaine.
  - **Graphique** : 
    - Courbe Précision : mesure la capacité du modèle à ne pas se tromper quand il prédit une classe. 
    - Courbe Recall : mesure la capacité du modèle à retrouver toutes les observations d'une classe.
    - Courbe F-Score : Moyenne harmonique de la précision et du recall 
  - **Interprétation** : 
    - Courbe Précision : Plus la précision est élevée, plus le modèle minimise le nombre de Faux Positif. 
    - Courbe Recall : Plus le recall est élevé, plus le modèle maximise le nombre de Vrai Positifs. 
    - Courbe F-Score : Plus le F1 Score est élevé, plus le modèle est performant. 
- **Tableau de référence**
   - récapitulatif des métriques en dessous du seuil de confiance fixé avec un pas de `0.01`.\
     On ajoute également comme informations le taux et le volume de reprise associés au seuil de confiance.  
- **Taux de "mal prédits" sous le seuil de confiance**
  - Évolution du taux de prédictions incorrectes faites en fonction du seuil de confiance. L'idée est de fixer un seuil pour lequel on retrouve un taux important de mauvaises prédictions en deça de ce seuil, étant donné qu'elles iront en reprise.

