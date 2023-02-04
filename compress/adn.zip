<?php
session_start();
header("Access-Control-Allow-Origin: *");
?>
<?php
 $usr='phpadmin';
$hst='localhost';
$pswd='244272';
$dtbs='DiskloudTech';
$cnnxn=mysqli_connect($hst,$usr,$pswd,$dtbs) or die ("Nous n'avons pas pu traîter vôtre demande (erreur serveur). Veuillez réessayer plus tard.");
?>
<?php
if (isset($_GET['Membre2']) && $_GET['userID']!='') {
    $Membre1=$_GET['userID'];
    $Membre1=htmlspecialchars($Membre1);
    $Membre1=mysqli_real_escape_string($cnnxn,$Membre1);
    $Membre2=$_GET['Membre2'];
    $Membre2=htmlspecialchars($Membre2);
    $Membre2=mysqli_real_escape_string($cnnxn,$Membre2);
    $vérification1="SELECT * FROM NeuralDiscussion WHERE Membre1='".$Membre2."' AND Membre2='".$Membre1."'";
    $résultat9=mysqli_query($cnnxn, $vérification1);
    if (mysqli_fetch_assoc($résultat9)){
        echo "conversation déjà existante";
        exit();
    }
    $vérification2="SELECT * FROM NeuralDiscussion WHERE Membre1='".$Membre1."' AND Membre2='".$Membre2."'" ;
    $résultat11=mysqli_query($cnnxn, $vérification2);
    if (mysqli_fetch_assoc($résultat11)){
        echo "conversation déjà existante";
        exit();
    }
    $commandess="INSERT INTO NeuralDiscussion (Membre1, Membre2) VALUES ('".$Membre1."', '".$Membre2."')";
    mysqli_query($cnnxn,$commandess) or die ("Echec de la requête");
    echo "Conversation créée.";
}
else if (!isset($_POST['userID']) || $_POST['userID']=='' && $_GET['Membre2']!='') {
    echo "Vous avez été déconnecté : <a href='neuralConnexion.html'>Vous pouvez vous reconnecter ! ici !</a>";
    exit();
}
if (isset($_POST['requête'])) {
    $demande=$_POST['requête'];
    htmlspecialchars($demande);
    mysqli_real_escape_string($cnnxn,$demande);
    $sqldemande="SELECT * FROM DiskloudTechUser WHERE ID LIKE '%$demande%' OR Pseudo LIKE '%$demande%' OR NomComplet LIKE '%$demande%'";
    $résultats=mysqli_query($cnnxn,$sqldemande) or die ("Serveur de données injoignable.");
    echo "<table class='résultats'><tr class='tbodyy' style='text-align : center;'><td>Tag</td><td>Pseudo</td><td colspan='2'>Nom complet</td></tr>";
    while ($ligne=mysqli_fetch_assoc($résultats)) {
        extract($ligne);
        echo "<tr><td>".$ID."</td><td>".$Pseudo."</td><td>".$NomComplet."</td><td style='width : 250px;'><input type='button' id='Aminum".$ID."' onclick='AjoutAmi(".$ID.")' class='boutonConnexion choixEC' style='width: auto ;' value='ajouter contact'></td></tr>";
    }
    echo "<tr><td colspan='4' style='text-align : center ;'>Fin des résultats</td></tr></table>";
}
?>
