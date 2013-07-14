<?php

/**
 * author  : Raz (http://raz-soft.com)
 * name    : Inno Setup Survey Extension - Server side sript demo
 * purpose : Save the user comments/feedback on a local server file. 
 *         
 *          Informations posted when the user submits the survey from IssSurvey Extension: 
 *           $name    = the name you pass on the IssSurvey Extension function     (for security purpose)
 *           $pass    = the password you pass on the IssSurvey Extension function (for security purpose)
 *           $cntinfo = user comments/feedback text base64 encoded string
 */

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />

<title>Innos Setup Survey Extension - Server side sript demo</title>
</head>
<b>Innos Setup Survey Extension - Server side sript demo</b><br>
<body>

 <?php
//get the sent informations
 $name=$_POST['name'];
 $pass=$_POST['pass'];
 $comm=$_POST['cntinfo']; 
 if ($_POST['IssSurvey'])       // it was sent by IssSurvey extension?
    $comm=base64_decode($comm); //user comments/feedback is send by IssSurvey in a base64 encoded string
 $submit=$_POST['submit'];
 
  //Check and save the informations to a local server file. 
  //Check for the hardcoded user/password set in the IssSurvey Extension for more security.
 if (($name=="demo") && ($comm) && ($pass=="demo") && $submit )
     {
          $fp=fopen("IssSurvey.txt","a"); //IssSurvey.txt file must exist and have write access (666)
          $dat=date("l dS of F Y h:i:s A");
          $r=fwrite($fp,"$dat name:  <b>$name</b> - <i>$comm </i> \r\n");
          $r=fclose($fp);
		  echo "<font color=red><b>Successfully submited. Thank you.</b></font>"; 
     }
 else
   {
?>
    <form action="IssSurvey.php" method="post">
    <p>name:<br> <input type="text"  size=20 name="name" value="demo"><br>
       pass:<br> <input type="text"  size=20 name="pass" value="demo"><br>
       feedback:<br> <TEXTAREA NAME="cntinfo" cols=48 rows=4></textarea><br>
    <input name ="submit" type="submit" value ="Submit"></p> 

    </form>
<?php
    if ($submit)
          echo "<font color=red><b>Invalid Submit!</b></font>";
             
   }
   
//read and display the Survey info      
  $fis=file('IssSurvey.txt');
  if (count($fis)>0)
  {
    echo "<hr>";
      for($i=0;$i<count($fis);$i++)
      {
        echo "$fis[$i] <br>";
      }
     echo "<hr>";
  }   
   
?>
  <br><center> <b>powered by</b><br><a href="http://raz-soft.com/"><img src="http://raz-soft.com/razav.gif" border=0></a></center>
 </body>
</html> 