#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector pnrPlaceCore(std::vector < std::string > x) {
  int n = x.size();
  int year;
  int birth_no;
  IntegerVector birthplace_vec(n);  

  for (int i = 0;i < n; ++i){
    year = atoi(x[i].substr(0,4).c_str());
    birth_no = atoi(x[i].substr(8,2).c_str());
    if ( year < 1990 ) { 
      if ( birth_no < 10 ) {
        if (year<1968){
          birthplace_vec[i] = 4; // Stockholm Stad
        } else {
          birthplace_vec[i] = 5; // Stockholms l�n (exkl. Stockholms stad)
        }
      } else if (birth_no>=10 && birth_no<=13) {
        birthplace_vec[i] = 6; // Stockholms l�n    
      } else if (birth_no>=14 && birth_no<=15) {
        birthplace_vec[i] = 7; // Uppsala l�n
      } else if (birth_no>=16 && birth_no<=18) {
        birthplace_vec[i] = 8; // S�dermanlans l�n
      } else if (birth_no>=19 && birth_no<=23) {
        birthplace_vec[i] = 9; // �sterg�tlands l�n
      } else if (birth_no>=24 && birth_no<=26) {
        birthplace_vec[i] = 10; // J�nk�pings l�n
      } else if (birth_no>=27 && birth_no<=28) {
        birthplace_vec[i] = 11; // Kronobergs l�n
      } else if (birth_no>=29 && birth_no<=31) {
        birthplace_vec[i] = 12; // Kalmar l�n
      } else if (birth_no==32) {
        birthplace_vec[i] = 13; // Gotlands l�n
      } else if (birth_no>=33 && birth_no<=34) {
        birthplace_vec[i] = 14; // Blekinge l�n
      } else if (birth_no>=35 && birth_no<=38) {
        birthplace_vec[i] = 15; // Kristiansstads l�n
      } else if (birth_no>=39 && birth_no<=45) {
        birthplace_vec[i] = 16; // Malm�hus l�n
      } else if (birth_no>=46 && birth_no<=47) {
        birthplace_vec[i] = 17; // Hallands l�n
      } else if (birth_no>=48 && birth_no<=54) {
        birthplace_vec[i] = 18; // G�teborgs och Bohus l�n 
      } else if (birth_no>=55 && birth_no<=58) {
        birthplace_vec[i] = 19; // �lvsborgs l�n
      } else if (birth_no>=59 && birth_no<=61) {
        birthplace_vec[i] = 20; // Skaraborgs l�n
      } else if (birth_no>=62 && birth_no<=64) {
        birthplace_vec[i] = 21; // V�rmlands l�n
      } else if (birth_no==65) {
        birthplace_vec[i] = 2; // Extranummer
      } else if (birth_no>=66 && birth_no<=68) {
        birthplace_vec[i] = 22; // �rebro l�n
      } else if (birth_no>=69 && birth_no<=70) {
        birthplace_vec[i] = 23; // V�stmanlands l�n
      } else if (birth_no>=71 && birth_no<=73) {
        birthplace_vec[i] = 24; // Kopparbergs l�n
      } else if (birth_no==74) {
        birthplace_vec[i] = 2; // Extranummer
      } else if (birth_no>=75 && birth_no<=77) {
        birthplace_vec[i] = 25; // G�vleborgs l�n
      } else if (birth_no>=78 && birth_no<=81) {
        birthplace_vec[i] = 26; // V�sternorrlands l�n
      } else if (birth_no>=82 && birth_no<=84) {
        birthplace_vec[i] = 27; // J�mtlands l�n
      } else if (birth_no>=85 && birth_no<=88) {
        birthplace_vec[i] = 28; // V�sterbottens l�n
      } else if (birth_no>=89 && birth_no<=92) {
        birthplace_vec[i] = 29; // Norrbottens l�n
      } else if (birth_no>=93 && birth_no<=99) {
        birthplace_vec[i] = 3; // Extranummer immigration
      }
    } else {
      birthplace_vec[i] = 1; // F�dd efter 1990 
    }
  }
    
  // birthplace_vec.attr("class") = "factor";
  
  return birthplace_vec;
}

/*** R
pnrPlace<- function(x){
  labels <- c("Born 1990 or later",
              "Extra number (non-immigrants)", 
              "Extra number (non-immigrants and immigrants)",
              "Stockholms stad",
              "Stockholms l�n (excluding Stockholm stad)",
              "Stockholms l�n",
              "Uppsala l�n",
              "S�dermanlands l�n",
              "�sterg�tlands l�n",
              "J�nk�pings l�n",
              "Kronobergs l�n",
              "Kalmar l�n",
              "Gotlands l�n",
              "Blekinge l�n",
              "Kristianstads l�n",
              "Malm�hus l�n",
              "Hallands l�n",
              "G�teborgs och Bohus l�n",
              "�lvsborgs l�n",
              "Skaraborgs l�n",
              "�rebro l�n",
              "V�stmanlands l�n",
              "Kopparbergs l�n",
              "G�vleborgs l�n",
              "V�sternorrlands l�n",
              "J�mtlands l�n",
              "V�sterbottens l�n",
              "Norrbottens l�n")
  
  return factor(pnrPlaceCore(x),labels=labels)
  }

*/


