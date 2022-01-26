#include <iostream>
#include<cmath>
#include <vector>
#include <random>
#include <iomanip>
#include <array>
#include <tuple>
#include<cstdlib>
#include <iomanip>
#include <chrono>
#include <ctime>
#include <thread>



using namespace std;
#include<vector>



vector<vector<int> > convertir(int n,int tab[6][6]){
    vector<vector<int> > v(n, vector<int> (n, 0));
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            v[i][j]=tab[i][j];
        }
        
    }
    return v;
}




int a[6][6] = {1,3,2,4,5,6,
                1,4,2,3,6,5,
                5,1,3,2,4,6,
                2,1,3,6,4,5,
                1,3,2,4,5,6,
                5,4,2,3,1,6,
                };


//vector<vector<int> > a1=convertir(6,a);
vector<vector<int> > b1=convertir(6,a);

int position(vector<int> & v, int j,int n){      //retourne la position de j à vecteur v avec n la taille données de v
    for(int i=0;i<n;i++){
        while (v[i]==j)
        {
            return i;
        }
        
    }
}



int score(vector<vector<int> > M,vector<int> x,int n){   //calcul de score d'une distribution de projet(x , qui est un vecteur)
    int res=0;                                    
   for(int i=0;i<n;i++) {
       res+=M[i][x[i]];
       
    }
    
    
   return res;
}

vector<int> mutation(vector<int> & v,double p,int n){      //fonction qui permet de faire mutation génétique aléatoire
    double pos_aleatoire;
    double vl_aleatoire;
    for (int j = 0; j < n; j++)
    {
        
        vl_aleatoire=rand()/double(RAND_MAX);         //générer un nombre aléatoire de 0 à 1


        if (vl_aleatoire<p)                               //on le compare avec le proba de mutation, si plus
        {                                                //grand , on fait la mutation sinon on fait pas 
            pos_aleatoire=j;
            while (pos_aleatoire==j)
            {
                pos_aleatoire=(rand() % (n)); //on le change avec une position aléatoire différent de position actulle
            }
            int temp=v[j];
            v[j]=v[pos_aleatoire];
            v[pos_aleatoire]=temp;         //Echange de 2 positions

        }
    }
    
    

    return v;
}



int cptzero(vector<int> v,int n){       // la fonction qui permet de compter le nombre de -1
    int cpt=0;                        //dans notre cas , ce sont les cases qui ne sont pas encore remplis 
    for (int i = 0; i < n; i++)
    {
        if (v[i]==-1)
        {
            cpt+=1;
        }
        
    }
    return cpt;
    
}


vector<int> poszero(vector<int> v,int n,int nbzero){  // la fonction qui renvoit les postions de cases encore pas remplis
    vector<int> res(nbzero);
    int j=0;
    for (int i = 0; i < n; i++)
    {
        if (v[i]==-1)
        {
            res[j]=i;
            j+=1;
        }
        
    }
    return res;
    
}


bool exitememe(vector<int> v,int n,int element){  // la fonction qui vérifie si dans v il existe element 
    int cpt=0;
    for (int i = 0; i < n; i++)
    {
        if (v[i]==element)
        {
            cpt+=1;
        }
        
    }
    if (cpt==0)          //if le compteur de element dans v est 0 c'est vrai sinon faux
    {
       return false;
    }
    else{
        return true;
    }
    
    
}


vector<int> projetdistribuer(vector<int> s,int n,int nbzero){   // algo qui cherche vecteur s différent de 1:n
    vector<int> res(nbzero);                          //c'est à dire les projets qui sont pas encore distribués
    int j=0;     // j parcout vecteur 1:n
    int pos=0;   //pos parcourt vecteur res qui est à templir
    while (j<n)
    {
        if (exitememe(s,n,j)==true)  //si le projet deja distribué
        {
            j+=1;                  //on passe au projet suivant
        }
        else{
            res[pos]=j;           //sinon on remplace res par projet n'a pas distribué
            j+=1;                 // on passe à projet suivante
            pos+=1;              //on passe à la position suivante de vecteur res
 
        }
        
    }
    return res;
    
    
    
}



const clock_t begin_time = clock();

int studentProjectGenetique(vector<vector<int> > M,int iter,double p,int n,vector<int> &pos_final,int &scoreMAX){
    double quantile=(3/4)*n*n;              
    vector<int> pos(n);
    int nbzero;
    int s1;
    int s2;
    vector<int> pos3;
    vector<int> pos2;
    vector<int> s3(n);
    vector<int> zeropos;
    vector<int> student_sans_projet;
    vector<int> projet_a_distribuer;
    
    for (int i = 0; i < n; i++)
    {
        pos[i]=i;                 //initialisation de vecteur pos par 0 à 1-n
    }
    pos2=mutation(pos,1,n);        //utiliser la fonction mutation avec proba 1 qui permet de générer un vecteur aléatoire de 0 à n-1


    while (score(M,pos2,n)<quantile)
    {
        pos2=mutation(pos,1,n);
    }




    for (int j = 0; j < iter; j++)
    {
        pos3=mutation(pos,1,n);
        while (score(M,pos3,n)<quantile)
    {
        pos3=mutation(pos,1,n);
    }

        int s1=score(M,pos2,n);
        int s2=score(M,pos3,n);

        
        for (int i = 0; i < n; i++)
        {
            s3[i]=-1;
        }
        

        if (s1>s2)
        {
            for (int i = 0; i < n; i++)
            {
                if (M[i][pos2[i]]>=M[i][pos3[i]])
                {
                    s3[i]=pos2[i];
                }
                
            }
            
            
        }
        else{
            for (int i = 0; i < n; i++){
                if (M[i][pos2[i]]<=M[i][pos3[i]])
                {
                    s3[i]=pos3[i];
                }
            }
        }
        

        nbzero=cptzero(s3,n);   // nb de position n'a pas encore rempli en s3
    
        if(nbzero!=0){
            zeropos=poszero(s3,n,nbzero);
            if (s1>s2)
            {
                for (int i = 0; i < nbzero; i++)
                {
                    if (M[zeropos[i]][pos2[zeropos[i]]]<M[zeropos[i]][pos3[zeropos[i]]] && exitememe(s3,n,pos3[zeropos[i]])==false)
                    {
                        s3[zeropos[i]]=pos3[zeropos[i]];
                    }
                    
                }
                
            }
            else{
                for (int i = 0; i < nbzero; i++){
                    if (M[zeropos[i]][pos2[zeropos[i]]]>M[zeropos[i]][pos3[zeropos[i]]] && exitememe(s3,n,pos2[zeropos[i]])==false)
                    {
                        s3[zeropos[i]]=pos2[zeropos[i]];
                    }
                }
            }
            
        }
        
        nbzero=cptzero(s3,n);


        if (nbzero!=0)
        {
            student_sans_projet=poszero(s3,n,nbzero);
            projet_a_distribuer=projetdistribuer(s3,n,nbzero);
            if (nbzero==1)
            {
                s3[student_sans_projet[0]]=projet_a_distribuer[0];
            }
            else{
                projet_a_distribuer=mutation(projet_a_distribuer,1,nbzero);
                for (int i = 0; i < nbzero; i++)
                {
                    s3[student_sans_projet[i]]=projet_a_distribuer[i];
                }
                
            }
            
        }



        s3=mutation(s3,p,n);
        pos2=s3;



    }

    pos_final=pos2;
    scoreMAX=score(M,pos2,n);
    std::cout <<endl<< "Time:"<< float( clock () - begin_time ) /  CLOCKS_PER_SEC;
    return 0;   
}







int main( int argc, char * argv[] ){ 
    
    /*    
   vector<int> a_res1=vector<int>(6);
    for (int i = 0; i < 6; i++)
    {
        a_res1[i]=i;
    }
    
    int a_res2=0;


    
    studentProjectGenetique(a1,20,0.001,6,a_res1,a_res2);
    
    
    for (int i = 0; i < 6; i++)
    {
        std::cout<<a_res1[i]<<std::endl;
    }
    
    
    
    std::cout<<a_res2<<std::endl;*/



    vector<int> b_res1=vector<int>(6);
    for (int i = 0; i < 6; i++)
    {
        b_res1[i]=i;
    }
    
    int b_res2=0;
    studentProjectGenetique(b1,50,0.001,6,b_res1,b_res2);
    
    cout<< endl;
    for (int i = 0; i < 6; i++)
    {
        std::cout<<b_res1[i]<<" ";
    }
    
    
    cout<< endl;
    std::cout<<b_res2<<std::endl;
    
    return 0;
}