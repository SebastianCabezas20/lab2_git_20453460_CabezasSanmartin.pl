/*
DOM
ID pregunta:Numero que corresponde al ID de la pregunta
ID respuesta:Numero que corresponde al ID de la respuesta
Autor: String que corresponde al autor de pregunta o respuesta
Fecha: Numeros que corresponden a la fecha tal que dia-mes-año
Texto: String
Etiquetas: lista con las etiquetas
ListaPregunta: lista que corresponde a la pregunta
listaPreguntas: lista que corresponden a las preguntas
listaPreguntas: lista que corresponden a las preguntas modificada.
Stack : Lista que contiene usuarios,preguntas y respuestas
Stack2 :Lista que contiene usuarios,preguntas y respuestas modificado
Username: string con el nombre del usuario
Pass: string con la contraseña del usuario
Lista: lista que puede corresponder a preguntas o respuestas.
Total: Numero que corresponde al total de elementos
Usuario: lista que corresponde al usuario
ListaRespuestas : Lista de Respuestas
ListaRespuestas2 : Lista de respuestas modificada
String: Corresponde a string con stack
ListaUsuarios: lista usuarios
ListaUsuarios2: lista usuarios modificado
ListaIDs: lista con ID de respuesta
Boolean: booleano que corresponde a true o false
Reputacion: numero que indica reputacion.

*/


%   Predicados
% pregunta(IDPregunta,Autor,Fecha,Texto,Etiquetas,ListaPregunta)
% respuesta(IDRespuesta,IDPregunta,Fecha,Texto,Etiquetas,ListaRespuesta)
% agregarUsuario(ListaUsuarios,Usuario,ListaUsuarios2).
% register(Stack,Username,Pass,Stack2).
% login(Stack,Username,Pass,Stack2)
% autenticar(ListaUsuarios,Username,Pass,Usuario)
% ask(Stack,Fecha,Texto,Etiquetas,Stack2)
% contador(Lista,Total)
% answer(Stack, Fecha, IDPregunta, Texto,Etiquetas,Stack2)
% verificarID(ListaPreguntas,IDPregunta)
% agregar(Lista,Lista)
% accept(Stack,IDPregunta,IDRespuesta,Stack2)
% agregarIdRespuesta(ListaPreguntas,IDPregunta,IDRespuesta,Usuario,ListaPreguntas2)
% verificarIDR([ListaRespuestas,IDRespuesta,ListaRespuestas2).
% stackToString(Stack,String)
% ordenarUsuarios(ListaUsuarios,String)
% stringU(Usuario,String)
% ordenarPreguntas([ListaPreguntas,listaRespuestas,String)
% stringP(ListaIDs,ListaRespuestas,String)
% buscador(IDRespuesta,ListaRespuestas,String)
% getQuestion(Stack,IDPregunta,Pregunta).
% verificarUsername(Usuario,IDPregunta,ListaPregunta)
% buscador2(ID,Lista,Elemento)
% getAnswer(Stack,IDPregunta,IDrespuesta,Respuesta)
% verificar2(ListaPreguntas,IDPregunta,IDrespuesta)
% idCorres(ListaIDs,ID)
% vote(Stack,Elemento,boolean,Stack2)
% votarNegativoR(IDRespuesta,ListaRespuestas,ListaRespuestas2,ListaUsuarios,ListaUsuarios2)
% votarPositivoR(IDRespuesta,ListaRespuesta,ListaRespuestas2,ListaUsuarios,ListaUsuarios2)
% votarNegativoP(IDPregunta,ListaPreguntas,ListaPreguntas2,ListaUsuarios,ListaUsuarios2)
% votarPositivoP(IDPregunta,ListaPreguntas,ListaPreguntas2,ListaUsuarios,ListaUsuarios2)
% reputacion(ListaUsuarios,Username,Boolean,Reputacion,ListaUsuarios2)

/*Metas
     primarias
  register(Stack,Username,Pass,Stack2).
  login(Stack,Username,Pass,Stack2)
  ask(Stack,Fecha,TextoPregunta,ListaEtiquetas,Stack2)
  answer(Stack, Fecha, IDPregunta, TextoRespuesta,ListaEtiquetas,Stack2)
  accept(Stack,IDPregunta,IDRespuesta,Stack2)
  stackToString(Stack,String)
  getQuestion(Stack,IDPregunta,Pregunta).
  getAnswer(Stack,IDPregunta,IDrespuesta,Respuesta)
  vote(Stack,Elemento,boolean,Stack2)
    Secundarias
  pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,ListaPregunta)
  respuesta(IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas,ListaRespuesta)
  agregarUsuario(ListaUsuarios,Usuario,ListaUsuarios2)
  autenticar(ListaUsuarios,Username,Pass,Usuario)
  contador(Lista,Total) answer(Stack, Fecha, IDPregunta,
  TextoRespuesta,ListaEtiquetas,Stack2)
  verificarID(ListaPreguntas,IDPregunta) agregar(Lista,Lista2)
  agregarIdRespuesta(ListaPreguntas,IDPregunta,IDRespuesta,Usuario,ListaPreguntas2)
  verificarIDR([ListaRespuestas,IDRespuesta,ListaRespuestas2).
  ordenarUsuarios(ListaUsuarios,String) stringU(Usuario,String)
  ordenarPreguntas([ListaPreguntas,listaRespuestas,String)
  stringP(ListaIDs,ListaRespuestas,String)
  stringU(Usuario,String)
  buscador(IDRespuesta,ListaRespuestas,String)
  verificarUsername(Usuario,IDPregunta,ListaPregunta)
  buscador2(ID,Lista,Elemento)
  verificar2(ListaPreguntas,IDPregunta,IDrespuesta)
  idCorres(ListaIDs,ID)
  votarNegativoR(IDRespuesta,ListaRespuestas,ListaRespuestas2,ListaUsuarios,ListaUsuarios2)
  votarPositivoR(IDRespuesta,ListaRespuesta,ListaRespuestas2,ListaUsuarios,ListaUsuarios2)
  votarNegativoP(IDPregunta,ListaPreguntas,ListaPreguntas2,ListaUsuarios,ListaUsuarios2)
  votarPositivoP(IDPregunta,ListaPreguntas,ListaPreguntas2,ListaUsuarios,ListaUsuarios2)
  reputacion(ListaUsuarios,Username,Boolean,Reputacion,ListaUsuarios2)


%TDA stack
%[[usuarios], [preguntas], [respuestas],usuarioActivo]

% TDA usuario
%[username,pass,0]
modificador
reputacion(usuarios,username,operacion,reputacion).

TDA lista usuarios
[[usuario1],[usuario2],[usuario3]...]
modificador
agregarUsuario(Usuarios,Username,Pass,UsuariosNuevos,[UsuarioAgregado]).


% TDA Pregunta
% [ID,Estado,[IDsrespuestas],Autor,Fecha,Pregunta,[Etiquetas],pregunta, V.P,V.N]
0 = cerrada 1 = abierta
Constructor
pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,[Pregunta]).
modificadores
agregarIdRespuesta(Preguntas,IDPregunta,IDRespuesta,Usuario,PreguntasConID)

 TDA Respuesta
 (IDRespuesta,estado,autor,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta,V.P,V.N)
 0 = no aceptada 1 = aceptada
 Constructor
respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,[respuesta]).
modificador
verificarIDR(Respuestas,ID repuestas , respuesta modificada)
*/


%hechos%

% Corresponde a un stack con usuarios,preguntas y respuestas, algunas de
% estas estan vinculadas
stack([[["user1","pass1",0],["user2","pass2",0],["user3","pass3",0],["user4","pass4",0]],
 [[1,1,[1,2],"user1",20-20-2020,"pregunta1",[et1,et2,et3],pregunta,0,0],
 [2,1,[3,4],"user2",20-20-2020,"pregunta2",[et1,et2,et3],pregunta,0,0],
 [3,1,[5,6],"user3",20-20-2020,"pregunta3",[et1,et2,et3],pregunta,0,0],
 [4,1,[],"user4",20-20-2020,"pregunta4",[et1,et2,et3],pregunta,0,0],
 [5,1,[],"user1",20-20-2020,"pregunta5",[et1,et2,et3],pregunta,0,0]],
 [[1,1,"user2",1,20-20-2020,"respuesta1",[et1,et2,et3],respuesta,0,0],
  [2,1,"user3",1,20-20-2020,"respuesta2",[et1,et2,et3],respuesta,0,0],
 [3,1,"user3",2,20-20-2020,"respuesta3",[et1,et2,et3],respuesta,0,0],
 [4,1,"user4",2,20-20-2020,"respuesta4",[et1,et2,et3],respuesta,0,0],
 [5,1,"user1",3,20-20-2020,"respuesta5",[et1,et2,et3],respuesta,0,0],
 [6,1,"user4",3,20-20-2020,"respuesta6",[et1,et2,et3],respuesta,0,0],
 [7,0,"user2",4,20-20-2020,"respuesta7",[et1,et2,et3],respuesta,0,0],
 [8,0,"user1",4,20-20-2020,"respuesta8",[et1,et2,et3],respuesta,0,0],
 [9,0,"user3",5,20-20-2020,"respuesta9",[et1,et2,et3],respuesta,0,0],
 [10,0,"user4",5,20-20-2020,"respuesta10",[et1,et2,et3],respuesta,0,0]],[]]).

% Corresponde a un stack con usuarios,preguntas y respuestas,sin
% respuestas vinculadas
stack2([[["user1","pass1",0],["user2","pass2",0],["user3","pass3",0],["user4","pass4",0]],
 [[1,1,[],"user1",20-20-2020,"pregunta1",[et1,et2,et3],pregunta,0,0],
 [2,1,[],"user2",20-20-2020,"pregunta2",[et1,et2,et3],pregunta,0,0],
 [3,1,[],"user3",20-20-2020,"pregunta3",[et1,et2,et3],pregunta,0,0],
 [4,1,[],"user4",20-20-2020,"pregunta4",[et1,et2,et3],pregunta,0,0],
 [5,1,[],"user1",20-20-2020,"pregunta5",[et1,et2,et3],pregunta,0,0]],
 [[1,1,"user3",1,20-20-2020,"respuesta1",[et1,et2,et3],respuesta,0,0],
  [2,1,"user4",1,20-20-2020,"respuesta2",[et1,et2,et3],respuesta,0,0],
 [3,1,"user2",2,20-20-2020,"respuesta3",[et1,et2,et3],respuesta,0,0],
 [4,1,"user2",2,20-20-2020,"respuesta4",[et1,et2,et3],respuesta,0,0],
 [5,1,"user2",3,20-20-2020,"respuesta5",[et1,et2,et3],respuesta,0,0],
 [6,1,"user1",3,20-20-2020,"respuesta6",[et1,et2,et3],respuesta,0,0],
 [7,0,"user2",4,20-20-2020,"respuesta7",[et1,et2,et3],respuesta,0,0],
 [8,0,"user1",4,20-20-2020,"respuesta8",[et1,et2,et3],respuesta,0,0],
 [9,0,"user3",5,20-20-2020,"respuesta9",[et1,et2,et3],respuesta,0,0],
 [10,0,"user4",5,20-20-2020,"respuesta10",[et1,et2,et3],respuesta,0,0]],[]]).

stackVacio([[],[],[],[]]).

%Entrada:ID,Usuario,Fecha,Texto pregunta,Lista etiquetas
%Salida:Lista pregunta
%Descr:Crea lista de preguntas en base a terminos de entrada
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,1,[],Nombre,Fecha,Pregunta,Etiquetas,pregunta,0,0]).

%Entrada:ID,Usuario,ID pregunta,Fecha,Texto pregunta,Lista etiquetas
%Salida:Lista pregunta
%%Descr:Crea lista de respuesta en base a terminos de entrada
respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,0,Username,IDPregunta,Fecha,Respuesta,Etiquetas,respuesta,0,0]).

%entrada:Usuario
%Salida:String
%descrip: ordena usuario en un string
stringU([Username,Pass,R],['   Nombre de Usuario: ' ,Username,'   Contraseña: ',Pass,"Reputacion:",R,"\n"]).

%                  Reglas
% REGISTRO

%Entrada:lista usuarios,username,pass
%Salida: Lista usuarios modificada
%Descr: agrega usuario a lista de usuarios
agregarUsuario([],Username,Pass,[[Username,Pass,0]]).
agregarUsuario([[Username,Pass,R]|Cola],Username,_,[[Username,Pass,R]|Cola]):-!,fail.
agregarUsuario([Primer|Siguientes],Username,Pass,[Primer|ColaNueva]):-
    agregarUsuario(Siguientes,Username,Pass,ColaNueva).

%Entrada:Stack,username,pass
%Salida: Stack modificado
%descrip: registra a un nuevo usuario
stackRegister([Usuarios,Preguntas,Respuestas,UsuarioActivo],Username,Pass,[UsuariosNuevos,Preguntas,Respuestas,UsuarioActivo]):-
    agregarUsuario(Usuarios,Username,Pass,UsuariosNuevos).



%LOGIN
%Entrada:Stack,username,pass
%Salida: Stack modificado
%descrip: Autenticacion de cuenta usuario
stackLogin([Usuarios,Preguntas,Respuestas,_],Username,Pass,[Usuarios,Preguntas,Respuestas,UsuarioActivo]):-
    autenticar(Usuarios,Username,Pass,UsuarioActivo).

    %Entrada:Lista usuarios,username,pass
    %Salida: Usuario
    %descrip: verifica que username y pass coincidan, si asi retorna usuario
   autenticar([],_,_,[]):-!,fail.
   autenticar([[Username,Pass,R]|_],Username,Pass,[Username,Pass,R]):-!.
   autenticar([_|Siguientes],Username,Pass,Activo):-
    autenticar(Siguientes,Username,Pass,Activo).

%  ASK
%Entrada:Stack,Fecha,Texto pregunta,lista etiquetas
%Salida: Stack modificado
%descrip: crea y agrega pregunta a stack
ask([Usuarios,Preguntas,Respuestas,[]],_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
ask([Usuarios,Preguntas,Respuestas,UsuarioActivo],Fecha,TextoPregunta,ListaEtiquetas,[Usuarios,NuevasPreguntas,Respuestas,[]]):-
    contador(Preguntas,ID),pregunta(ID,UsuarioActivo,Fecha,TextoPregunta,ListaEtiquetas,PreguntaNueva),
    agregar(PreguntaNueva,Preguntas,NuevasPreguntas).

    %entrada:Lista
    %Salida:numero con total de elementos
    %descrip: contabiliza la cantidad de elementos en una lista
    contador([],1).
    contador([_|Siguientes],Total):-contador(Siguientes,IDanterior),Total is IDanterior +1.

% ANSWER
%entrada:Stack,Fecha,ID de pregunta,Texto de pregunta,lista de etiquetas
%Salida:Stack modificado
%descrip: Crea y agrega respuesta a stack
answer([Usuarios,Preguntas,Respuestas,[]],_,_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
answer([Usuarios,Preguntas,Respuestas,UsuarioActivo], Fecha, IDPregunta, TextoRespuesta,
ListaEtiquetas,[Usuarios,Preguntas,RespuestasNuevas,[]]):-
    contador(Respuestas,IDRespuesta),verificarID(Preguntas,IDPregunta),
respuesta(IDRespuesta,UsuarioActivo,IDPregunta,Fecha,TextoRespuesta,ListaEtiquetas,
         Respuesta),agregar(Respuesta,Respuestas,RespuestasNuevas).

%entrada:Lista preguntas,ID pregunta
%Salida: boolean
%descrip: verifica que existe pregunta
verificarID([],_):-!,fail.
verificarID([[ID|_]|_],ID).
verificarID([_|Cola],ID):-verificarID(Cola,ID).

%entrada: Elemento
%Salida: Lista con elemento agregado
%descrip: agrega elemento a lista por la cabeza
agregar(X,[],[X]).
agregar(X,[H|C],[X,H|C]).

%    ACCEPT
%entrada:Stack,ID de pregunta, ID de respuesta
%Salida:Stack modificado
%descrip: vincula respuesta a pregunta, mediante la aceptacion
accept([Usuarios,Preguntas,Respuestas,[]],_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
accept([Usuarios,Preguntas,Respuestas,UsuarioActivo],IDPregunta,IDRespuesta,
       [Usuarios,PreguntasVerificadas,RespuestasVerificadas,[]]):-
    verificarIDR(Respuestas,IDRespuesta,RespuestasVerificadas),
    agregarIdRespuesta(Preguntas,IDPregunta,IDRespuesta,UsuarioActivo,PreguntasVerificadas).

    %entrada:Lista Preguntas,ID pregunta,ID respuesta,Usuario
    %Salida:Lista preguntas modificada
    %descrip: agrega ID a ID de respuesta, mientras verifica que el usuario activo corresponda al autor y verifica que exista la pregunta
    agregarIdRespuesta([],_,_,_,[]):-!,fail.
    agregarIdRespuesta([[IDPregunta,E,IDs,Usuario|T]|Cola],IDPregunta,IDRespuesta,[Usuario|_],[[IDPregunta,E,IDsN,Usuario|T]|Cola]):-agregar(IDRespuesta,IDs,IDsN).%se agrega ID si coincide el usuario y el ID.
    agregarIdRespuesta([Primero|Cola],IDPregunta,IDRespuesta,Usuario,[Primero|Cn]):-
    agregarIdRespuesta(Cola,IDPregunta,IDRespuesta,Usuario,Cn).

    %entrada:Lista respuestas, ID respuesta
    %Salida:Lista respuestas modificada
    %descrip: Verifica que exista respuesta y se cambia a aceptada
    verificarIDR([],_,[]):-!,fail.
    verificarIDR([[ID,0|T]|Cola],ID,[[ID,1|T]|Cola]).%aceptamos Respuesta si existe
    verificarIDR([Primera|Cola],ID,[Primera|Cola2]):-verificarIDR(Cola,ID,Cola2).


  %  STACK-STRING
    %entrada:Stack
    %Salida:String
    %descrip: ordena los usuarios,preguntas y respuestas en un string
  stackToString([Usuarios,Preguntas,Respuestas,[]],[StringUsuarios,StringPreguntas]):-
    ordenarUsuarios(Usuarios,StringUsuarios),ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).
  stackToString([_,Preguntas,Respuestas,UsuarioActivo],[StringActivo,"\n",StringPreguntas]):-stringU(UsuarioActivo,StringActivo),
  ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).

    %entrada:Lista usuarios
    %Salida:String
    %descrip: Ordena usuarios en un string
    ordenarUsuarios([],"\n").
    ordenarUsuarios([Primero|Cola],[NuevoPrimero|ColaNueva]):-
    stringU(Primero,NuevoPrimero),ordenarUsuarios(Cola,ColaNueva).


    %entrada:Lista Preguntas
    %Salida:String
    %descrip: ordena preguntas y sus respectivas respuestas
    ordenarPreguntas([],_,'\n\n').
    ordenarPreguntas([[ID,_,IDs,A,_,PP,E,_,P,N]|SigPreguntas],Respuestas,["    El usuario",A,"Pregunta:",PP,"   Etiquetas:",E,"\n     Like:",P,"Dislike:",N,"        ID:",ID,"\n",PregResp|ColaNueva]):-
    stringP(IDs,Respuestas,PregResp),ordenarPreguntas(SigPreguntas,Respuestas,ColaNueva).

    %entrada:Lista IDs,Lista de respuestas
    %Salida:String
    %descrip: une string de cada respuesta
    stringP([],_,"\n").
    stringP([PrimerID|SigID],Respuestas,[[E|Cola]]):-
    buscador(PrimerID,Respuestas,E),stringP(SigID,Respuestas,Cola).

    %entrada:ID Respuesta,Respuestas
    %Salida: String
    %descrip: busca y ordena respuesta a un string
    buscador(_,[],[]).
    buscador(ID,[[ID,_,U,_,_,R,E,_,G,N]|_],["          El usuario:" ,U,"respondio" ,R ,"  Etiquetas:",E,"\n                        Like:",G,"Dislike:",N,"        ID:",ID,"\n"]):-!.
    buscador(ID,[_|SigID],Respuesta):-
     buscador(ID,SigID,Respuesta).



% VOTE
%entrada:Stack,ID pregunta
%Salida: Pregunta
%descrip: retorna la pregunta elegida
getQuestion([_,_,_,[]],_,[]):-!,fail.
getQuestion([_,Preguntas,_,UsuarioActivo],IDPregunta,Pregunta):-
    verificarUser(Preguntas,IDPregunta,UsuarioActivo),buscador2(IDPregunta,Preguntas,Pregunta).

    %entrada:Lista preguntas,ID pregunta,Usuario
    %Salida: Boolean
    %descrip: verifica que el usuario sea autor de esa pregunta
    verificarUser([],_,_):-!,fail.
    verificarUser([[I,_,_,U|_]|_],I,[U|_]).
    verificarUser([_|Cola],I,Usuario):-verificarUser(Cola,I,Usuario).

    %entrada:ID,Lista
    %Salida: Elemento(pregunta o respuesta)
    %descrip: busca un pregunta o respuesta en base a su id
    buscador2(ID,[[ID|Cola]|_],[ID|Cola]).
    buscador2(ID,[_|Sig],Pregunta):-buscador2(ID,Sig,Pregunta).

%entrada:Stack,ID pregunta, ID respuesta
%Salida: Respuesta
%descrip: retorna la respuesta escojida
getAnswer([_,_,_,[]],_,_,[]):-!,fail.
getAnswer([_,Preguntas,Respuestas,_],IDPregunta,IDrespuesta,Respuesta):-verificar2(Preguntas,IDPregunta,IDrespuesta),buscador2(IDrespuesta,Respuestas,Respuesta).%verificar que es respuesta de pregunta

   %entrada:Lista preguntas,ID pregunta,ID respuesta
   %Salida: Boolean
   %descrip: verifica que la respuesta corresponda a pregunta
   verificar2([],_,_):-!,fail.
   verificar2([[IDPregunta,_,IDs|_]|_],IDPregunta,IDrespuesta):-idCorres(IDs,IDrespuesta).
   verificar2([_|Cola],IDP,IDR):-verificar2(Cola,IDP,IDR).

   %entrada:Lista IDs,ID respuesta
   %Salida: Boolean
   %descrip: verifica que exista id en lista de ids
   idCorres([],_):-!,fail.
   idCorres([ID|_],ID).
   idCorres([_|Cola],ID):-idCorres(Cola,ID).

%entrada: Stack,Elemento(pregunta o respuesta),boolean
%Salida: Stack modificado
%descrip: permite realizar un voto y actualiza puntos de reputacion
vote([_,_,_,[]],[_],_,[_]):-!,fail.
%respuesta NEGATIVA
vote([U,P,Respuestas,[Username|_]],[ID,_,_,_,_,_,_,respuesta|_],false,[UNN,P,RN,[]]):-votarNegativoR(ID,Respuestas,RN,U,UN),reputacion(UN,Username,resta,1,UNN).
%respuesta POSITIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,_,respuesta|_],true,[UN,P,RN,[]]):-votarPositivoR(ID,Respuestas,RN,U,UN).
%pregunta NEGATIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],false,[UN,PN,R,[]]):-votarNegativoP(ID,P,PN,U,UN).
%pregunta POSITIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],true,[UN,PN,R,[]]):-votarPositivoP(ID,P,PN,U,UN).

%Entrada:ID respuesta,Lista respuestas,Lista usuarios
%Salida:Lista respuestas modificada y lista de usuarios modificada
% descrip: permite realizar un voto negativo a una respuesta y realiza
% resta a la reputacion de usuario que le dan el voto
votarNegativoR(_,[],[],_,[]).
votarNegativoR(ID,[[ID,Z,A,B,C,D,E,F,G,N]|SR],[[ID,Z,A,B,C,D,E,F,G,NN]|SR],Usuarios,UsuariosN):-NN is N+1,reputacion(Usuarios,A,resta,2,UsuariosN).
votarNegativoR(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarNegativoR(ID,Rs,Rss,Usuarios,UsuariosN).

%Entrada:ID respuesta,Lista respuestas,Lista usuarios
%Salida:Lista respuestas modificada y lista de usuarios modificada
% descrip: permite realizar un voto positivo a una respuesta y realiza
% suma a la reputacion de usuario que le dan el voto.
votarPositivoR(_,[],[],_,[]).
votarPositivoR(ID,[[ID,Z,A,B,C,D,E,F,G,N]|SR],[[ID,Z,A,B,C,D,E,F,GG,N]|SR],Usuarios,UsuariosN):-GG is G+1,reputacion(Usuarios,A,suma,10,UsuariosN).
votarPositivoR(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarPositivoR(ID,Rs,Rss,Usuarios,UsuariosN).

%Entrada:ID preguntas,Lista preguntas,Lista usuarios
%Salida:Lista preguntas modificada y lista de usuarios modificada
% descrip: permite realizar un voto negativo a una pregunta y realiza
% resta a la reputacion de usuario que le dan el voto
votarNegativoP(_,[],[],_,[]).
votarNegativoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,P,NN]|SR],Usuarios,UsuariosN):-NN is N+1,reputacion(Usuarios,C,resta,2,UsuariosN).
votarNegativoP(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarNegativoP(ID,Rs,Rss,Usuarios,UsuariosN).

%Entrada:ID preguntas,Lista preguntas,Lista usuarios
%Salida:Lista preguntas modificada y lista de usuarios modificada
% descrip: permite realizar un voto positivo a una pregunta y realiza
% suma a la reputacion de usuario que le dan el voto
votarPositivoP(_,[],[],_,[]).
votarPositivoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,PP,N]|SR],Usuarios,UsuariosN):-PP is P+1,reputacion(Usuarios,C,suma,10,UsuariosN).
votarPositivoP(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarPositivoP(ID,Rs,Rss,Usuarios,UsuariosN).

%entrada:Lista Usuarios,Nombre usuario,Operacion
%Salida: Lista usuarios modificado
%Descrip: Permite realizar la suma a la reputacion de cierto usuario
reputacion([],_,_,_,[]):-!.
reputacion([[Username,Pass,R]|C],Username,suma,Reputacion,[[Username,Pass,RR]|C]):-RR is Reputacion+R.
reputacion([PrimerUsuario|SigUsuario],Username,suma,Reputacion,[PrimerUsuario|Cola]):-
   reputacion(SigUsuario,Username,suma,Reputacion,Cola).

%entrada:Lista Usuarios,Nombre usuario,Operacion
%Salida: Lista usuarios modificado
%Descrip: Permite realizar la suma a la reputacion de cierto usuario
reputacion([],_,_,_,[]):-!.
reputacion([[Username,Pass,R]|C],Username,resta,Reputacion,[[Username,Pass,RR]|C]):-RR is R-Reputacion.
reputacion([PrimerUsuario|SigUsuario],Username,resta,Reputacion,[PrimerUsuario|Cola]):-
   reputacion(SigUsuario,Username,resta,Reputacion,Cola).

/*
EJEMPLOS
REGISTER
stack(Stack),stackRegister(Stack,"userNuevo","passNuevo",Stack2).
stack(Stack),stackRegister(Stack,"user20","1234",Stack2).
stack(Stack),stackRegister(Stack,"user1","pass1",Stack2).

LOGIN
-- primera correcta,segunda contraseña incorrecta,tercera usuarionoexiste

stack(Stack),stackLogin(Stack,"user1","pass1",Stack2).
stack(Stack),stackLogin(Stack,"user2","pass1",Stack2).
stack(Stack),stackLogin(Stack,"user29","pass29",Stack2).

ASK
---Primeracorrecta,segunda error de pass,sin login.
stack(Stack),stackLogin(Stack,"user1","pass1",Stack2),ask(Stack2,20-12-2020,"PreguntaE1",[et1,et2,et3],Stack3).
stack(Stack),stackLogin(Stack,"user1","pass2",Stack2),ask(Stack2,14-12-2020,"PreguntaE2",[Et1,et2,et3],Stack3).
stack(Stack),ask(Stack,24-12-2020,"PreguntaE3",[et1,et2,et3],Stack2).

ANSWER ---primera correcta,segunda pregunta no existe, tercera error son
login
stack(Stack),stackLogin(Stack,"user2","pass2",Stack2),answer(Stack2,20-12-2020,1,"RespuestaE1",[Et1,et2,et3],Stack3).
stack(Stack),stackLogin(Stack,"user2","pass2",Stack2),answer(Stack2,20-12-2020,20,"RespuestaE2",[Et1,et2,et3],Stack3).
stack(Stack),answer(Stack2,20-12-2020,1,"RespuestaE3",[Et1,et2,et3],Stack3).



ACCEPT
-Primera correcta,segunda el usuario no le pertenece,tercero sin login
stack(Stack),stackLogin(Stack,"user4","pass4",Stack2),accept(Stack2,4,7,Stack3).
stack(Stack),stacklogin(Stack,"user4","pass4",Stack2),accept(Stack2,5,9,Stack3).
stack(Stack),accept(Stack2,4,7,Stack3).

STACKTOSTRING
--
stack(Stack),stackLogin(Stack,"user1","pass1",Stack2),stackToString(Stack2,String),write(String).
stack(Stack),stackToString(Stack2,String),write(String).
stack(Stack),stackLogin(Stack,"user2","pass2",Stack2),stackToString(Stack2,String),write(String).

VOTE
-----------
stack(Stack),stackLogin(Stack,"user1","pass1",Stack2),getQuestion(Stack2,1,P),vote(Stack2,P,true,Stack3).
stack(Stack),stackLogin(Stack,"user1","pass1",Stack2),getQuestion(Stack2,1,P),vote(Stack2,P,false,Stack3).
stack(Stack),stackLogin(Stack,"user1","pass1",Stack2),getAnswer(Stack2,1,,2,R),vote(Stack2,R,true,Stack3).

*/


















