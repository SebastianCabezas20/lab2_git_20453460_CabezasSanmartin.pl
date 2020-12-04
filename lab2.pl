/*
EJEMPLO

stack(Stack),register(Stack,"userNuevo","passNuevo",Stack2).
stack2(Stack),register(Stack,"user20","1234",Stack2).
stack(Stack),register(Stack,"user1","pass1",Stack2).
-- primera correcta,segunda contraseña incorrecta,tercera usuarionoexiste
stack(Stack),login(Stack,"user1","pass1",Stack2).
stack(Stack),login(Stack,"user2","pass1",Stack2).
stack(Stack),login(Stack,"user29","pass29",Stack2).
---Primeracorrecta,segunda error de pass,sin login.
stack(Stack),login(Stack,"user1","pass1",Stack2),ask(Stack2,20-12-2020,"PreguntaE1",[et1,et2,et3],Stack3).
stack(Stack),login(Stack,"user1","pass2",Stack2),ask(Stack2,14-12-2020,"PreguntaE2",[Et1,et2,et3],Stack3).
stack(Stack),ask(Stack,24-12-2020,"PreguntaE3",[et1,et2,et3],Stack2).
---primera correcta,segunda pregunta no existe, tercera error son login
stack(Stack),login(Stack,"user2","pass2",Stack2),answer(Stack2,20-12-2020,1,"RespuestaE1",[Et1,et2,et3],Stack3).
stack(Stack),login(Stack,"user2","pass2",Stack2),answer(Stack2,20-12-2020,20,"RespuestaE2",[Et1,et2,et3],Stack3).
stack(Stack),answer(Stack2,20-12-2020,1,"RespuestaE3",[Et1,et2,et3],Stack3).
-Primera correcta,segunda el usuario no le pertenece,tercero sin login
stack(Stack),login(Stack,"user4","pass4",Stack2),accept(Stack2,4,7,Stack3).
stack(Stack),login(Stack,"user4","pass4",Stack2),accept(Stack2,5,9,Stack3).
stack(Stack),accept(Stack2,4,7,Stack3).
--
stack(Stack),login(Stack,"user1","pass1",Stack2),stackToString(Stack2,String),write(String).
stack(Stack),stackToString(Stack2,String),write(String).
stack(Stack),login(Stack,"user1","pass1",Stack2),stackToString(Stack2,String),write(String).
-----------
stack(Stack),login(Stack,"user1","pass1",Stack2),getQuestion(Stack2,1,P),vote(Stack2,P,true,Stack3).
stack(Stack),login(Stack,"user1","pass1",Stack2),getQuestion(Stack2,1,P),vote(Stack2,P,false,Stack3).
stack(Stack),login(Stack,"user1","pass1",Stack2),getAnswer(Stack2,1,,2,R),vote(Stack2,R,true,Stack3).




%TDA stack
%([usuarios], [preguntas], [respuestas],usuarioActivo)

% TDA usuario
%[username,pass,0]
modificador
agregarUsuario(Usuarios,Username,Pass,UsuariosNuevos,[UsuarioAgregado]).
reputacion(usuarios,username,operacion,reputacion).


% TDA Pregunta
% [ID,Estado,[IDsrespuestas],Autor,Fecha,Pregunta,[Etiquetas],pregunta, V.P,V.N]
0 = cerrada 1 = abierta
Constructor
pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,[Pregunta]).
modificadores
agregarIdRespuesta(Preguntas,IDPregunta,IDRespuesta,Usuario,PreguntasConID)

% TDA Respuesta
% (IDRespuesta,estado,autor,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta,V.P,V.N)
% 0 = no aceptada 1 = aceptada
Constructor
respuesta(IDRespuesta,Autor,IDPregunta,Fecha,Respuesta,Etiquetas,[Respuesta]).

modificadores
verificarID([Respuestas],ID,[RespuestasVerificadas])





*/


%   Predicados
% pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,ListaPregunta)
% respuesta(IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas,ListaRespuesta)
% agregarUsuario(ListaUsuarios,Usuario,ListaUsuarios2).
% register(Stack,Username,Pass,Stack2).
% login(Stack,Username,Pass,Stack2)
% autenticar(ListaUsuarios,Username,Pass,Usuario)
% ask(Stack,Fecha,TextoPregunta,ListaEtiquetas,Stack2)
% contador(Lista,Total)
% answer(Stack, Fecha, IDPregunta, TextoRespuesta,ListaEtiquetas,Stack2)
% verificarID(ListaPreguntas,IDPregunta)
% agregar(Lista,Lista2)
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

  */

%hechos%


%Stack 1
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


stack2([[["user1","pass1",0],["user2","pass2",0],["user3","pass3",0],["user4","pass4",0]],
 [],
 [],[]]).


%reglas



pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,1,[],Nombre,Fecha,Pregunta,Etiquetas,pregunta,0,0]).

respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,0,Username,IDPregunta,Fecha,Respuesta,Etiquetas,respuesta,0,0]).

% REGISTRO
agregarUsuario([],Username,Pass,[[Username,Pass,0]]).
agregarUsuario([[Username,Pass,R]|Cola],Username,_,[[Username,Pass,R]|Cola]):-!,fail.
agregarUsuario([Primer|Siguientes],Username,Pass,[Primer|ColaNueva]):-
    agregarUsuario(Siguientes,Username,Pass,ColaNueva).

register([Usuarios,Preguntas,Respuestas,UsuarioActivo],Username,Pass,[UsuariosNuevos,Preguntas,Respuestas,UsuarioActivo]):-
    agregarUsuario(Usuarios,Username,Pass,UsuariosNuevos).



%  LOGIN
   login([Usuarios,Preguntas,Respuestas,_],Username,Pass,[Usuarios,Preguntas,Respuestas,UsuarioActivo]):-
    autenticar(Usuarios,Username,Pass,UsuarioActivo).

   autenticar([],_,_,[]):-!,fail.
   autenticar([[Username,Pass,R]|_],Username,Pass,[Username,Pass,R]):-!.
   autenticar([_|Siguientes],Username,Pass,Activo):-
    autenticar(Siguientes,Username,Pass,Activo).

%  ASK
%
ask([Usuarios,Preguntas,Respuestas,[]],_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
%
ask([Usuarios,Preguntas,Respuestas,UsuarioActivo],Fecha,TextoPregunta,ListaEtiquetas,[Usuarios,NuevasPreguntas,Respuestas,[]]):-
    contador(Preguntas,ID),pregunta(ID,UsuarioActivo,Fecha,TextoPregunta,ListaEtiquetas,PreguntaNueva),
    agregar(PreguntaNueva,Preguntas,NuevasPreguntas).

    contador([],1).
    contador([_|Siguientes],Total):-contador(Siguientes,IDanterior),Total is IDanterior +1.

% ANSWER
answer([Usuarios,Preguntas,Respuestas,[]],_,_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.

answer([Usuarios,Preguntas,Respuestas,UsuarioActivo], Fecha, IDPregunta, TextoRespuesta,
ListaEtiquetas,[Usuarios,Preguntas,RespuestasNuevas,[]]):-
    contador(Respuestas,IDRespuesta),verificarID(Preguntas,IDPregunta),
respuesta(IDRespuesta,UsuarioActivo,IDPregunta,Fecha,TextoRespuesta,ListaEtiquetas,
         Respuesta),agregar(Respuesta,Respuestas,RespuestasNuevas).

verificarID([],_):-!,fail.
verificarID([[ID|_]|_],ID).
verificarID([_|Cola],ID):-verificarID(Cola,ID).

agregar(X,[],[X]).
agregar(X,[H|C],[X,H|C]).

%    ACCEPT
accept([Usuarios,Preguntas,Respuestas,[]],_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
accept([Usuarios,Preguntas,Respuestas,UsuarioActivo],IDPregunta,IDRespuesta,
       [Usuarios,PreguntasVerificadas,RespuestasVerificadas,[]]):-
    verificarIDR(Respuestas,IDRespuesta,RespuestasVerificadas),
    agregarIdRespuesta(Preguntas,IDPregunta,IDRespuesta,UsuarioActivo,PreguntasVerificadas).

    agregarIdRespuesta([],_,_,_,[]):-!,fail.
    agregarIdRespuesta([[IDPregunta,E,IDs,Usuario|T]|Cola],IDPregunta,IDRespuesta,[Usuario|_],[[IDPregunta,E,IDsN,Usuario|T]|Cola]):-agregar(IDRespuesta,IDs,IDsN).%se agrega ID si coincide el usuario y el ID.
    agregarIdRespuesta([Primero|Cola],IDPregunta,IDRespuesta,Usuario,[Primero|Cn]):-
    agregarIdRespuesta(Cola,IDPregunta,IDRespuesta,Usuario,Cn).
%agregamos 1 a aceptada respuesta
    verificarIDR([],_,[]):-!,fail.
    verificarIDR([[ID,0|T]|Cola],ID,[[ID,1|T]|Cola]).%aceptamos Respuesta si existe
    verificarIDR([Primera|Cola],ID,[Primera|Cola2]):-verificarIDR(Cola,ID,Cola2).


%  STACK-STRING

  stackToString([Usuarios,Preguntas,Respuestas,[]],[StringUsuarios,StringPreguntas]):-
    ordenarUsuarios(Usuarios,StringUsuarios),ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).
  stackToString([_,Preguntas,Respuestas,UsuarioActivo],[StringActivo,StringPreguntas]):-stringU(UsuarioActivo,StringActivo),
  ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).

    ordenarUsuarios([],"\n").
    ordenarUsuarios([Primero|Cola],[NuevoPrimero|ColaNueva]):-
    stringU(Primero,NuevoPrimero),ordenarUsuarios(Cola,ColaNueva).
    stringU([Username,Pass,R],['   Nombre de Usuario: ' ,Username,'   Contraseña: ',Pass,"Reputacion:",R,"\n"]).


    ordenarPreguntas([],_,'\n\n').
    ordenarPreguntas([[ID,_,IDs,A,_,PP,E,_,P,N]|SigPreguntas],Respuestas,["    El usuario",A,"Pregunta:",PP,"   Etiquetas:",E,"\n     Like:",P,"Dislike:",N,"        ID:",ID,"\n",PregResp|ColaNueva]):-
    stringP(IDs,Respuestas,PregResp),ordenarPreguntas(SigPreguntas,Respuestas,ColaNueva).

    stringP([],_,"\n").
    stringP([PrimerID|SigID],Respuestas,[[E|Cola]]):-
    buscador(PrimerID,Respuestas,E),stringP(SigID,Respuestas,Cola).
    buscador(_,[],[]).
    buscador(ID,[[ID,_,U,_,_,R,E,_,G,N]|_],["          El usuario:" ,U,"respondio" ,R ,"  Etiquetas:",E,"\n                        Like:",G,"Dislike:",N,"        ID:",ID,"\n"]):-!.
    buscador(ID,[_|SigID],Respuesta):-
     buscador(ID,SigID,Respuesta).



% VOTE
getQuestion([_,_,_,[]],_,[]):-!,fail.
getQuestion([_,Preguntas,_,UsuarioActivo],IDPregunta,Pregunta):-
    verificarUser(Preguntas,IDPregunta,UsuarioActivo),buscador2(IDPregunta,Preguntas,Pregunta).

    verificarUser([],_,_):-!,fail.
    verificarUser([[I,_,_,U|_]|_],I,[U|_]).
    verificarUser([_|Cola],I,Usuario):-verificarUser(Cola,I,Usuario).

    buscador2(ID,[[ID|Cola]|_],[ID|Cola]).
    buscador2(ID,[_|Sig],Pregunta):-buscador2(ID,Sig,Pregunta).

getAnswer([_,_,_,[]],_,_,[]):-!,fail.
getAnswer([_,Preguntas,Respuestas,_],IDPregunta,IDrespuesta,Respuesta):-verificar2(Preguntas,IDPregunta,IDrespuesta),buscador2(IDrespuesta,Respuestas,Respuesta).%verificar que es respuesta de pregunta

   verificar2([],_,_):-!,fail.
   verificar2([[IDPregunta,_,IDs|_]|_],IDPregunta,IDrespuesta):-idCorres(IDs,IDrespuesta).
   verificar2([_|Cola],IDP,IDR):-verificar2(Cola,IDP,IDR).
   idCorres([],_):-!,fail.
   idCorres([ID|_],ID).
   idCorres([_|Cola],ID):-idCorres(Cola,ID).


vote([_,_,_,[]],[_],_,[_]):-!,fail.
%respuesta NEGATIVA
vote([U,P,Respuestas,[Username|_]],[ID,_,_,_,_,_,_,respuesta|_],false,[UNN,P,RN,[]]):-votarNegativoR(ID,Respuestas,RN,U,UN),reputacion(UN,Username,resta,1,UNN).
%respuesta POSITIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,_,respuesta|_],true,[UN,P,RN,[]]):-votarPositivoR(ID,Respuestas,RN,U,UN).
%pregunta NEGATIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],false,[UN,PN,R,[]]):-votarNegativoP(ID,P,PN,U,UN).
%pregunta POSITIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],true,[UN,PN,R,[]]):-votarPositivoP(ID,P,PN,U,UN).

votarNegativoR(_,[],[],_,[]).
votarNegativoR(ID,[[ID,Z,A,B,C,D,E,F,G,N]|SR],[[ID,Z,A,B,C,D,E,F,G,NN]|SR],Usuarios,UsuariosN):-NN is N+1,reputacion(Usuarios,A,resta,2,UsuariosN).
votarNegativoR(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarNegativoR(ID,Rs,Rss,Usuarios,UsuariosN).

votarPositivoR(_,[],[],_,[]).
votarPositivoR(ID,[[ID,Z,A,B,C,D,E,F,G,N]|SR],[[ID,Z,A,B,C,D,E,F,GG,N]|SR],Usuarios,UsuariosN):-GG is G+1,reputacion(Usuarios,A,suma,10,UsuariosN).
votarPositivoR(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarPositivoR(ID,Rs,Rss,Usuarios,UsuariosN).

votarNegativoP(_,[],[],_,[]).
votarNegativoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,P,NN]|SR],Usuarios,UsuariosN):-NN is N+1,reputacion(Usuarios,C,resta,2,UsuariosN).
votarNegativoP(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarNegativoP(ID,Rs,Rss,Usuarios,UsuariosN).

votarPositivoP(_,[],[],_,[]).
votarPositivoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,PP,N]|SR],Usuarios,UsuariosN):-PP is P+1,reputacion(Usuarios,C,suma,10,UsuariosN).
votarPositivoP(ID,[R|Rs],[R|Rss],Usuarios,UsuariosN):-votarPositivoP(ID,Rs,Rss,Usuarios,UsuariosN).

reputacion([],_,_,_,[]):-!.
reputacion([[Username,Pass,R]|C],Username,suma,Reputacion,[[Username,Pass,RR]|C]):-RR is Reputacion+R.
reputacion([PrimerUsuario|SigUsuario],Username,suma,Reputacion,[PrimerUsuario|Cola]):-
   reputacion(SigUsuario,Username,suma,Reputacion,Cola).

reputacion([],_,_,_,[]):-!.
reputacion([[Username,Pass,R]|C],Username,resta,Reputacion,[[Username,Pass,RR]|C]):-RR is R-Reputacion.
reputacion([PrimerUsuario|SigUsuario],Username,resta,Reputacion,[PrimerUsuario|Cola]):-
   reputacion(SigUsuario,Username,resta,Reputacion,Cola).














