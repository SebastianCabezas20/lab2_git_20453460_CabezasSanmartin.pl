/*
EJEMPLO
 stack3(A),login(A,"user1","pass1",Stack3),ask(Stack3,20-20-2020,"Mi
 pregunta",[et1,et2,et3],Stack4),login(Stack4,"user1","pass1",Stack5),answer(Stack5,20-20-2020,1,"respuesta1",[et1,et2,et3],Stack6).

stack3(A),login(A,"user1","pass1",Stack3),ask(Stack3,20-20-2020,"Mi pregunta",[et1,et2,et3],Stack4),login(Stack4,"user1","pass1",Stack5),answer(Stack5,20-20-2020,1,"respuesta1",[et1,et2,et3],Stack6),login(Stack6,"user1","pass1",Stack7),accept(Stack7,1,1,Stack8),login(Stack8,"user2","pass2",Stack9),ask(Stack9,20-20-2020,"mi pregunta2",[et1,et2,et3],Stack10),stackToString(Stack10,String),write(String).
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
% getQuestion(Stack,IDPregunta,Pregunta):
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


%hechos

%reglas

%TDA stack
%([usuarios], [preguntas], [respuestas],usuarioActivo)

% TDA usuario
%[username,pass,0]


% TDA Pregunta
% [ID,Estado,[IDsrespuestas],Autor,Fecha,Pregunta,[Etiquetas],pregunta, V.P,V.N] 0 =
% cerrada 1 = abierta Constructor
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,1,[],Nombre,Fecha,Pregunta,Etiquetas,pregunta,0,0]).

% TDA Respuesta
% (IDRespuesta,estado,autor,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta,V.P,V.N)
% 0 = no aceptada 1 = aceptada Constructor
respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,0,Username,IDPregunta,Fecha,Respuesta,Etiquetas,respuesta,0,0]).




%Stack 1
stack([[[user1,pass1,0],[user2,pass2,0],[user2,pass2,0]],
 [[1, sebastian, 20-20-20, "pregunta1?", [et1, et2, et3]],
  [2, valentina, 20-20-20, "pregunta2?", [et1, et2, et3]],
  [1, guillermo, 20-20-20, "pregunta3?", [et1, et2, et3]]],
 [[1, 1, 20-20-20, "respuesta1", [et1, et2, et3]],
  [2, 2, 20-20-20, "respuesta2", [et1, et2, et3]],
  [3, 3, 20-20-20, "respuesta3", [et1, et2, et3]]]]).
stack2([[[user1,pass1,0],[user2,pass2,0],[user3,pass2,0]],
 [[1,nombreGenerico, 20-20-20, "pregunta1?", [et1, et2, et3]]],
 [[1, 1, 20-20-20, "respuesta1", [et1, et2, et3]]]]).

stack3([[["user1","pass1",0],["user2","pass2",0],["user3","pass2",0]],
 [],
 [],[]]).


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
    agregarIdRespuesta([Primero|Cola],IDPregunta,IDRespuesta,[Usuario|Pass],[Primero|Cn]):-
    agregarIdRespuesta(Cola,IDPregunta,IDRespuesta,[Usuario,Pass],Cn).
%agregamos 1 a aceptada respuesta
    verificarIDR([],_,[]):-!,fail.
    verificarIDR([[ID,0|T]|Cola],ID,[[ID,1|T]|Cola]).%aceptamos Respuesta si existe
    verificarIDR([Primera|Cola],ID,[Primera,Cola2]):-verificarIDR(Cola,ID,Cola2).


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
    ordenarPreguntas([[ID,_,IDs,A,_,PP,E,_,P,N]|SigPreguntas],Respuestas,["   El usuario",A,"Pregunta:",PP,"   Etiquetas:",E,"\n     Like:",P,"Dislike:",N,"        ID:",ID,"\n",PregResp|ColaNueva]):-
    stringP(IDs,Respuestas,PregResp),ordenarPreguntas(SigPreguntas,Respuestas,ColaNueva).

    stringP([],_,"\n").
    stringP([PrimerID|SigID],Respuestas,[ "      RESPUESTAS:\n",[E|Cola]]):-
    buscador(PrimerID,Respuestas,E),stringP(SigID,Respuestas,Cola).
    buscador(_,[],[]).
    buscador(ID,[[ID,_,U,_,_,R,E,_,G,N]|_],["          El usuario:" ,U,"respondio" ,R ,"  Etiquetas:",E,"\n      Like:",G,"Dislike:",N,"        ID:",ID,"\n"]):-!.
    buscador(ID,[_|SigID],Respuesta):-
     buscador(ID,SigID,Respuesta).

% VOTE
getQuestion([_,_,_,[]],_,[]):-!,fail.
getQuestion([_,Preguntas,_,UsuarioActivo],IDPregunta,Pregunta):-
    verificarUsername(UsuarioActivo,IDPregunta,Preguntas),buscador2(IDPregunta,Preguntas,Pregunta).

    verificarUsername(_,_,[]):-!,fail.
    verificarUsername([Username|_],ID,[[ID,_,_,Username|_]|_]).
    verificarUsername([Usuario],ID,[_|SiguientePregunta]):-verificarUsername(Usuario,ID,SiguientePregunta).

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



















