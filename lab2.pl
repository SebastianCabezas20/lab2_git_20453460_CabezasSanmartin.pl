/*
EJEMPLO
 stack3(A),login(A,"user1","pass1",Stack3),ask(Stack3,20-20-2020,"Mi
 pregunta",[et1,et2,et3],Stack4),login(Stack4,"user1","pass1",Stack5),answer(Stack5,20-20-2020,1,"respuesta1",[et1,et2,et3],Stack6).

stack3(A),login(A,"user1","pass1",Stack3),ask(Stack3,20-20-2020,"Mi pregunta",[et1,et2,et3],Stack4),login(Stack4,"user1","pass1",Stack5),answer(Stack5,20-20-2020,1,"respuesta1",[et1,et2,et3],Stack6),login(Stack6,"user1","pass1",Stack7),accept(Stack7,1,1,Stack8).
*/

%DOM
%Nombre,Pass,ID,Autor,Fecha,Respuesta;pregunta
%
%   Predicados
%usuario(Nombre,pass,Lista)
%pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,Lista)
%respuesta(IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas,Lista)
%agregarUsuarioActivo(Lista,Usuario,Lista).
%register(Lista,Username,Pass,Lista).

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

stack3([[["user1","pass1"],["user2","pass2"],["user3","pass2"]],
 [],
 [],[]]).


% FUNCION DE REGISTRO
agregarUsuario([],Username,Pass,[[Username,Pass,0]]).
agregarUsuario([[Username|Pass]|Cola],Username,_,[[Username|Pass]|Cola]):-!.
agregarUsuario([Primer|Siguientes],Username,Pass,[Primer|ColaNueva]):-
    agregarUsuario(Siguientes,Username,Pass,ColaNueva).

register([Usuarios,Preguntas,Respuestas,UsuarioActivo],Username,Pass,[UsuariosNuevos,Preguntas,Respuestas,UsuarioActivo]):-
    agregarUsuario(Usuarios,Username,Pass,UsuariosNuevos).



%  FUNCION LOGIN
   login([Usuarios,Preguntas,Respuestas,_],Username,Pass,[Usuarios,Preguntas,Respuestas,UsuarioActivo]):-
    autenticar(Usuarios,Username,Pass,UsuarioActivo).

   autenticar([],_,_,[]):-!,fail.
   autenticar([[Username,Pass]|_],Username,Pass,[Username,Pass]):-!.
   autenticar([_|Siguientes],Username,Pass,Activo):-
    autenticar(Siguientes,Username,Pass,Activo).

%  .FUNCION ASK
%
ask([Usuarios,Preguntas,Respuestas,[]],_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
%
ask([Usuarios,Preguntas,Respuestas,UsuarioActivo],Fecha,TextoPregunta,ListaEtiquetas,[Usuarios,NuevasPreguntas,Respuestas,[]]):-
    contador(Preguntas,ID),pregunta(ID,UsuarioActivo,Fecha,TextoPregunta,ListaEtiquetas,PreguntaNueva),
    agregar(PreguntaNueva,Preguntas,NuevasPreguntas).

    contador([],1).
    contador([_|Siguientes],ID):-contador(Siguientes,IDanterior),ID is IDanterior +1.

% FUNCION ANSWER
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

%  FUNCION PRINCIPAL ACCEPT
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


% FUNCION STACK-STRING

  stackToString([Usuarios,Preguntas,Respuestas,[]],[StringUsuarios,StringPreguntas]):-
    ordenarUsuarios(Usuarios,StringUsuarios),ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).
  stackToString([_,Preguntas,Respuestas,UsuarioActivo],[StringActivo,StringPreguntas]):-stringU(UsuarioActivo,StringActivo),
  ordenarPreguntas(Preguntas,Respuestas,StringPreguntas).

    ordenarUsuarios([],"\n").
    ordenarUsuarios([Primero|Cola],[NuevoPrimero|ColaNueva]):-
    stringU(Primero,NuevoPrimero),ordenarUsuarios(Cola,ColaNueva).
    stringU([Username,Pass],['   Nombre de Usuario: ' ,Username,'   Contraseņa: ',Pass,"\n"]).


    ordenarPreguntas([],_,'\n\n').
    ordenarPreguntas([[_,_,IDs,A,_,PP,_,_,P,N]|SigPreguntas],Respuestas,["El usuario",A,"Pregunta:",PP,"\n  Like",P,"Dislike",N,"\nRESPUESTAS:\n",PregResp|ColaNueva]):-%%completa
    stringP(IDs,Respuestas,PregResp),ordenarPreguntas(SigPreguntas,Respuestas,ColaNueva).

    stringP([],_,[]).
    stringP([PrimerID|SigID],Respuestas,[E|Cola]):-
    buscador(PrimerID,Respuestas,E),stringP(SigID,Respuestas,Cola).
    buscador(_,[],[]).
    buscador(ID,[[ID,_,_,_,_,R,_,_,G,N]|_],[ID,"    Respuesta:",R," Like:",G,"Dislike:",N]):-!.
    buscador(ID,[_|SigID],Respuesta):-
     buscador(ID,SigID,Respuesta).






% VOTE
getQuestion([_,_,_,[]],_,[]):-!,fail.
getQuestion([_,Preguntas,_,UsuarioActivo],IDPregunta,Pregunta):-
    verificarUsername(UsuarioActivo,IDPregunta,Preguntas),buscador2(IDPregunta,Preguntas,Pregunta).

    verificarUsername(_,_,[]):-!,fail.
    verificarUsername([Username|_],ID,[[ID,_,Username|_]|_]).
    verificarUsername([Usuario],ID,[_,SiguientePregunta]):-verificarUsername(Usuario,ID,SiguientePregunta).

    buscador2(ID,[[ID|Cola]|_],[ID|Cola]).
    buscador2(ID,[_|Sig],Pregunta):-buscador2(ID,Sig,Pregunta).

getAnswer([_,_,_,[]],_,_,[]):-!,fail.
getAnswer([_,Preguntas,Respuestas,_],IDPregunta,IDrespuesta,Respuesta):-verificar2(Preguntas,IDPregunta,IDrespuesta),buscador2(IDrespuesta,Respuestas,Respuesta).%verificar que es respuesta de pregunta

   verificar2([],_,_):-!,fail.
   verificar2([[IDPregunta,_,[IDs]|_]|_],IDPregunta,IDrespuesta):-idCorres(IDs,IDrespuesta).
   verificar2([_|Cola],IDP,IDR):-verificar2(Cola,IDP,IDR).
   idCorres([],_):-!,fail.
   idCorres([ID|_],ID).
   idCorres([_|Cola],ID):-idCorres(Cola,ID).

% verificar de que el id de la respuesta sea con la pregunta peroooo
% desde la respuesta

%respuesta NEGATIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,_,respuesta|_],false,[U,P,RN,[]]):-votarNegativoR(ID,Respuestas,RN).
%respuesta POSITIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,_,respuesta|_],true,[U,P,RN,[]]):-votarPositivoR(ID,Respuestas,RN).
%pregunta NEGATIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],false,[U,PN,R,[]]):-votarNegativoP(ID,P,PN).
%pregunta POSITIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],true,[U,PN,R,[]]):-votarPositivoP(ID,P,PN).

votarNegativoR(_,[],[]).
votarNegativoR(ID,[[ID,A,B,C,D,E,F,G,N]|SR],[[ID,A,B,C,D,E,F,G,NN]|SR]):-NN is N+1.%[]
votarNegativoR(ID,[R|Rs],[R|Rss]):-votarNegativoR(ID,Rs,Rss).

votarPositivoR(_,[],[]).
votarPositivoR(ID,[[ID,A,B,C,D,E,F,G,N]|SR],[[ID,A,B,C,D,E,F,GG,N]|SR]):-GG is G+1.
votarPositivoR(ID,[R|Rs],[R|Rss]):-votarPositivoR(ID,Rs,Rss).

votarNegativoP(_,[],[]).
votarNegativoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,P,NN]|SR]):-NN is N+1.
votarNegativoP(ID,[R|Rs],[R|Rss]):-votarNegativoP(ID,Rs,Rss).

votarPositivoP(_,[],[]).
votarPositivoP(ID,[[ID,A,B,C,D,E,F,G,P,N]|SR],[[ID,A,B,C,D,E,F,G,PP,N]|SR]):-PP is P+1.
votarPositivoP(ID,[R|Rs],[R|Rss]):-votarPositivoP(ID,Rs,Rss).




%(Idrespuesta,Estado,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta)
%[ID,Estado,[IDs respuestas],Autor,Fecha,Pregunta,[Etiquetas],pregunta]
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%
%


















