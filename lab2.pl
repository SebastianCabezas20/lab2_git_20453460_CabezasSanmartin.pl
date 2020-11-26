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
% [ID,Estado,[IDs
% respuestas],Autor,Fecha,Pregunta,[Etiquetas],pregunta, V.P,V.N] 0 =
% cerrada 1 = abierta Constructor
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,1,[[]],Nombre,Fecha,Pregunta,Etiquetas,pregunta,0,0]).

% TDA Respuesta
% (IDRespuesta,estado,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta,V.P,V.N)
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
    agregar(Preguntas,PreguntaNueva,NuevasPreguntas).

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
    agregarIdRespuesta([[IDPregunta,E,[H|C],Usuario|T]|Cola],IDPregunta,IDRespuesta,[Usuario|_],[[IDPregunta,E,[IDRespuesta,H|C]|T]|Cola]).%se agrega ID si coincide el usuario y el ID.
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
    stringU([Username,Pass],['   Nombre de Usuario: ' ,Username,'   Contraseña: ',Pass,"\n"]).


    ordenarPreguntas([],_,'\n\n').
    ordenarPreguntas([[_,_,IDs|_]|SigPreguntas],Respuestas,['Pregunta',PregResp|ColaNueva]):-%%completar Info de pregunta
    stringP(IDs,Respuestas,PregResp),ordenarPreguntas(SigPreguntas,Respuestas,ColaNueva).

    stringP([],_,[]).
    stringP([PrimerID|SigID],Respuestas,[E|Cola]):-
    buscador(PrimerID,Respuestas,E),stringP(SigID,Respuestas,Cola).
    buscador(_,[],[]).
    buscador(ID,[[ID|_]|_],[    "Respuesta con ID",ID]):-!.%%completar datos de respuesta a entregar
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
getAnswer([_,Preguntas,Respuestas,_],IDPregunta,IDrespuesta,Respuesta):-verificar2(Preguntas,Respuestas,IDPregunta,IDrespuesta),buscador2(IDrespuesta,Respuestas,Respuesta).%verificar que es respuesta de pregunta
   verificar2([],_,_,_):-!,fail.
   verificar2([[IDPregunta,_,[IDs]|_]|_],[Respuestas],IDPregunta,IDrespuesta):-idCorres(IDs,IDrespuesta,Respuestas).
    verificar2([_|Cola],[Respuestas],IDP,IDR):-verificar2(Cola,Respuestas,IDP,IDR).
% verificar de que el id de la respuesta sea con la pregunta peroooo
% desde la respuesta

%respuesta NEGATIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,respuesta|_],Boolean,[U,P,RN,[]]):-Boolean is false,votarNegativoR(ID,Respuestas,RN).
%respuesta POSITIVA
vote([U,P,Respuestas,_],[ID,_,_,_,_,_,respuesta|_],Boolean,[U,P,RN,[]]):-Boolean is true,votarPositivoR(ID,Respuestas,RN).
%pregunta NEGATIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],Boolean,[U,PN,R,[]]):-Boolean is false,votarNegativoP(ID,P,PN).
%pregunta POSITIVA
vote([U,P,R,_],[ID,_,_,_,_,_,_,pregunta|_],Boolean,[U,PN,R,[]]):-Boolean is true,votarPositivoP(ID,P,PN).

votarNegativoR(_,[],[]).
votarNegativoR(ID,[[ID,A,B,C,D,E,F,G,N]|SR],[[ID,A,B,C,D,E,F,G,NN]|SR]):-NN is N+1.
votarNegativoR(ID,[R|Rs],[R|Rss]):-votarNegativoR(ID,Rs,Rss).




%(IDRespuesta,estado,IDPregunta,Fecha,Respuesta,[Etiquetas],respuesta)
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











































