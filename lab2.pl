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
%(username,pass,reputacion)
%Constructor
usuario(Nombre,Pass,[[Nombre,Pass,0]]).

% TDA Pregunta
% (ID ,Autor,Fecha,Pregunta,[Etiquetas][IDs respuesta])
% Constructor
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,Nombre,Fecha,Pregunta,Etiquetas,[]]).

% TDA Respuesta
% (IDRespuesta ,IDPregunta,Fecha,Respuesta,[Etiquetas])
% Constructor
respuesta(IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas]).
%modificador
agregarPregunta([],Pregunta,[Pregunta]).
agregarPregunta([Cabeza|Cola],Pregunta,[Pregunta,Cabeza|Cola]).

% TDA usuarioActivo
% agregar
agregarUsuarioActivo2([A,B,C],Usuario,[A,B,C,Usuario]).
%Remover
removerUsuarioActivo2([A,B,C,_],[A,B,C]).


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

stack3([[[user1,pass1,0],[user2,pass2,0],[user3,pass2,0]],
 [],
 [],[]]).


% FUNCION DE REGISTRO
verificar([Username|_],Username).%verifica si es el nombre

agregarUsuario([Usuario],Username,Pass,[Usuario|A]):-not(verificar(Usuario,Username)),usuario(Username,Pass,A).
agregarUsuario([Primer|Siguientes],Username,Pass,[Primer|ColaNueva]):-
    not(verificar(Primer,Username)),agregarUsuario(Siguientes,Username,Pass,ColaNueva).

register([A,B,C],Username,Pass,[P,B,C]):-agregarUsuario(A,Username,Pass,P).

%  FUNCION LOGIN
   login([A,B,C,_],Username,Pass,[A,B,C,E]):-autenticar(A,Username,Pass,E).
   login([],_,_,[]).


   autenticar([Usuario],Username,Pass,P):-verificarLogin(Usuario,Username,Pass,P).
   autenticar([Primer|Siguientes],Username,Pass,Activo):-
   verificarLogin(Primer,Username,Pass,Activo);autenticar(Siguientes,Username,Pass,Activo).

   verificarLogin([Username,Pass|Cola],Username,Pass,[Username,Pass|Cola]).

%  .FUNCION ASK
%
ask([],_,_,_,[]).
ask([_,_,_,D],_,_,_,[]):-D is[],fail.
%
ask([Usuarios,Preguntas,Respuestas,UsuarioActivo],Fecha,TextoPregunta,ListaEtiquetas,[Usuarios,NuevasPreguntas,Respuestas,[]]):-
    contador(Preguntas,ID),pregunta(ID,UsuarioActivo,Fecha,TextoPregunta,ListaEtiquetas,PreguntaNueva),
    agregarPregunta(Preguntas,PreguntaNueva,NuevasPreguntas).

    contador([],1).
    contador([_|Siguientes],ID):-contador(Siguientes,IDanterior),ID is IDanterior +1.




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
%
%
%
%
%
%



















