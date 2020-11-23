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
% (ID ,Autor,Fecha,Pregunta,[Etiquetas])
% Constructor
pregunta(ID,Autor,Fecha,Pregunta,Etiquetas,[ID,Autor,Fecha,Pregunta,Etiquetas]).

% TDA Respuesta
% (IDRespuesta ,IDPregunta,Fecha,Respuesta,[Etiquetas])
% Constructor
respuesta(IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,IDPregunta,Fecha,Respuesta,Etiquetas]).

% TDA usuarioActivo
% agregar usuario
agregarUsuarioActivo([],Usuario,[Usuario]).
agregarUsuarioActivo([PLista|Clista],Usuario,[PLista|ColaNueva]):-
    agregarUsuarioActivo(Clista,Usuario,ColaNueva).

agregarUsuarioActivo2([A,B,C],Usuario,[A,B,C,Usuario]).
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

% FUNCION DE REGISTRO
verificar([Username|_],Username).

%agregarUsuario([],Username,Pass,A):-usuario(Username,Pass,A).
agregarUsuario([Usuario],Username,Pass,[Usuario|A]):-not(verificar(Usuario,Username)),usuario(Username,Pass,A).
agregarUsuario([Primer|Siguientes],Username,Pass,[Primer|ColaNueva]):-
    not(verificar(Primer,Username)),agregarUsuario(Siguientes,Username,Pass,ColaNueva).

register([A,B,C],Username,Pass,[P,B,C]):-agregarUsuario(A,Username,Pass,P).
%
%
%
%



















