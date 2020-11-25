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
%[username,pass]


% TDA Pregunta
% [ID ,Autor,Fecha,Pregunta,[Etiquetas][IDs respuesta]]
% Constructor
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,Nombre,Fecha,Pregunta,Etiquetas,[]]).

% TDA Respuesta
% (IDRespuesta ,IDPregunta,Fecha,Respuesta,[Etiquetas])
% Constructor
respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,Username,IDPregunta,Fecha,Respuesta,Etiquetas]).
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

stack3([[["user1","pass1"],["user2","pass2"],["user3","pass2"]],
 [],
 [],[]]).


% FUNCION DE REGISTRO
agregarUsuario([],Username,Pass,[[Username,Pass]]).
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
    agregarPregunta(Preguntas,PreguntaNueva,NuevasPreguntas).

    contador([],1).
    contador([_|Siguientes],ID):-contador(Siguientes,IDanterior),ID is IDanterior +1.

% FUNCION ANSWER
answer([Usuarios,Preguntas,Respuestas,[]],_,_,_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.

answer([Usuarios,Preguntas,Respuestas,UsuarioActivo], Fecha, IDPregunta, TextoRespuesta,
ListaEtiquetas,[Usuarios,Preguntas,RespuestasNuevas,[]]):-
    contador(Respuestas,IDRespuesta),verificarID(Preguntas,IDPregunta),
respuesta(IDRespuesta,UsuarioActivo,IDPregunta,Fecha,TextoRespuesta,ListaEtiquetas,
         Respuesta).agregarRespuesta(Respuesta,Respuestas,RespuestasNuevas).

verificarID([],_):-!,fail.
verificarID([[ID|_]|_],ID).
verificarID([_|Cola],ID):-verificarID(Cola,ID).




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



















