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
%[username,pass]


% TDA Pregunta
% [ID,estado,[IDs respuestas],Autor,Fecha,Pregunta,[Etiquetas]
%  0 = cerrada    1 = abierta
% Constructor
pregunta(ID,[Nombre|_],Fecha,Pregunta,Etiquetas,[ID,1,[[]],Nombre,Fecha,Pregunta,Etiquetas]).

% TDA Respuesta
% (IDRespuesta,estado,IDPregunta,Fecha,Respuesta,[Etiquetas])
% 0 = no aceptada   1 = aceptada
% Constructor
respuesta(IDRespuesta,[Username|_],IDPregunta,Fecha,Respuesta,Etiquetas,
          [IDRespuesta,0,Username,IDPregunta,Fecha,Respuesta,Etiquetas]).
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
         Respuesta),agregarRespuesta(Respuesta,Respuestas,RespuestasNuevas).

verificarID([],_):-!,fail.
verificarID([[ID|_]|_],ID).
verificarID([_|Cola],ID):-verificarID(Cola,ID).

agregarRespuesta(X,[],[X]).
agregarRespuesta(X,[H|C],[X,H|C]).

%  FUNCION PRINCIPAL ACCEPT
accept([Usuarios,Preguntas,Respuestas,[]],_,_,[Usuarios,Preguntas,Respuestas,[]]):-!,fail.
accept([Usuarios,Preguntas,Respuestas,UsuarioActivo],IDPregunta,IDRespuesta,
       [Usuarios,PreguntasVerificadas,Respuestas,[]]):-
    verificarIDR(Respuestas,IDRespuesta),
    agregarIdRespuesta(Preguntas,IDPregunta,IDRespuesta,UsuarioActivo,PreguntasVerificadas).

    agregarIdRespuesta([],_,_,[]):-!,fail.
    agregarIdRespuesta([[IDPregunta,E,[H|C],Usuario|T]|Cola],IDPregunta,IDRespuesta,[Usuario|_],[[IDPregunta,E,[IDRespuesta,H|C]|T]|Cola]).
    agregarIdRespuesta([Primero|Cola],IDPregunta,IDRespuesta,[Usuario|Pass],[Primero|Cn]):-
    agregarIdRespuesta(Cola,IDPregunta,IDRespuesta,[Usuario,Pass],Cn).

    verificarIDR([],_):-!,fail.
    verificarIDR([[ID|_]|_],ID).
    verificarIDR([_|Cola],ID):-verificarIDR(Cola,ID).


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



















