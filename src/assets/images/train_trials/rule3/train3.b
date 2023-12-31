% eastbound(A) :- attended(A), in_front(A,B).

:- set(i,2).
:- set(verbose,2).
:- set(minpos, 2).
:- set(caching,True).
:- set(cache_clauselength,5).
:- set(clauselength,4).
:- set(good,true).
:- set(goodfile,"components/all_dynamic_files/good_clauses.txt").

:- discontiguous attended/1.
:- discontiguous load/3.
:- discontiguous short/1.
:- discontiguous closed/1.
:- discontiguous long/1.
:- discontiguous open/1.
:- discontiguous double/1.
:- discontiguous jagged/1.
:- discontiguous shape/2.
:- discontiguous wheels/2.
:- discontiguous has_car/2.
:- discontiguous nth/2.
:- discontiguous in_front/2.

:- modeh(1,eastbound(+train)).
:- modeb(1,load(+car,#shape,#int)).
:- modeb(1,short(+car)).
:- modeb(1,closed(+car)).
:- modeb(1,long(+car)).
:- modeb(1,open(+car)).
:- modeb(1,double(+car)).
:- modeb(1,jagged(+car)).
:- modeb(1,attended(+car)).
:- modeb(1,shape(+car,#shape)).
:- modeb(1,wheels(+car,#int)).
:- modeb(1, nth_car(+car,#int)).
:- modeb(1, in_front(+car,-car)).
:- modeb(*,has_car(+train,-car)).

:- determination(eastbound/1,load/3).
:- determination(eastbound/1,attended/1).
:- determination(eastbound/1,short/1).
:- determination(eastbound/1,closed/1).
:- determination(eastbound/1,long/1).
:- determination(eastbound/1,open/1).
:- determination(eastbound/1,double/1).
:- determination(eastbound/1,jagged/1).
:- determination(eastbound/1,shape/2).
:- determination(eastbound/1,wheels/2).
:- determination(eastbound/1,has_car/2).
:- determination(eastbound/1,nth_car/2).
:- determination(eastbound/1,in_front/2).

% type definitions
car(car_1).

shape(elipse).  shape(hexagon).  shape(rectangle).  shape(u_shaped).
shape(triangle). shape(circle). shape(nil). shape(bucket).

train(east1).  train(east2).  train(east3).  train(east4).  train(east5).
train(west6).  train(west7).  train(west8).  train(west9).  train(west10).

% Train 1
has_car(east1, car_1).
shape(car_1, bucket).
short(car_1).
load(car_1,rectangle,2).
open(car_1).

has_car(east1, car_2).
short(car_2).
shape(car_2, rectangle).
load(car_2, circle,1).
open(car_2).

has_car(east1, car_3).
short(car_3).
shape(car_3, u_shaped).
load(car_3, circle, 1).
open(car_3).

% Train 2
has_car(east2, car_4).
shape(car_4, elipse).
short(car_4).
load(car_4,triangle,1).
closed(car_4).

has_car(east2, car_5).
short(car_5).
shape(car_5, bucket).
load(car_5, circle,1).
closed(car_5).

has_car(east2, car_6).
short(car_6).
shape(car_6, u_shaped).
load(car_6, circle, 1).
open(car_6).

has_car(east2, car_7).
long(car_7).
shape(car_7, rectangle).
load(car_7, hexagon, 1).
closed(car_7).

% Train 3
has_car(east3, car_8).
shape(car_8, rectangle).
short(car_8).
load(car_8,triangle,1).
open(car_8).

has_car(east3, car_9).
short(car_9).
shape(car_9, hexagon).
load(car_9, diamond,1).
closed(car_9).

% Train 5!!
has_car(east5, car_10).
shape(car_10, u_shaped).
short(car_10).
load(car_10,triangle,1).
open(car_10).

has_car(east5, car_11).
short(car_11).
shape(car_11, bucket).
load(car_11, circle,1).
open(car_11).

has_car(east5, car_12).
short(car_12).
shape(car_12, u_shaped).
load(car_12, rectangle,1).
open(car_12).

% Train 6
has_car(east6, car_13).
shape(car_13, rectangle).
long(car_13).
load(car_13,rectangle,1).
open(car_13).

has_car(east6, car_15).
long(car_15).
shape(car_15, rectangle).
load(car_15, circle,1).
open(car_15).

has_car(east6, car_14).
long(car_14).
shape(car_14, rectangle).
load(car_14, hexagon,1).
open(car_14).

% Train 9
has_car(east9, car_16).
shape(car_16, rectangle).
long(car_16).
load(car_16,circle,1).
open(car_16).

has_car(east9, car_17).
long(car_17).
shape(car_17, rectangle).
load(car_17, rectangle,1).
closed(car_17).




