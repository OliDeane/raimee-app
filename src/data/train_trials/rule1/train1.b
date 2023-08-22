:- set(i,2).
:- set(verbose,2).
:- set(minpos, 1).
:- set(noise, 3).
:- set(caching,True).
%:- set(cache_clauselength,5).
:- set(clauselength,10).
:- set(good,true).
:- set(goodfile,"src/components/rule_induction/dynamic_files/good_clauses.txt").

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
:- discontiguous nth_car/2.
:- discontiguous in_front/2.
:- discontiguous not_double/1.

:- modeh(1,eastbound(+train)).
:- modeb(1,load(+car,#shape,#int)).
:- modeb(1,short(+car)).
:- modeb(1,closed(+car)).
:- modeb(1,long(+car)).
:- modeb(1,open(+car)).
:- modeb(1,double(+car)).
:- modeb(1,not_double(+car)).
:- modeb(1,jagged(+car)).
:- modeb(1,attended(+car)).
:- modeb(1,shape(+car,#shape)).
:- modeb(1,wheels(+car,#int)).
:- modeb(1,nth_car(+car,#int)).
:- modeb(1,in_front(+car,-car)).
:- modeb(*,has_car(+train,-car)).

:- determination(eastbound/1,load/3).
:- determination(eastbound/1,attended/1).
:- determination(eastbound/1,short/1).
:- determination(eastbound/1,closed/1).
:- determination(eastbound/1,long/1).
:- determination(eastbound/1,open/1).
:- determination(eastbound/1,double/1).
:- determination(eastbound/1,not_double/1).
:- determination(eastbound/1,jagged/1).
:- determination(eastbound/1,shape/2).
:- determination(eastbound/1,wheels/2).
:- determination(eastbound/1,has_car/2).
:- determination(eastbound/1,nth_car/2).
:- determination(eastbound/1,in_front/2).

% type definitions

shape(elipse).  shape(hexagon).  shape(rectangle).  shape(u_shaped).
shape(triangle). shape(circle). shape(nil). shape(bucket).

train(train1).  train(train2).  train(train3).  train(train4).  train(train5).
train(train6).  train(train7).  train(train8).  train(train9).  train(train10).

has_car(train1,car_11).
nth_car(car_11,1).
shape(car_11,rectangle).
short(car_11).
not_double(car_11).
load(car_11,triangle,1).
rooftype(car_11,peaked).
closed(car_11).

has_car(train1,car_12).
nth_car(car_12,2).
shape(car_12,bucket).
short(car_12).
not_double(car_12).
load(car_12,triangle,1).
open(car_12).

has_car(train2,car_21).
nth_car(car_21,1).
shape(car_21,hexagon).
short(car_21).
not_double(car_21).
load(car_21,circle,1).
rooftype(car_21,flat).
closed(car_21).

has_car(train2,car_22).
nth_car(car_22,2).
shape(car_22,ellipse).
short(car_22).
not_double(car_22).
load(car_22,triangle,1).
rooftype(car_22,arc).
closed(car_22).

has_car(train2,car_23).
nth_car(car_23,3).
shape(car_23,bucket).
short(car_23).
not_double(car_23).
load(car_23,circle,1).
open(car_23).

has_car(train3,car_31).
nth_car(car_31,1).
shape(car_31,bucket).
short(car_31).
not_double(car_31).
load(car_31,circle,1).
rooftype(car_31,flat).
closed(car_31).

has_car(train3,car_32).
nth_car(car_32,2).
shape(car_32,hexagon).
short(car_32).
not_double(car_32).
load(car_32,triangle,1).
rooftype(car_32,flat).
closed(car_32).

has_car(train3,car_33).
nth_car(car_33,3).
shape(car_33,rectangle).
short(car_33).
double(car_33).
load(car_33,triangle,1).
open(car_33).

has_car(train3,car_34).
nth_car(car_34,4).
shape(car_34,rectangle).
short(car_34).
double(car_34).
load(car_34,triangle,1).
open(car_34).

has_car(train4,car_41).
nth_car(car_41,1).
shape(car_41,u_shaped).
short(car_41).
not_double(car_41).
load(car_41,triangle,1).
open(car_41).

has_car(train4,car_42).
nth_car(car_42,2).
shape(car_42,u_shaped).
short(car_42).
not_double(car_42).
load(car_42,circle,1).
open(car_42).

has_car(train4,car_43).
nth_car(car_43,3).
shape(car_43,rectangle).
short(car_43).
not_double(car_43).
load(car_43,circle,1).
open(car_43).

has_car(train4,car_44).
nth_car(car_44,4).
shape(car_44,bucket).
short(car_44).
not_double(car_44).
load(car_44,triangle,1).
open(car_44).

has_car(train5,car_51).
nth_car(car_51,1).
shape(car_51,ellipse).
short(car_51).
not_double(car_51).
load(car_51,triangle,1).
rooftype(car_51,arc).
closed(car_51).

has_car(train5,car_52).
nth_car(car_52,2).
shape(car_52,bucket).
short(car_52).
not_double(car_52).
load(car_52,circle,1).
rooftype(car_52,peaked).
closed(car_52).

has_car(train5,car_53).
nth_car(car_53,3).
shape(car_53,u_shaped).
short(car_53).
not_double(car_53).
load(car_53,circle,1).
open(car_53).

has_car(train5,car_54).
nth_car(car_54,4).
shape(car_54,rectangle).
long(car_54).
not_double(car_54).
load(car_54,hexagon,1).
rooftype(car_54,flat).
closed(car_54).

has_car(train6,car_61).
nth_car(car_61,1).
shape(car_61,rectangle).
long(car_61).
not_double(car_61).
load(car_61,rectangle,0).
rooftype(car_61,flat).
closed(car_61).

has_car(train6,car_62).
nth_car(car_62,2).
shape(car_62,u_shaped).
short(car_62).
not_double(car_62).
load(car_62,circle,1).
open(car_62).

has_car(train6,car_63).
nth_car(car_63,3).
shape(car_63,bucket).
short(car_63).
not_double(car_63).
load(car_63,circle,1).
open(car_63).

has_car(train6,car_64).
nth_car(car_64,4).
shape(car_64,rectangle).
long(car_64).
not_double(car_64).
load(car_64,circle,3).
open(car_64).

has_car(train7,car_71).
nth_car(car_71,1).
shape(car_71,rectangle).
long(car_71).
not_double(car_71).
load(car_71,rectangle,3).
rooftype(car_71,flat).
closed(car_71).

has_car(train7,car_72).
nth_car(car_72,2).
shape(car_72,rectangle).
long(car_72).
not_double(car_72).
load(car_72,rectangle,1).
open(car_72).

has_car(train7,car_73).
nth_car(car_73,3).
shape(car_73,rectangle).
short(car_73).
not_double(car_73).
load(car_73,triangle,1).
open(car_73).

has_car(train8,car_81).
nth_car(car_81,1).
shape(car_81,rectangle).
short(car_81).
not_double(car_81).
load(car_81,circle,1).
open(car_81).

has_car(train8,car_82).
nth_car(car_82,2).
shape(car_82,u_shaped).
short(car_82).
not_double(car_82).
load(car_82,circle,1).
open(car_82).

has_car(train8,car_83).
nth_car(car_83,3).
shape(car_83,hexagon).
short(car_83).
not_double(car_83).
load(car_83,circle,1).
rooftype(car_83,flat).
closed(car_83).

has_car(train9,car_91).
nth_car(car_91,1).
shape(car_91,ellipse).
short(car_91).
not_double(car_91).
load(car_91,triangle,1).
rooftype(car_91,arc).
closed(car_91).

has_car(train9,car_92).
nth_car(car_92,2).
shape(car_92,rectangle).
long(car_92).
not_double(car_92).
load(car_92,rectangle,1).
rooftype(car_92,flat).
closed(car_92).

has_car(train10,car_101).
nth_car(car_101,1).
shape(car_101,bucket).
short(car_101).
not_double(car_101).
load(car_101,rectangle,1).
open(car_101).

has_car(train10,car_102).
nth_car(car_102,2).
shape(car_102,rectangle).
short(car_102).
not_double(car_102).
load(car_102,circle,1).
open(car_102).

has_car(train10,car_103).
nth_car(car_103,3).
shape(car_103,bucket).
short(car_103).
not_double(car_103).
load(car_103,rectangle,1).
rooftype(car_103,flat).
closed(car_103).

has_car(train10,car_104).
nth_car(car_104,4).
shape(car_104,bucket).
short(car_104).
not_double(car_104).
load(car_104,circle,1).
rooftype(car_104,peaked).
closed(car_104).

