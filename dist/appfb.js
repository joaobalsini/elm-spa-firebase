'use strict';

var app = firebase.initializeApp(config);
var database = app.database();

var UNITSPATH = "units"
var MATERIALSPATH = "materials"

function addUnit(unit){
  var promise = database
    .ref(UNITSPATH)
    .push(unit);
  return promise;
}

function updateUnit(unit){
  var id = unit.id;
  var promise = database
    .ref(UNITSPATH + "/" + id)
    .set(unit);
  return promise;
}

function deleteUnit(unit){
  var id = unit.id;
  var promise = database
    .ref(UNITSPATH + "/" + id)
    .remove();
  return promise;
}

function unitListener(){
  return database.ref(UNITSPATH);
}

function addMaterial(material){
  var promise = database
    .ref(MATERIALSPATH)
    .push(material);
  return promise;
}

function updateMaterial(material){
  var id = material.id;
  var promise = database
    .ref(MATERIALSPATH + "/" + id)
    .set(material);
  return promise;
}

function deleteMaterial(material){
  var id = material.id;
  var promise = database
    .ref(MATERIALSPATH + "/" + id)
    .remove();
  return promise;
}

function materialListener(){
  return database.ref(MATERIALSPATH);
}
