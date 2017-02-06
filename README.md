This app is test and a initial guide to implementing a single page app using Elm 0.18 and Firebase.

In this REPO I will create a basic app trying to predict several use cases for a common app.
- Login and Logout
- Role based user authorizations
- CRUD operations using Firebase
- Validations (local and database - like checking uniqueness)
- Showing proper notifications errors
- Belongs_to and Has_many relations


Everything done by myself in pure elm, just used semantic ui because I like beautiful things.

But, first things first. How can you use and see whats happening here?

1) clone this repo (and install elm if not installed).
2) npm install
3) elm-package install
4) create a firebaseConfig.js file dist/ folder. In this file you will need to add something like this:

'use strict';

// Initialize Firebase
var config = {
  apiKey: "apiKey",
  authDomain: "app_name.firebaseapp.com",
  databaseURL: "https://app_name.firebaseio.com",
  storageBucket: "app_name.appspot.com",
  messagingSenderId: "senderId"
};

To have this you will need to go to the firebase page, create a new project, enter the project and click "Add firebase to your web app" (or something like that)

5) gulp
6) Open chrome and go to localhost:4000
