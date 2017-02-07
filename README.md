# This app is test and a initial guide to implementing a single page app using Elm 0.18 and Firebase

**In this REPO I will create a basic app trying to predict several use cases for a common app. Everything done by myself in pure elm, just used semantic ui because I like beautiful things.**

**Task list**
- [x] Routing for single page app
- [x] Basic CRUD operations with firebase
- [x] Showing basic notifications
- [ ] Login and Logout
- [ ] Basic Validations (offline)
- [ ] Belongs_to and Has_many relations
- [ ] Advanced Validations (like name uniqueness, cannot delete if belongs_to something)
- [ ] Showing advanced notification errors
- [ ] Role based user authorizations



**But, first things first. How can you use and see whats happening here?**

- clone this repo (and install elm if not installed).
- npm install
- elm-package install
- Create firebase account and get firebase codes. To have this you will need to go to the firebase page, create a new project, enter the project and click "Add firebase to your web app" (or something like that), and then go to 5.

- create a firebaseConfig.js file dist/ folder. In this file you will need to add something like this:

```
'use strict';

// Initialize Firebase
var config = {
  apiKey: "apiKey",
  authDomain: "app_name.firebaseapp.com",
  databaseURL: "https://app_name.firebaseio.com",
  storageBucket: "app_name.appspot.com",
  messagingSenderId: "senderId"
};
```

- gulp (default task does everything one needs)
- Open chrome and go to localhost:4000
